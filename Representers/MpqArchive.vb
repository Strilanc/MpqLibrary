<Serializable()>
    Public Class MPQException
    Inherits Exception
    Public Sub New(ByVal message As String, Optional ByVal innerException As Exception = Nothing)
        MyBase.New(message, innerException)
    End Sub
End Class

Public Enum MpqLanguageId As UInteger
    Neutral = 0
    Chinese = &H404
    Czech = &H405
    German = &H407
    English = &H409
    Spanish = &H40A
    French = &H40C
    Italian = &H410
    Japanese = &H411
    Korean = &H412
    Dutch = &H413
    Polish = &H415
    Portuguese = &H416
    Russsian = &H419
    EnglishUK = &H809
End Enum
Public Enum DecoratoratedFileIndex As UInteger
    NoFile = &HFFFFFFFFUI
    DeletedFile = &HFFFFFFFEL
End Enum
Public Enum FileFlags As UInteger
    Imploded = 1UI << 8
    Compressed = 1UI << 9
    Encrypted = 1UI << 16
    AdjustedKey = 1UI << 17
    Continuous = 1UI << 24
    Exists = 1UI << 31
End Enum

''' <summary>
''' Opens MPQ files used by Blizzard and others.
''' </summary>
''' 
''' <copyright>
''' Copyright (C) 2008 Craig Gidney, craig.gidney@gmail.com
'''
''' This source was adepted from the C version of mpqlib.
''' The C version belongs to the following authors,
'''
''' Maik Broemme, mbroemme@plusserver.de
''' 
''' This program is free software; you can redistribute it and/or modify
''' it under the terms of the GNU General Public License as published by
''' the Free Software Foundation; either version 2 of the License, or
''' (at your option) any later version.
'''
''' This program is distributed in the hope that it will be useful,
''' but WITHOUT ANY WARRANTY; without even the implied warranty of
''' MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
''' GNU General Public License for more details.
'''
''' You should have received a copy of the GNU General Public License
''' along with this program; if not, write to the Free Software
''' Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
''' </copyright>
''' 
''' <remarks>
''' Targetted at warcraft 3 maps, may not work for other MPQs.
''' </remarks>
Public Class MpqArchive
    Public Const ID_MPQ As UInteger = &H1A51504D 'MPQ\x1A

    Public ReadOnly streamFactory As Func(Of IO.Stream)
    Public ReadOnly hashTable As MpqHashTable 'Map from hashes filesnames to file table indexes
    Public ReadOnly fileTable As MpqFileTable 'Stores the position, size, and other information about all files in the archive

    Public archiveSize As UInteger 'in bytes
    Public ReadOnly hashTablePosition As UInteger 'Absolute position within the parent file
    Public ReadOnly fileTablePosition As UInteger 'Absolute position within the parent file
    Public ReadOnly numHashTableEntries As UInteger 'Number of entries
    Public ReadOnly numFileTableEntries As UInteger 'Number of entries
    Public ReadOnly fileBlockSize As UInteger 'Size of the blocks files in the archive are divided into
    Public ReadOnly archivePosition As UInteger 'Position of MPQ archive in the file

    Public Sub New(ByVal path As String,
                   Optional ByVal access As IO.FileAccess = IO.FileAccess.Read,
                   Optional ByVal share As IO.FileShare = IO.FileShare.Read)
        Me.New(Function() New IO.FileStream(path, IO.FileMode.Open, access, share))
    End Sub

    Public Sub New(ByVal streamFactory As Func(Of IO.Stream))
        Contract.Requires(streamFactory IsNot Nothing)
        Me.streamFactory = streamFactory
        Using stream = streamFactory()
            If stream Is Nothing Then Throw New ArgumentException("streamFactory returned a null stream.")
            Using br = New IO.BinaryReader(stream)
                'Find valid header
                Dim found = False
                For Me.archivePosition = 0 To CUInt(stream.Length - 1) Step 512
                    stream.Seek(archivePosition, IO.SeekOrigin.Begin)

                    Dim id = br.ReadUInt32()
                    Dim headerSize = br.ReadUInt32()
                    archiveSize = br.ReadUInt32()
                    br.ReadUInt16() 'format version
                    fileBlockSize = br.ReadUInt16() 'order of block size relative to &H200 (corrected later)
                    hashTablePosition = br.ReadUInt32() 'relative to position of archive (corrected later)
                    fileTablePosition = br.ReadUInt32() 'relative to position of archive (corrected later)
                    numHashTableEntries = br.ReadUInt32()
                    numFileTableEntries = br.ReadUInt32()
                    If numFileTableEntries > numHashTableEntries Then numFileTableEntries = numHashTableEntries

                    'Protected MPQs mess with values
                    If archiveSize = 0 Then
                        archiveSize = CUInt(stream.Length) - archivePosition
                    End If

                    'Check for invalid signature
                    If id <> ID_MPQ Then Continue For
                    If hashTablePosition >= archiveSize Then Continue For
                    If fileTablePosition >= archiveSize Then Continue For

                    'Valid signature!
                    found = True
                    Exit For
                Next archivePosition

                If Not found Then Throw New MPQException("MPQ archive header not found.")

                stream.Close()

                'Correct values
                fileBlockSize = 512UI << CInt(fileBlockSize) 'correct size to actual size in bytes
                hashTablePosition += archivePosition 'correct position from relative to absolute
                fileTablePosition += archivePosition 'correct position from relative to absolute

                'Load tables
                hashTable = New MpqHashTable(Me)
                fileTable = New MpqFileTable(Me)
            End Using
        End Using
    End Sub

    <Pure()>
    Public Function OpenFile(ByVal fileIndex As UInteger) As IO.Stream
        Contract.Requires(fileIndex >= 0)
        Contract.Ensures(Contract.Result(Of IO.Stream)() IsNot Nothing)
        If fileIndex >= fileTable.fileEntries.Count Then Throw New IO.IOException("File ID not in file table")
        Return New MpqFileStream(Me, fileTable.fileEntries(CInt(fileIndex)))
    End Function
    <Pure()>
    Public Function OpenFile(ByVal filename As String) As IO.Stream
        Contract.Requires(filename IsNot Nothing)
        Contract.Ensures(Contract.Result(Of IO.Stream)() IsNot Nothing)
        Dim fileIndex = hashTable.hash(filename).fileIndex
        If fileIndex >= fileTable.fileEntries.Count Then Throw New IO.IOException("File ID not in file table")
        Return New MpqFileStream(Me, fileTable.fileEntries(CInt(fileIndex)), filename)
    End Function

    Public Sub RepackInto(ByVal stream As IO.Stream)
        Contract.Requires(stream IsNot Nothing)
        Contract.Requires(stream.CanWrite)
        Contract.Requires(stream.CanSeek)

        'keep only valid file entries
        Dim validFiles = New List(Of Mpq.MpqFileTable.FileEntry)
        Dim validFileData = New List(Of IO.Stream)
        Dim idMap As New Dictionary(Of UInteger, UInteger)
        Dim id = 0UI
        For i = 0UI To CUInt(fileTable.fileEntries.Count - 1)
            Dim i_ = i
            If (From hash In hashTable.hashes
                        Where hash.Exists() _
                        AndAlso hash.fileIndex = i_).None Then
                Continue For
            End If

            validFileData.Add(New IO.BufferedStream(OpenFile(i)))
            validFiles.Add(fileTable.fileEntries(CInt(i)))
            idMap(i) = id
            id += 1UI
        Next i

        'Write containing file header
        Dim bf = New IO.BufferedStream(stream)
        Dim bw = New IO.BinaryWriter(bf)
        Using fileData = New IO.BufferedStream(streamFactory())
            For i = 0 To archivePosition - 1
                bf.WriteByte(CByte(fileData.ReadByte()))
            Next i
        End Using

        'Store positions
        Dim archiveHeaderPosition = bf.Position()
        Dim fileTablePosition = archiveHeaderPosition + 32
        Dim hashTablePosition = fileTablePosition + 16 * validFiles.Count
        Dim dataPosition = hashTablePosition + 16 * hashTable.hashes.Count

        'Write file data
        bf.Position = dataPosition
        For i = 0 To validFileData.Count - 1
            Dim xx As New MpqFileTable.FileEntry()
            xx.filePosition = CUInt(bf.Position)
            xx.actualSize = CUInt(validFileData(i).Length)

            bf.WriteByte(Mpq.MpqFileStream.CompressionFlags.ZLibDeflate)
            Using out = New IO.BufferedStream(New ZLibStream(bf, IO.Compression.CompressionMode.Compress, True))
                For j = 0 To validFileData(i).Length - 1
                    out.WriteByte(CByte(validFileData(i).ReadByte))
                Next j
            End Using

            xx.compressedSize = CUInt(bf.Position) - xx.filePosition
            xx.flags = FileFlags.Continuous Or FileFlags.Compressed Or FileFlags.Exists
            validFiles(i) = xx
            validFileData(i).Close()
        Next i
        Dim endPos = bf.Position

        'Write file table
        bf.Position = fileTablePosition
        With New IO.BinaryWriter(
                New MpqStreamEncrypter(HashFilenameUsing("(block table)", CryptTableIndex.CypherKeyHash)).
                    ConvertWriteOnlyStream(bf))
            For Each f In validFiles
                .Write(CUInt(f.filePosition - archiveHeaderPosition))
                .Write(f.compressedSize)
                .Write(f.actualSize)
                .Write(f.flags)
            Next f
            .Flush()
        End With

        'Write hash table
        bf.Position = hashTablePosition
        With New IO.BinaryWriter(
                New MpqStreamEncrypter(HashFilenameUsing("(hash table)", CryptTableIndex.CypherKeyHash)).
                    ConvertWriteOnlyStream(bf))
            idMap(DecoratoratedFileIndex.NoFile) = DecoratoratedFileIndex.NoFile
            idMap(DecoratoratedFileIndex.DeletedFile) = DecoratoratedFileIndex.DeletedFile
            For Each h In hashTable.hashes
                If Not h.Exists OrElse Not idMap.ContainsKey(h.fileIndex) Then
                    If h.fileIndex <> DecoratoratedFileIndex.NoFile Then
                        h = New MpqHashTable.HashEntry
                        h.fileIndex = DecoratoratedFileIndex.DeletedFile
                        h.key = 0
                        h.language = 0
                    End If
                End If
                .Write(h.key)
                .Write(h.language)
                .Write(idMap(h.fileIndex))
            Next h
            .Flush()
        End With

        'write header
        bf.Position = archiveHeaderPosition
        Dim w = New IO.BinaryWriter(bf)
        w.Write(MpqArchive.ID_MPQ)
        w.Write(32UI) 'header size
        w.Write(CUInt(endPos - archiveHeaderPosition))
        w.Write(21536US) 'format version
        w.Write(CShort(Math.Log(Me.fileBlockSize \ &H200, 2)))
        w.Write(CUInt(hashTablePosition - archiveHeaderPosition))
        w.Write(CUInt(fileTablePosition - archiveHeaderPosition))
        w.Write(Me.numHashTableEntries)
        w.Write(validFileData.Count)
        bf.Flush()
    End Sub

    Public Sub MarkFileAsAddedAndAppendData(ByVal filename As String, ByVal data As IO.Stream)
        Contract.Requires(filename IsNot Nothing)
        Contract.Requires(data IsNot Nothing)
        Contract.Requires(Not hashTable.contains(filename))
        Contract.Ensures(hashTable.contains(filename))

        Dim n = 0UI
        Dim f = New MpqFileTable.FileEntry

        Using bf = New IO.BufferedStream(Me.streamFactory())
            bf.Position = bf.Length
            f.filePosition = CUInt(bf.Position)

            bf.WriteByte(Mpq.MpqFileStream.CompressionFlags.ZLibDeflate)
            Using out = New ZLibStream(bf, IO.Compression.CompressionMode.Compress)
                Do
                    Dim i = data.ReadByte()
                    If i = -1 Then Exit Do
                    out.WriteByte(CByte(i))
                    n += 1UI
                Loop
                out.Flush()

                f.compressedSize = CUInt(bf.Position - f.filePosition)
            End Using
        End Using

        archiveSize += n

        f.flags = FileFlags.Continuous Or FileFlags.Compressed Or FileFlags.Exists
        f.actualSize = n
        fileTable.fileEntries.Add(f)

        Dim h = hashTable.getEmpty(filename)
        h.fileIndex = CType(fileTable.fileEntries.Count - 1, Mpq.DecoratoratedFileIndex)
        h.language = MpqLanguageId.Neutral
        h.key = Mpq.Common.HashFileName(filename)
    End Sub

    Public Sub MarkFileAsRemoved(ByVal filename As String)
        Contract.Requires(filename IsNot Nothing)
        Contract.Ensures(Not hashTable.contains(filename))

        If Not hashTable.contains(filename) Then Return
        hashTable.hash(filename).fileIndex = DecoratoratedFileIndex.DeletedFile
    End Sub
End Class
