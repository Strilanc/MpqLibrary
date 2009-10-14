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

Public Enum LanguageId As UInteger
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
    Russian = &H419
    EnglishUK = &H809
End Enum

''' <summary>
''' Opens MPQ files used by Blizzard and others.
''' </summary>
''' <remarks>
''' Targetted at warcraft 3 maps, may not work for other MPQs.
''' </remarks>
Public Class Archive
    Private Const MagicHeaderValue As UInteger = &H1A51504D 'MPQ\x1A

    Friend ReadOnly Position As UInteger
    Friend ReadOnly Size As UInteger

    Public ReadOnly Hashtable As Hashtable 'Map from hashes filesnames to file table indexes
    Public ReadOnly BlockTable As BlockTable 'Stores the position, size, and other information about all files in the archive

    Friend ReadOnly streamFactory As Func(Of IO.Stream)
    Friend ReadOnly FileChunkSize As UInteger 'Size of the chunks files in the archive are divided into

    Public Sub New(ByVal fileName As String)
        Contract.Requires(fileName IsNot Nothing)
        Me.streamFactory = Function() New IO.FileStream(fileName, IO.FileMode.Open, IO.FileAccess.Read, IO.FileShare.Read)
        Dim stream = streamFactory()
        Using reader = New IO.BinaryReader(stream)
            Dim hashtableSize As UInteger
            Dim blockTableSize As UInteger
            Dim hashtableOffset As UInteger
            Dim blockTableOffset As UInteger

            'Find valid header
            Dim found = False
            For Me.Position = 0 To CUInt(stream.Length - 1) Step 512
                stream.Seek(Position, IO.SeekOrigin.Begin)

                Dim fileMagicValue = reader.ReadUInt32()
                Dim headerSize = reader.ReadUInt32()
                Me.Size = reader.ReadUInt32()
                reader.ReadUInt16() 'format version
                FileChunkSize = reader.ReadUInt16() 'order of block size relative to &H200 (corrected later)
                hashtableOffset = reader.ReadUInt32() 'relative to position of archive (corrected later)
                blockTableOffset = reader.ReadUInt32() 'relative to position of archive (corrected later)
                hashtableSize = reader.ReadUInt32()
                blockTableSize = reader.ReadUInt32()

                'Protected MPQs mess with values
                If Me.Size = 0 Then
                    Me.Size = CUInt(stream.Length) - Position
                End If

                'Check for invalid signature
                If fileMagicValue <> MagicHeaderValue Then Continue For
                If CULng(hashtableOffset) + CULng(hashtableSize) >= Size Then Continue For
                If blockTableOffset >= Size Then Continue For

                found = True
                Exit For
            Next Me.Position

            If Not found Then Throw New IO.InvalidDataException("MPQ archive header not found.")

            'Correct values
            If blockTableOffset + blockTableSize * 16 > Me.Size Then
                blockTableSize = (Me.Size - blockTableOffset) \ 16UI
            End If
            Me.FileChunkSize = 512UI << CInt(Me.FileChunkSize)

            'Load tables
            BlockTable = New BlockTable(stream, Me.Position + blockTableOffset, blockTableSize)
            Hashtable = New Hashtable(stream, Me.Position + hashtableOffset, hashtableSize, CUInt(BlockTable.Size))
        End Using
    End Sub

    <Pure()>
    Public Function OpenFileInBlock(ByVal blockIndex As UInteger) As IO.Stream
        Contract.Requires(blockIndex >= 0)
        Contract.Ensures(Contract.Result(Of IO.Stream)() IsNot Nothing)

        Dim block = BlockTable.TryGetBlock(blockIndex)
        If block Is Nothing Then Throw New InvalidOperationException("Invalid block index.")
        If (block.Properties And BlockProperties.Used) <> 0 Then Throw New InvalidOperationException("Block is empty.")
        Return New FileReader(streamFactory(), Me.Position, Me.FileChunkSize, block)
    End Function
    <Pure()>
    Public Function OpenFileByName(ByVal fileName As String) As IO.Stream
        Contract.Requires(fileName IsNot Nothing)
        Contract.Ensures(Contract.Result(Of IO.Stream)() IsNot Nothing)

        Dim blockIndex = Hashtable(fileName).blockIndex
        Dim block = BlockTable.TryGetBlock(blockIndex)
        If block Is Nothing Then Throw New InvalidOperationException("Invalid block index.")
        If (block.Properties And BlockProperties.Used) = 0 Then Throw New InvalidOperationException("Block is empty.")
        Return New FileReader(streamFactory(), Me.Position, Me.FileChunkSize, block, fileName)
    End Function

    Public Sub WriteToStream(ByVal stream As IO.Stream)
        Contract.Requires(stream IsNot Nothing)
        Contract.Requires(stream.CanWrite)
        Contract.Requires(stream.CanSeek)

        'keep only valid file entries
        Dim validBlocks = New List(Of Block)
        Dim validFileData = New List(Of IO.Stream)
        Dim idMap As New Dictionary(Of UInteger, UInteger)
        Dim id = 0UI
        For i = 0 To Me.BlockTable.Size - 1
            Dim i_ = CUInt(i)
            If (From hash In Me.Hashtable.Entries
                          Where hash.Exists() _
                          AndAlso hash.BlockIndex = i_).
                          None Then
                Continue For
            End If

            validFileData.Add(New IO.BufferedStream(Me.OpenFileInBlock(i_)))
            validBlocks.Add(Me.BlockTable.TryGetBlock(i_))
            idMap(i_) = id
            id += 1UI
        Next i

        'Write containing file header
        Dim bf = New IO.BufferedStream(stream)
        Using fileData = New IO.BufferedStream(Me.streamFactory())
            For i = 0 To Me.Position - 1
                bf.WriteByte(CByte(fileData.ReadByte()))
            Next i
        End Using

        'Store positions
        Dim archiveHeaderPosition = bf.Position()
        Dim blockTableOffset = 32UI
        Dim hashtableOffset = blockTableOffset + 16UI * CUInt(validBlocks.Count)
        Dim dataOffset = hashtableOffset + 16UI * CUInt(Me.Hashtable.Size)

        'Write file data
        bf.Position = archiveHeaderPosition + dataOffset
        For i = 0 To validFileData.Count - 1
            bf.WriteByte(CompressionTypes.ZLibDeflate)
            Using out = New IO.BufferedStream(New ZLibStream(bf, IO.Compression.CompressionMode.Compress, True))
                For j = 0 To validFileData(i).Length - 1
                    out.WriteByte(CByte(validFileData(i).ReadByte))
                Next j
            End Using

            validBlocks(i) = New Block(offset:=CUInt(bf.Position),
                                          fileSize:=CUInt(validFileData(i).Length),
                                          length:=CUInt(bf.Position - dataOffset - archiveHeaderPosition),
                                          properties:=BlockProperties.Continuous Or BlockProperties.Compressed Or BlockProperties.Used)
            validFileData(i).Close()
        Next i
        Dim endPos = bf.Position

        'Write block table
        bf.Position = archiveHeaderPosition + blockTableOffset
        With New IO.BinaryWriter(
                New StreamEncrypter(HashString("(block table)", CryptTableIndex.CypherKeyHash)).
                    ConvertWriteOnlyStream(bf))
            For Each f In validBlocks
                .Write(f.Offset)
                .Write(f.Length)
                .Write(f.FileSize)
                .Write(f.Properties)
            Next f
            .Flush()
        End With

        'Write hash table
        bf.Position = archiveHeaderPosition + hashtableOffset
        With New IO.BinaryWriter(
                New StreamEncrypter(HashString("(hash table)", CryptTableIndex.CypherKeyHash)).
                    ConvertWriteOnlyStream(bf))
            idMap(Hashtable.BlockIndex.NoFile) = Hashtable.BlockIndex.NoFile
            idMap(Hashtable.BlockIndex.DeletedFile) = Hashtable.BlockIndex.DeletedFile
            For Each h In Hashtable.Entries
                If Not h.Exists OrElse Not idMap.ContainsKey(h.BlockIndex) Then
                    If h.BlockIndex <> Hashtable.BlockIndex.NoFile Then
                        h = New HashEntry(fileKey:=0,
                                          language:=LanguageId.Neutral,
                                          blockIndex:=Hashtable.BlockIndex.DeletedFile,
                                          invalid:=False)
                    End If
                End If
                .Write(h.FileKey)
                .Write(h.Language)
                .Write(idMap(h.BlockIndex))
            Next h
            .Flush()
        End With

        'Write header
        bf.Position = archiveHeaderPosition
        bf.Write(MagicHeaderValue)
        bf.Write(32UI) 'header size
        bf.Write(CUInt(endPos - archiveHeaderPosition))
        bf.Write(21536US) 'format version
        bf.Write(CUShort(Math.Log(Me.FileChunkSize \ &H200, 2)).Bytes)
        bf.Write(hashtableOffset)
        bf.Write(blockTableOffset)
        bf.Write(CUInt(Me.Hashtable.Size))
        bf.Write(CUInt(validFileData.Count))
        bf.Flush()
    End Sub
End Class
