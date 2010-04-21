'' Copyright (C) 2008 Craig Gidney, craig.gidney@gmail.com
''
'' This source was adepted from the C version of mpqlib.
'' The C version belongs to the following authors,
''
'' Maik Broemme, mbroemme@plusserver.de
'' 
'' This program is free software; you can redistribute it and/or modify
'' it under the terms of the GNU General Public License as published by
'' the Free Software Foundation; either version 2 of the License, or
'' (at your option) any later version.
''
'' This program is distributed in the hope that it will be useful,
'' but WITHOUT ANY WARRANTY; without even the implied warranty of
'' MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
'' GNU General Public License for more details.
''
'' You should have received a copy of the GNU General Public License
'' along with this program; if not, write to the Free Software
'' Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

Public Enum LanguageId As UInteger
    Neutral = 0

    Chinese = &H404
    Czech = &H405
    Danish = &H406
    German = &H407
    Greek = &H408
    English = &H409
    Spanish = &H40A
    Finnish = &H40B
    French = &H40C
    Hebrew = &H40D
    Hungarian = &H40E

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

    Private ReadOnly _archiveOffset As UInteger
    Private ReadOnly _hashtable As Hashtable 'Map from hashes filesnames to file table indexes
    Private ReadOnly _blockTable As BlockTable 'Stores the position, size, and other information about all files in the archive
    Private ReadOnly _streamFactory As Func(Of NonNull(Of IRandomReadableStream))
    Private ReadOnly _fileChunkSize As UInteger 'Size of the chunks files in the archive are divided into

    <ContractInvariantMethod()> Private Sub ObjectInvariant()
        Contract.Invariant(_hashtable IsNot Nothing)
        Contract.Invariant(_blockTable IsNot Nothing)
        Contract.Invariant(_streamFactory IsNot Nothing)
    End Sub

    Public ReadOnly Property Hashtable As Hashtable
        Get
            Contract.Ensures(Contract.Result(Of Hashtable)() IsNot Nothing)
            Return _hashtable
        End Get
    End Property
    Public ReadOnly Property BlockTable As BlockTable
        Get
            Contract.Ensures(Contract.Result(Of BlockTable)() IsNot Nothing)
            Return _blockTable
        End Get
    End Property

    Public Sub New(ByVal streamFactory As Func(Of NonNull(Of IRandomReadableStream)),
                   ByVal archiveOffset As UInt32,
                   ByVal hashtable As Hashtable,
                   ByVal blockTable As BlockTable,
                   ByVal fileChunkSize As UInt32)
        Contract.Requires(streamFactory IsNot Nothing)
        Contract.Requires(blockTable IsNot Nothing)
        Contract.Requires(hashtable IsNot Nothing)
        Me._streamFactory = streamFactory
        Me._archiveOffset = archiveOffset
        Me._hashtable = hashtable
        Me._blockTable = blockTable
        Me._fileChunkSize = fileChunkSize
    End Sub

    Public Shared Function FromStreamFactory(ByVal streamFactory As Func(Of NonNull(Of IRandomReadableStream))) As Archive
        Contract.Requires(streamFactory IsNot Nothing)
        Contract.Ensures(Contract.Result(Of Archive)() IsNot Nothing)

        Using stream = streamFactory().Value
            For archiveOffset = 0UI To CUInt(stream.Length - 128) Step 512UI
                Contract.Assume(archiveOffset < stream.Length)
                Dim archive = FromStreamTryOffset(streamFactory, stream, archiveOffset)
                If archive IsNot Nothing Then
                    Return archive
                End If
            Next archiveOffset
        End Using

        Throw New IO.InvalidDataException("MPQ archive header not found.")
    End Function
    Private Shared Function FromStreamTryOffset(ByVal streamFactory As Func(Of NonNull(Of IRandomReadableStream)),
                                                ByVal stream As IRandomReadableStream,
                                                ByVal archiveOffset As UInt32) As Archive
        Contract.Requires(streamFactory IsNot Nothing)
        Contract.Requires(stream IsNot Nothing)
        Contract.Requires(archiveOffset < stream.Length)

        'Find valid header
        stream.Position = archiveOffset

        Dim fileMagicValue = stream.ReadUInt32()
        Dim headerSize = stream.ReadUInt32()
        Dim archiveSize = stream.ReadUInt32()
        Dim formatVersion = stream.ReadUInt16()
        Dim fileChunkSizePower = stream.ReadUInt16()
        Dim hashtableOffset = stream.ReadUInt32() 'relative to archiveOffset
        Dim blockTableOffset = stream.ReadUInt32() 'relative to archiveOffset
        Dim hashtableSize = stream.ReadUInt32()
        Dim blockTableSize = stream.ReadUInt32()

        'Protected MPQs mess with values
        If archiveSize = 0 Then
            archiveSize = CUInt(stream.Length) - archiveOffset
        End If

        'Check for invalid signature
        If fileMagicValue <> MagicHeaderValue Then Return Nothing
        If CULng(hashtableOffset) + CULng(hashtableSize) >= archiveSize Then Return Nothing
        If blockTableOffset >= archiveSize Then Return Nothing

        'Correct values
        If blockTableOffset + blockTableSize * 16 > archiveSize Then
            blockTableSize = (archiveSize - blockTableOffset) \ 16UI
        End If
        Dim fileChunkSize = 512UI << fileChunkSizePower

        'Load tables
        Contract.Assume(archiveOffset + blockTableOffset < stream.Length)
        Dim blockTable = MPQ.BlockTable.FromStream(stream, archiveOffset + blockTableOffset, blockTableSize)
        Contract.Assume(archiveOffset + hashtableOffset < stream.Length)
        Dim hashtable = MPQ.Hashtable.FromStream(stream, archiveOffset + hashtableOffset, hashtableSize, CUInt(blockTable.Size))

        Return New Archive(streamFactory,
                           archiveOffset,
                           hashtable,
                           blockTable,
                           fileChunkSize)
    End Function
    Public Shared Function FromFile(ByVal archiveFilePath As InvariantString) As Archive
        Contract.Ensures(Contract.Result(Of Archive)() IsNot Nothing)
        Return FromStreamFactory(Function() New IO.FileStream(archiveFilePath, IO.FileMode.Open, IO.FileAccess.Read, IO.FileShare.Read).AsRandomReadableStream.AsNonNull)
    End Function

    <Pure()>
    Public Function OpenFileInBlock(ByVal blockIndex As UInteger) As IRandomReadableStream
        Contract.Requires(blockIndex >= 0)
        Contract.Ensures(Contract.Result(Of IRandomReadableStream)() IsNot Nothing)

        Dim block = BlockTable.TryGetBlock(blockIndex)
        If block Is Nothing Then Throw New InvalidOperationException("Invalid block index.")
        If (block.Properties And BlockProperties.Used) = 0 Then Throw New InvalidOperationException("Block is empty.")
        Return New FileReader(_streamFactory().Value, _archiveOffset, _fileChunkSize, block)
    End Function
    <Pure()>
    Public Function OpenFileByName(ByVal archiveFilePath As InvariantString) As IRandomReadableStream
        Contract.Ensures(Contract.Result(Of IRandomReadableStream)() IsNot Nothing)

        If Not _hashtable.Contains(archiveFilePath) Then Throw New InvalidOperationException("No archive file named '{0}'.".Frmt(archiveFilePath))
        Dim blockIndex = _hashtable(archiveFilePath).BlockIndex
        Dim block = BlockTable.TryGetBlock(blockIndex)
        If block Is Nothing Then Throw New InvalidOperationException("Invalid block index.")
        If (block.Properties And BlockProperties.Used) = 0 Then Throw New InvalidOperationException("Block is empty.")
        Return New FileReader(_streamFactory().Value, _archiveOffset, _fileChunkSize, block, archiveFilePath)
    End Function

    <ContractVerification(False)>
    Public Sub WriteToStream(ByVal stream As IO.Stream)
        Contract.Requires(stream IsNot Nothing)
        Contract.Requires(stream.CanWrite)
        Contract.Requires(stream.CanSeek)

        'keep only valid file entries
        Dim validBlocks = New List(Of Block)
        Dim validFileData = New List(Of IRandomReadableStream)
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

            validFileData.Add(Me.OpenFileInBlock(i_))
            validBlocks.Add(Me.BlockTable.TryGetBlock(i_))
            idMap(i_) = id
            id += 1UI
        Next i

        'Write containing file header
        Dim bf = New IO.BufferedStream(stream)
        Using fileData = _streamFactory().Value
            For i = 0 To _archiveOffset - 1
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
            validFileData(i).Dispose()
        Next i
        Dim endPos = bf.Position

        'Write block table
        bf.Position = archiveHeaderPosition + blockTableOffset
        With New EncipherStream(bf.AsWritableStream, HashString("(block table)", CryptTableIndex.CipherKeyHash))
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
        With New EncipherStream(bf.AsWritableStream, HashString("(hash table)", CryptTableIndex.CipherKeyHash))
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
        bf.Write(CUShort(Math.Log(_fileChunkSize \ &H200, 2)).Bytes)
        bf.Write(hashtableOffset)
        bf.Write(blockTableOffset)
        bf.Write(CUInt(Me.Hashtable.Size))
        bf.Write(CUInt(validFileData.Count))
        bf.Flush()
    End Sub
End Class
