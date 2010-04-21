' Copyright (C) 2008 Craig Gidney, craig.gidney@gmail.com
'
' This source was adepted from the C version of mpqlib.
' The C version belongs to the following authors,
'
' Maik Broemme, mbroemme@plusserver.de
' 
' This program is free software; you can redistribute it and/or modify
' it under the terms of the GNU General Public License as published by
' the Free Software Foundation; either version 2 of the License, or
' (at your option) any later version.
'
' This program is distributed in the hope that it will be useful,
' but WITHOUT ANY WARRANTY; without even the implied warranty of
' MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
' GNU General Public License for more details.
'
' You should have received a copy of the GNU General Public License
' along with this program; if not, write to the Free Software
' Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

<Flags()>
Friend Enum CompressionTypes As Byte
    Huffman = 1 << 0
    ZLibDeflate = 1 << 1
    PkWareImplode = 1 << 3
    BZip2 = 1 << 4
    IMA_ADPCM_MONO = 1 << 6
    IMA_ADPCM_STEREO = 1 << 7
End Enum

''' <summary>
''' Exposes an IRandomReadableStream around a file stored in an MPQ Archive.
''' </summary>
Friend Class FileReader
    Implements IRandomReadableStream

    'context
    Private ReadOnly _archivePosition As UInteger
    Private ReadOnly _chunkSize As UInteger
    Private ReadOnly _block As Block
    Private ReadOnly _baseStream As IRandomReadableStream
    'data
    Private ReadOnly _decryptionKey As ModInt32
    Private ReadOnly _canDecrypt As Boolean
    Private ReadOnly _numChunks As UInteger
    Private ReadOnly _chunkOffsetTable As IReadableList(Of ModInt32) 'Starts/ends of the file blocks (stored in offset table if encrypted or compressed)
    'state
    Private _logicalStreamPosition As UInteger
    Private _curChunkStream As IReadableStream
    Private _numBlockBytesLeft As UInteger

    <ContractInvariantMethod()> Private Sub ObjectInvariant()
        Contract.Invariant(_block IsNot Nothing)
        Contract.Invariant(_baseStream IsNot Nothing)
        Contract.Invariant(_chunkOffsetTable Is Nothing OrElse _chunkOffsetTable.Count = _numChunks + 1)
        Contract.Invariant(_numBlockBytesLeft = 0 OrElse _curChunkStream IsNot Nothing)
    End Sub

    '''<summary>Creates a stream for the file with the given index, and uses the given name for decryption.</summary>
    '''<remarks>Can still compute the decryption key if the blockOffsetTable is stored in the file.</remarks>
    <ContractVerification(False)>
    Friend Sub New(ByVal baseStream As IRandomReadableStream,
                   ByVal archivePosition As UInteger,
                   ByVal chunkSize As UInteger,
                   ByVal block As Block,
                   Optional ByVal knownFilename As String = Nothing)
        Contract.Requires(baseStream IsNot Nothing)
        Contract.Requires(block IsNot Nothing)

        Me._block = block
        Me._archivePosition = archivePosition
        Me._chunkSize = chunkSize
        Me._baseStream = baseStream
        Me._numChunks = CUInt(Math.Ceiling(block.FileSize / chunkSize))
        Dim mightBePastFile = False

        'Sanity check
        If (block.Properties And BlockProperties.Used) = 0 Then
            Throw New IO.InvalidDataException("The given block is empty.")
        ElseIf block.Offset > baseStream.Length Then
            Throw New IO.InvalidDataException("File starts past the end of the archive")
        ElseIf block.Offset + block.Length > baseStream.Length Then
            '[File seems to end past end of mpq archive, but it may have a negative offset table]
            If (block.Properties And BlockProperties.Continuous) <> 0 _
                            OrElse (block.Properties And (BlockProperties.Compressed Or BlockProperties.Encrypted)) = 0 Then
                '[No offset table]
                Throw New IO.InvalidDataException("File runs past the end of the archive")
            Else
                '[wait until block offset table is loaded to check]
                mightBePastFile = True
            End If
        End If

        'Check if fileName supplied
        If knownFilename IsNot Nothing Then
            _canDecrypt = True
            _decryptionKey = block.GetFileDecryptionKey(knownFilename)
        End If

        'Read offset table
        If (block.Properties And BlockProperties.Continuous) = 0 AndAlso block.Length <> block.FileSize Then
            If (block.Properties And (BlockProperties.Compressed Or BlockProperties.Encrypted)) <> 0 Then
                Dim chunkOffsetCount = CInt(_numChunks) + 1

                Dim tableSize = CUInt(chunkOffsetCount * 4)

                'Check for encryption [in case the flags are lying]
                'first value in the offset table should be the size of the offset table
                baseStream.Position = archivePosition + block.Offset
                If baseStream.ReadUInt32 <> tableSize Then
                    'Add encrypted flag
                    block = New Block(block.Offset,
                                      block.Length,
                                      block.FileSize,
                                      block.Properties Or BlockProperties.Encrypted)
                End If

                'Decryption
                If CBool(block.Properties And BlockProperties.Encrypted) Then
                    If Not _canDecrypt Then
                        'try to decrypt using known plaintext attack
                        baseStream.Position = archivePosition + block.Offset
                        _decryptionKey = BreakFileDecryptionKey(encryptedValue1:=baseStream.ReadUInt32,
                                                                encryptedValue2:=baseStream.ReadUInt32,
                                                                plainValue1:=tableSize)
                        _decryptionKey += 1 'the key for a block is offset by the block number (offset table is considered block -1)
                        _canDecrypt = True
                    End If
                    'wrap
                    baseStream.Position = archivePosition + block.Offset
                    _curChunkStream = New DecipherStream(baseStream, _decryptionKey - 1)
                Else
                    baseStream.Position = archivePosition + block.Offset
                    _curChunkStream = baseStream
                End If

                'Read
                Me._chunkOffsetTable = (From blockIndex In chunkOffsetCount.Range
                                        Select New ModInt32(_curChunkStream.ReadUInt32())
                                        ).ToReadableList
                For Each chunkOffset In Me._chunkOffsetTable
                    If mightBePastFile Then
                        If (archivePosition + chunkOffset + block.Offset).UnsignedValue > baseStream.Length Then
                            Throw New IO.InvalidDataException("File passes the end of MPQ Archive")
                        End If
                    End If
                Next chunkOffset
            End If
        End If
    End Sub

    '''<summary>Seeks to the start of a block and preps for reading it</summary>
    <ContractVerification(False)>
    Private Sub GotoBlock(ByVal blockIndex As UInteger)
        Contract.Requires(_chunkOffsetTable Is Nothing OrElse blockIndex < _chunkOffsetTable.Count)
        Contract.Requires(_chunkOffsetTable IsNot Nothing OrElse blockIndex = 0)
        Contract.Ensures(_curChunkStream IsNot Nothing)

        'Seek
        _logicalStreamPosition = blockIndex * _chunkSize
        Dim blockOffset As ModInt32
        If (_block.Properties And BlockProperties.Continuous) <> 0 Then
            If blockIndex > 0 Then Throw New IO.IOException("Attempted to read a second block of a continuous file (should only have 1 block).")
            blockOffset = 0
        ElseIf _chunkOffsetTable IsNot Nothing Then
            blockOffset = _chunkOffsetTable(CInt(blockIndex))
        Else
            blockOffset = blockIndex * _chunkSize
        End If
        _baseStream.Position = (_archivePosition + blockOffset + _block.Offset).UnsignedValue
        _curChunkStream = _baseStream
        _numBlockBytesLeft = Math.Min(_chunkSize, CUInt(Length - Position))

        'Decryption layer
        If (_block.Properties And BlockProperties.Encrypted) <> 0 Then
            If Not _canDecrypt Then Throw New IO.IOException("Couldn't decrypt MPQ block data.")
            _curChunkStream = New DecipherStream(_curChunkStream, _decryptionKey + blockIndex)
        End If

        'Decompression layer
        Dim compressed = (_block.Properties And BlockProperties.Compressed) <> 0
        If _block.Length - If(_chunkOffsetTable Is Nothing, 0, _chunkOffsetTable.Count * 4) = _block.FileSize Then
            compressed = False 'no blocks are compressed if the filesize matches
        ElseIf (_block.Properties And BlockProperties.Continuous) = 0 Then
            If _chunkOffsetTable(CInt(blockIndex + 1)) - _chunkOffsetTable(CInt(blockIndex)) = _numBlockBytesLeft Then
                compressed = False 'this block isn't compressed if the blocksize matches
            End If
        End If
        If compressed Then
            Dim header = DirectCast(_curChunkStream.ReadByte(), CompressionTypes)

            'BZIP2
            If (header And CompressionTypes.BZip2) <> 0 Then
                Dim s = New ICSharpCode.SharpZipLib.BZip2.BZip2InputStream(_curChunkStream.AsStream)
                Contract.Assume(s.CanRead)
                _curChunkStream = s.AsReadableStream
                header = header And Not CompressionTypes.BZip2
            End If

            'PKWARE_IMPLODED
            If (header And CompressionTypes.PkWareImplode) <> 0 Then
                _curChunkStream = PkWareDecompressionStream.FromSubStream(_curChunkStream)
                header = header And Not CompressionTypes.PkWareImplode
            End If

            'DEFLATE
            If (header And CompressionTypes.ZLibDeflate) <> 0 Then
                Dim s = New ZLibStream(_curChunkStream.AsStream, IO.Compression.CompressionMode.Decompress)
                Contract.Assume(s.CanRead)
                _curChunkStream = s.AsReadableStream
                header = header And Not CompressionTypes.ZLibDeflate
            End If

            'HUFFMAN
            If (header And CompressionTypes.Huffman) <> 0 Then
                _curChunkStream = HuffmanDecompressionStream.FromSubStream(_curChunkStream)
                header = header And Not CompressionTypes.Huffman
            End If

            'STEREO WAVE
            If (header And CompressionTypes.IMA_ADPCM_STEREO) <> 0 Then
                _curChunkStream = New WaveDecompressionStream(_curChunkStream, numChannels:=2)
                header = header And Not CompressionTypes.IMA_ADPCM_STEREO
            End If

            'MONO WAVE
            If (header And CompressionTypes.IMA_ADPCM_MONO) <> 0 Then
                _curChunkStream = New WaveDecompressionStream(_curChunkStream, numChannels:=1)
                header = header And Not CompressionTypes.IMA_ADPCM_MONO
            End If

            'Unknown Compression
            If header <> 0 Then
                Throw New IO.IOException("Don't know how to decompress Unknown Compression.")
            End If
        End If
    End Sub

    Public ReadOnly Property Length() As Long Implements IRandomReadableStream.Length
        Get
            Return _block.FileSize
        End Get
    End Property

    '''<summary>The position in the decompressed/decrypted file.</summary>
    <ContractVerification(False)>
    Public Property Position() As Long Implements IRandomReadableStream.Position
        Get
            Return _logicalStreamPosition
        End Get
        Set(ByVal value As Long)
            'Check for invalid positions
            If value < 0 Then Throw New ArgumentOutOfRangeException("value", "Position can't go before beginning of stream.")
            If value > Length Then Throw New ArgumentOutOfRangeException("value", "Position can't go past end of stream.")
            'Go to position within block
            GotoBlock(CUInt(value \ _chunkSize))
            Dim offset = CInt(value Mod _chunkSize)
            If offset > 0 Then _curChunkStream.ReadExact(offset)
        End Set
    End Property

    <ContractVerification(False)>
    Public Function Read(ByVal maxCount As Integer) As IReadableList(Of Byte) Implements IReadableStream.Read
        Dim result = New List(Of Byte)(capacity:=maxCount)
        While result.Count < maxCount And Position < Length
            'Go to next block when the current one finishes
            If _numBlockBytesLeft <= 0 Then
                GotoBlock(CUInt(Position \ _chunkSize))
                If (_block.Properties And BlockProperties.Continuous) <> 0 Then _numBlockBytesLeft = CUInt(Length - Position)
            End If

            'Prep read
            Dim numToRead = Min({CInt(Length - Position),
                                 maxCount - result.Count,
                                 CInt(_numBlockBytesLeft)})
            Contract.Assume(numToRead > 0)
            _logicalStreamPosition += CUInt(numToRead)
            _numBlockBytesLeft -= CUInt(numToRead)

            'Read
            Dim readData = _curChunkStream.Read(numToRead)
            If readData.Count = 0 Then Exit While
            If readData.Count = maxCount Then Return readData
            result.AddRange(readData)
        End While
        Return result.AsReadableList
    End Function

    Public Sub Dispose() Implements IDisposable.Dispose
        If _curChunkStream IsNot Nothing Then _curChunkStream.Dispose()
        _baseStream.Dispose()
    End Sub
End Class
