Imports MPQ.Library

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
''' Exposes a readable and seekable IO.Stream around a file stored in an MPQ Archive.
''' </summary>
Friend Class FileReader
    Implements IRandomReadableStream

    'context
    Private ReadOnly archivePosition As UInteger
    Private ReadOnly chunkSize As UInteger
    Private ReadOnly block As Block
    Private ReadOnly baseStream As IRandomReadableStream
    'data
    Private ReadOnly decryptionKey As ModInt32
    Private ReadOnly canDecrypt As Boolean
    Private ReadOnly numChunks As UInteger
    Private ReadOnly chunkOffsetTable() As ModInt32 'Starts/ends of the file blocks (stored in offset table if encrypted or compressed)
    'state
    Private logicalStreamPosition As UInteger
    Private curChunkStream As IReadableStream
    Private numBlockBytesLeft As UInteger

    '''<summary>Creates a stream for the file with the given index, and uses the given name for decryption.</summary>
    '''<remarks>Can still compute the decryption key if the blockOffsetTable is stored in the file.</remarks>
    Friend Sub New(ByVal baseStream As IRandomReadableStream,
                   ByVal archivePosition As UInteger,
                   ByVal chunkSize As UInteger,
                   ByVal block As Block,
                   Optional ByVal knownFilename As String = Nothing)
        Contract.Requires(baseStream IsNot Nothing)
        Contract.Requires(block IsNot Nothing)

        Me.block = block
        Me.archivePosition = archivePosition
        Me.chunkSize = chunkSize
        Me.baseStream = baseStream
        Me.numChunks = CUInt(Math.Ceiling(block.FileSize / chunkSize))
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
            canDecrypt = True
            decryptionKey = block.GetFileDecryptionKey(knownFilename)
        End If

        'Read offset table
        If (block.Properties And BlockProperties.Continuous) = 0 AndAlso block.Length <> block.FileSize Then
            If (block.Properties And (BlockProperties.Compressed Or BlockProperties.Encrypted)) <> 0 Then
                curChunkStream = baseStream
                ReDim chunkOffsetTable(0 To CInt(numChunks) + 1 - 1)

                baseStream.Position = archivePosition + block.Offset
                Dim tableSize = CUInt(chunkOffsetTable.Length * 4)

                'Check for encryption [in case the flags are lying]
                'first value in the offset table should be the size of the offset table
                If baseStream.ReadUInt32 <> tableSize Then
                    'Add encrypted flag
                    block = New Block(block.Offset,
                                      block.Length,
                                      block.FileSize,
                                      block.Properties Or BlockProperties.Encrypted)
                End If

                'Decryption
                baseStream.Position = archivePosition + block.Offset
                If (block.Properties And BlockProperties.Encrypted) <> 0 Then
                    If Not canDecrypt Then
                        'try to decrypt using known plaintext attack
                        decryptionKey = BreakFileDecryptionKey(cypherValue1:=baseStream.ReadUInt32,
                                                               cyphervalue2:=baseStream.ReadUInt32,
                                                               targetValue1:=tableSize)
                        canDecrypt = True
                        decryptionKey += 1 'the key for a block is offset by the block number (offset table is considered block -1)
                    End If
                    'wrap
                    curChunkStream = New DecypherStream(curChunkStream, decryptionKey - 1)
                End If

                'Read
                baseStream.Position = archivePosition + block.Offset
                For blockIndex = 0 To chunkOffsetTable.Length - 1
                    chunkOffsetTable(blockIndex) = curChunkStream.ReadUInt32()
                    If mightBePastFile Then
                        If CUInt(archivePosition + block.Offset + chunkOffsetTable(blockIndex)) > baseStream.Length Then
                            Throw New IO.InvalidDataException("File passes the end of MPQ Archive")
                        End If
                    End If
                Next blockIndex
            End If
        End If
    End Sub

    '''<summary>Seeks to the start of a block and preps for reading it</summary>
    Private Sub GotoBlock(ByVal blockIndex As UInteger)
        'Seek
        logicalStreamPosition = blockIndex * chunkSize
        Dim blockOffset As ModInt32
        If (block.Properties And BlockProperties.Continuous) <> 0 Then
            If blockIndex > 0 Then Throw New IO.IOException("Attempted to read a second block of a continuous file (should only have 1 block).")
            blockOffset = 0
        ElseIf chunkOffsetTable IsNot Nothing Then
            blockOffset = chunkOffsetTable(CInt(blockIndex))
        Else
            blockOffset = blockIndex * chunkSize
        End If
        baseStream.Position = archivePosition + block.Offset + blockOffset
        curChunkStream = baseStream
        numBlockBytesLeft = Math.Min(chunkSize, CUInt(Length - Position))

        'Decryption layer
        If (block.Properties And BlockProperties.Encrypted) <> 0 Then
            If Not canDecrypt Then Throw New IO.IOException("Couldn't decrypt MPQ block data.")
            curChunkStream = New DecypherStream(curChunkStream, decryptionKey + blockIndex)
        End If

        'Decompression layer
        Dim compressed = (block.Properties And BlockProperties.Compressed) <> 0
        If block.Length - If(chunkOffsetTable Is Nothing, 0, chunkOffsetTable.Length * 4) = block.FileSize Then
            compressed = False 'no blocks are compressed if the filesize matches
        ElseIf (block.Properties And BlockProperties.Continuous) = 0 Then
            If chunkOffsetTable(CInt(blockIndex + 1)) - chunkOffsetTable(CInt(blockIndex)) = numBlockBytesLeft Then
                compressed = False 'this block isn't compressed if the blocksize matches
            End If
        End If
        If compressed Then
            Dim header = CType(curChunkStream.ReadByte(), CompressionTypes)

            'BZIP2
            If (header And CompressionTypes.BZip2) <> 0 Then
                curChunkStream = New ICSharpCode.SharpZipLib.BZip2.BZip2InputStream(curChunkStream.AsStream).AsReadableStream
                header = header And Not CompressionTypes.BZip2
            End If

            'PKWARE_IMPLODED
            If (header And CompressionTypes.PkWareImplode) <> 0 Then
                curChunkStream = curChunkStream.ConvertUsing(New PkWareDecompressor())
                header = header And Not CompressionTypes.PkWareImplode
            End If

            'DEFLATE
            If (header And CompressionTypes.ZLibDeflate) <> 0 Then
                curChunkStream = New ZLibStream(curChunkStream.AsStream, IO.Compression.CompressionMode.Decompress).AsReadableStream
                header = header And Not CompressionTypes.ZLibDeflate
            End If

            'HUFFMAN
            If (header And CompressionTypes.Huffman) <> 0 Then
                curChunkStream = curChunkStream.ConvertUsing(New HuffmanDecompressor())
                header = header And Not CompressionTypes.Huffman
            End If

            'STEREO WAVE
            If (header And CompressionTypes.IMA_ADPCM_STEREO) <> 0 Then
                curChunkStream = curChunkStream.ConvertUsing(New WaveDecompressor(2))
                header = header And Not CompressionTypes.IMA_ADPCM_STEREO
            End If

            'MONO WAVE
            If (header And CompressionTypes.IMA_ADPCM_MONO) <> 0 Then
                curChunkStream = curChunkStream.ConvertUsing(New WaveDecompressor(1))
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
            Return block.FileSize
        End Get
    End Property

    '''<summary>The position in the decompressed/decrypted file.</summary>
    Public Property Position() As Long Implements IRandomReadableStream.Position
        Get
            Return logicalStreamPosition
        End Get
        Set(ByVal value As Long)
            'Check for invalid positions
            If value < 0 Then Throw New ArgumentOutOfRangeException("value", "Position can't go before beginning of stream.")
            If value > Length Then Throw New ArgumentOutOfRangeException("value", "Position can't go past end of stream.")
            'Go to position within block
            GotoBlock(CUInt(value \ chunkSize))
            Dim offset = CInt(value Mod chunkSize)
            If offset > 0 Then curChunkStream.ReadExact(offset)
        End Set
    End Property

    Public Function Read(ByVal maxCount As Integer) As IReadableList(Of Byte) Implements IReadableStream.Read
        Dim result = New List(Of Byte)(capacity:=maxCount)
        While result.Count < maxCount And Position < Length
            'Go to next block when the current one finishes
            If numBlockBytesLeft <= 0 Then
                GotoBlock(CUInt(Position \ chunkSize))
                If (block.Properties And BlockProperties.Continuous) <> 0 Then numBlockBytesLeft = CUInt(Length - Position)
            End If

            'Prep read
            Dim numToRead = Min({CInt(Length - Position),
                                 maxCount - result.Count,
                                 CInt(numBlockBytesLeft)})
            logicalStreamPosition += CUInt(numToRead)
            numBlockBytesLeft -= CUInt(numToRead)

            'Read
            Dim readData = curChunkStream.ReadExact(numToRead)
            If readData.Count = maxCount Then Return readData
            result.AddRange(readData)
        End While
        Return result.AsReadableList
    End Function

    Public Sub Dispose() Implements IDisposable.Dispose
        If curChunkStream IsNot Nothing Then curChunkStream.Dispose()
        baseStream.Dispose()
    End Sub
End Class
