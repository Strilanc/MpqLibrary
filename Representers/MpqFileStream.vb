﻿Imports Mpq.MpqFileTable

''' <summary>
''' Exposes an IO.Stream around a file stored in an MPQ Archive.
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
Public Class MpqFileStream
    Inherits IO.Stream

    'Init State
    Public ReadOnly archive As MpqArchive 'The MPQ archive storing this file
    Public ReadOnly fileTableEntry As FileEntry 'The file's entry in the MPQ archive's file table
    Public ReadOnly numBlocks As UInteger 'The number of blocks the file is made up of
    Public ReadOnly decryptionKey As ModInt32 'Computed from the filename or extracted from known block offset table value
    Public ReadOnly canDecrypt As Boolean = False 'Indicates whether or not the decryptionKey is known
    Private ReadOnly blockOffsetTable() As ModInt32 = Nothing 'Starts/ends of the file blocks (stored in offset table if encrypted or compressed)
    Private ReadOnly baseStream As IO.Stream 'stream from mpq file

    'Reading State
    Private logicalPosition As UInteger = 0 'logical position within the decompressed file
    Private blockStream As IO.Stream 'current block stream
    Private numBlockBytesLeft As UInteger = 0 'bytes left to read in the current block stream

    Public Enum CompressionFlags As Byte
        Huffman = 1 << 0
        ZLibDeflate = 1 << 1
        PkWareImplode = 1 << 3
        BZip2 = 1 << 4
        IMA_ADPCM_MONO = 1 << 6
        IMA_ADPCM_STEREO = 1 << 7
    End Enum

    '''<summary>Creates a stream for the file with the given index, and uses the given name for decryption.</summary>
    '''<remarks>Can still compute the decryption key if the blockOffsetTable is stored in the file.</remarks>
    Friend Sub New(ByVal archive As MpqArchive,
                   ByVal fileTableEntry As MpqFileTable.FileEntry,
                   Optional ByVal knownFilename As String = Nothing)
        Contract.Requires(archive IsNot Nothing)
        Contract.Requires(fileTableEntry IsNot Nothing)

        Me.fileTableEntry = fileTableEntry
        Me.archive = archive
        Me.baseStream = New IO.BufferedStream(archive.streamFactory())
        numBlocks = CUInt(Math.Ceiling(fileTableEntry.actualSize / archive.fileBlockSize))
        Dim mightBePastFile = False

        'Sanity check
        If (fileTableEntry.flags And FileFlags.Exists) = 0 Then
            Throw New IO.IOException("File ID is in File Table, but is flagged as does not exist.")
        ElseIf fileTableEntry.filePosition > archive.archiveSize + archive.archivePosition Then
            Throw New IO.IOException("File starts past the end of the MPQ Archive")
        ElseIf fileTableEntry.filePosition + fileTableEntry.compressedSize > archive.archiveSize + archive.archivePosition Then
            '[File seems to end past end of mpq archive, but it may have a negative offset table]
            If (fileTableEntry.flags And FileFlags.Continuous) <> 0 Or (fileTableEntry.flags And (FileFlags.Compressed Or FileFlags.Encrypted)) = 0 Then
                '[No offset table]
                Throw New IO.InvalidDataException("File passes the end of the MPQ Archive")
            Else
                '[wait until block offset table is loaded to check]
                mightBePastFile = True
            End If
        End If

        'Check if filename supplied
        If knownFilename IsNot Nothing Then
            canDecrypt = True
            decryptionKey = GetFileDecryptionKey(knownFilename, fileTableEntry, archive)
        End If

        'Read offset table
        If (fileTableEntry.flags And FileFlags.Continuous) = 0 AndAlso fileTableEntry.compressedSize <> fileTableEntry.actualSize Then
            If (fileTableEntry.flags And (FileFlags.Compressed Or FileFlags.Encrypted)) <> 0 Then
                blockStream = baseStream
                ReDim blockOffsetTable(0 To CInt(numBlocks))

                baseStream.Seek(fileTableEntry.filePosition, IO.SeekOrigin.Begin)
                Dim tableSize = CUInt(blockOffsetTable.Length * 4)

                'Check for encryption [in case the flags are lying]
                With New IO.BinaryReader(baseStream)
                    'first value in the offset table should be the size of the offset table
                    If .ReadUInt32() <> tableSize Then
                        'this file must be encrypted
                        fileTableEntry.flags = fileTableEntry.flags Or FileFlags.Encrypted
                    End If
                End With

                'Decryption
                baseStream.Seek(fileTableEntry.filePosition, IO.SeekOrigin.Begin)
                If (fileTableEntry.flags And FileFlags.Encrypted) <> 0 Then
                    If Not canDecrypt Then
                        'try to decrypt using known plaintext attack
                        decryptionKey = BreakFileDecryptionKey(baseStream, tableSize)
                        canDecrypt = True
                        decryptionKey += 1 'the key for a block is offset by the block number (offset table is considered block -1)
                    End If
                    'wrap
                    blockStream = New MpqStreamDecrypter(decryptionKey - 1).ConvertReadOnlyStream(blockStream)
                End If

                'Read
                baseStream.Seek(fileTableEntry.filePosition, IO.SeekOrigin.Begin)
                With New IO.BinaryReader(blockStream)
                    For block_index = 0 To blockOffsetTable.Length - 1
                        blockOffsetTable(block_index) = .ReadUInt32()
                        If mightBePastFile Then
                            If CUInt(fileTableEntry.filePosition + blockOffsetTable(block_index)) > archive.archiveSize Then
                                Throw New IO.InvalidDataException("File passes the end of MPQ Archive")
                            End If
                        End If
                    Next block_index
                End With
            End If
        End If
    End Sub

    '''<summary>Seeks to the start of a block and preps for reading it</summary>
    Private Sub gotoBlock(ByVal blockIndex As UInteger)
        'Seek
        logicalPosition = blockIndex * archive.fileBlockSize
        Dim blockOffset As ModInt32
        If (fileTableEntry.flags And FileFlags.Continuous) <> 0 Then
            If blockIndex > 0 Then Throw New IO.IOException("Attempted to read a second block of a continuous file (should only have 1 block).")
            blockOffset = 0
        ElseIf blockOffsetTable IsNot Nothing Then
            blockOffset = blockOffsetTable(CInt(blockIndex))
        Else
            blockOffset = blockIndex * archive.fileBlockSize
        End If
        baseStream.Seek(blockOffset + fileTableEntry.filePosition, IO.SeekOrigin.Begin)
        blockStream = baseStream
        numBlockBytesLeft = Math.Min(archive.fileBlockSize, CUInt(Length - Position))

        'Decryption layer
        If (fileTableEntry.flags And FileFlags.Encrypted) <> 0 Then
            If Not canDecrypt Then Throw New IO.IOException("Couldn't decrypt MPQ block data.")
            blockStream = New MpqStreamDecrypter(decryptionKey + blockIndex).ConvertReadOnlyStream(blockStream)
        End If

        'Decompression layer
        Dim compressed = (fileTableEntry.flags And FileFlags.Compressed) <> 0
        If fileTableEntry.compressedSize - If(blockOffsetTable Is Nothing, 0, blockOffsetTable.Length * 4) = fileTableEntry.actualSize Then
            compressed = False 'no blocks are compressed if the filesize matches
        ElseIf (fileTableEntry.flags And FileFlags.Continuous) = 0 Then
            If blockOffsetTable(CInt(blockIndex + 1)) - blockOffsetTable(CInt(blockIndex)) = numBlockBytesLeft Then
                compressed = False 'this block isn't compressed if the blocksize matches
            End If
        End If
        If compressed Then
            Dim header = CType(blockStream.ReadByte(), CompressionFlags)

            'BZIP2
            If (header And CompressionFlags.BZip2) <> 0 Then
                blockStream = New ICSharpCode.SharpZipLib.BZip2.BZip2InputStream(blockStream)
                header = header And Not CompressionFlags.BZip2
            End If

            'PKWARE_IMPLODED
            If (header And CompressionFlags.PkWareImplode) <> 0 Then
                blockStream = New MpqPkWareDecoder().ConvertReadOnlyStream(blockStream)
                header = header And Not CompressionFlags.PkWareImplode
            End If

            'DEFLATE
            If (header And CompressionFlags.ZLibDeflate) <> 0 Then
                blockStream = New ZLibStream(blockStream, IO.Compression.CompressionMode.Decompress)
                header = header And Not CompressionFlags.ZLibDeflate
            End If

            'HUFFMAN
            If (header And CompressionFlags.Huffman) <> 0 Then
                blockStream = New MpqHuffmanDecoder().ConvertReadOnlyStream(blockStream)
                header = header And Not CompressionFlags.Huffman
            End If

            'STEREO WAVE
            If (header And CompressionFlags.IMA_ADPCM_STEREO) <> 0 Then
                blockStream = New MpqWaveDecoder(2).ConvertReadOnlyStream(blockStream)
                header = header And Not CompressionFlags.IMA_ADPCM_STEREO
            End If

            'MONO WAVE
            If (header And CompressionFlags.IMA_ADPCM_MONO) <> 0 Then
                blockStream = New MpqWaveDecoder(1).ConvertReadOnlyStream(blockStream)
                header = header And Not CompressionFlags.IMA_ADPCM_MONO
            End If

            'Unknown Compression
            If header <> 0 Then
                Throw New IO.IOException("Don't know how to decompress Unknown Compression.")
            End If
        End If
    End Sub

    Public Overrides ReadOnly Property Length() As Long
        Get
            Return fileTableEntry.actualSize
        End Get
    End Property

    '''<summary>The position in the decompressed/decrypted file.</summary>
    Public Overrides Property Position() As Long
        Get
            Return logicalPosition
        End Get
        Set(ByVal value As Long)
            'Check for invalid positions
            If value < 0 Then Throw New ArgumentOutOfRangeException("Position can't go before beginning of stream.")
            If value > Length Then Throw New ArgumentOutOfRangeException("Position can't go past end of stream.")
            'Go to position within block
            gotoBlock(CUInt(value \ archive.fileBlockSize))
            Dim offset = CInt(value Mod archive.fileBlockSize)
            If offset <> 0 Then
                If blockStream.CanSeek Then
                    blockStream.Seek(offset, IO.SeekOrigin.Current)
                Else
                    Dim bb(0 To offset - 1) As Byte
                    blockStream.Read(bb, 0, offset)
                End If
            End If
        End Set
    End Property

    Public Overrides Function Seek(ByVal offset As Long, ByVal origin As System.IO.SeekOrigin) As Long
        If origin = IO.SeekOrigin.Current Then offset += Position
        If origin = IO.SeekOrigin.End Then offset += Length
        Position = offset
    End Function

    Public Overrides Function Read(ByVal buffer() As Byte, ByVal offset As Integer, ByVal count As Integer) As Integer
        Dim numCopied = 0
        While numCopied < count And Position < Length
            'Go to next block when the current one finishes
            If numBlockBytesLeft <= 0 Then
                gotoBlock(CUInt(Position \ archive.fileBlockSize))
                If (fileTableEntry.flags And FileFlags.Continuous) <> 0 Then numBlockBytesLeft = CUInt(Length - Position)
            End If

            'Delegate read to block stream
            Dim numToRead = Math.Min(CInt(Length - Position), Math.Min(count - numCopied, CInt(numBlockBytesLeft)))
            Dim n = blockStream.Read(buffer, offset, numToRead)
            If n <= 0 Then
                n = numToRead
                Debug.Print("Weird Error: " + n.ToString() + " bytes missing from block.") '[one of the lich hero pissed .wavs sets this off]
            End If

            'Update state
            logicalPosition += CUInt(n)
            offset += n
            numCopied += n
            numBlockBytesLeft -= CUInt(n)
        End While
        Return CInt(numCopied)
    End Function

    Public Overrides ReadOnly Property CanRead() As Boolean
        Get
            Return True
        End Get
    End Property
    Public Overrides ReadOnly Property CanSeek() As Boolean
        Get
            Return True
        End Get
    End Property

#Region "Not Supported"
    Public Overrides ReadOnly Property CanWrite() As Boolean
        Get
            Return False
        End Get
    End Property
    Public Overrides Sub Flush()
    End Sub
    Public Overrides Sub SetLength(ByVal value As Long)
        Throw New NotSupportedException()
    End Sub
    Public Overrides Sub Write(ByVal buffer() As Byte, ByVal offset As Integer, ByVal count As Integer)
        Throw New NotSupportedException()
    End Sub
    Public Overrides Sub Close()
        If blockStream IsNot Nothing Then blockStream.Close()
        baseStream.Close()
        MyBase.Close()
    End Sub
#End Region
End Class
