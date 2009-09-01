'''<summary>
'''The file table from an MPQ Archive.
'''Each entry in the file table tells you where some file is and how to access it.
'''</summary>
Public Class MpqFileTable
    Public ReadOnly fileEntries As New List(Of FileEntry)
    Public ReadOnly archive As MpqArchive

    <DebuggerDisplay("{ToString}")>
    Public Class FileEntry
        Public filePosition As UInteger 'Absolute position of the file within the parent file of the archive
        Public compressedSize As UInteger 'Size of the file data stored in the archive in bytes
        Public actualSize As UInteger 'Actual size of the file in bytes
        Public flags As FileFlags 'Properties of the file
        Public Overrides Function ToString() As String
            Return "Position={0}, ActualSize={1}, CompressedSize={2}, flags={3}".Frmt(filePosition, actualSize, compressedSize, flags.EnumFlagsToString())
        End Function
    End Class

    '''<summary>Reads the file table from an MPQ archive</summary>
    Public Sub New(ByVal archive As MpqArchive)
        Contract.Requires(archive IsNot Nothing)
        Me.archive = archive

        'Read (with decryption)
        Using stream = archive.streamFactory()
            stream.Seek(archive.fileTablePosition, IO.SeekOrigin.Begin)
            Using br = New IO.BinaryReader( _
                        New IO.BufferedStream( _
                         New MpqStreamDecrypter(HashFilenameUsing("(block table)", CryptTableIndex.CypherKeyHash)).ConvertReadOnlyStream(stream)))
                For i = 0 To CInt(archive.numFileTableEntries) - 1
                    Dim f = New FileEntry()
                    f.filePosition = br.ReadUInt32()
                    f.compressedSize = br.ReadUInt32()
                    f.actualSize = br.ReadUInt32()
                    f.flags = CType(br.ReadUInt32(), FileFlags)
                    fileEntries.Add(f)
                Next i
            End Using
        End Using

        'Correct positions from relative to absolute
        For Each entry In fileEntries
            entry.filePosition += archive.archivePosition
        Next entry
    End Sub
End Class
