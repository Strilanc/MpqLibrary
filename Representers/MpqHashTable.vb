'''<summary>
'''The hashtable from an MPQ archive.
'''Each entry tells you a file index and the hash of the file's original name.
'''The hashtable doesn't tell you a file's original name, only what it hashes to.
'''</summary>
Public Class MpqHashTable
    Public ReadOnly hashes As New List(Of HashEntry)
    Public ReadOnly archive As MpqArchive

    <DebuggerDisplay("{ToString}")>
    Public Class HashEntry
        Public key As ULong 'hashed file name
        Public language As MpqLanguageId 'LANG_ID
        Public fileIndex As DecoratoratedFileIndex 'index in file table

        Public ReadOnly Property Exists As Boolean
            Get
                Return fileIndex <> DecoratoratedFileIndex.NoFile AndAlso
                       fileIndex <> DecoratoratedFileIndex.DeletedFile AndAlso
                       language.EnumValueIsDefined
            End Get
        End Property

        Public Overrides Function ToString() As String
            Return "Key={0}, Language={1}, File Index={2}".frmt(key, language, fileIndex)
        End Function
    End Class

    '''<summary>Reads the hashtable from an MPQ archive</summary>
    Public Sub New(ByVal archive As MpqArchive)
        Contract.Requires(archive IsNot Nothing)
        Me.archive = archive

        'Prepare reader
        Using stream = archive.streamFactory()
            stream.Seek(archive.hashTablePosition, IO.SeekOrigin.Begin)
            Using br = New IO.BinaryReader(
                        New IO.BufferedStream(
                         New MpqStreamDecrypter(HashFilenameUsing("(hash table)", CryptTableIndex.CypherKeyHash)).ConvertReadOnlyStream(stream)))

                'Read values
                For repeat = 1UI To archive.numHashTableEntries
                    Dim h As New HashEntry()
                    h.key = br.ReadUInt64()
                    h.language = CType(br.ReadUInt32(), MpqLanguageId)
                    h.fileIndex = CType(br.ReadUInt32(), DecoratoratedFileIndex)
                    hashes.Add(h)
                Next repeat
            End Using
        End Using
    End Sub

    ''' <summary>
    ''' Returns the hash entry containing the file.
    ''' If there is no such entry, returns the entry the file should be placed in.
    ''' </summary>
    Private Function FindFileSlot(ByVal filename As String) As HashEntry
        Contract.Requires(filename IsNot Nothing)

        Dim nameKey = Common.HashFileName(filename)
        Dim offset = CInt(CUInt(HashFilenameUsing(filename, CryptTableIndex.PositionHash)) Mod CUInt(hashes.Count))
        Dim firstEmptyEntry As HashEntry = Nothing
        For i = 0 To hashes.Count - 1
            Dim curEntry = hashes((i + offset) Mod hashes.Count)
            If Not curEntry.Exists Then
                If firstEmptyEntry Is Nothing Then firstEmptyEntry = curEntry
                If curEntry.fileIndex = DecoratoratedFileIndex.NoFile Then Exit For
                If curEntry.fileIndex = DecoratoratedFileIndex.DeletedFile Then Continue For
            End If
            If curEntry.key = nameKey Then
                'removed; replaced by language id validation? uncomment later if problems re-emerge
                'If hashes((offset + start + 1) Mod hashes.Count).key = key Then
                'This mpq is protected, the first file is a fake to cause WE to crash, but wc3 skips it
                'Continue For
                'End If

                If curEntry.Exists Then
                    If curEntry.fileIndex >= archive.fileTable.fileEntries.Count Then
                        Throw New MPQException("Invalid MPQ hash table entry accessed. The entry's file index points outside the file table.")
                    End If
                    Return curEntry
                End If
            End If
        Next i
        Return firstEmptyEntry
    End Function

    '''<summary>
    '''Returns the MPQHash corresponding to the given filename.
    '''Throws an exception if there is no hash for the filename
    '''</summary>
    Public Function hash(ByVal filename As String) As HashEntry
        Contract.Requires(filename IsNot Nothing)
        If Not contains(filename) Then Throw New IO.IOException("Filekey not in Hash Table")
        Return FindFileSlot(filename)
    End Function

    <Pure()>
    Public Function contains(ByVal filename As String) As Boolean
        Contract.Requires(filename IsNot Nothing)
        Dim h = FindFileSlot(filename)
        Return h IsNot Nothing AndAlso h.Exists
    End Function

    Public Function getEmpty(ByVal filename As String) As HashEntry
        Contract.Requires(filename IsNot Nothing)
        If contains(filename) Then Throw New IO.IOException("Filekey already in Hash Table")
        Return FindFileSlot(filename)
    End Function
End Class
