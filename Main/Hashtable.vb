'''<summary>
'''The hashtable from an MPQ archive.
'''Each entry tells you a file index and the hash of the file's original name.
'''The hashtable doesn't tell you a file's original name, only what it hashes to.
'''</summary>
Public Class Hashtable
    Private ReadOnly _hashes As IReadableList(Of HashEntry)

    <ContractInvariantMethod()> Private Sub ObjectInvariant()
        Contract.Invariant(_hashes IsNot Nothing)
    End Sub

    Public Enum BlockIndex As UInteger
        NoFile = &HFFFFFFFFUI
        DeletedFile = &HFFFFFFFEL
    End Enum

    Public Sub New(ByVal hashes As IEnumerable(Of HashEntry))
        Contract.Requires(hashes IsNot Nothing)
        Me._hashes = hashes.ToReadableList
    End Sub

    Public Shared Function FromStream(ByVal encryptedStream As IRandomReadableStream,
                                      ByVal position As UInt32,
                                      ByVal hashtableEntryCount As UInt32,
                                      ByVal blockTableEntryCount As UInt32) As Hashtable
        Contract.Requires(encryptedStream IsNot Nothing)
        Contract.Requires(position < encryptedStream.Length)
        Contract.Ensures(Contract.Result(Of Hashtable)() IsNot Nothing)
        encryptedStream.Position = position

        Dim stream = New DecipherStream(encryptedStream, HashString("(hash table)", CryptTableIndex.CipherKeyHash))
        Dim hashData = (From repeat In hashtableEntryCount.Range
                        Let fileKey = stream.ReadUInt64()
                        Let language = DirectCast(stream.ReadUInt32(), LanguageId)
                        Let blockIndex = DirectCast(stream.ReadUInt32(), Hashtable.BlockIndex)
                        Let invalid = blockIndex >= blockTableEntryCount AndAlso
                                      blockIndex <> Hashtable.BlockIndex.DeletedFile AndAlso
                                      blockIndex <> Hashtable.BlockIndex.NoFile
                        ).Cache

        Return New Hashtable(From entry In hashData
                             Select New HashEntry(entry.fileKey,
                                                  entry.language,
                                                  entry.blockIndex,
                                                  entry.invalid))
    End Function

    Public ReadOnly Property Entries As IEnumerable(Of HashEntry)
        Get
            Return _hashes
        End Get
    End Property
    Public ReadOnly Property Size As Integer
        Get
            Contract.Ensures(Contract.Result(Of Integer)() >= 0)
            Return _hashes.Count
        End Get
    End Property

    ''' <summary>
    ''' Returns the hash entry containing the file.
    ''' If there is no such entry, returns the entry the file should be placed in.
    ''' </summary>
    <ContractVerification(False)>
    Private Function FindFileSlot(ByVal archiveFilePath As InvariantString) As HashEntry?
        Dim nameKey = HashFileName(archiveFilePath)
        Dim offset = CInt(HashString(archiveFilePath, CryptTableIndex.PositionHash).UnsignedValue Mod CUInt(_hashes.Count))
        Dim firstEmptyEntry As HashEntry? = Nothing
        For i = 0 To _hashes.Count - 1
            Dim curEntry = _hashes((i + offset) Mod _hashes.Count)
            If Not curEntry.Exists Then
                If firstEmptyEntry Is Nothing Then firstEmptyEntry = curEntry
                If curEntry.BlockIndex = BlockIndex.NoFile Then Exit For
                If curEntry.BlockIndex = BlockIndex.DeletedFile Then Continue For
            End If

            If curEntry.Exists AndAlso curEntry.FileKey = nameKey Then
                If curEntry.Invalid Then
                    Throw New IO.InvalidDataException("File name points to invalid hash table entry.")
                End If
                Return curEntry
            End If
        Next i
        Return firstEmptyEntry
    End Function

    <ContractVerification(False)>
    Default Public ReadOnly Property Entry(ByVal archiveFilePath As InvariantString) As HashEntry
        Get
            Contract.Requires(Contains(archiveFilePath))
            Return FindFileSlot(archiveFilePath).Value
        End Get
    End Property

    Public ReadOnly Property Contains(ByVal archiveFilePath As InvariantString) As Boolean
        Get
            Dim h = FindFileSlot(archiveFilePath)
            Return h.HasValue AndAlso h.Value.Exists
        End Get
    End Property
End Class

<DebuggerDisplay("{ToString}")>
Public Structure HashEntry
    Public ReadOnly FileKey As ULong
    Public ReadOnly Language As LanguageId
    Public ReadOnly BlockIndex As Hashtable.BlockIndex
    Public ReadOnly Invalid As Boolean

    Public Sub New(ByVal fileKey As ULong,
                   ByVal language As LanguageId,
                   ByVal blockIndex As Hashtable.BlockIndex,
                   ByVal invalid As Boolean)
        Me.FileKey = fileKey
        Me.Language = language
        Me.BlockIndex = blockIndex
        Me.Invalid = invalid
    End Sub
    Public ReadOnly Property Exists As Boolean
        Get
            Return BlockIndex <> Hashtable.BlockIndex.NoFile AndAlso
                   BlockIndex <> Hashtable.BlockIndex.DeletedFile AndAlso
                   Language.EnumValueIsDefined
        End Get
    End Property

    Public Overrides Function ToString() As String
        Return "Key={0}, Language={1}, Block={2}".Frmt(FileKey, Language, BlockIndex)
    End Function
End Structure
