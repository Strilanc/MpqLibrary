Imports MPQ.Library

'''<summary>
'''The hashtable from an MPQ archive.
'''Each entry tells you a file index and the hash of the file's original name.
'''The hashtable doesn't tell you a file's original name, only what it hashes to.
'''</summary>
Public Class Hashtable
    Private ReadOnly _hashes As IReadableList(Of HashEntry)

    Public Enum BlockIndex As UInteger
        NoFile = &HFFFFFFFFUI
        DeletedFile = &HFFFFFFFEL
    End Enum


    <ContractInvariantMethod()> Private Sub ObjectInvariant()
        Contract.Invariant(_hashes IsNot Nothing)
    End Sub

    '''<summary>Reads the hashtable from an MPQ archive</summary>
    Public Sub New(ByVal hashes As IReadableList(Of HashEntry))
        Contract.Requires(hashes IsNot Nothing)
        Me._hashes = hashes
    End Sub

    Public Shared Function FromStream(ByVal encryptedStream As IRandomReadableStream,
                                      ByVal position As UInt32,
                                      ByVal hashTableSize As UInt32,
                                      ByVal blockTableSize As UInt32) As Hashtable
        Contract.Requires(encryptedStream IsNot Nothing)
        encryptedStream.Position = position

        Dim stream = encryptedStream.ConvertUsing(New StreamDecrypter(HashString("(hash table)", CryptTableIndex.CypherKeyHash)))
        Dim hashes = New List(Of HashEntry)
        For repeat = 0 To hashTableSize - 1
            Dim fileKey = stream.ReadUInt64()
            Dim language = CType(stream.ReadUInt32(), LanguageId)
            Dim blockIndex = CType(stream.ReadUInt32(), Hashtable.BlockIndex)
            Dim invalid = blockIndex >= blockTableSize _
                          AndAlso blockIndex <> Hashtable.BlockIndex.DeletedFile _
                          AndAlso blockIndex <> Hashtable.BlockIndex.NoFile
            hashes.Add(New HashEntry(fileKey, language, blockIndex, invalid))
        Next repeat

        Return New Hashtable(hashes.ToReadableList)
    End Function

    Public ReadOnly Property Entries As IEnumerable(Of HashEntry)
        Get
            Contract.Ensures(Contract.Result(Of IEnumerable(Of HashEntry))() IsNot Nothing)
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
    Private Function FindFileSlot(ByVal fileName As String) As HashEntry
        Contract.Requires(fileName IsNot Nothing)

        Dim nameKey = HashFileName(fileName)
        Dim offset = CInt(CUInt(HashString(fileName, CryptTableIndex.PositionHash)) Mod CUInt(_hashes.Count))
        Dim firstEmptyEntry As HashEntry = Nothing
        For i = 0 To _hashes.Count - 1
            Dim curEntry = _hashes((i + offset) Mod _hashes.Count)
            If Not curEntry.Exists Then
                If firstEmptyEntry Is Nothing Then firstEmptyEntry = curEntry
                If curEntry.blockIndex = BlockIndex.NoFile Then Exit For
                If curEntry.blockIndex = BlockIndex.DeletedFile Then Continue For
            End If

            If curEntry.Exists AndAlso curEntry.fileKey = nameKey Then
                If curEntry.Invalid Then
                    Throw New IO.InvalidDataException("File name points to invalid hash table entry.")
                End If
                Return curEntry
            End If
        Next i
        Return firstEmptyEntry
    End Function

    Default Public ReadOnly Property Entry(ByVal fileName As String) As HashEntry
        Get
            Contract.Requires(fileName IsNot Nothing)
            Contract.Requires(Contains(fileName))
            Contract.Ensures(Contract.Result(Of HashEntry)() IsNot Nothing)
            Return FindFileSlot(fileName)
        End Get
    End Property

    Public ReadOnly Property Contains(ByVal fileName As String) As Boolean
        Get
            Contract.Requires(fileName IsNot Nothing)
            Dim h = FindFileSlot(fileName)
            Return h IsNot Nothing AndAlso h.Exists
        End Get
    End Property
End Class

<DebuggerDisplay("{ToString}")>
Public Class HashEntry
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
            Return blockIndex <> Hashtable.BlockIndex.NoFile AndAlso
                   blockIndex <> Hashtable.BlockIndex.DeletedFile AndAlso
                   language.EnumValueIsDefined
        End Get
    End Property

    Public Overrides Function ToString() As String
        Return "Key={0}, Language={1}, Block={2}".Frmt(fileKey, language, blockIndex)
    End Function
End Class
