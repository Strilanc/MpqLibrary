''' <summary>A rainbow table mapping mpq hashes to strings.</summary>
Public Class ListFile
    Private ReadOnly _map As New Dictionary(Of UInt64, InvariantString)

    Public Sub New()
    End Sub
    Public Sub New(ByVal initialPaths As IEnumerable(Of InvariantString))
        Contract.Requires(initialPaths IsNot Nothing)
        For Each item In initialPaths
            Include(item)
        Next item
    End Sub

    <ContractInvariantMethod()> Private Sub ObjectInvariant()
        Contract.Invariant(_map IsNot Nothing)
    End Sub

    '''<summary>Determines if the given string is included.</summary>
    Public ReadOnly Property Contains(ByVal path As String) As Boolean
        Get
            Contract.Requires(path IsNot Nothing)
            Return _map.ContainsKey(Cryptography.HashFileName(path))
        End Get
    End Property
    '''<summary>Determines if the given hash belongs to an included string.</summary>
    Public ReadOnly Property Contains(ByVal hash As UInt64) As Boolean
        Get
            Return _map.ContainsKey(hash)
        End Get
    End Property

    '''<summary>Reverses the given incuded hash.</summary>
    Default Public ReadOnly Property ValueOf(ByVal hash As UInt64) As InvariantString
        Get
            Contract.Requires(Me.Contains(hash))
            Return _map(hash)
        End Get
    End Property

    '''<summary>Reverses the given hash, or returns null if the hash is not included.</summary>
    <Pure()>
    <ContractVerification(False)>
    Public Function TryGetValueOf(ByVal hash As UInt64) As InvariantString?
        Contract.Ensures(Contract.Result(Of InvariantString?)().HasValue = Me.Contains(hash))
        If Not Me.Contains(hash) Then Return Nothing
        Return ValueOf(hash)
    End Function
    '''<summary>Enumerates the included strings.</summary>
    Public ReadOnly Property IncludedStrings() As IEnumerable(Of InvariantString)
        Get
            Return _map.Values
        End Get
    End Property
    '''<summary>Returns the number of included strings.</summary>
    Public ReadOnly Property Count As Integer
        Get
            Contract.Ensures(Contract.Result(Of Integer)() >= 0)
            Return _map.Count
        End Get
    End Property

    '''<summary>Ensures a string is included.</summary>
    Public Sub Include(ByVal path As InvariantString)
        _map(Cryptography.HashFileName(path)) = path
    End Sub
    '''<summary>Ensures strings from a sequence are included.</summary>
    Public Sub IncludeRange(ByVal paths As IEnumerable(Of InvariantString))
        Contract.Requires(paths IsNot Nothing)
        For Each item In paths
            Me.Include(item)
        Next item
    End Sub
    '''<summary>Ensures files mentioned in an archive's listfile are included.</summary>
    <ContractVerification(False)>
    Public Sub IncludeArchiveListFile(ByVal archive As Archive)
        Contract.Requires(archive IsNot Nothing)
        Using reader = New IO.StreamReader(archive.OpenFileByName("(listfile)").AsStream)
            Do Until reader.EndOfStream
                Me.Include(reader.ReadLine)
            Loop
        End Using
    End Sub
End Class
