''' <summary>A rainbow table mapping mpq fileName hashes to fileNames.</summary>
Public Class ListFile
    Private ReadOnly map As New Dictionary(Of UInt64, String)

    Public Sub New()
    End Sub
    Public Sub New(ByVal fileNames As IEnumerable(Of String))
        Contract.Requires(filenames IsNot Nothing)
        For Each item In fileNames
            Contract.Assume(item IsNot Nothing)
            Include(item)
        Next item
    End Sub

    '''<summary>Determines if the given fileName s included.</summary>
    Public ReadOnly Property Contains(ByVal fileName As String) As Boolean
        Get
            Contract.Requires(fileName IsNot Nothing)
            Return map.ContainsKey(Cryptography.HashFileName(fileName))
        End Get
    End Property
    '''<summary>Determines if the given hash belongs to an included fileName.</summary>
    Public ReadOnly Property Contains(ByVal hash As UInt64) As Boolean
        Get
            Return map.ContainsKey(hash)
        End Get
    End Property

    '''<summary>Determines the included fileName for the given hash.</summary>
    Default Public ReadOnly Property FileName(ByVal hash As UInt64) As String
        Get
            Contract.Requires(Me.Contains(hash))
            Contract.Ensures(Contract.Result(Of String)() IsNot Nothing)
            Contract.Assume(map(hash) IsNot Nothing)
            Return map(hash)
        End Get
    End Property
    '''<summary>Determines the included fileName for the given hash, or returns null if there is no matching fileName.</summary>
    <Pure()>
    Public Function TryGetFileName(ByVal hash As UInt64) As String
        Contract.Ensures((Contract.Result(Of String)() IsNot Nothing) = (Me.Contains(hash)))
        If Not Me.Contains(hash) Then Return Nothing
        Return Filename(hash)
    End Function
    '''<summary>Enumerates the included fileNames.</summary>
    Public ReadOnly Property FileNames() As IEnumerable(Of String)
        Get
            Return map.Values
        End Get
    End Property
    '''<summary>Returns the number of included fileNames.</summary>
    Public ReadOnly Property Size As Integer
        Get
            Contract.Ensures(Contract.Result(Of Integer)() >= 0)
            Return map.Count
        End Get
    End Property

    '''<summary>Ensures a fileName is included.</summary>
    Public Sub Include(ByVal fileName As String)
        Contract.Requires(fileName IsNot Nothing)
        map(Cryptography.HashFileName(fileName)) = fileName
    End Sub
    '''<summary>Ensures a sequence of fileNames are included.</summary>
    Public Sub IncludeRange(ByVal fileNames As IEnumerable(Of String))
        Contract.Requires(filenames IsNot Nothing)
        For Each item In fileNames
            Contract.Assume(item IsNot Nothing)
            Me.Include(item)
        Next item
    End Sub
    '''<summary>Ensures files mentioned in an archive's listfile are included.</summary>
    Public Sub IncludeArchiveListFile(ByVal archive As Archive)
        Contract.Requires(archive IsNot Nothing)
        Using reader = New IO.StreamReader(archive.OpenFileByName("(listfile)").AsStream)
            Do Until reader.EndOfStream
                Me.Include(reader.ReadLine)
            Loop
        End Using
    End Sub
End Class
