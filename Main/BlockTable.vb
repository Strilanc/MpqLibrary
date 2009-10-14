<Flags()>
Public Enum BlockProperties As UInteger
    Imploded = 1UI << 8
    Compressed = 1UI << 9
    Encrypted = 1UI << 16
    AdjustedKey = 1UI << 17
    Continuous = 1UI << 24
    Used = 1UI << 31
End Enum

'''<summary>
'''The file table from an MPQ Archive.
'''Each entry in the file table tells you where some file is and how to access it.
'''</summary>
Public Class BlockTable
    Private ReadOnly blocks As New List(Of Block)

    '''<summary>Reads the file table from an MPQ archive</summary>
    Friend Sub New(ByVal stream As IO.Stream,
                   ByVal position As Long,
                   ByVal entryCount As UInteger)
        Contract.Requires(stream IsNot Nothing)

        'Read (with decryption)
        stream.Seek(position, IO.SeekOrigin.Begin)
        Using br = New IO.BinaryReader(
                    New IO.BufferedStream(
                     New StreamDecrypter(HashString("(block table)", CryptTableIndex.CypherKeyHash)).
                      ConvertReadOnlyStream(stream)))
            For i = 0 To entryCount - 1
                Dim offset = br.ReadUInt32
                If offset > stream.Length Then Exit For 'implicit end of table [length not included in calculation on purpose]
                blocks.Add(New Block(offset,
                                     length:=br.ReadUInt32(),
                                     fileSize:=br.ReadUInt32(),
                                     properties:=CType(br.ReadUInt32(), BlockProperties)))
            Next i
        End Using
    End Sub

    '''<summary>Determines the number of blocks in the table.</summary>
    Public ReadOnly Property Size As Integer
        Get
            Contract.Ensures(Contract.Result(Of Integer)() >= 0)
            Return blocks.Count
        End Get
    End Property

    '''<summary>Returns the block at the given index. Null if the index is out of range.</summary>
    <Pure()>
    Public Function TryGetBlock(ByVal index As UInteger) As Block
        If index >= blocks.Count Then Return Nothing
        Contract.Assume(CInt(index) >= 0)
        Return blocks(CInt(index))
    End Function
End Class

''' <summary>
''' Points to file data or empty space within an mpq archive.
''' </summary>
<DebuggerDisplay("{ToString}")>
Public Class Block
    Public ReadOnly Offset As UInteger 'Relative to the archive
    Public ReadOnly Length As UInteger
    Public ReadOnly FileSize As UInteger
    Public ReadOnly Properties As BlockProperties
    Public Sub New(ByVal offset As UInteger,
                   ByVal length As UInteger,
                   ByVal fileSize As UInteger,
                   ByVal properties As BlockProperties)
        Me.Offset = offset
        Me.Length = length
        Me.FileSize = fileSize
        Me.Properties = properties
    End Sub
    Public Overrides Function ToString() As String
        Return "Offset={0}, Length={1}, FileSize={2}, Properties={3}".Frmt(Offset, FileSize, Length, Properties.EnumFlagsToString())
    End Function
End Class
