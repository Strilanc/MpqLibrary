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
    Private ReadOnly _blocks As IReadableList(Of Block)

    <ContractInvariantMethod()> Private Sub ObjectInvariant()
        Contract.Invariant(_blocks IsNot Nothing)
    End Sub

    Public Sub New(ByVal blocks As IReadableList(Of Block))
        Contract.Requires(blocks IsNot Nothing)
        Me._blocks = blocks
    End Sub

    Public Shared Function FromStream(ByVal encryptedStream As IRandomReadableStream,
                                      ByVal position As UInt32,
                                      ByVal entryCount As UInt32) As BlockTable
        Contract.Requires(encryptedStream IsNot Nothing)
        Contract.Requires(position < encryptedStream.Length)
        Contract.Ensures(Contract.Result(Of BlockTable)() IsNot Nothing)
        encryptedStream.Position = position

        Dim stream = New DecipherStream(encryptedStream, HashString("(block table)", CryptTableIndex.CipherKeyHash))
        Dim blocks = New List(Of Block)
        For i = 0 To entryCount - 1
            Dim offset = stream.ReadUInt32
            If offset > encryptedStream.Length Then Exit For 'implicit end of table [length not included in calculation on purpose]
            blocks.Add(New Block(offset:=offset,
                                 length:=stream.ReadUInt32(),
                                 fileSize:=stream.ReadUInt32(),
                                 properties:=CType(stream.ReadUInt32(), BlockProperties)))
        Next i

        Return New BlockTable(blocks.ToReadableList)
    End Function

    '''<summary>Determines the number of blocks in the table.</summary>
    Public ReadOnly Property Size As Integer
        Get
            Contract.Ensures(Contract.Result(Of Integer)() >= 0)
            Return _blocks.Count
        End Get
    End Property

    '''<summary>Returns the block at the given index. Null if the index is out of range.</summary>
    <Pure()>
    Public Function TryGetBlock(ByVal index As UInt32) As Block
        Contract.Requires(index >= 0)
        If index >= _blocks.Count Then Return Nothing
        Return _blocks(CInt(index))
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
