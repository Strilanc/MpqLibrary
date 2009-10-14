Imports Strilbrary.Numerics
Imports Strilbrary.Streams
Imports Strilbrary.Enumeration
Imports Microsoft.VisualStudio.TestTools.UnitTesting
Imports MPQ.Cryptography.Cryptography_Accessor
Imports MPQ.Cryptography

<TestClass()>
Public Class BlockTableTest
    Private Shared Function BuildTestBlockTableData() As IO.Stream
        Dim input = New IO.MemoryStream()
        Dim inputCypher = New StreamEncrypter_Accessor(HashString("(block table)", CryptTableIndex.CypherKeyHash)).ConvertWriteOnlyStream(input)

        inputCypher.Write(1UI)
        inputCypher.Write(2UI)
        inputCypher.Write(3UI)
        inputCypher.Write(MPQ.BlockProperties.Used Or MPQ.BlockProperties.Encrypted)

        inputCypher.Write(UInteger.MaxValue)
        inputCypher.Write(UInteger.MaxValue)
        inputCypher.Write(UInteger.MaxValue)
        inputCypher.Write(MPQ.BlockProperties.Used Or MPQ.BlockProperties.Encrypted)

        inputCypher.Flush()
        input.Position = 0
        Return input
    End Function

    <TestMethod()>
    Public Sub ConstructTest_SingleBlock()
        Dim table = New MPQ.BlockTable_Accessor(BuildTestBlockTableData, 0, 1)
        Assert.IsTrue(table.Size = 1)
        Assert.IsTrue(table.TryGetBlock(0).Offset = 1)
        Assert.IsTrue(table.TryGetBlock(0).Length = 2)
        Assert.IsTrue(table.TryGetBlock(0).FileSize = 3)
        Assert.IsTrue(table.TryGetBlock(0).Properties = (MPQ.BlockProperties.Used Or MPQ.BlockProperties.Encrypted))
    End Sub

    <TestMethod()>
    Public Sub ConstructTest_OverSized()
        Dim table = New MPQ.BlockTable_Accessor(BuildTestBlockTableData, 0, 100)
        Assert.IsTrue(table.Size = 1)
    End Sub
End Class
