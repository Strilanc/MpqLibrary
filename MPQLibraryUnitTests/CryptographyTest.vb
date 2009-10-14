Imports Strilbrary.Numerics
Imports Strilbrary.Streams
Imports Strilbrary.Enumeration
Imports Microsoft.VisualStudio.TestTools.UnitTesting
Imports MPQ.Cryptography.Cryptography_Accessor

<TestClass()>
Public Class CryptographyTest
    <TestMethod()>
    Public Sub HashStringTest()
        Assert.IsTrue(HashString("filename", CryptTableIndex.CypherKeyHash) = 656171036UI)
        Assert.IsTrue(HashString("filename", CryptTableIndex.PositionHash) = 3454989946UI)
        Assert.IsTrue(HashString("filename", CryptTableIndex.NameHashHigh) = 3531035226UI)
        Assert.IsTrue(HashString("filename", CryptTableIndex.NameHashLow) = 2343067650UI)
    End Sub

    <TestMethod()>
    Public Sub HashFileNameTest()
        Assert.IsTrue(HashFileName("filename") = 15165680819037036546UL)
    End Sub

    <TestMethod()>
    Public Sub GetFileDecryptionKeyTest()
        Assert.IsTrue(656171036 = GetFileDecryptionKey(New MPQ.Block(0, 0, 0, MPQ.BlockProperties.AdjustedKey), "filename"))
        Assert.IsTrue(656171035 = GetFileDecryptionKey(New MPQ.Block(2, 3, 5, MPQ.BlockProperties.AdjustedKey), "filename"))
        Assert.IsTrue(656171036 = GetFileDecryptionKey(New MPQ.Block(2, 3, 5, 0), "filename"))
    End Sub

    <TestMethod()>
    Public Sub BreakFileDecryptionKeyTest()
        Assert.IsTrue(0 = BreakFileDecryptionKey(cypherValue1:=New Byte() {132, 150, 44, 15}.ToUInt32,
                                                 cyphervalue2:=New Byte() {159, 18, 221, 43}.ToUInt32,
                                                 targetValue1:=New Byte() {2, 3, 5, 7}.ToUInt32))
    End Sub
End Class
