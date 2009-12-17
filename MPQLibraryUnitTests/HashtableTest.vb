Imports Strilbrary.Streams
Imports Strilbrary.Enumeration
Imports Microsoft.VisualStudio.TestTools.UnitTesting
Imports MPQ.Cryptography.Cryptography_Accessor
Imports MPQ.Cryptography

<TestClass()>
Public Class HashtableTest
    Private Shared Function BuildTestHashtableData() As IO.Stream
        Dim input = New IO.MemoryStream()
        Dim inputCypher = New StreamEncrypter_Accessor(HashString("(hash table)", CryptTableIndex.CypherKeyHash)).ConvertWriteOnlyStream(input)

        inputCypher.Write(CULng(Long.MaxValue))
        inputCypher.Write(MPQ.LanguageId.English)
        inputCypher.Write(MPQ.Hashtable.BlockIndex.DeletedFile)

        inputCypher.Write(1UL)
        inputCypher.Write(MPQ.LanguageId.Russian)
        inputCypher.Write(0UI)

        inputCypher.Flush()
        input.Position = 0
        Return input
    End Function

    <TestMethod()>
    Public Sub ConstructTest_SingleBlock()
        Dim table = New MPQ.Hashtable_Accessor(BuildTestHashtableData, 0, 2, 1)
        Assert.IsTrue(table.Size = 2)
        Dim entries = table.Entries.ToArray()
        Assert.IsTrue(entries(0).FileKey = Long.MaxValue)
        Assert.IsTrue(entries(0).BlockIndex = MPQ.Hashtable.BlockIndex.DeletedFile)
        Assert.IsTrue(entries(0).Language = MPQ.LanguageId.English)
        Assert.IsTrue(entries(1).FileKey = 1)
        Assert.IsTrue(entries(1).BlockIndex = 0)
        Assert.IsTrue(entries(1).Language = MPQ.LanguageId.Russian)
    End Sub
End Class
