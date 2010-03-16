Imports Strilbrary.Collections
Imports Strilbrary.Streams
Imports Microsoft.VisualStudio.TestTools.UnitTesting

<TestClass()>
Public Class StreamCypherTest
    Private Shared ReadOnly primes As IReadableList(Of Byte) = New Byte() {2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31}.AsReadableList
    <TestMethod()>
    Public Sub EncryptTest_FromPrimes()
        Dim output = New IO.MemoryStream()
        Call New MPQ.Cryptography.EncipherStream(output.AsWritableStream, 0).Write(primes)
        output.Position = 0
        Assert.IsTrue(output.ReadRemaining.SequenceEqual({132, 150, 44, 15, 25, 129, 181, 35, 37, 135, 217}))
    End Sub
    <TestMethod()>
    Public Sub DecryptTest_FromPrimes()
        Dim input = New IO.MemoryStream(primes.ToArray).AsReadableStream
        Dim output = New MPQ.Cryptography.DecipherStream(input, 0).ReadRemaining
        Assert.IsTrue(output.SequenceEqual({132, 150, 44, 15, 159, 18, 221, 43, 159, 190, 179}))
    End Sub

    <TestMethod()>
    Public Sub EncryptTest_ToPrimes()
        Dim output = New IO.MemoryStream()
        Call New MPQ.Cryptography.EncipherStream(output.AsWritableStream, 0).Write(New Byte() {132, 150, 44, 15, 159, 18, 221, 43, 159, 190, 179}.AsReadableList)
        output.Position = 0
        Assert.IsTrue(output.ReadRemaining.SequenceEqual(primes))
    End Sub
    <TestMethod()>
    Public Sub DecryptTest_ToPrimes()
        Dim input = New IO.MemoryStream({132, 150, 44, 15, 25, 129, 181, 35, 37, 135, 217}).AsReadableStream
        Dim output = New MPQ.Cryptography.DecipherStream(input, 0).ReadRemaining
        Assert.IsTrue(output.SequenceEqual(primes))
    End Sub
End Class
