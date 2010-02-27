Imports Strilbrary.Collections
Imports Strilbrary.Streams
Imports MPQ.Library
Imports Microsoft.VisualStudio.TestTools.UnitTesting

<TestClass()>
Public Class StreamCypherTest
    Private Shared ReadOnly primes As Byte() = {2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31}
    <TestMethod()>
    Public Sub EncryptTest_FromPrimes()
        Dim converter = New MPQ.Cryptography.StreamEncrypter(0)
        Dim input = New IO.MemoryStream(primes).AsReadableStream
        Dim output = input.ConvertUsing(converter)
        Assert.IsTrue(output.readRemaining.SequenceEqual({132, 150, 44, 15, 25, 129, 181, 35, 23, 29, 31}))
    End Sub
    <TestMethod()>
    Public Sub DecryptTest_FromPrimes()
        Dim converter = New MPQ.Cryptography.StreamDecrypter(0)
        Dim input = New IO.MemoryStream(primes).AsReadableStream
        Dim output = input.ConvertUsing(converter)
        Assert.IsTrue(output.ReadRemaining().SequenceEqual({132, 150, 44, 15, 159, 18, 221, 43, 23, 29, 31}))
    End Sub

    <TestMethod()>
    Public Sub EncryptTest_ToPrimes()
        Dim converter = New MPQ.Cryptography.StreamEncrypter(0)
        Dim input = New IO.MemoryStream({132, 150, 44, 15, 159, 18, 221, 43, 23, 29, 31}).AsReadableStream
        Dim output = input.ConvertUsing(converter)
        Assert.IsTrue(output.ReadRemaining().SequenceEqual(primes))
    End Sub
    <TestMethod()>
    Public Sub DecryptTest_ToPrimes()
        Dim converter = New MPQ.Cryptography.StreamDecrypter(0)
        Dim input = New IO.MemoryStream({132, 150, 44, 15, 25, 129, 181, 35, 23, 29, 31}).AsReadableStream
        Dim output = input.ConvertUsing(converter)
        Assert.IsTrue(output.ReadRemaining().SequenceEqual(primes))
    End Sub
End Class
