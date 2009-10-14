Imports Strilbrary.Numerics
Imports Strilbrary.Streams
Imports Strilbrary.Enumeration
Imports Microsoft.VisualStudio.TestTools.UnitTesting
Imports MPQ.Cryptography

<TestClass()>
Public Class StreamCypherTest
    Private Shared ReadOnly primes As Byte() = {2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31}
    <TestMethod()>
    Public Sub EncryptTest_FromPrimes()
        Dim converter = New MPQ.Cryptography.StreamEncrypter_Accessor(0)
        Dim input = New IO.MemoryStream(primes)
        Dim output = converter.ConvertReadOnlyStream(input)
        Assert.IsTrue(output.ReadRemaining().HasSameItemsAs({132, 150, 44, 15, 25, 129, 181, 35, 23, 29, 31}))
    End Sub
    <TestMethod()>
    Public Sub DecryptTest_FromPrimes()
        Dim converter = New MPQ.Cryptography.StreamDecrypter_Accessor(0)
        Dim input = New IO.MemoryStream(primes)
        Dim output = converter.ConvertReadOnlyStream(input)
        Assert.IsTrue(output.ReadRemaining().HasSameItemsAs({132, 150, 44, 15, 159, 18, 221, 43, 23, 29, 31}))
    End Sub

    <TestMethod()>
    Public Sub EncryptTest_ToPrimes()
        Dim converter = New MPQ.Cryptography.StreamEncrypter_Accessor(0)
        Dim input = New IO.MemoryStream({132, 150, 44, 15, 159, 18, 221, 43, 23, 29, 31})
        Dim output = converter.ConvertReadOnlyStream(input)
        Assert.IsTrue(output.ReadRemaining().HasSameItemsAs(primes))
    End Sub
    <TestMethod()>
    Public Sub DecryptTest_ToPrimes()
        Dim converter = New MPQ.Cryptography.StreamDecrypter_Accessor(0)
        Dim input = New IO.MemoryStream({132, 150, 44, 15, 25, 129, 181, 35, 23, 29, 31})
        Dim output = converter.ConvertReadOnlyStream(input)
        Assert.IsTrue(output.ReadRemaining().HasSameItemsAs(primes))
    End Sub
End Class
