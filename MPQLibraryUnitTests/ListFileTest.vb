Imports Microsoft.VisualStudio.TestTools.UnitTesting
Imports MPQ.Cryptography

<TestClass()>
Public Class ListFileTest
    <TestMethod()>
    Public Sub ConstructTest()
        Assert.IsTrue(New MPQ.ListFile().Size = 0)
        Assert.IsTrue(New MPQ.ListFile({}).Size = 0)
        Assert.IsTrue(New MPQ.ListFile({"a", "b"}).Size = 2)
        Assert.IsTrue(New MPQ.ListFile({"a", "b", "c"}).Size = 3)
    End Sub

    <TestMethod()>
    Public Sub ContainsTest_FileName()
        Dim list = New MPQ.ListFile({"a", "b", "c"})
        Assert.IsTrue(list.Contains("a"))
        Assert.IsTrue(list.Contains("b"))
        Assert.IsTrue(list.Contains("c"))
        Assert.IsTrue(Not list.Contains("d"))
    End Sub

    <TestMethod()>
    Public Sub ContainsTest_Hash()
        Dim list = New MPQ.ListFile({"a", "b", "c"})
        Assert.IsTrue(list.Contains(HashFileName("a")))
        Assert.IsTrue(list.Contains(HashFileName("b")))
        Assert.IsTrue(list.Contains(HashFileName("c")))
        Assert.IsTrue(Not list.Contains(HashFileName("d")))
    End Sub

    <TestMethod()>
    Public Sub SizeTest()
        Dim list = New MPQ.ListFile({"a", "b", "c"})
        Assert.IsTrue(list.Size = 3)
        list.Include("c")
        Assert.IsTrue(list.Size = 3)
        list.Include("d")
        Assert.IsTrue(list.Size = 4)
    End Sub

    <TestMethod()>
    Public Sub FileNamesTest()
        Dim list = New MPQ.ListFile({"a", "b", "c"})
        Dim filenames = list.FileNames.ToArray
        Assert.IsTrue(filenames.Length = 3)
        Assert.IsTrue(filenames.Contains("a"))
        Assert.IsTrue(filenames.Contains("b"))
        Assert.IsTrue(filenames.Contains("c"))
    End Sub

    <TestMethod()>
    Public Sub FileNameTest()
        Dim list = New MPQ.ListFile({"a", "b", "c"})
        Assert.IsTrue(list.FileName(HashFileName("a")) = "a")
        Assert.IsTrue(list.FileName(HashFileName("b")) = "b")
        Assert.IsTrue(list.FileName(HashFileName("c")) = "c")
    End Sub

    <TestMethod()>
    Public Sub TryGetFileNameTest()
        Dim list = New MPQ.ListFile({"a", "b", "c"})
        Assert.IsTrue(list.TryGetFileName(HashFileName("a")) = "a")
        Assert.IsTrue(list.TryGetFileName(HashFileName("b")) = "b")
        Assert.IsTrue(list.TryGetFileName(HashFileName("c")) = "c")
        Assert.IsTrue(list.TryGetFileName(HashFileName("d")) = Nothing)
    End Sub

    <TestMethod()>
    Public Sub IncludeTest()
        Dim list = New MPQ.ListFile()
        Assert.IsTrue(Not list.Contains("a"))
        list.Include("a")
        Assert.IsTrue(list.Size = 1)
        Assert.IsTrue(list.Contains("a"))
    End Sub

    <TestMethod()>
    Public Sub IncludeRangeTest()
        Dim list = New MPQ.ListFile()
        list.IncludeRange({"a", "b", "c"})
        Assert.IsTrue(list.Size = 3)
        Assert.IsTrue(list.Contains("a"))
        Assert.IsTrue(list.Contains("b"))
        Assert.IsTrue(list.Contains("c"))
    End Sub
End Class
