Imports Microsoft.VisualStudio.TestTools.UnitTesting
Imports MPQ.Cryptography

<TestClass()>
Public Class ListFileTest
    <TestMethod()>
    Public Sub ConstructTest()
        Assert.IsTrue(New MPQ.ListFile().Count = 0)
        Assert.IsTrue(New MPQ.ListFile({}).Count = 0)
        Assert.IsTrue(New MPQ.ListFile({"a", "b"}).Count = 2)
        Assert.IsTrue(New MPQ.ListFile({"a", "b", "c"}).Count = 3)
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
        Assert.IsTrue(list.Count = 3)
        list.Include("c")
        Assert.IsTrue(list.Count = 3)
        list.Include("d")
        Assert.IsTrue(list.Count = 4)
    End Sub

    <TestMethod()>
    Public Sub FileNamesTest()
        Dim list = New MPQ.ListFile({"a", "b", "c"})
        Dim filenames = list.IncludedStrings.ToArray
        Assert.IsTrue(filenames.Length = 3)
        Assert.IsTrue(filenames.Contains("a"))
        Assert.IsTrue(filenames.Contains("b"))
        Assert.IsTrue(filenames.Contains("c"))
    End Sub

    <TestMethod()>
    Public Sub FileNameTest()
        Dim list = New MPQ.ListFile({"a", "b", "c"})
        Assert.IsTrue(list.ValueOf(HashFileName("a")) = "a")
        Assert.IsTrue(list.ValueOf(HashFileName("b")) = "b")
        Assert.IsTrue(list.ValueOf(HashFileName("c")) = "c")
    End Sub

    <TestMethod()>
    Public Sub TryGetFileNameTest()
        Dim list = New MPQ.ListFile({"a", "b", "c"})
        Assert.IsTrue(list.TryGetValueOf(HashFileName("a")).Value = "a")
        Assert.IsTrue(list.TryGetValueOf(HashFileName("b")).Value = "b")
        Assert.IsTrue(list.TryGetValueOf(HashFileName("c")).Value = "c")
        Assert.IsTrue(list.TryGetValueOf(HashFileName("d")) Is Nothing)
    End Sub

    <TestMethod()>
    Public Sub IncludeTest()
        Dim list = New MPQ.ListFile()
        Assert.IsTrue(Not list.Contains("a"))
        list.Include("a")
        Assert.IsTrue(list.Count = 1)
        Assert.IsTrue(list.Contains("a"))
    End Sub

    <TestMethod()>
    Public Sub IncludeRangeTest()
        Dim list = New MPQ.ListFile()
        list.IncludeRange({"a", "b", "c"})
        Assert.IsTrue(list.Count = 3)
        Assert.IsTrue(list.Contains("a"))
        Assert.IsTrue(list.Contains("b"))
        Assert.IsTrue(list.Contains("c"))
    End Sub
End Class
