Imports System
Imports Microsoft.VisualStudio.TestTools.UnitTesting
Imports MPQ.Library
Imports System.Threading

<TestClass()>
Public Class PushEnumeratorTest
    <TestMethod()>
    Public Sub TestPushPullWait()
        Dim value = 0
        Dim lock = New ManualResetEvent(initialState:=False)
        Dim e = New PushEnumerator(Of Integer)(Sub(enumerator)
                                                   If enumerator.MoveNext Then value = enumerator.Current
                                                   lock.Set()
                                               End Sub)
        e.Push({2}.ToList.GetEnumerator)
        Assert.IsTrue(lock.WaitOne(millisecondsTimeout:=1000))
        e.PushDone()
        Assert.IsTrue(value = 2)
    End Sub
    <TestMethod()>
    Public Sub TestPushDoneWait()
        Dim value = 0
        Dim lock = New ManualResetEvent(initialState:=False)
        Dim e = New PushEnumerator(Of Integer)(Sub(enumerator)
                                                   If Not enumerator.MoveNext Then value = 3
                                                   lock.Set()
                                               End Sub)
        e.PushDone()
        Assert.IsTrue(lock.WaitOne(millisecondsTimeout:=1000))
        Assert.IsTrue(value = 3)
    End Sub
    <TestMethod()>
    <ExpectedException(GetType(InvalidOperationException))>
    Public Sub TestOverPush()
        Dim e = New PushEnumerator(Of Integer)(Sub(enumerator)
                                               End Sub)
        e.PushDone()
        e.Push({2}.ToList.GetEnumerator)
    End Sub
    <TestMethod()>
    <ExpectedException(GetType(InvalidOperationException))>
    Public Sub TestOverPushDone()
        Dim e = New PushEnumerator(Of Integer)(Sub(enumerator)
                                               End Sub)
        e.PushDone()
        e.PushDone()
    End Sub
    <TestMethod()>
    Public Sub TestSum()
        Dim value = 0
        Dim lock = New ManualResetEvent(initialState:=False)
        Dim e = New PushEnumerator(Of Integer)(Sub(enumerator)
                                                   While enumerator.MoveNext
                                                       value += enumerator.Current
                                                   End While
                                                   lock.Set()
                                               End Sub)
        e.Push({2, 3, 4, 5}.ToList.GetEnumerator)
        e.PushDone()
        Assert.IsTrue(lock.WaitOne(millisecondsTimeout:=1000))
        Assert.IsTrue(value = 2 + 3 + 4 + 5)
    End Sub
End Class
