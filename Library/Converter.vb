Namespace Library
    <ContractClass(GetType(ContractClassForIConverter(Of ,)))>
    Public Interface IConverter(Of In TInput, Out TOutput)
        Function Convert(ByVal sequence As IEnumerator(Of TInput)) As IEnumerator(Of TOutput)
    End Interface

    <ContractClassFor(GetType(IConverter(Of ,)))>
    Public NotInheritable Class ContractClassForIConverter(Of TInput, TOutput)
        Implements IConverter(Of TInput, TOutput)
        Public Function Convert(ByVal sequence As IEnumerator(Of TInput)) As IEnumerator(Of TOutput) Implements IConverter(Of TInput, TOutput).Convert
            Contract.Requires(sequence IsNot Nothing)
            Contract.Ensures(Contract.Result(Of IEnumerator(Of TOutput))() IsNot Nothing)
            Throw New NotSupportedException()
        End Function
    End Class

    Public Module ExtensionsForIConverter
        <Extension()> <Pure()>
        Public Function ConvertUsing(Of TIn, TOut)(ByVal sequence As IEnumerable(Of TIn),
                                                   ByVal converter As IConverter(Of TIn, TOut)) As IEnumerable(Of TOut)
            Contract.Requires(sequence IsNot Nothing)
            Contract.Requires(converter IsNot Nothing)
            Contract.Ensures(Contract.Result(Of IEnumerable(Of TOut))() IsNot Nothing)
            Return sequence.Transform(AddressOf converter.Convert)
        End Function

        <Extension()> <Pure()>
        Public Function AsStream(ByVal enumerable As IEnumerable(Of Byte)) As IReadableStream
            Contract.Requires(enumerable IsNot Nothing)
            Contract.Ensures(Contract.Result(Of IReadableStream)() IsNot Nothing)
            Return enumerable.GetEnumerator.AsStream
        End Function
        <Extension()> <Pure()>
        Public Function AsStream(ByVal enumerator As IEnumerator(Of Byte)) As IReadableStream
            Contract.Requires(enumerator IsNot Nothing)
            Contract.Ensures(Contract.Result(Of IReadableStream)() IsNot Nothing)
            Return New EnumeratorStream(enumerator)
        End Function
        <Extension()> <Pure()>
        Public Function AsEnumerator(ByVal stream As IReadableStream) As IEnumerator(Of Byte)
            Contract.Requires(stream IsNot Nothing)
            Contract.Ensures(Contract.Result(Of IEnumerator(Of Byte))() IsNot Nothing)
            Return New Enumerator(Of Byte)(Function(controller)
                                               Dim r = stream.TryReadByte()
                                               If Not r.HasValue Then Return controller.Break()
                                               Return r.Value
                                           End Function,
                                           AddressOf stream.Dispose)
        End Function

        <Extension()> <Pure()>
        Public Function AsWritePushEnumerator(Of T)(ByVal stream As IWritableStream,
                                                    ByVal converter As IConverter(Of T, Byte)) As PushEnumerator(Of T)
            Contract.Requires(converter IsNot Nothing)
            Contract.Requires(stream IsNot Nothing)
            Contract.Ensures(Contract.Result(Of PushEnumerator(Of T))() IsNot Nothing)
            Return New PushEnumerator(Of T)(Sub(sequenceT)
                                                Dim sequence = converter.Convert(sequenceT)
                                                While sequence.MoveNext
                                                    stream.Write({sequence.Current}.ToReadableList)
                                                End While
                                            End Sub,
                                            AddressOf stream.Dispose)
        End Function
        <Extension()> <Pure()>
        Public Function AsStream(ByVal enumerator As PushEnumerator(Of Byte)) As IWritableStream
            Contract.Requires(enumerator IsNot Nothing)
            Contract.Ensures(Contract.Result(Of IWritableStream)() IsNot Nothing)
            Return New PushEnumeratorStream(enumerator)
        End Function

        <Extension()> <Pure()>
        Public Function ConvertUsing(ByVal stream As IReadableStream, ByVal converter As IConverter(Of Byte, Byte)) As IReadableStream
            Contract.Requires(stream IsNot Nothing)
            Contract.Requires(converter IsNot Nothing)
            Contract.Ensures(Contract.Result(Of IReadableStream)() IsNot Nothing)
            Return converter.Convert(stream.AsEnumerator).AsStream
        End Function
        <Extension()> <Pure()>
        Public Function ConvertUsing(ByVal stream As IWritableStream, ByVal converter As IConverter(Of Byte, Byte)) As IWritableStream
            Contract.Requires(converter IsNot Nothing)
            Contract.Requires(stream IsNot Nothing)
            Contract.Ensures(Contract.Result(Of IWritableStream)() IsNot Nothing)
            Return stream.AsWritePushEnumerator(converter).AsStream
        End Function

        <Extension()>
        Public Function MoveNextAndReturn(Of T)(ByVal enumerator As IEnumerator(Of T)) As T
            Contract.Requires(enumerator IsNot Nothing)
            If Not enumerator.MoveNext Then Throw New InvalidOperationException("Ran past end of enumerator")
            Return enumerator.Current()
        End Function
    End Module

    '''<summary>Exposes an IEnumerator as a read-only stream.</summary>
    Friend NotInheritable Class EnumeratorStream
        Inherits FutureDisposable
        Implements IReadableStream
        Private ReadOnly _sequence As IEnumerator(Of Byte)

        <ContractInvariantMethod()> Private Shadows Sub ObjectInvariant()
            Contract.Invariant(_sequence IsNot Nothing)
        End Sub

        Public Sub New(ByVal sequence As IEnumerator(Of Byte))
            Contract.Requires(sequence IsNot Nothing)
            Me._sequence = sequence
        End Sub

        Public Function Read(ByVal maxCount As Integer) As IReadableList(Of Byte) Implements IReadableStream.Read
            If FutureDisposed.State <> FutureState.Unknown Then Throw New ObjectDisposedException(Me.GetType.Name)
            Dim result = New List(Of Byte)
            While result.Count < maxCount AndAlso _sequence.MoveNext
                result.Add(_sequence.Current)
            End While
            Return result.AsReadableList
        End Function

        Protected Overrides Function PerformDispose(ByVal finalizing As Boolean) As IFuture
            If finalizing Then Return Nothing
            _sequence.Dispose()
            Return Nothing
        End Function
    End Class

    Friend NotInheritable Class PushEnumeratorStream
        Inherits FutureDisposable
        Implements IWritableStream
        Private ReadOnly _pusher As PushEnumerator(Of Byte)

        <ContractInvariantMethod()> Private Sub ObjectInvariant()
            Contract.Invariant(_pusher IsNot Nothing)
        End Sub

        Public Sub New(ByVal pusher As PushEnumerator(Of Byte))
            Contract.Requires(pusher IsNot Nothing)
            Me._pusher = pusher
        End Sub

        Public Sub Write(ByVal data As IReadableList(Of Byte)) Implements IWritableStream.Write
            If FutureDisposed.State <> FutureState.Unknown Then Throw New ObjectDisposedException(Me.GetType.Name)
            _pusher.Push(data.GetEnumerator)
        End Sub

        Protected Overrides Function PerformDispose(ByVal finalizing As Boolean) As IFuture
            If finalizing Then Return Nothing
            _pusher.PushDone()
            _pusher.Dispose()
            Return Nothing
        End Function

        Public Sub Flush() Implements IWritableStream.Flush
        End Sub
    End Class
End Namespace
