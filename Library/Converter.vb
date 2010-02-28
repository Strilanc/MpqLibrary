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
        Public Function ConvertUsing(ByVal stream As IReadableStream, ByVal converter As IConverter(Of Byte, Byte)) As IReadableStream
            Contract.Requires(stream IsNot Nothing)
            Contract.Requires(converter IsNot Nothing)
            Contract.Ensures(Contract.Result(Of IReadableStream)() IsNot Nothing)
            Return converter.Convert(stream.AsEnumerator).AsStream
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
End Namespace
