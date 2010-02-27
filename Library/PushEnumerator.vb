Namespace Library
    ''' <summary>
    ''' Reverses the direction of an enumerator, allowing you to push values instead of pulling them.
    ''' </summary>
    Public NotInheritable Class PushEnumerator(Of T)
        Inherits FutureDisposable
        Private finished As Boolean
        Private ReadOnly sequenceQueue As New Queue(Of IEnumerator(Of T))
        Private ReadOnly coroutine As Coroutine
        Private ReadOnly disposer As action

        <ContractInvariantMethod()> Private Sub ObjectInvariant()
            Contract.Invariant(coroutine IsNot Nothing)
            Contract.Invariant(sequenceQueue IsNot Nothing)
        End Sub

        Public Sub New(ByVal consumer As Action(Of IEnumerator(Of T)),
                       Optional ByVal disposer As action = Nothing)
            'Contract.Assume(consumer IsNot Nothing)
            Contract.Requires(consumer IsNot Nothing) 'commented because events screw with Contracts and NotifyingDisposable has an event

            Me.disposer = disposer
            Dim consumer_ = consumer
            Me.coroutine = New Coroutine(
                Sub(coroutineController)
                    'Construct the blocking sequence
                    Dim curSubsequence As IEnumerator(Of T) = Nothing
                    Dim sequence = New Enumerator(Of T)(
                        Function(enumController)
                            'Break if there are no elements to return
                            If finished Then Return enumController.Break

                            'Move to next element, and when current sequence runs out grab another one
                            While curSubsequence Is Nothing OrElse Not curSubsequence.MoveNext
                                If curSubsequence IsNot Nothing Then
                                    curSubsequence.Dispose()
                                End If
                                If sequenceQueue.Count <= 0 Then
                                    'Wait for more elements
                                    Call coroutineController.Yield()

                                    'Break if there are no more elements to return
                                    If finished Then Return enumController.Break
                                End If

                                'Grab next sequence of elements to return
                                curSubsequence = sequenceQueue.Dequeue()
                            End While

                            Return curSubsequence.Current
                        End Function
                    )

                    'Consume the sequence
                    Call consumer_(sequence)
                    'Dump any more pushed values
                    While sequence.MoveNext
                    End While
                End Sub
            )
        End Sub

        ''' <summary>Adds more elements for the consumer, and blocks until they have been consumed.</summary>
        Public Sub Push(ByVal sequence As IEnumerator(Of T))
            Contract.Requires(sequence IsNot Nothing)
            If finished Then Throw New InvalidOperationException("Can't push after Done.")
            sequenceQueue.Enqueue(sequence)
            coroutine.Continue()
        End Sub
        ''' <summary>Notifies the consumer that there are no elements, and blocks until the consumer finishes.</summary>
        Public Sub PushDone()
            If finished Then Throw New InvalidOperationException("Can't push after Done.")
            finished = True
            coroutine.Continue()
        End Sub

        Protected Overrides Function PerformDispose(ByVal finalizing As Boolean) As ifuture
            If finalizing Then Return Nothing
            If disposer IsNot Nothing Then Call disposer()
            coroutine.Dispose()
            Return Nothing
        End Function
    End Class
End Namespace
