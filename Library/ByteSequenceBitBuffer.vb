Namespace Library
    Public NotInheritable Class ByteSequenceBitBuffer
        Private ReadOnly _buf As New BitBuffer
        Private ReadOnly _sequence As IEnumerator(Of Byte)

        <ContractInvariantMethod()> Private Sub ObjectInvariant()
            Contract.Invariant(_buf IsNot Nothing)
            Contract.Invariant(_sequence IsNot Nothing)
        End Sub

        Public Sub New(ByVal sequence As IEnumerator(Of Byte))
            Contract.Requires(sequence IsNot Nothing)
            Me._sequence = sequence
        End Sub
        Public ReadOnly Property BufferedBitCount As Integer
            Get
                Return _buf.BitCount
            End Get
        End Property
        Public Function TryBufferBits(ByVal bitCount As Integer) As Boolean
            Contract.Requires(bitCount >= 0)
            Contract.Ensures(Contract.Result(Of Boolean)() = (_buf.BitCount >= bitCount))
            While _buf.BitCount < bitCount
                If Not _sequence.MoveNext Then Return False
                _buf.QueueByte(_sequence.Current())
            End While
            Return True
        End Function
        Public Function Take(ByVal bitCount As Integer) As BitWord64
            Contract.Requires(bitCount >= 0)
            Contract.Requires(bitCount <= BitWord64.MaxSize)
            If Not TryBufferBits(bitCount) Then Throw New InvalidOperationException("Ran past end of sequence.")
            Return _buf.Take(bitCount)
        End Function
        Public Function TakeBit() As Boolean
            Return Take(1).Bits <> 0
        End Function
        Public Function TakeByte() As Byte
            Return CByte(Take(8).Bits)
        End Function
        Public Function TakeUInt16() As UInt16
            Return CUShort(Take(16).Bits)
        End Function
        Public Function TakeUInt32() As UInt32
            Return CUInt(Take(32).Bits)
        End Function
    End Class
End Namespace
