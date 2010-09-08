Namespace Compression
    Friend Module WaveData 'ADPCM
        Friend ReadOnly stepIndexDeltaTable() As Integer = {
            -1, 0, -1, 4, -1, 2, -1, 6,
            -1, 1, -1, 5, -1, 3, -1, 7,
            -1, 1, -1, 5, -1, 3, -1, 7,
            -1, 2, -1, 4, -1, 6, -1, 8
        }
        Friend ReadOnly stepSizeTable() As Integer = {
            7, 8, 9, 10, 11, 12, 13, 14, 16, 17, 19, 21, 23, 25, 28, 31, 34, 37, 41, 45, 50, 55, 60, 66, 73, 80, 88, 97, 107, 118,
            130, 143, 157, 173, 190, 209, 230, 253, 279, 307, 337, 371, 408, 449, 494, 544, 598, 658, 724, 796, 876, 963, 1060,
            1166, 1282, 1411, 1552, 1707, 1878, 2066, 2272, 2499, 2749, 3024, 3327, 3660, 4026, 4428, 4871, 5358, 5894, 6484,
            7132, 7845, 8630, 9493, 10442, 11487, 12635, 13899, 15289, 16818, 18500, 20350, 22385, 24623, 27086, 29794, 32767
        }
    End Module

    Friend Class WaveDecompressionStream
        Implements IReadableStream

        Private ReadOnly _numChannels As Integer
        Private ReadOnly _subStream As IReadableStream
        Private ReadOnly _outBitBuffer As BitBuffer = New BitBuffer()
        Private ReadOnly _stepIndex(0 To 1) As Integer
        Private ReadOnly _prediction(0 To 1) As Integer
        Private _nextChannel As Int32
        Private _stepShift As Byte

        <ContractInvariantMethod()> Private Sub ObjectInvariant()
            Contract.Invariant(_outBitBuffer IsNot Nothing)
            Contract.Invariant(_subStream IsNot Nothing)
            Contract.Invariant(_stepIndex IsNot Nothing)
            Contract.Invariant(_prediction IsNot Nothing)
            Contract.Invariant(_stepIndex.Length = 2)
            Contract.Invariant(_prediction.Length = 2)
            Contract.Invariant(_numChannels > 0)
            Contract.Invariant(_numChannels <= 2)
            Contract.Invariant(_nextChannel >= 0)
            Contract.Invariant(_nextChannel < _numChannels)
            Contract.Invariant(_prediction(0) >= Short.MinValue)
            Contract.Invariant(_prediction(0) <= Short.MaxValue)
            Contract.Invariant(_prediction(1) >= Short.MinValue)
            Contract.Invariant(_prediction(1) <= Short.MaxValue)
        End Sub

        Public Sub New(ByVal subStream As IReadableStream, ByVal numChannels As Integer)
            Contract.Requires(subStream IsNot Nothing)
            Contract.Requires(numChannels > 0)
            Contract.Requires(numChannels <= 2)
            Me._numChannels = numChannels
            Me._subStream = subStream
            Init()
        End Sub
        <ContractVerification(False)>
        Private Sub Init()
            _subStream.ReadByte()
            _stepShift = _subStream.ReadByte
            For i = 0 To _numChannels - 1
                _stepIndex(i) = &H2C
                _prediction(i) = _subStream.ReadByte
                _outBitBuffer.QueueUInt16(CType(CShort(_prediction(i)), ModInt16).UnsignedValue)
            Next i
        End Sub

        <ContractVerification(False)>
        Public Function Read(ByVal maxCount As Int32) As IReadableList(Of Byte) Implements IReadableStream.Read
            Dim result = New List(Of Byte)(capacity:=maxCount)

            Do
                'Output buffered values
                While _outBitBuffer.BitCount >= 8 AndAlso result.Count < maxCount
                    result.Add(_outBitBuffer.TakeByte())
                End While

                'Read next value
                If result.Count >= maxCount Then Exit Do
                Dim tb = _subStream.TryReadByte()
                If tb Is Nothing Then Exit Do
                Dim b = tb.Value

                'process value
                Dim channel = _nextChannel
                _nextChannel = (_nextChannel + 1) Mod _numChannels
                If b.HasBitSet(7) Then 'special cases
                    Dim c = b.LowMasked(7)
                    Select Case c
                        Case 0 'small step adjustment, with repetition of last prediction
                            _stepIndex(channel) -= 1
                        Case 2 'dead value
                            Continue Do
                        Case Else 'large step adjustment
                            _stepIndex(channel) += If(c = 1, 8, -8)
                            _stepIndex(channel) = _stepIndex(channel).Between(0, stepSizeTable.Length - 1)
                            _nextChannel = channel 'use this channel again in the next iteration
                            Continue Do
                    End Select
                Else 'update predictions
                    'deltas
                    Dim stepSize = stepSizeTable(_stepIndex(channel))
                    Dim deltaPrediction = stepSize >> _stepShift
                    For Each e In b.Bits.Take(6) '[b is big endian]
                        If e Then deltaPrediction += stepSize
                        stepSize >>= 1
                    Next e
                    'update
                    _prediction(channel) += deltaPrediction * If(b.HasBitSet(6), -1, 1)
                    _stepIndex(channel) += stepIndexDeltaTable(b.LowMasked(5))
                End If

                'keep channel states from going out of range
                _prediction(channel) = _prediction(channel).Between(Short.MinValue, Short.MaxValue)
                _stepIndex(channel) = _stepIndex(channel).Between(0, stepSizeTable.Length - 1)

                'output prediction
                _outBitBuffer.QueueUInt16(CType(CShort(_prediction(channel)), ModInt16).UnsignedValue)
            Loop

            Return result.AsReadableList
        End Function

        Public Sub Dispose() Implements IDisposable.Dispose
            _subStream.Dispose()
        End Sub
    End Class
End Namespace
