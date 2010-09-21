Imports Strilbrary.Values

Namespace Cryptography
    '''<summary>Decrypts/Encrypts mpq data, one byte at a time.</summary>
    Public NotInheritable Class CipherEngine
        Private Shared ReadOnly Table As ModInt32() = cryptTable(CryptTableIndex.CipherTable)

        Private _k1 As ModInt32
        Private _k2 As ModInt32

        Private _partialPlainValue As UInt32
        Private _partialBitsCount As Byte
        Private _partialXorMask As UInt32

        <ContractInvariantMethod()> Private Sub ObjectInvariant()
            Contract.Invariant(_partialBitsCount Mod 8 = 0)
            Contract.Invariant(_partialBitsCount < 32)
        End Sub

        Public Sub New(ByVal key As ModInt32)
            Me._k1 = key
            Me._k2 = &HEEEEEEEE + Table(_k1.SignedValue And &HFF)
            Me._partialXorMask = (_k1 + _k2).UnsignedValue
        End Sub

        Public Function EncryptNext(ByVal plainValue As Byte) As Byte
            Dim encryptedValue = plainValue Xor CByte(_partialXorMask And &HFFUI)
            Advance(plainValue)
            Return encryptedValue
        End Function
        Public Function DecryptNext(ByVal encryptedValue As Byte) As Byte
            Dim plainValue = encryptedValue Xor CByte(_partialXorMask And &HFFUI)
            Advance(plainValue)
            Return plainValue
        End Function

        Private Sub Advance(ByVal plainByte As Byte)
            _partialPlainValue = _partialPlainValue Or (CUInt(plainByte) << _partialBitsCount)
            _partialXorMask >>= 8
            _partialBitsCount += CByte(8)

            If _partialBitsCount = 32 Then
                _k1 = ((_k1.ShiftRotateRight(11) Xor &HFFE00000) + &H11000000) Or &H111111 '[vulnerability: the OR loses entropy in _k1]
                _k2 = _k2 * 33 + _partialPlainValue + 3 + Table(_k1.SignedValue And &HFF)
                _partialXorMask = (_k1 + _k2).UnsignedValue
                _partialBitsCount = 0
                _partialPlainValue = 0
            End If
            Contract.Assume(_partialBitsCount < 32)
            Contract.Assume(_partialBitsCount Mod 8 = 0)
        End Sub
    End Class

    Public NotInheritable Class DecipherStream
        Implements IReadableStream

        Private ReadOnly _substream As IReadableStream
        Private ReadOnly _engine As CipherEngine

        <ContractInvariantMethod()> Private Sub ObjectInvariant()
            Contract.Invariant(_substream IsNot Nothing)
            Contract.Invariant(_engine IsNot Nothing)
        End Sub

        Public Sub New(ByVal subStream As IReadableStream, ByVal key As ModInt32)
            Contract.Requires(subStream IsNot Nothing)
            Me._substream = subStream
            Me._engine = New CipherEngine(key)
        End Sub

        <ContractVerification(False)>
        Public Function Read(ByVal maxCount As Int32) As IReadableList(Of Byte) Implements IReadableStream.Read
            Return (From b In _substream.Read(maxCount) Select _engine.DecryptNext(b)).ToReadableList
        End Function
        Public Sub Dispose() Implements IDisposable.Dispose
            _substream.Dispose()
        End Sub
    End Class

    Public NotInheritable Class EncipherStream
        Implements IWritableStream

        Private ReadOnly _substream As IWritableStream
        Private ReadOnly _engine As CipherEngine

        <ContractInvariantMethod()> Private Sub ObjectInvariant()
            Contract.Invariant(_substream IsNot Nothing)
            Contract.Invariant(_engine IsNot Nothing)
        End Sub

        Public Sub New(ByVal subStream As IWritableStream, ByVal key As ModInt32)
            Contract.Requires(subStream IsNot Nothing)
            Me._substream = subStream
            Me._engine = New CipherEngine(key)
        End Sub

        Public Sub Write(ByVal data As IReadableList(Of Byte)) Implements IWritableStream.Write
            _substream.Write((From b In data Select _engine.EncryptNext(b)).ToReadableList)
        End Sub
        Public Sub Flush() Implements IWritableStream.Flush
            _substream.Flush()
        End Sub
        Public Sub Dispose() Implements IDisposable.Dispose
            _substream.Dispose()
        End Sub
    End Class
End Namespace
