Imports MPQ.Library
Imports Strilbrary.Values

Namespace Cryptography
    '''<summary>Decrypts/Encrypts mpq data, one byte at a time.</summary>
    Public NotInheritable Class CypherEngine
        Private Shared ReadOnly Table As ModInt32() = cryptTable(CryptTableIndex.CypherTable)

        Private _k1 As ModInt32
        Private _k2 As ModInt32

        Private _curPlainValue As UInt32
        Private _usedBitCount As Byte
        Private _mask As UInt32

        <ContractInvariantMethod()> Private Sub ObjectInvariant()
            Contract.Invariant(_usedBitCount Mod 8 = 0)
            Contract.Invariant(_usedBitCount <= 32)
        End Sub

        Public Sub New(ByVal key As ModInt32)
            Me._k1 = key
            Me._k2 = &HEEEEEEEE
            PrepMask()
        End Sub
        Private Sub PrepMask()
            _mask = _k1 + _k2 + Table(_k1 And &HFF)
            _usedBitCount = 0
            _curPlainValue = 0
        End Sub

        Public Function EncryptNext(ByVal plainByte As Byte) As Byte
            Dim cypherByte = plainByte Xor CByte(_mask And &HFFUI)
            Advance(plainByte)
            Return cypherByte
        End Function
        Public Function DecryptNext(ByVal cypherByte As Byte) As Byte
            Dim plainByte = cypherByte Xor CByte(_mask And &HFFUI)
            Advance(plainByte)
            Return plainByte
        End Function

        Private Sub Advance(ByVal plainByte As Byte)
            _curPlainValue = _curPlainValue Or (CUInt(plainByte) << _usedBitCount)
            _mask >>= 8
            _usedBitCount += CByte(8)

            If _usedBitCount = 32 Then
                _k2 = _curPlainValue + (_k2 + Table(_k1 And &HFF)) * 33 + 3
                _k1 = (_k1 >> 11) Or (((Not _k1) << 21) + &H11111111) '[vulnerability: causes k1 to lose entropy via bits being forced set]
                PrepMask()
            End If
        End Sub
    End Class

    Public NotInheritable Class DecypherStream
        Implements IReadableStream

        Private ReadOnly _substream As IReadableStream
        Private ReadOnly _engine As CypherEngine

        <ContractInvariantMethod()> Private Sub ObjectInvariant()
            Contract.Invariant(_substream IsNot Nothing)
            Contract.Invariant(_engine IsNot Nothing)
        End Sub

        Public Sub New(ByVal substream As IReadableStream, ByVal key As ModInt32)
            Contract.Requires(substream IsNot Nothing)
            Me._substream = substream
            Me._engine = New CypherEngine(key)
        End Sub

        Public Function Read(ByVal maxCount As Int32) As IReadableList(Of Byte) Implements IReadableStream.Read
            Return (From b In _substream.Read(maxCount) Select _engine.DecryptNext(b)).ToReadableList
        End Function
        Public Sub Dispose() Implements IDisposable.Dispose
            _substream.Dispose()
        End Sub
    End Class

    Public NotInheritable Class EncypherStream
        Implements IWritableStream

        Private ReadOnly _substream As IWritableStream
        Private ReadOnly _engine As CypherEngine

        <ContractInvariantMethod()> Private Sub ObjectInvariant()
            Contract.Invariant(_substream IsNot Nothing)
            Contract.Invariant(_engine IsNot Nothing)
        End Sub

        Public Sub New(ByVal substream As IWritableStream, ByVal key As ModInt32)
            Contract.Requires(substream IsNot Nothing)
            Me._substream = substream
            Me._engine = New CypherEngine(key)
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
