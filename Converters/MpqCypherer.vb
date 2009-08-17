Namespace Crypt
    '''<summary>Implements a Converter for MPQ encryption and decryption</summary>
    Public Class MpqCypherer
        Implements IConverter(Of Byte)
        Private ReadOnly decrypt As Boolean
        Private ReadOnly initialKey1 As ModInt32
        Private Shared ReadOnly initialKey2 As ModInt32 = &HEEEEEEEE
        Private ReadOnly bytes(0 To 3) As Byte
        Public Enum modes
            encrypt
            decrypt
        End Enum

        Public Sub New(ByVal key As ModInt32, ByVal mode As modes)
            Me.initialKey1 = key
            Select Case mode
                Case modes.encrypt
                    Me.decrypt = False
                Case modes.decrypt
                    Me.decrypt = True
                Case Else
                    Throw mode.ValueShouldBeImpossibleException()
            End Select
        End Sub

        Public Function Convert(ByVal sequence As IEnumerator(Of Byte)) As IEnumerator(Of Byte) Implements IConverter(Of Byte).Convert
            Dim k1 = initialKey1
            Dim k2 = initialKey2
            Dim T = cryptTable(HashType.ENCRYPT)
            Return New Enumerator(Of Byte)(
                Function(controller)
                    If Not sequence.MoveNext Then  Return controller.Break()
                    bytes(0) = sequence.Current
                    For i = 1 To 3
                        If Not sequence.MoveNext Then  Return controller.Sequence(bytes.SubArray(0, i))
                        bytes(i) = sequence.Current
                    Next i

                    Dim v As ModInt32 = bytes.ToUInt32(ByteOrder.LittleEndian)
                    Dim s = T(k1 And &HFF)
                    Dim c = v Xor (k1 + k2 + s)

                    k1 = (k1 >> 11) Or (((Not k1) << 21) + &H11111111) '[vulnerability: causes k1 to lose entropy via bits being forced set]
                    k2 = If(decrypt, c, v) + (k2 + s) * 33 + 3

                    Return controller.Sequence(CUInt(c).Bytes(ByteOrder.LittleEndian))
                End Function)
        End Function
    End Class
End Namespace