Namespace Crypt
    '''<summary>Decrypts encrypted data in an MPQ file.</summary>
    Friend Class MpqStreamEncrypter
        Inherits AbstractMpqStreamCypher
        Public Sub New(ByVal key As ModInt32)
            MyBase.new(key)
        End Sub
        Protected Overrides Function SelectPlainValue(ByVal inputValue As ModInt32, ByVal outputValue As ModInt32) As ModInt32
            Return inputValue
        End Function
    End Class

    '''<summary>Encrypts data for placement in an MPQ file.</summary>
    Friend Class MpqStreamDecrypter
        Inherits AbstractMpqStreamCypher
        Public Sub New(ByVal key As ModInt32)
            MyBase.new(key)
        End Sub
        Protected Overrides Function SelectPlainValue(ByVal inputValue As ModInt32, ByVal outputValue As ModInt32) As ModInt32
            Return outputValue
        End Function
    End Class

    '''<summary>Outlines the algorithm for encryption and decryption of data in MPQ files.</summary>
    Friend MustInherit Class AbstractMpqStreamCypher
        Implements IConverter(Of Byte, Byte)
        Private ReadOnly initialKey1 As ModInt32
        Private Shared ReadOnly initialKey2 As ModInt32 = &HEEEEEEEE

        Public Sub New(ByVal key As ModInt32)
            Me.initialKey1 = key
        End Sub

        Protected MustOverride Function SelectPlainValue(ByVal inputValue As ModInt32, ByVal outputValue As ModInt32) As ModInt32

        Public Function Convert(ByVal sequence As IEnumerator(Of Byte)) As IEnumerator(Of Byte) Implements IConverter(Of Byte, Byte).Convert
            Dim k1 = initialKey1
            Dim k2 = initialKey2
            Dim T = cryptTable(CryptTableIndex.CypherTable)
            Return New Enumerator(Of Byte)(
                Function(controller)
                    'Get next dword to cypher (with no cyphering on remainder bytes)
                    If Not sequence.MoveNext Then  Return controller.Break()
                    Dim data As New List(Of Byte)(4)
                    Do
                        data.Add(sequence.Current)
                        If data.Count >= 4 Then  Exit Do
                        If Not sequence.MoveNext Then  Return controller.Sequence(data)
                    Loop

                    'Cypher
                    Dim inputValue As ModInt32 = data.ToUInt32(ByteOrder.LittleEndian)
                    Dim tableValue = T(k1 And &HFF)
                    Dim outputValue = inputValue Xor (k1 + k2 + tableValue)
                    k1 = (k1 >> 11) Or (((Not k1) << 21) + &H11111111) '[vulnerability: causes k1 to lose entropy via bits being forced set]
                    k2 = SelectPlainValue(inputValue, outputValue) + (k2 + tableValue) * 33 + 3

                    Return controller.Sequence(CUInt(outputValue).Bytes(ByteOrder.LittleEndian))
                End Function)
        End Function
    End Class
End Namespace