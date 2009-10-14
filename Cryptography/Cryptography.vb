''''<summary>
''''Encapsulates the decryption process used in MPQ files.
''''</summary>
''''<copyright>
''''Copyright (C) 2008 Craig Gidney, craig.gidney@gmail.com
''''
''''This source was adepted from the C version of mpqlib.
''''The C version belongs to the following authors,
''''
''''Maik Broemme, mbroemme@plusserver.de
''''
''''This program is free software; you can redistribute it and/or modify
''''it under the terms of the GNU General Public License as published by
''''the Free Software Foundation; either version 2 of the License, or
''''(at your option) any later version.
''''
''''This program is distributed in the hope that it will be useful,
''''but WITHOUT ANY WARRANTY; without even the implied warranty of
''''MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
''''GNU General Public License for more details.
''''
''''You should have received a copy of the GNU General Public License
''''along with this program; if not, write to the Free Software
''''Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
''''</copyright>

Namespace Cryptography
    Friend Module Cryptography
        Friend Enum CryptTableIndex As Integer
            PositionHash = 0
            NameHashLow = 1
            NameHashHigh = 2
            CypherKeyHash = 3
            CypherTable = 4
        End Enum
        Friend ReadOnly cryptTable As Dictionary(Of CryptTableIndex, ModInt32()) = ComputeCryptTable()

        '''<summary>Creates the encryption table used for MPQ data</summary>
        Private Function ComputeCryptTable() As Dictionary(Of CryptTableIndex, ModInt32())
            Const TableSize As Integer = 256 * 5
            Dim table(0 To TableSize - 1) As ModInt32
            Dim k As ModInt32 = &H100001
            Dim pos = 0

            While table(pos) = 0 '[every value in the table will have been initialized when this condition is no longer met]
                For word = 1 To 2
                    k = CUInt(k * 125 + 3) Mod 2796203UI
                    table(pos) <<= 16 '[don't overwrite value from first iteration]
                    table(pos) = table(pos) Or (k And &HFFFF)
                Next word

                pos += 256
                If pos > TableSize - 1 Then pos -= TableSize - 1
            End While

            Dim d = New Dictionary(Of CryptTableIndex, ModInt32())
            For Each h In EnumValues(Of CryptTableIndex)()
                Contract.Assume(h >= 0)
                Contract.Assume(h < 5)
                d(h) = table.SubArray(CInt(h) * 256, 256)
            Next h
            Return d
        End Function

        '''<summary>Hashes a string into a key</summary>
        <Pure()>
        Public Function HashString(ByVal fileName As String, ByVal hashType As CryptTableIndex) As ModInt32
            Dim k1 As ModInt32 = &H7FED7FED
            Dim k2 As ModInt32 = &HEEEEEEEE
            Dim T = cryptTable(hashType)
            For Each b In (From c In fileName.ToUpperInvariant Select CByte(Asc(c)))
                k1 = (k1 + k2) Xor T(b)
                k2 = b + k1 + k2 * 33 + 3
            Next b
            Return k1
        End Function

        '''<summary>Computes the 'hash name' of a file.</summary>
        <Pure()>
        Public Function HashFileName(ByVal fileName As String) As UInt64
            Contract.Requires(fileName IsNot Nothing)
            Return CULng(HashString(fileName, CryptTableIndex.NameHashLow)) Or CULng(HashString(fileName, CryptTableIndex.NameHashHigh)) << 32
        End Function

        '''<summary>Computes the decryption key of a file with known fileName</summary>
        <Extension()> <Pure()>
        Public Function GetFileDecryptionKey(ByVal block As Block,
                                             ByVal fileName As String) As ModInt32
            Contract.Requires(fileName IsNot Nothing)
            Dim key = HashString(fileName.Split("\"c).Last, CryptTableIndex.CypherKeyHash)

            'adjusted keys are offset by the file position
            If (block.Properties And BlockProperties.AdjustedKey) <> 0 Then
                key = block.FileSize Xor (key + block.Offset)
            End If

            Return key
        End Function

        '''<summary>Attempts to recover the decryption key of a file using a known plaintext attack</summary>
        '''<remarks>
        '''Encryption:
        '''   seed1 = *VALUE_TO_FIND*
        '''   seed2 = 0xEEEEEEEEL
        '''   seed2b = seed2 + T[seed1 and 0xFF]
        '''   encryptedByte1 = targetByte1 Xor (seed1 + seed2b)
        '''Decryption:
        '''   Let s = encryptedByte1 xor targetByte1
        '''   Notice s = seed1 + seed2b
        '''   Notice s = seed1 + seed2 + T[seed1 and 0xFF]
        '''   Let n = s - seed2
        '''   Notice n = seed1 + T[seed1 and 0xFF]
        '''   Notice seed1 = n - T[seed1 and 0xFF]
        '''   Notice the right side has only 256 possible values because seed1 is AND-ed with 0xFF
        '''   Brute force seed1 by trying every possible value of (seed1 and 0xHFF) in the right side
        '''</remarks>
        Public Function BreakFileDecryptionKey(ByVal cypherValue1 As UInt32,
                                               ByVal cyphervalue2 As UInt32,
                                               ByVal targetValue1 As UInt32) As ModInt32
            'Prep
            Dim T = cryptTable(CryptTableIndex.CypherTable)
            Dim e1 As ModInt32 = cypherValue1
            Dim e2 As ModInt32 = cyphervalue2
            'Initial values
            Dim k2 As ModInt32 = &HEEEEEEEE
            Dim s = e1 Xor targetValue1 'undo xor
            Dim n = s - k2 'undo addition

            'Brute force value of k1 by trying all possible values of (k1 & 0xFF)
            Dim haveMin = False
            Dim minKey As ModInt32
            Dim minVal As ModInt32
            For possibleValue = 0 To &HFF
                Dim k1 = n - T(possibleValue)
                If (k1 And &HFF) <> possibleValue Then Continue For 'doesn't satisfy basic constraint

                Using testStream = New IO.MemoryStream(capacity:=8)
                    testStream.Write(cypherValue1)
                    testStream.Write(cyphervalue2)
                    testStream.Seek(0, IO.SeekOrigin.Begin)
                    Using reader = New IO.BinaryReader(New StreamDecrypter(k1).ConvertReadOnlyStream(testStream))
                        'check decryption for correctness
                        If reader.ReadUInt32() <> targetValue1 Then Continue For 'doesn't match plaintext
                        'keep track of key with lowest second value [lower values are more likely plaintexts]
                        Dim u = reader.ReadUInt32()
                        If Not haveMin OrElse CUInt(u) < CUInt(minVal) Then
                            minKey = k1
                            minVal = u
                            haveMin = True
                        End If
                    End Using
                End Using
            Next possibleValue

            If Not haveMin Then Throw New IO.InvalidDataException("No possible decryption key for provided plaintext.")
            Return minKey
        End Function
    End Module
End Namespace
