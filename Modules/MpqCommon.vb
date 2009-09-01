Imports Mpq.Crypt

Namespace Common
    Public Module Common
        '''<summary>Computes the 'hash name' of a file.</summary>
        <Pure()>
        Public Function HashFileName(ByVal filename As String) As UInt64
            Contract.Requires(filename IsNot Nothing)
            Return CULng(HashFilenameUsing(filename, CryptTableIndex.NameHashLow)) Or CULng(HashFilenameUsing(filename, CryptTableIndex.NameHashHigh)) << 32
        End Function

        <Pure()>
        Public Function GetFileNameSlash(ByVal path As String) As String
            Contract.Requires(path IsNot Nothing)
            Contract.Ensures(Contract.Result(Of String)() IsNot Nothing)
            Dim words = path.Split("\"c, "/"c)
            Return words(words.Length - 1)
        End Function

        Public Sub WriteToFile(ByVal archive As MpqArchive, ByVal targetPath As String, ByVal ParamArray commands() As String)
            Using bbf As New IO.BufferedStream(New IO.FileStream(targetPath, IO.FileMode.OpenOrCreate, IO.FileAccess.ReadWrite, IO.FileShare.None))
                Dim w = New IO.BinaryWriter(bbf)
                Dim stream = archive.streamFactory()
                Dim sep = System.Text.ASCIIEncoding.ASCII.GetBytes(Environment.NewLine + "===" + Environment.NewLine)

                'before archive
                stream.Seek(0, IO.SeekOrigin.Begin)
                With New IO.BinaryReader(New IO.BufferedStream(stream))
                    For i = 0 To CInt(archive.archivePosition) - 1
                        w.Write(.ReadByte())
                    Next i
                End With

                Dim file_streams As New Dictionary(Of UInteger, IO.Stream)
                Dim actual_size_map As New Dictionary(Of UInteger, Integer)
                Dim compressed_files As New HashSet(Of UInteger)
                Dim del_files As New HashSet(Of UInteger)

                'Buffer mpq files into memory streams
                For i = 0 To archive.fileTable.fileEntries.Count - 1
                    Dim u = CUInt(i)
                    file_streams(u) = archive.OpenFile(u)
                    actual_size_map(u) = CInt(file_streams(u).Length)
                Next i

                'Apply commands
                For i = 0 To commands.Length - 1 Step 2
                    Dim k = HashFileName(commands(i))
                    Dim com_name = commands(i + 1).ToLower.Split(" "c)(0)
                    Dim com_arg = If(com_name = commands(i + 1), "", commands(i + 1).Substring(com_name.Length + 1))
                    If com_name = "add" Then
                        Dim h = archive.hashTable.getEmpty(commands(i))
                        h.key = k
                        h.language = 0
                        h.fileIndex = CType(del_files.First(), DecoratoratedFileIndex)
                        del_files.Remove(h.fileIndex)
                        file_streams(h.fileIndex) = New IO.MemoryStream
                        actual_size_map(h.fileIndex) = 0
                        compressed_files.Remove(h.fileIndex)
                        Continue For
                    End If

                    Dim found = False
                    For Each e In archive.hashTable.hashes
                        If e.key = k Then
                            found = True
                            Dim u = e.fileIndex
                            If del_files.Contains(u) Then Throw New InvalidOperationException("Can't delete then apply more operations.")
                            Select Case com_name
                                Case "delete"
                                    del_files.Add(u)
                                    e.fileIndex = DecoratoratedFileIndex.DeletedFile

                                Case "replace"
                                    file_streams(u) = New IO.FileStream(com_arg, IO.FileMode.Open, IO.FileAccess.Read, IO.FileShare.Read)
                                    actual_size_map(u) = CInt(file_streams(u).Length)
                                    compressed_files.Remove(u)

                                Case "prepend"
                                    If compressed_files.Contains(u) Then Throw New InvalidOperationException("Can't compress then prepend.")
                                    Dim bb = com_arg.FromHexStringToBytes
                                    file_streams(u) = New ConcatStream({New IO.MemoryStream(bb), file_streams(u)})
                                    actual_size_map(u) += bb.Length

                                Case "append"
                                    If compressed_files.Contains(u) Then Throw New InvalidOperationException("Can't compress then append.")
                                    Dim bb = com_arg.FromHexStringToBytes
                                    file_streams(u) = New ConcatStream({file_streams(u), New IO.MemoryStream(bb)})
                                    actual_size_map(u) += bb.Length

                                Case "compress"
                                    compressed_files.Add(e.fileIndex)

                                    'Divide into blocks
                                    Dim cur_s = file_streams(u)
                                    Dim block_size = CInt(archive.fileBlockSize)
                                    Dim table_size = CInt(Math.Ceiling(cur_s.Length / block_size) + 1)
                                    Dim blocks(0 To table_size - 2) As IO.MemoryStream
                                    Dim bb(0 To block_size - 1) As Byte
                                    For b = 0 To blocks.Length - 1
                                        cur_s.Seek(b * block_size, IO.SeekOrigin.Begin)
                                        Dim n = cur_s.Read(bb, 0, bb.Length)
                                        blocks(b) = New IO.MemoryStream
                                        blocks(b).Write(bb, 0, n)
                                    Next b

                                    'Compress blocks
                                    For j = 0 To blocks.Length - 1
                                        Dim b = blocks(j)
                                        Dim m = New IO.MemoryStream()
                                        m.WriteByte(Mpq.MpqFileStream.CompressionFlags.ZLibDeflate)
                                        Using df As New ZLibStream(m, IO.Compression.CompressionMode.Compress, True)
                                            b.Seek(0, IO.SeekOrigin.Begin)
                                            Dim n = b.Read(bb, 0, bb.Length)
                                            df.Write(bb, 0, n)
                                            df.Flush()
                                        End Using
                                        blocks(j) = m
                                        blocks(j).Seek(0, IO.SeekOrigin.Begin)
                                    Next j

                                    'Write
                                    Dim new_s = New IO.MemoryStream
                                    Dim br = New IO.BinaryWriter(new_s)
                                    Dim tt = CUInt(table_size * 4)
                                    br.Write(tt)
                                    For Each b In blocks
                                        tt += CUInt(b.Length)
                                        br.Write(CUInt(tt))
                                    Next b
                                    For Each b In blocks
                                        b.Seek(0, IO.SeekOrigin.Begin)
                                        bb = ReadAllStreamBytes(b)
                                        br.Write(bb, 0, bb.Length)
                                    Next b
                                    new_s.Seek(0, IO.SeekOrigin.Begin)
                                    file_streams(u) = new_s

                                Case Else
                                    Throw New InvalidOperationException("Unrecognized operation: " + com_name)
                            End Select
                        End If
                    Next e
                    If Not found Then Throw New InvalidOperationException("No file matched operation key.")
                Next i

                'Build new file table layout
                Dim numFileEntries = 0UI
                Dim fileIndexMap As New Dictionary(Of UInteger, UInteger)
                For i = 0 To archive.fileTable.fileEntries.Count - 1
                    Dim e = archive.fileTable.fileEntries(i)
                    Dim u = CUInt(i)
                    If Not del_files.Contains(u) Then
                        fileIndexMap(u) = numFileEntries
                        numFileEntries += 1UI
                    End If
                Next i

                'Write header
                w.Write(MpqArchive.ID_MPQ)
                w.Write(32UI)
                Dim sizePos = w.BaseStream.Position()
                w.Write(0UI) 'size placeholder
                w.Write(21536US)
                w.Write(CShort(Math.Log(archive.fileBlockSize \ &H200, 2)))
                w.Write(32UI + numFileEntries * 16UI)
                w.Write(32UI)
                w.Write(archive.numHashTableEntries)
                w.Write(numFileEntries)

                'Write file table
                Dim t = CUInt(32 + 16 * (numFileEntries + archive.hashTable.hashes.Count))
                With New IO.BinaryWriter(
                      New MpqStreamEncrypter(HashFilenameUsing("(block table)", CryptTableIndex.CypherKeyHash)).
                          ConvertWriteOnlyStream(bbf))
                    For i = 0 To archive.fileTable.fileEntries.Count - 1
                        Dim e = archive.fileTable.fileEntries(i)
                        Dim u = CUInt(i)
                        If del_files.Contains(u) Then Continue For
                        .Write(t + CUInt(sep.Length))
                        .Write(CUInt(file_streams(u).Length))
                        .Write(CUInt(actual_size_map(u)))
                        If compressed_files.Contains(u) Then
                            .Write(FileFlags.Exists Or FileFlags.Compressed)
                        Else
                            .Write(FileFlags.Exists Or FileFlags.Continuous)
                        End If
                        t += CUInt(file_streams(u).Length) + CUInt(sep.Length)
                    Next i
                End With

                'Write hash table
                With New IO.BinaryWriter(
                        New MpqStreamEncrypter(HashFilenameUsing("(hash table)", CryptTableIndex.CypherKeyHash)).
                            ConvertWriteOnlyStream(bbf))
                    For Each e In archive.hashTable.hashes
                        .Write(e.key)
                        .Write(e.language)
                        If del_files.Contains(e.fileIndex) OrElse Not fileIndexMap.ContainsKey(e.fileIndex) Then
                            .Write(DecoratoratedFileIndex.DeletedFile)
                        Else
                            .Write(fileIndexMap(e.fileIndex))
                        End If
                    Next e
                End With

                'Write mpq files
                For i = 0 To archive.fileTable.fileEntries.Count - 1
                    Dim u = CUInt(i)
                    If del_files.Contains(u) Then Continue For
                    w.Write(sep, 0, sep.Length)
                    file_streams(u).Seek(0, IO.SeekOrigin.Begin)
                    Dim bb = ReadAllStreamBytes(file_streams(u))
                    w.Write(bb, 0, CInt(file_streams(u).Length))
                Next i

                'Go back and write size
                w.BaseStream.Seek(sizePos, IO.SeekOrigin.Begin)
                w.Write(t)
                w.Close()
                stream.Close()
            End Using
        End Sub

        Public Sub readStreamMPQListFile(ByVal s As IO.Stream, ByVal map As Dictionary(Of UInt64, String))
            Try
                With New IO.StreamReader(s)
                    While Not .EndOfStream
                        Dim path As String = .ReadLine()
                        map(HashFileName(path)) = path
                    End While
                End With
            Catch e As Exception
                'invalid list file
            End Try
        End Sub

        Public Sub readMPQListFile(ByVal mpqa As MpqArchive, ByVal map As Dictionary(Of UInt64, String))
            map(HashFileName("(listfile)")) = "(listfile)"
            Try
                readStreamMPQListFile(mpqa.OpenFile("(listfile)"), map)
            Catch e As Exception
                'no list file
            End Try
        End Sub

        Public Function listMPQ(ByVal mpqa As MpqArchive, ByVal map As Dictionary(Of UInt64, String)) As List(Of String)
            Dim L As New List(Of String)
            readMPQListFile(mpqa, map)
            For Each h As MpqHashTable.HashEntry In mpqa.hashTable.hashes
                If h.fileIndex = DecoratoratedFileIndex.DeletedFile Then Continue For
                If h.fileIndex = DecoratoratedFileIndex.NoFile Then Continue For
                If map.ContainsKey(h.key) Then
                    L.Add(map(h.key))
                Else
                    L.Add("unknown@" + h.fileIndex.ToString() + "=" + Hex(h.key))
                End If
            Next h
            Return L
        End Function

        Public Sub write_stream_to_disk(ByVal src As IO.Stream, ByVal dst As String)
            Dim b = New IO.BufferedStream(src)
            Using f = New IO.BufferedStream(New IO.FileStream(dst, IO.FileMode.Create, IO.FileAccess.Write))
                Do
                    Dim i = src.ReadByte()
                    If i = -1 Then Exit Do
                    f.WriteByte(CByte(i))
                Loop
            End Using
        End Sub

        Public Sub extractMPQ(ByVal targetpath As String, ByVal archive As MpqArchive, ByVal map As Dictionary(Of UInt64, String))
            targetpath = targetpath.Replace(IO.Path.AltDirectorySeparatorChar, IO.Path.DirectorySeparatorChar)
            If targetpath(targetpath.Length - 1) <> IO.Path.AltDirectorySeparatorChar Then
                targetpath += IO.Path.AltDirectorySeparatorChar
            End If
            readMPQListFile(archive, map)
            For Each h As MpqHashTable.HashEntry In archive.hashTable.hashes
                If h.fileIndex = DecoratoratedFileIndex.DeletedFile Then Continue For
                If h.fileIndex = DecoratoratedFileIndex.NoFile Then Continue For
                Dim filename = ""
                Dim m As IO.Stream = Nothing
                Try
                    'Open file
                    If map.ContainsKey(h.key) Then
                        filename = map(h.key)
                        m = archive.OpenFile(filename)
                    Else
                        filename = "Unknown" + CStr(h.key)
                        m = archive.OpenFile(h.fileIndex)
                    End If
                    'Create sub directories
                    Dim ss() As String = filename.Split("\"c, "/"c)
                    Dim curpath As String = targetpath
                    For i = 0 To ss.Length - 2
                        curpath += ss(i) + "\"
                        If Not IO.Directory.Exists(curpath) Then IO.Directory.CreateDirectory(curpath)
                    Next i
                    'Write to file
                    Dim buffer(0 To 511) As Byte
                    write_stream_to_disk(m, targetpath + filename)
                    Debug.Print("Extracted " + filename)
                Catch e As IO.InvalidDataException
                    Debug.Print("Error extracting " + filename + ": " + e.ToString)
                Catch e As IO.IOException
                    Debug.Print("Error extracting " + filename + ": " + e.ToString)
                End Try
                If m IsNot Nothing Then m.Close()
            Next h
        End Sub































        '    Private Sub XXX()
        '        Dim archive = New Mpq.MpqArchive("C:\Program Files (x86)\Warcraft III\Maps\Test\Castle Fight v1.13b.mpq")
        '        Dim file = archive.OpenFile("war3map.w3i")
        '        Dim data(0 To CInt(file.Length) - 1) As Byte
        '        file.Read(data, 0, data.Length)

        '    End Sub
        '    Private Sub compare(ByVal s1 As IO.Stream, ByVal s2 As IO.Stream)
        '        Do
        '            Dim b1 = s1.ReadByte, b2 = s2.ReadByte
        '            If b1 <> b2 Then b1 = b1
        '            If b1 = -1 Or b2 = -1 Then Exit Do
        '        Loop
        '    End Sub
        Private Sub FixJ(ByVal reader As IO.StreamReader, ByVal writer As IO.StreamWriter)
            Dim bufferedLine = ""
            Dim wasReturning = False
            Dim lineBuffer As New List(Of String)
            Dim dataName = "jdyy6inydoi5jhr7rejt94h" + Environment.TickCount.ToString()
            Dim shhName As String = Nothing
            While Not reader.EndOfStream
                Dim temp = reader.ReadLine().Trim()
                If temp = "" Then Continue While

                If temp Like "*this==null*" Then
                    temp = temp.Replace("this==null", "this==0")
                End If
                If temp Like "*,.*" Then
                    temp = temp.Replace(",.", ",0.")
                End If
                If temp Like "*(.*" Then
                    temp = temp.Replace("(.", "(0.")
                End If

                lineBuffer.Add(temp)
                If lineBuffer.Count < 7 Then Continue While
                If lineBuffer.Count > 7 Then lineBuffer.RemoveAt(0)

                Dim fail = False
                If lineBuffer(0) Like "function * takes * * returns *" AndAlso
                            Not lineBuffer(0) Like "function * takes * *,* * returns *" AndAlso
                            lineBuffer(1) Like "return*" AndAlso
                            lineBuffer(2) Like "return*" AndAlso
                            lineBuffer(3) Like "endfunction*" Then
                    'simple return bug function

                    While lineBuffer(0).Contains("  ")
                        lineBuffer(0) = lineBuffer(0).Replace("  ", " ")
                    End While
                    Dim words = lineBuffer(0).Split(" "c)
                    Dim argType = words(3)
                    Dim argName = words(4)
                    Dim retType = words(6)
                    Select Case retType
                        Case "integer"
                            Select Case argType
                                Case "handle"
                                    writer.WriteLine(lineBuffer(0))
                                    writer.WriteLine("    return GetHandleId(" + argName + ")")
                                    writer.WriteLine("endfunction")
                                Case Else
                                    Throw New OperationFailedException
                            End Select
                        Case Else
                            Throw New OperationFailedException
                    End Select
                    lineBuffer.RemoveRange(0, 4)
                ElseIf lineBuffer(0) Like "function * takes handle *,*string * returns *" AndAlso
                        Not lineBuffer(0) Like "function * takes * *,* *,* * returns *" AndAlso
                        lineBuffer(1) Like "return*" AndAlso
                        lineBuffer(2) Like "return*" AndAlso
                        lineBuffer(3) Like "endfunction*" Then
                    'Handlevar gethandlehandle

                    lineBuffer(0) = lineBuffer(0).Replace(",", " , ")
                    While lineBuffer(0).Contains("  ")
                        lineBuffer(0) = lineBuffer(0).Replace("  ", " ")
                    End While

                    Dim words = lineBuffer(0).Split(" "c)
                    Dim retType = words.Last
                    writer.WriteLine(lineBuffer(0))
                    Dim fx = ""
                    Select Case retType
                        Case "unit", "item", "trigger", "timer", "player",
                                "effect", "group", "force", "trackable", "dialog", "unitpool", "itempool"
                            fx = "return Load{0}Handle({1}, StringHash({3}), GetHandleId({2}))"
                        Case "triggeraction"
                            fx = "return LoadTriggerActionHandle({1}, StringHash({3}), GetHandleId({2}))"
                        Case "unitpool"
                            fx = "return LoadUnitPoolHandle({1}, StringHash({3}), GetHandleId({2}))"
                        Case "itempool"
                            fx = "return LoadItemPoolHandle({1}, StringHash({3}), GetHandleId({2}))"
                        Case Else
                            fx = "call BJDebugMsg(""Illegal return conversion operation. Police en route."")" + Environment.NewLine + "return null"
                    End Select
                    writer.WriteLine(fx.Frmt(retType.Substring(0, 1).ToUpper + retType.Substring(1), dataName, words(4), words(7)))
                    writer.WriteLine("endfunction")
                    lineBuffer.RemoveRange(0, 4)
                ElseIf lineBuffer(0) Like "function * takes handle *,*string *,*handle * returns nothing*" AndAlso
                                    lineBuffer(1) Like "if *==*null*then" AndAlso
                                    lineBuffer(2) Like "call FlushStoredInteger*(*,*,*)*" AndAlso
                                    lineBuffer(3) Like "else*" AndAlso
                                    lineBuffer(4) Like "call StoreInteger(*,*(*)*,*,*(*)*)*" AndAlso
                                    lineBuffer(5) Like "endif*" AndAlso
                                    lineBuffer(6) Like "endfunction*" Then
                    'end of handlevars SetHandleHandle
                    lineBuffer(0) = lineBuffer(0).Replace(",", " , ")
                    While lineBuffer(0).Contains("  ")
                        lineBuffer(0) = lineBuffer(0).Replace("  ", " ")
                    End While

                    Dim words = lineBuffer(0).Split(" "c)
                    shhName = words(1)
                    writer.WriteLine("function {0} takes handle subject, string name, agent value returns nothing".Frmt(shhName))
                    writer.WriteLine("call SaveAgentHandle({0}, StringHash(name), GetHandleId(subject), value)".Frmt(dataName))
                    writer.WriteLine("endfunction")
                    writer.WriteLine("function SetHandleLightning takes handle subject, string name, lightning value returns nothing")
                    writer.WriteLine("call SaveLightningHandle({0}, StringHash(name), GetHandleId(subject), value)".Frmt(dataName))
                    writer.WriteLine("endfunction")
                    lineBuffer.RemoveRange(0, 7)
                ElseIf shhName IsNot Nothing AndAlso lineBuffer(0) Like "call {0}(*)*".Frmt(shhName) AndAlso lineBuffer(0).ToLower Like "*lightning*" Then
                    writer.WriteLine("call SetHandleLightning{0}".Frmt(lineBuffer(0).Substring("call ".Length + shhName.Length)))
                    lineBuffer.RemoveRange(0, 1)
                Else
                    writer.WriteLine(lineBuffer(0))
                    If lineBuffer(0) Like "globals*" Then
                        writer.WriteLine("hashtable {0} = InitHashtable()".Frmt(dataName))
                    End If
                End If
            End While

            For Each line In lineBuffer
                writer.WriteLine(line)
            Next line
        End Sub
        Public Sub FixMap(ByVal path As String)
            IO.File.Delete(path & ".temp")
            IO.File.Delete(path & ".copy.w3x")
            IO.File.Delete(path & ".j")
            IO.File.Copy(path, path & ".temp")

            Dim archive = New Mpq.MpqArchive(path & ".temp", IO.FileAccess.ReadWrite, IO.FileShare.ReadWrite)
            Dim j = If(archive.hashTable.contains("war3map.j"), "war3map.j", "scripts\war3map.j")
            Using file = archive.OpenFile(j)
                Using reader = New IO.StreamReader(file)
                    Using writer = New IO.StreamWriter(New IO.FileStream(path & ".j", IO.FileMode.CreateNew, IO.FileAccess.Write, IO.FileShare.None))
                        FixJ(reader, writer)
                    End Using
                End Using
            End Using

            archive.MarkFileAsRemoved(j)
            Using f = New IO.FileStream(path & ".j", IO.FileMode.Open, IO.FileAccess.Read)
                archive.MarkFileAsAddedAndAppendData("war3map.j", f)
            End Using
            Using f = New IO.FileStream(path & ".copy.w3x", IO.FileMode.CreateNew, IO.FileAccess.Write)
                archive.RepackInto(f)
            End Using

            IO.File.Delete(path & ".temp")
        End Sub
        Public Sub FixPred()
            FixMap("C:\Program Files (x86)\Warcraft III\Maps\Test\pred.mpq")
        End Sub
    End Module
End Namespace
