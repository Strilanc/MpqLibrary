Namespace Compression
    Friend Class HuffmanDecompressionStream
        Inherits DisposableWithTask
        Implements IReadableStream

        Private ReadOnly _buf As New BitBuffer()
        Private ReadOnly _tree As HuffmanTree
        Private ReadOnly _subStream As IReadableStream
        Private ReadOnly _isZeroTree As Boolean
        Private _finished As Boolean

        <ContractInvariantMethod()> Private Sub ObjectInvariant()
            Contract.Invariant(_buf IsNot Nothing)
            Contract.Invariant(_tree IsNot Nothing)
            Contract.Invariant(_subStream IsNot Nothing)
        End Sub

        Public Sub New(ByVal subStream As IReadableStream,
                       ByVal tree As HuffmanTree,
                       ByVal isZeroTree As Boolean)
            Contract.Requires(subStream IsNot Nothing)
            Contract.Requires(tree IsNot Nothing)
            Me._subStream = subStream
            Me._tree = tree
            Me._isZeroTree = isZeroTree
        End Sub
        Public Shared Function FromSubStream(ByVal subStream As IReadableStream) As HuffmanDecompressionStream
            Contract.Requires(subStream IsNot Nothing)
            Contract.Ensures(Contract.Result(Of HuffmanDecompressionStream)() IsNot Nothing)

            Dim treeIndex = subStream.ReadByte()
            If treeIndex >= frequencyTables.Length Then Throw New IO.InvalidDataException("Invalid huffman tree index.")
            Dim table = frequencyTables(treeIndex)
            Contract.Assume(table IsNot Nothing)
            Contract.Assume(table.Length < 256)
            Dim tree = New HuffmanTree(table)

            Return New HuffmanDecompressionStream(subStream:=subStream,
                                                  tree:=tree,
                                                  isZeroTree:=treeIndex = 0)
        End Function

        Private Function ReadNextLeaf() As HuffmanNode
            Contract.Requires(Not _finished)
            Contract.Ensures(Contract.Result(Of HuffmanNode)() IsNot Nothing)

            'Start at root
            Contract.Assume(_tree.nodes IsNot Nothing)
            Contract.Assume(_tree.nodes.Count > 0)
            Dim cur = _tree.nodes(0).Value
            'Fall based on the next bit, until at a leaf
            While cur.type = HuffmanNodeType.Internal
                If _buf.BitCount <= 0 Then _buf.QueueByte(_subStream.ReadByte)
                cur = If(_buf.TakeBit(), cur.rightChild, cur.leftChild)
                Contract.Assume(cur IsNot Nothing)
            End While
            Return cur
        End Function

        Public Function TryReadByte() As Byte?
            If _finished Then Return Nothing

            'Interpret the next leaf
            Dim leaf = ReadNextLeaf()
            Select Case leaf.type
                Case HuffmanNodeType.EndOfStream
                    _finished = True
                    Return Nothing

                Case HuffmanNodeType.NewValue
                    _buf.QueueByte(_subStream.ReadByte)
                    Dim newValue = _buf.TakeByte()
                    _tree.Increase(newValue)
                    If Not _isZeroTree Then _tree.Increase(newValue)
                    Return newValue

                Case HuffmanNodeType.Value
                    If _isZeroTree Then _tree.Increase(leaf.value)
                    Return leaf.value

                Case Else
                    Throw leaf.type.MakeImpossibleValueException()
            End Select
        End Function

        Public Function Read(ByVal maxCount As Integer) As IReadableList(Of Byte) Implements IReadableStream.Read
            Dim result = New List(Of Byte)(capacity:=maxCount)
            For i = 0 To maxCount - 1
                Dim v = TryReadByte()
                If Not v.HasValue Then Exit For
                result.Add(v.Value)
            Next i
            Return result.AsReadableList
        End Function

        Protected Overrides Function PerformDispose(ByVal finalizing As Boolean) As System.Threading.Tasks.Task
            If finalizing Then Return Nothing
            _subStream.Dispose()
            Return Nothing
        End Function
    End Class

    Friend Enum HuffmanNodeType
        Internal
        Value
        EndOfStream
        NewValue
    End Enum
    Friend Class HuffmanNode
        Public leftChild As HuffmanNode
        Public rightChild As HuffmanNode
        Public parent As HuffmanNode
        Public value As Byte
        Public freq As UInteger
        Public type As HuffmanNodeType

        Public Sub New(ByVal type As HuffmanNodeType, ByVal freq As UInteger)
            Me.type = type
            Me.freq = freq
        End Sub
        Public Sub New(ByVal value As Byte, ByVal freq As UInteger)
            Me.value = value
            Me.freq = freq
            Me.type = HuffmanNodeType.Value
        End Sub
        Public Sub New(ByVal leftChild As HuffmanNode, ByVal rightChild As HuffmanNode)
            Contract.Requires(leftChild IsNot Nothing)
            Contract.Requires(rightChild IsNot Nothing)
            Me.leftChild = leftChild
            Me.rightChild = rightChild
            Me.leftChild.parent = Me
            Me.rightChild.parent = Me
            Me.freq = leftChild.freq + rightChild.freq
            Me.type = HuffmanNodeType.Internal
        End Sub
    End Class

    '''<summary>A binary tree with the property that the sum over the leafs of depth*frequency is minimized</summary>
    Friend Class HuffmanTree
        Public ReadOnly nodes As New List(Of NonNull(Of HuffmanNode)) 'sorted list of the nodes in the tree
        Public ReadOnly leafMap As New Dictionary(Of Integer, NonNull(Of HuffmanNode)) 'takes a value and gives the leaf containing that value

        <ContractInvariantMethod()> Private Sub ObjectInvariant()
            Contract.Invariant(nodes IsNot Nothing)
            Contract.Invariant(nodes.Count > 0)
            Contract.Invariant(leafMap IsNot Nothing)
        End Sub

        '''<summary>Constructs the initial tree using the given frequency table</summary>
        Public Sub New(ByVal freqTable As UInteger())
            Contract.Requires(freqTable IsNot Nothing)
            Contract.Requires(freqTable.Length < 256)
            'leafs
            For i = 0 To freqTable.Length - 1
                If freqTable(i) > 0 Then
                    Insert(New HuffmanNode(CByte(i), freqTable(i)))
                End If
            Next i
            'special leafs
            Insert(New HuffmanNode(HuffmanNodeType.EndOfStream, 1))
            Insert(New HuffmanNode(HuffmanNodeType.NewValue, 1))
            'tree
            For i = nodes.Count - 1 To 1 Step -1
                Insert(New HuffmanNode(nodes(i), nodes(i - 1)))
            Next i 'decrementing by 1 is enough because the new node shifted the list
        End Sub

        '''<summary>Adds the node to the sorted list of nodes, and adds leafs to the value map</summary>
        Private Sub Insert(ByVal n As HuffmanNode)
            Contract.Requires(n IsNot Nothing)
            Contract.Ensures(nodes.Count > 0)
            If n.value <> -1 Then leafMap(n.value) = n
            For i = 0 To nodes.Count - 1
                If n.freq > nodes(i).Value.freq Then
                    nodes.Insert(i, n)
                    Return
                End If
            Next i
            nodes.Add(n)
        End Sub

        '''<summary>Increases the frequency of the given value, and updates the tree to maintain optimality</summary>
        Public Sub Increase(ByVal val As Byte)
            Dim n As HuffmanNode

            'Create a new node for the value if it isn't even in the tree
            If Not leafMap.ContainsKey(val) Then
                'add the new value node by pairing it with the lowest frequency node
                '[this transformation maintains the optimality of the tree]
                n = New HuffmanNode(val, 0)
                leafMap(val) = n
                Dim sibling = nodes(nodes.Count - 1).Value
                Dim grandparent = sibling.parent
                Contract.Assume(grandparent IsNot Nothing)
                Dim parent = New HuffmanNode(n, sibling)
                parent.parent = grandparent
                If grandparent.rightChild Is sibling Then grandparent.rightChild = parent Else grandparent.leftChild = parent
                nodes(nodes.Count - 1) = parent
                nodes.Add(sibling)
                nodes.Add(n)
            End If

            'Get the leaf with the value to increase
            n = leafMap(val)

            'Increase the frequency of the leaf and its ancestors and restructure the tree to match
            While n IsNot Nothing
                n.freq += 1UI
                'find the new position the node must occupy in the ordered list
                Dim i = nodes.IndexOf(n) - 1
                While i >= 0 AndAlso nodes(i).Value.freq < n.freq
                    i -= 1
                End While
                i += 1
                'if the node has to change positions, we need to update the tree and the list
                Contract.Assume(i >= 0)
                Dim m = nodes(i).Value
                If m IsNot n Then
                    'switch places in list
                    nodes(nodes.IndexOf(n)) = m
                    nodes(i) = n
                    'switch subtrees rooted at n and m
                    '[note that m has the same frequency as n used to have, so m's new ancestors will not need to be updated]
                    Dim t As HuffmanNode
                    If m.parent Is n.parent Then
                        Contract.Assume(m.parent IsNot Nothing)
                        t = m.parent.rightChild
                        m.parent.rightChild = m.parent.leftChild
                        m.parent.leftChild = t
                    Else
                        If m.parent IsNot Nothing Then If m.parent.rightChild Is m Then m.parent.rightChild = n Else m.parent.leftChild = n
                        If n.parent IsNot Nothing Then If n.parent.rightChild Is n Then n.parent.rightChild = m Else n.parent.leftChild = m
                    End If
                    t = m.parent
                    m.parent = n.parent
                    n.parent = t
                End If
                'repeat for ancestors
                n = n.parent
            End While
        End Sub
    End Class

    Friend Module HuffmanData
        '''<summary>The frequency tables used to construct the initial huffman trees</summary>
        Friend ReadOnly frequencyTables()() As UInteger = {
            New UInteger() {
                10, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2
            },
            New UInteger() {
                84, 22, 22, 13, 12, 8, 6, 5, 6, 5, 6, 3, 4, 4, 3, 5, 14, 11, 20, 19, 19, 9, 11, 6, 5, 4, 3, 2, 3, 2, 2, 2,
                13, 7, 9, 6, 6, 4, 3, 2, 4, 3, 3, 3, 3, 3, 2, 2, 9, 6, 4, 4, 4, 4, 3, 2, 3, 2, 2, 2, 2, 3, 2, 4,
                8, 3, 4, 7, 9, 5, 3, 3, 3, 3, 2, 2, 2, 3, 2, 2, 3, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 2, 1, 2, 2,
                6, 10, 8, 8, 6, 7, 4, 3, 4, 4, 2, 2, 4, 2, 3, 3, 4, 3, 7, 7, 9, 6, 4, 3, 3, 2, 1, 2, 2, 2, 2, 2,
                10, 2, 2, 3, 2, 2, 1, 1, 2, 2, 2, 6, 3, 5, 2, 3, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 3, 1, 1, 1,
                2, 1, 1, 1, 1, 1, 1, 2, 4, 4, 4, 7, 9, 8, 12, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 3,
                4, 1, 2, 4, 5, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 4, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                2, 1, 1, 1, 1, 1, 1, 1, 3, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 2, 2, 1, 1, 2, 2, 2, 6, 75
            },
            New UInteger() {
                0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 39, 0, 0, 35, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                255, 1, 1, 1, 1, 1, 1, 1, 2, 2, 1, 1, 6, 14, 16, 4, 6, 8, 5, 4, 4, 3, 3, 2, 2, 3, 3, 1, 1, 2, 1, 1,
                1, 4, 2, 4, 2, 2, 2, 1, 1, 4, 1, 1, 2, 3, 3, 2, 3, 1, 3, 6, 4, 1, 1, 1, 1, 1, 1, 2, 1, 2, 1, 1,
                1, 41, 7, 22, 18, 64, 10, 10, 17, 37, 1, 3, 23, 16, 38, 42, 16, 1, 35, 35, 47, 16, 6, 7, 2, 9, 1, 1, 1, 1, 1
            },
            New UInteger() {
                255, 11, 7, 5, 11, 2, 2, 2, 6, 2, 2, 1, 4, 2, 1, 3, 9, 1, 1, 1, 3, 4, 1, 1, 2, 1, 1, 1, 2, 1, 1, 1,
                5, 1, 1, 1, 13, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 3, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1,
                10, 4, 2, 1, 6, 3, 2, 1, 1, 1, 1, 1, 3, 1, 1, 1, 5, 2, 3, 4, 3, 3, 3, 2, 1, 1, 1, 2, 1, 2, 3, 3,
                1, 3, 1, 1, 2, 5, 1, 1, 4, 3, 5, 1, 3, 1, 3, 3, 2, 1, 4, 3, 10, 6, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                2, 2, 1, 10, 2, 5, 1, 1, 2, 7, 2, 23, 1, 5, 1, 1, 14, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                6, 2, 1, 4, 5, 1, 1, 2, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                1, 1, 1, 1, 1, 1, 1, 1, 7, 1, 1, 2, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 17
            },
            New UInteger() {
                255, 251, 152, 154, 132, 133, 99, 100, 62, 62, 34, 34, 19, 19, 24, 23
            },
            New UInteger() {
                255, 241, 157, 158, 154, 155, 154, 151, 147, 147, 140, 142, 134, 136, 128, 130,
                124, 124, 114, 115, 105, 107, 95, 96, 85, 86, 74, 75, 64, 65, 55, 55,
                47, 47, 39, 39, 33, 33, 27, 28, 23, 23, 19, 19, 16, 16, 13, 13,
                11, 11, 9, 9, 8, 8, 7, 7, 6, 5, 5, 4, 4, 4, 25, 24
            },
            New UInteger() {
                195, 203, 245, 65, 255, 123, 247, 33, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                191, 204, 242, 64, 253, 124, 247, 34, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                122, 70
            },
            New UInteger() {
                195, 217, 239, 61, 249, 124, 233, 30, 253, 171, 241, 44, 252, 91, 254, 23,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                189, 217, 236, 61, 245, 125, 232, 29, 251, 174, 240, 44, 251, 92, 255, 24,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                112, 108
            },
            New UInteger() {
                186, 197, 218, 51, 227, 109, 216, 24, 229, 148, 218, 35, 223, 74, 209, 16,
                238, 175, 228, 44, 234, 90, 222, 21, 244, 135, 233, 33, 246, 67, 252, 18,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                176, 199, 216, 51, 227, 107, 214, 24, 231, 149, 216, 35, 219, 73, 208, 17,
                233, 178, 226, 43, 232, 92, 221, 21, 241, 135, 231, 32, 247, 68, 255, 19,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                95, 158
            }
        }
    End Module
End Namespace
