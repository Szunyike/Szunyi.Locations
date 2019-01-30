Imports Bio.IO.GenBank

Imports System.Runtime.CompilerServices
Imports System.Windows.Forms

Module Location_Extension
    <Extension()>
    Public Function TSS(Location As Bio.IO.GenBank.Location) As Integer
        Try
            If Location.Operator = LocationOperator.Complement Then
                Return Location.LocationEnd
            ElseIf Location.Operator = LocationOperator.Join AndAlso Location.SubLocations.Count > 0 AndAlso Location.SubLocations.First.Operator = LocationOperator.Complement Then
                Return Location.LocationEnd
            Else
                Return Location.LocationStart
            End If
        Catch ex As Exception
            Dim kj As Int16 = 56
        End Try

    End Function
    <Extension()>
    Public Function PAS(Location As Bio.IO.GenBank.Location) As Integer
        If Location.Operator = LocationOperator.Complement Then
            Return Location.LocationStart
        ElseIf Location.Operator = LocationOperator.Join AndAlso Location.SubLocations.Count > 0 AndAlso Location.SubLocations.First.Operator = LocationOperator.Complement Then
            Return Location.LocationStart
        Else
            Return Location.LocationEnd
        End If
    End Function
    <Extension()>
    Public Function IsComplementer(Location As Bio.IO.GenBank.Location) As Boolean
        If Location.Operator = LocationOperator.Complement Then
            Return True
        ElseIf Location.Operator = LocationOperator.Join AndAlso Location.SubLocations.Count > 0 AndAlso Location.SubLocations.First.Operator = LocationOperator.Complement Then
            Return True
        Else
            Return False
        End If
    End Function
    <Extension()>
    Public Function Current_Position(Location As Bio.IO.GenBank.Location, Loc_By As Locations_By) As Integer
        Select Case Loc_By
            Case Locations_By.LE
                Return Location.LocationEnd
            Case Locations_By.LS
                Return Location.LocationStart
            Case Locations_By.PAS
                Return Location.PAS
            Case Locations_By.TSS
                Return Location.TSS
            Case Else
                Return 0
        End Select

    End Function

    <Extension()>
    Public Function TSS(Location As Bio.IO.GenBank.ILocation) As Integer
        If Location.Operator = LocationOperator.Complement Then
            Return Location.LocationEnd
        ElseIf Location.Operator = LocationOperator.Join AndAlso Location.SubLocations.Count > 0 AndAlso Location.SubLocations.First.Operator = LocationOperator.Complement Then
            Return Location.LocationEnd
        Else
            Return Location.LocationStart
        End If
    End Function
    <Extension()>
    Public Function PAS(Location As Bio.IO.GenBank.ILocation) As Integer
        If Location.Operator = LocationOperator.Complement Then
            Return Location.LocationStart
        ElseIf Location.Operator = LocationOperator.Join AndAlso Location.SubLocations.Count > 0 AndAlso Location.SubLocations.First.Operator = LocationOperator.Complement Then
            Return Location.LocationStart
        Else
            Return Location.LocationEnd
        End If
    End Function
    <Extension()>
    Public Function IsComplementer(Location As Bio.IO.GenBank.ILocation) As Boolean
        If Location.Operator = LocationOperator.Complement Then
            Return True
        ElseIf Location.Operator = LocationOperator.Join AndAlso Location.SubLocations.Count > 0 AndAlso Location.SubLocations.First.Operator = LocationOperator.Complement Then
            Return True
        Else
            Return False
        End If
    End Function
    <Extension()>
    Public Function Current_Position(Location As Bio.IO.GenBank.ILocation, Loc_By As Locations_By) As Integer
        Select Case Loc_By
            Case Locations_By.LE
                Return Location.LocationEnd
            Case Locations_By.LS
                Return Location.LocationStart
            Case Locations_By.PAS
                Return Location.PAS
            Case Locations_By.TSS
                Return Location.TSS
            Case Else
                Return 0
        End Select
    End Function
End Module

<Flags()>
Public Enum Locations_By As Integer
    TSS = 1
    PAS = 2
    LS = 4
    LE = 8
    Intron = 16

End Enum
'  is_done = is_done Or todo.wash_covers

Public Class OverLapping_Locations
    Public Class wLength
        Public Shared Function Get_Maximal_Overlapping_Features(Basic As FeatureItem, Features As List(Of FeatureItem), wOrientation As Boolean) As List(Of FeatureItem)
            Dim All As New List(Of Dictionary(Of Integer, List(Of FeatureItem)))
            All.Add(Get_5_Prime_Overhangs(Basic, Features, wOrientation))
            All.Add(Get_3_Prime_Overhangs(Basic, Features, wOrientation))
            All.Add(Get_Inner_Feature_Items(Basic, Features, wOrientation))

            Return Merge(All)
        End Function
        Private Shared Function Merge(All As List(Of Dictionary(Of Integer, List(Of FeatureItem)))) As List(Of FeatureItem)
            Dim out As New Dictionary(Of Integer, List(Of FeatureItem))
            For Each ls In All
                For Each Item In ls
                    If out.ContainsKey(Item.Key) = False Then out.Add(Item.Key, New List(Of FeatureItem))
                    out(Item.Key).AddRange(Item.Value)
                Next
            Next
            Dim Keys = out.Keys
            Dim max = Keys.Max
            Return out(max)
        End Function


        Public Shared Function Get_Inner_Feature_Items(Basic As FeatureItem, Features As List(Of FeatureItem), wOrientation As Boolean) As Dictionary(Of Integer, List(Of FeatureItem))
            Dim out = From x In Features Where x.Location.LocationStart >= Basic.Location.LocationStart And
                                         x.Location.LocationEnd <= Basic.Location.LocationEnd

            Dim res As New Dictionary(Of Integer, List(Of FeatureItem))
            For Each Item In out
                Dim OL_Length = Item.Location.LocationEnd - Item.Location.LocationStart
                If wOrientation = True Then
                    If Item.Location.IsComplementer = Basic.Location.IsComplementer Then
                        If res.ContainsKey(OL_Length) = False Then res.Add(OL_Length, New List(Of FeatureItem))
                        res(OL_Length).Add(Item)
                    End If
                Else
                    If res.ContainsKey(OL_Length) = False Then res.Add(OL_Length, New List(Of FeatureItem))
                    res(OL_Length).Add(Item)
                End If
            Next
            Return res

        End Function
        Public Shared Function Get_5_Prime_Overhangs(basic As FeatureItem, Features As List(Of FeatureItem), wOrientation As Boolean) As Dictionary(Of Integer, List(Of FeatureItem))
            Dim out = From x In Features Where x.Location.LocationStart <= basic.Location.LocationStart And
                                         x.Location.LocationEnd <= basic.Location.LocationEnd And
                                         x.Location.LocationEnd > basic.Location.LocationStart

            Dim res As New Dictionary(Of Integer, List(Of FeatureItem))
            For Each Item In out
                Dim OL_Length = Item.Location.LocationEnd - basic.Location.LocationStart
                If wOrientation = True Then
                    If Item.Location.IsComplementer = basic.Location.IsComplementer Then
                        If res.ContainsKey(OL_Length) = False Then res.Add(OL_Length, New List(Of FeatureItem))
                        res(OL_Length).Add(Item)
                    End If
                Else
                    If res.ContainsKey(OL_Length) = False Then res.Add(OL_Length, New List(Of FeatureItem))
                    res(OL_Length).Add(Item)
                End If
            Next
            Return res

        End Function
        Public Shared Function Get_3_Prime_Overhangs(basic As FeatureItem, Features As List(Of FeatureItem), wOrientation As Boolean) As Dictionary(Of Integer, List(Of FeatureItem))
            Dim out = From x In Features Where x.Location.LocationStart > basic.Location.LocationStart And
                                         x.Location.LocationEnd > basic.Location.LocationEnd And
                                         x.Location.LocationStart < basic.Location.LocationEnd

            Dim res As New Dictionary(Of Integer, List(Of FeatureItem))
            For Each Item In out
                Dim OL_Length = basic.Location.LocationEnd - Item.Location.LocationStart
                If wOrientation = True Then
                    If Item.Location.IsComplementer = basic.Location.IsComplementer Then
                        If res.ContainsKey(OL_Length) = False Then res.Add(OL_Length, New List(Of FeatureItem))
                        res(OL_Length).Add(Item)
                    End If
                Else
                    If res.ContainsKey(OL_Length) = False Then res.Add(OL_Length, New List(Of FeatureItem))
                    res(OL_Length).Add(Item)
                End If
            Next
            Return res

        End Function

    End Class
    ''' <summary>
    ''' 'Basic is longer then 
    ''' </summary>
    ''' <param name="Basic"></param>
    ''' <param name="Features"></param>
    ''' <returns></returns>
    Public Shared Function Get_Inner_Feature_Items_wOrientation(Basic As FeatureItem, Features As List(Of FeatureItem)) As List(Of FeatureItem)
        Dim out = From x In Features Where x.Location.LocationStart >= Basic.Location.LocationStart And
                                         x.Location.LocationEnd <= Basic.Location.LocationEnd And
                                            Basic.Location.IsComplementer = x.Location.IsComplementer

        If out.Count > 0 Then Return out.ToList
        Return New List(Of FeatureItem)

    End Function
    ''' <summary>
    ''' 'Basic is longer then 
    ''' </summary>
    ''' <param name="Basic"></param>
    ''' <param name="Features"></param>
    ''' <returns></returns>
    Public Shared Function Get_Inner_Feature_Items_woOrientation(Basic As FeatureItem, Features As List(Of FeatureItem)) As List(Of FeatureItem)
        Dim out = From x In Features Where x.Location.LocationStart >= Basic.Location.LocationStart And
                                         x.Location.LocationEnd <= Basic.Location.LocationEnd

        If out.Count > 0 Then Return out.ToList
        Return New List(Of FeatureItem)

    End Function
    Public Shared Function Get_5_Prime_Overhangs_wOrientation(basic As FeatureItem, Features As List(Of FeatureItem)) As List(Of FeatureItem)
        Dim out = From x In Features Where x.Location.LocationStart < basic.Location.LocationStart And
                                         x.Location.LocationEnd < basic.Location.LocationEnd And
                                         x.Location.LocationEnd >= basic.Location.LocationStart And
                                         basic.Location.IsComplementer = x.Location.IsComplementer
                  Order By x.Location.LocationEnd - basic.Location.LocationStart Descending

        If out.Count > 0 Then Return out.ToList
        Return New List(Of FeatureItem)

    End Function
    Public Shared Function Get_5_Prime_Overhangs_woOrientation(basic As FeatureItem, Features As List(Of FeatureItem)) As List(Of FeatureItem)
        Dim out = From x In Features Where x.Location.LocationStart < basic.Location.LocationStart And
                                         x.Location.LocationEnd < basic.Location.LocationEnd And
                                         x.Location.LocationEnd > basic.Location.LocationStart
                  Order By x.Location.LocationEnd - basic.Location.LocationStart Descending

        If out.Count > 0 Then Return out.ToList
        Return New List(Of FeatureItem)

    End Function
    Public Shared Function Get_3_Prime_Overhangs_wOrientation(basic As FeatureItem, Features As List(Of FeatureItem)) As List(Of FeatureItem)
        Dim out = From x In Features Where x.Location.LocationStart > basic.Location.LocationStart And
                                         x.Location.LocationEnd > basic.Location.LocationEnd And
                                         x.Location.LocationStart < basic.Location.LocationEnd And
                                         basic.Location.IsComplementer = x.Location.IsComplementer
                  Order By basic.Location.LocationEnd - x.Location.LocationStart Descending

        If out.Count > 0 Then Return out.ToList
        Return New List(Of FeatureItem)

    End Function
    Public Shared Function Get_3_Prime_Overhangs_woOrientation(basic As FeatureItem, Features As List(Of FeatureItem)) As List(Of FeatureItem)
        Dim out = From x In Features Where x.Location.LocationStart > basic.Location.LocationStart And
                                         x.Location.LocationEnd > basic.Location.LocationEnd And
                                         x.Location.LocationStart < basic.Location.LocationEnd
                  Order By basic.Location.LocationEnd - x.Location.LocationStart Descending

        If out.Count > 0 Then Return out.ToList
        Return New List(Of FeatureItem)

    End Function
    Public Shared Function Get_5_and_3_Prime_Overhangs(basic As FeatureItem, Features As List(Of FeatureItem)) As List(Of FeatureItem)
        Dim out = From x In Features Where x.Location.LocationStart < basic.Location.LocationStart And x.Location.LocationEnd > basic.Location.LocationEnd And basic.Location.Operator = x.Location.Operator

        If out.Count > 0 Then Return out.ToList
        Return New List(Of FeatureItem)
    End Function
    Public Shared Function Get_Biggers_Overhangs_wOrientation(basic As FeatureItem, Features As List(Of FeatureItem)) As List(Of FeatureItem)
        Dim out = From x In Features Where x.Location.LocationStart < basic.Location.LocationStart And
                                         x.Location.LocationEnd > basic.Location.LocationEnd And
                                         basic.Location.IsComplementer = x.Location.IsComplementer

        If out.Count > 0 Then Return out.ToList
        Return New List(Of FeatureItem)
    End Function
    Public Shared Function Get_Biggers_Overhangs_woOrientation(basic As FeatureItem, Features As List(Of FeatureItem)) As List(Of FeatureItem)
        Dim out = From x In Features Where x.Location.LocationStart < basic.Location.LocationStart And
                                         x.Location.LocationEnd > basic.Location.LocationEnd

        If out.Count > 0 Then Return out.ToList
        Return New List(Of FeatureItem)
    End Function
    Public Shared Function Get_All_Overlaps_Features(basic As FeatureItem, Features As List(Of FeatureItem)) As List(Of FeatureItem)
        Dim out As New List(Of FeatureItem)
        out.AddRange(Get_Inner_Feature_Items_wOrientation(basic, Features))
        out.AddRange(Get_5_Prime_Overhangs_wOrientation(basic, Features))
        out.AddRange(Get_3_Prime_Overhangs_wOrientation(basic, Features))
        out.AddRange(Get_Biggers_Overhangs_wOrientation(basic, Features))

        Dim sg = out.Distinct.ToList

        Return sg
    End Function

    Public Shared Function Get_Longest_OverLapping_Item_wOrientation(tr As FeatureItem, Features As List(Of FeatureItem)) As FeatureItem
        Dim F = Get_5_Prime_Overhangs_wOrientation(tr, Features)
        Dim T = Get_3_Prime_Overhangs_wOrientation(tr, Features)
        Dim I = Get_Inner_Feature_Items_wOrientation(tr, Features)
        Dim B = Get_Biggers_Overhangs_wOrientation(tr, Features)

        If F.Count = 0 And T.Count = 0 Then
            If I.Count Or B.Count > 0 Then
                Return B.First
            Else
                Dim res = From x In Features Where x.Location.TSS <= tr.Location.TSS And x.Location.PAS >= tr.Location.PAS And tr.Location.IsComplementer = x.Location.IsComplementer

                If res.Count > 0 Then
                    Return res.First
                Else
                    Return Nothing
                End If
            End If
        End If
        If F.Count > 0 Then
            If T.Count = 0 Then
                Return F.First
            Else
                If F.First.Location.LocationEnd - tr.Location.LocationStart >= tr.Location.LocationEnd - T.First.Location.LocationStart Then
                    Return F.First
                Else
                    Return T.First
                End If
            End If
        End If
        Return T.First
    End Function
    Public Shared Function Get_Longest_OverLapping_Item_woOrientation(tr As FeatureItem, Features As List(Of FeatureItem)) As FeatureItem
        Dim F = Get_5_Prime_Overhangs_woOrientation(tr, Features)
        Dim T = Get_3_Prime_Overhangs_woOrientation(tr, Features)
        Dim I = Get_Inner_Feature_Items_woOrientation(tr, Features)
        Dim B = Get_Biggers_Overhangs_woOrientation(tr, Features)

        If F.Count = 0 And T.Count = 0 Then
            If I.Count > 0 Then
                Return I.First
            End If
            If B.Count > 0 Then
                Return B.First
            End If
            Dim res = From x In Features Where x.Location.LocationStart <= tr.Location.LocationStart And x.Location.LocationEnd >= tr.Location.LocationEnd
            If res.Count > 0 Then
                Return res.First
            Else
                Return Nothing
            End If

        End If
        If F.Count > 0 Then
            If T.Count = 0 Then
                Return F.First
            Else
                If F.First.Location.LocationEnd - tr.Location.LocationStart >= tr.Location.LocationEnd - T.First.Location.LocationStart Then
                    Return F.First
                Else
                    Return T.First
                End If
            End If
        End If
        Return T.First
    End Function

    ''' <summary>
    ''' Retrun empty List , or location groups are not overlappings
    ''' </summary>
    ''' <param name="potential_Introns_Locations"></param>
    ''' <returns></returns>
    Public Shared Iterator Function Get_Non_OverLappingGroups(potential_Introns_Locations As IEnumerable(Of ILocation)) As IEnumerable(Of List(Of ILocation))

        Select Case potential_Introns_Locations.Count
            Case 0
                Yield New List(Of ILocation)
            Case 1
                Yield potential_Introns_Locations.ToList
            Case Else
                potential_Introns_Locations = From x In potential_Introns_Locations Order By x.LocationStart Ascending
                Dim t = potential_Introns_Locations.ToList
                Dim OLs = Get_Overlapping_Locis(t)
                Dim res As New List(Of TreeNode)
                Dim cIndex As Integer = 0
                For cIndex = 0 To t.Count - 1

                    Dim x As New Windows.Forms.TreeNode(cIndex)
                    res.Add(GetGraph(t, OLs, x, cIndex))
                Next
                Dim out As New List(Of String)
                For Each Node In res
                    Dim sg As New List(Of String)
                    sg.Add(Node.Text)
                    Dim Ls As New List(Of String)
                    out.AddRange(Get_Node_FullText(Node, sg, Ls))
                Next
                For Each item In out
                    Dim s = Split(item, vbTab)
                    Dim kk As New List(Of ILocation)
                    For Each s1 In s
                        kk.Add(t(s1))
                    Next
                    Yield kk
                Next
                Dim kj As Int16 = 54
        End Select

    End Function
    Public Shared Function Get_Node_FullText(nodes As System.Windows.Forms.TreeNode, t As List(Of String), ls As List(Of String)) As List(Of String)

        For Each node As System.Windows.Forms.TreeNode In nodes.Nodes
            If node.Text = "" Then
                ls.Add(Szunyi.Common.Text.General.GetText(t, vbTab))

            Else
                t.Add(node.Text)
                Get_Node_FullText(node, t, ls)
                t.RemoveAt(t.Count - 1)
            End If

        Next
        Return ls
    End Function
    Private Shared Function GetGraph(Locis As List(Of ILocation), OLs As Dictionary(Of ILocation, List(Of Integer)), res As TreeNode, cIndex As Integer) As Windows.Forms.TreeNode
        res.Nodes.Add(New System.Windows.Forms.TreeNode("")) ' Close
        For i1 = cIndex + 1 To Locis.Count - 1
            If OLs(Locis(cIndex)).Contains(i1) = True Then
                ' Do Nothing It is ovelapping
            Else
                res.Nodes.Add(New Windows.Forms.TreeNode(i1))
                GetGraph(Locis, OLs, res.Nodes.Item(res.Nodes.Count - 1), i1)
            End If

        Next
        Return res
    End Function
    Private Shared Function Get_Overlapping_Locis(Locis As List(Of ILocation)) As Dictionary(Of ILocation, List(Of Integer))
        Dim out As New Dictionary(Of ILocation, List(Of Integer))
        For Each Loci In Locis
            out.Add(Loci, New List(Of Integer))
            Dim t = From x In Locis Where (x.LocationStart >= Loci.LocationStart And x.LocationStart <= Loci.LocationEnd) Or (x.LocationEnd >= Loci.LocationStart And x.LocationEnd <= Loci.LocationStart)

            For Each Item In t
                out(Loci).Add(Locis.IndexOf(Item))
            Next

        Next
        Return out
    End Function
    Private Shared Function Get_Overlapping_Locis(Locis As List(Of Bio.IO.GenBank.Location)) As Dictionary(Of ILocation, List(Of Integer))
        Dim out As New Dictionary(Of ILocation, List(Of Integer))
        For Each Loci In Locis
            out.Add(Loci, New List(Of Integer))
            Dim t = From x In Locis Where (x.LocationStart >= Loci.LocationStart And x.LocationStart <= Loci.LocationEnd) Or (x.LocationEnd >= Loci.LocationStart And x.LocationEnd <= Loci.LocationStart)

            For Each Item In t
                out(Loci).Add(Locis.IndexOf(Item))
            Next
            out(Loci).AddRange(t.ToList)
        Next
        Return out
    End Function
End Class