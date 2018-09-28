Imports Bio.IO.GenBank
Imports System.Runtime.CompilerServices
Module Location_Extension
    <Extension()>
    Public Function TSS(Location As Bio.IO.GenBank.Location) As Integer
        If Location.Operator = LocationOperator.Complement Then
            Return Location.LocationEnd
        ElseIf Location.Operator = LocationOperator.Join AndAlso Location.SubLocations.Count > 0 AndAlso Location.SubLocations.First.Operator = LocationOperator.Complement Then
            Return Location.LocationEnd
        Else
            Return Location.LocationStart
        End If
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

End Module

Public Class Basic_Location
    Public Property IsComplementer As Boolean
    Public Property Location As Bio.IO.GenBank.Location
    Public Property Obj As Object
    Public Property SeqID As String
    Public Sub New(Location As Bio.IO.GenBank.Location)
        Me.Location = Location

    End Sub
    Public Sub New(Location As Bio.IO.GenBank.Location, obj As Object)
        Me.Location = Location
        Me.Obj = obj
    End Sub
    Public Sub New(Location As Bio.IO.GenBank.Location, obj As Object, seq As Bio.Sequence)
        Me.Location = Location
        Me.Obj = obj
        Me.SeqID = seq.ID
    End Sub
End Class
Public Class BLs_Binary_Comparers
    Public Property TSS_wStrand As New _ByTSS_wStrand
    Public Property TSS_woStrand As New _ByTSS_woStrand
    Public Property PAS_wStrand As New _ByPAS_wStrand
    Public Property PAS_woStrand As New _ByPAS_woStrand
    Public Property LS_wStrand As New _ByStart_wStrand
    Public Property LS_woStrand As New _ByStart_woStrand
    Public Property LE_wStrand As New _ByEnd_wStrand
    Public Property LE_woStrand As New _ByEnd_woStrand

    ''' <summary.location.>
    ''' Sort/Find By.location. SeqID,OPerator and After By.location. StartPosition From Location
    ''' </summary.location.>
    Public Class _ByStart_wStrand
        Implements IComparer(Of Basic_Location)

        Public Function Compare(x As Basic_Location, y As Basic_Location) As Integer Implements IComparer(Of Basic_Location).Compare
            If x.SeqID <> y.SeqID Then Return x.SeqID.CompareTo(y.SeqID)
            If x.Location.IsComplementer <> y.Location.IsComplementer Then Return x.Location.IsComplementer.CompareTo(y.Location.IsComplementer)
            Return x.Location.LocationStart.CompareTo(y.Location.LocationStart)
        End Function


    End Class
    ''' <summary.location.>
    ''' Sort/Find By.location. Seq ID and After By.location. StartPosition From Location
    ''' </summary.location.>
    Public Class _ByEnd_wStrand
        Implements IComparer(Of Basic_Location)

        Public Function Compare(x As Basic_Location, y As Basic_Location) As Integer Implements IComparer(Of Basic_Location).Compare
            If x.SeqID <> y.SeqID Then Return x.SeqID.CompareTo(y.SeqID)
            If x.Location.IsComplementer <> y.Location.IsComplementer Then Return x.Location.IsComplementer.CompareTo(y.Location.IsComplementer)
            Return x.Location.LocationEnd.CompareTo(y.Location.LocationEnd)
        End Function

    End Class

    ''' <summary.location.>
    ''' Sort/Find By.location. Seq ID and After By.location.TSS From Location
    ''' </summary.location.>
    Public Class _ByTSS_wStrand
        Implements IComparer(Of Basic_Location)

        Public Function Compare(x As Basic_Location, y As Basic_Location) As Integer Implements IComparer(Of Basic_Location).Compare
            If x.SeqID <> y.SeqID Then Return x.SeqID.CompareTo(y.SeqID)
            If x.Location.IsComplementer <> y.Location.IsComplementer Then Return x.Location.IsComplementer.CompareTo(y.Location.IsComplementer)
            Return x.Location.TSS.CompareTo(y.Location.TSS)
        End Function

    End Class
    ''' <summary.location.>
    ''' Sort/Find By.location. Seq ID ,strandand After By.location.PAS From Location
    ''' </summary.location.>
    Public Class _ByPAS_wStrand
        Implements IComparer(Of Basic_Location)

        Public Function Compare(x As Basic_Location, y As Basic_Location) As Integer Implements IComparer(Of Basic_Location).Compare
            If x.SeqID <> y.SeqID Then Return x.SeqID.CompareTo(y.SeqID)
            If x.Location.IsComplementer <> y.Location.IsComplementer Then Return x.Location.IsComplementer.CompareTo(y.Location.IsComplementer)
            Return x.Location.PAS.CompareTo(y.Location.PAS)
        End Function

    End Class

    ''' <summary.location.>
    ''' Sort/Find By.location. SeqID, After By.location. StartPosition From Location
    ''' </summary.location.>
    Public Class _ByStart_woStrand
        Implements IComparer(Of Basic_Location)

        Public Function Compare(x As Basic_Location, y As Basic_Location) As Integer Implements IComparer(Of Basic_Location).Compare
            If x.SeqID <> y.SeqID Then Return x.SeqID.CompareTo(y.SeqID)
            Return x.Location.LocationEnd.CompareTo(y.Location.LocationEnd)
        End Function


    End Class
    ''' <summary.location.>
    ''' Sort/Find By.location. Seq ID and After By.location. StartPosition From Location
    ''' </summary.location.>
    Public Class _ByEnd_woStrand
        Implements IComparer(Of Basic_Location)

        Public Function Compare(x As Basic_Location, y As Basic_Location) As Integer Implements IComparer(Of Basic_Location).Compare
            If x.SeqID <> y.SeqID Then Return x.SeqID.CompareTo(y.SeqID)
            Return x.Location.LocationStart.CompareTo(y.Location.LocationStart)
        End Function

    End Class

    ''' <summary.location.>
    ''' Sort/Find By.location. Seq ID and After By.location.TSS From Location
    ''' </summary.location.>
    Public Class _ByTSS_woStrand
        Implements IComparer(Of Basic_Location)

        Public Function Compare(x As Basic_Location, y As Basic_Location) As Integer Implements IComparer(Of Basic_Location).Compare
            If x.SeqID <> y.SeqID Then Return x.SeqID.CompareTo(y.SeqID)
            Return x.Location.TSS.CompareTo(y.Location.TSS)
        End Function

    End Class
    ''' <summary.location.>
    ''' Sort/Find By.location. Seq ID ,strandand After By.location.PAS From Location
    ''' </summary.location.>
    Public Class _ByPAS_woStrand
        Implements IComparer(Of Basic_Location)

        Public Function Compare(x As Basic_Location, y As Basic_Location) As Integer Implements IComparer(Of Basic_Location).Compare
            If x.SeqID <> y.SeqID Then Return x.SeqID.CompareTo(y.SeqID)
            Return x.Location.PAS.CompareTo(y.Location.PAS)
        End Function

    End Class
End Class
Public Class Location_Finder
    Public Property All_UnSorted_BLs As List(Of Basic_Location)
    Public Property By_TSS_ As List(Of Basic_Location)
    Public Property By_PAS As List(Of Basic_Location)
    Public Property wStrand As Boolean
    Public Property wIntron As Boolean
    Public Property comps As BLs_Binary_Comparers
    Public Sub New(Locations As List(Of Basic_Location), wsStrand As Boolean, wIntron As Boolean)
        Me.comps = New BLs_Binary_Comparers
        Me.wStrand = wsStrand
        Me.wIntron = wIntron
        By_TSS_LS.AddRange(Locations)
        By_PAS_LE.AddRange(Locations)
        If Me.wStrand = True Then
            By_TSS_LS.Sort(comps.TSS_wStrand)
            By_PAS_LE.Sort(comps.PAS_wStrand)
        Else
            By_TSS_LS.Sort(comps.TSS_woStrand)
            By_PAS_LE.Sort(comps.PAS_woStrand)
        End If

    End Sub


    Public Function Get_BLs(Location As Basic_Location, width As Integer) As List(Of Basic_Location)
        Dim Locations_Near_TSS = Get_Locations_Near_TSS(Location, width, width)
        Dim Locations_Near_PAS = Get_Locations_Near_PAS(Location, width, width)
        Dim Common = Locations_Near_PAS.Intersect(Locations_Near_TSS)
        If Common.Count = 0 Then
            Return New List(Of Basic_Location)
        Else
            Return Common.ToList
        End If

    End Function
    Public Function Get_BLs(BL As Basic_Location, TSS_5 As Integer, TSS_3 As Integer, PAS_5 As Integer, PAS_3 As Integer) As List(Of Basic_Location)
        Dim Locations_Near_TSS = Get_Locations_Near_TSS(BL, TSS_5, TSS_3)
        ' TEST
        Dim Res = From x In Locations_Near_TSS Where x.Location.TSS < BL.Location.TSS - TSS_5 Or x.Location.TSS > BL.Location.TSS + TSS_5
        If Res.Count > 0 Then
            Dim kj As Int16 = 54
        End If


        Dim Locations_Near_PAS = Get_Locations_Near_PAS(BL, PAS_5, PAS_3)
        Dim Resii = From x In Locations_Near_PAS Where x.Location.PAS < BL.Location.PAS - TSS_5 Or x.Location.PAS > BL.Location.PAS + TSS_5
        If Resii.Count > 0 Then
            Dim kj As Int16 = 54
        End If
        Dim Common = Locations_Near_PAS.Intersect(Locations_Near_TSS)
        If Common.Count = 0 Then
            Return New List(Of Basic_Location)
        Else
            Return Common.ToList
        End If

    End Function
    Private Function Get_Locations_Near_PAS(Location As Basic_Location, PAS_5 As Integer, PAS_3 As Integer) As List(Of Basic_Location)
        Dim F_Location = Basic_Location_Modifier.Get_Basic_Location(Location.Location, -PAS_5, Sort_Locations_By.PAS)
        Dim First_Index As Integer
        Dim Index As Integer
        If Me.wStrand = True Then
            Index = Me.By_PAS_LE.BinarySearch(F_Location, comps.PAS_wStrand)
        Else
            Index = Me.By_PAS_LE.BinarySearch(F_Location, comps.PAS_woStrand)
        End If

        If Index < -1 Then
            First_Index = Not (Index) 'index of the first element that is larger than value or size of array
            If First_Index = Me.By_PAS_LE.Count Then
                Return New List(Of Basic_Location) ' 
            Else

            End If
        Else
            First_Index = Index
            For i1 = Index To 0 Step -1
                If Me.By_PAS_LE(i1).Location.PAS = Location.Location.PAS Then
                    First_Index = i1
                Else
                    Exit For
                End If
            Next
        End If
        Dim L_Location = Basic_Location_Modifier.Get_Basic_Location(Location.Location, PAS_3, Sort_Locations_By.PAS)
        Dim Last_Index As Integer

        If Me.wStrand = True Then
            Index = Me.By_PAS_LE.BinarySearch(L_Location, comps.PAS_wStrand)
        Else
            Index = Me.By_PAS_LE.BinarySearch(L_Location, comps.PAS_woStrand)
        End If
        If Index <= -1 Then
            Last_Index = Not (Index)
            Last_Index -= 1 'index of the first element that is larger than value or size of array

        Else
            Last_Index = Index
            For i1 = Index + 1 To Me.By_TSS_LS.Count - 1
                If Me.By_PAS_LE(i1).Location.PAS <= L_Location.Location.PAS Then
                    Last_Index = i1
                Else
                    Exit For
                End If
            Next
        End If
        If Last_Index >= First_Index Then
            Return Me.By_PAS_LE.GetRange(First_Index, Last_Index - First_Index)
        End If
        Return New List(Of Basic_Location)
    End Function

    Private Function Get_Locations_Near_TSS(Location As Basic_Location, TSS_5 As Integer, TSS_3 As Integer) As List(Of Basic_Location)
        Dim F_Location = Basic_Location_Modifier.Get_Basic_Location(Location.Location, -TSS_5, Sort_Locations_By.TSS)
        Dim First_Index As Integer
        Dim Index As Integer
        If Me.wStrand = True Then
            Index = Me.By_TSS_LS.BinarySearch(F_Location, comps.TSS_wStrand)
        Else
            Index = Me.By_TSS_LS.BinarySearch(F_Location, comps.TSS_woStrand)
        End If

        If Index < 0 Then
            First_Index = Not (Index) 'index of the first element that is larger than value or size of array
            If First_Index = Me.By_TSS_LS.Count Then
                Return New List(Of Basic_Location) ' 
            End If
        Else
            First_Index = Index
            For i1 = Index To 0 Step -1
                If Me.By_TSS_LS(i1).Location.TSS = Location.Location.TSS Then
                    First_Index = i1
                Else
                    Exit For
                End If
            Next
        End If
        Dim L_Location = Basic_Location_Modifier.Get_Basic_Location(Location.Location, TSS_3, Sort_Locations_By.TSS)
        Dim Last_Index As Integer

        If Me.wStrand = True Then
            Index = Me.By_TSS_LS.BinarySearch(L_Location, comps.TSS_wStrand)
        Else
            Index = Me.By_TSS_LS.BinarySearch(L_Location, comps.TSS_woStrand)
        End If
        If Index <= -1 Then
            Last_Index = Not (Index)
            Last_Index -= 1 'index of the first element that is larger than value or size of array

        Else
            For i1 = Index + 1 To Me.By_TSS_LS.Count - 1
                If Me.By_TSS_LS(i1).Location.TSS <= L_Location.Location.TSS Then
                    Last_Index = i1
                Else
                    Exit For
                End If
            Next
        End If
        If Last_Index >= First_Index Then
            If First_Index > -1 And Last_Index > -1 Then
                Return Me.By_TSS_LS.GetRange(First_Index, Last_Index - First_Index)
            Else
                Dim kj As Int16 = -43
            End If
        End If
        Return New List(Of Basic_Location)
    End Function


End Class
Public Class Basic_Location_Modifier
    Public Shared Property LociBuilder As New Bio.IO.GenBank.LocationBuilder
    Public Shared Function Get_Basic_Location(Location As Bio.IO.GenBank.Location, range As Integer, ExtendBy As Sort_Locations_By) As Basic_Location
        Dim l As New Location
        Select Case ExtendBy

            Case Sort_Locations_By.TSS
                l = Modify_Location(Location.TSS + range, Location.PAS, Location.IsComplementer)


            Case Sort_Locations_By.LE
                l = Modify_Location(Location.LocationStart, Location.LocationEnd + range, Location.IsComplementer)
            Case Sort_Locations_By.TSS_PAS
                l = Modify_Location(Location.LocationStart + range, Location.LocationEnd + range, Location.IsComplementer)


            Case Sort_Locations_By.PAS

                l = Modify_Location(Location.TSS, Location.PAS + range, Location.IsComplementer)


            Case Sort_Locations_By.LS

                l = Modify_Location(Location.LocationStart + range, Location.LocationEnd, Location.IsComplementer)

        End Select
        Return New Basic_Location(l)

    End Function
    Public Shared Function Modify_Location(st As Integer, endy As Integer, IsC As Boolean) As Location
        Dim s As Integer = st
        Dim e As Integer = endy
        If e < s Then
            Dim tmp = e
            e = s
            s = tmp
        End If
        If IsC = True Then
            If s < e Then

            End If
            Return LociBuilder.GetLocation("complement(" & s & ".." & e & ")")
        Else
            Return LociBuilder.GetLocation(s & ".." & e)
        End If
    End Function
End Class
Public Enum Sort_Locations_By
    TSS = 16
    PAS = 17
    LS = 18
    LE = 19
    TSS_PAS = 20
    LS_LE = 21

End Enum