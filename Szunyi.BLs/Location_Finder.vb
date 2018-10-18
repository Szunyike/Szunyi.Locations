Imports Szunyi.BLs

Public Class Basic_Location_Finder
    Public Property All_UnSorted_BLs As List(Of Basic_Location)
    Public Property By_Enum As New Dictionary(Of Locations_By, List(Of Basic_Location))

    Public Property wStrand As Boolean
    Public Property wIntron As Boolean
    Public Property comps As BLs_Binary_Comparers

    Public Sub New(Locations As IEnumerable(Of Basic_Location), wsStrand As Boolean, wIntron As Boolean)
        Me.comps = New BLs_Binary_Comparers
        Me.wStrand = wsStrand
        Me.wIntron = wIntron
        All_UnSorted_BLs = Locations

    End Sub
    Public Shared Function Get_Same_Introns(Loci As Basic_Location, Locis As IEnumerable(Of Basic_Location),
                                            Optional Length As Integer = 0) As IEnumerable(Of Basic_Location)


    End Function
    Private Function Get_Comparer(Loc_By As Locations_By)
        If Me.wStrand = True Then
            Select Case Loc_By
                Case Locations_By.TSS
                    Return Me.comps.TSS_wStrand
                Case Locations_By.PAS
                    Return Me.comps.PAS_wStrand
                Case Locations_By.LS
                    Return Me.comps.LS_wStrand
                Case Locations_By.LE
                    Return Me.comps.LE_wStrand
            End Select
        Else
            Select Case Loc_By
                Case Locations_By.TSS
                    Return Me.comps.TSS_woStrand
                Case Locations_By.PAS
                    Return Me.comps.PAS_woStrand
                Case Locations_By.LS
                    Return Me.comps.LS_woStrand
                Case Locations_By.LE
                    Return Me.comps.LE_woStrand
                Case Else
                    Return Nothing
            End Select
        End If

    End Function

    Private Function Get_Sorted(Loc_By As Locations_By) As List(Of Basic_Location)
        If Me.By_Enum.ContainsKey(Loc_By) = False Then
            Me.By_Enum.Add(Loc_By, New List(Of Basic_Location))
            Me.By_Enum(Loc_By).AddRange(Me.All_UnSorted_BLs)
            Me.By_Enum(Loc_By).Sort(Get_Comparer(Loc_By))
        End If
        Return Me.By_Enum(Loc_By)
    End Function

    Private Function Get_Item(Index As Integer, type As Locations_By) As Basic_Location
        Return Me.By_Enum(type)(Index)
    End Function
    Private Function Get_Index(loci As Basic_Location, type As Locations_By, comp As Object) As Integer
        Return Me.By_Enum(type).BinarySearch(loci, Get_Comparer(type))
    End Function
    Private Function Get_First_Index(Index As Integer, Type As Locations_By, loci As Basic_Location) As Integer
        Dim First_Index As Integer = Index
        For i1 = Index To 0 Step -1
            Dim cValue = Me.By_Enum(Type)(i1).Location.Current_Position(Type)
            Select Case Type
                Case Locations_By.TSS
                    If cValue = loci.Location.TSS Then
                        First_Index = i1
                    Else Exit For
                    End If
                Case Locations_By.PAS
                    If cValue = loci.Location.PAS Then
                        First_Index = i1
                    Else Exit For
                    End If
                Case Locations_By.LS
                    If cValue = loci.Location.LocationStart Then
                        First_Index = i1
                    Else Exit For
                    End If
                Case Locations_By.LE
                    If cValue = loci.Location.LocationEnd Then
                        First_Index = i1
                    Else Exit For
                    End If
                Case Else
                    Dim kj As Int16 = 54
            End Select
        Next
        Return First_Index
    End Function
    Private Function Get_Last_Index(Index As Integer, Type As Locations_By, loci As Basic_Location) As Integer
        Dim First_Index As Integer = Index
        For i1 = Index To Me.By_Enum(Type).Count - 1
            Dim cValue = Me.By_Enum(Type)(i1).Location.Current_Position(Type)
            Select Case Type
                Case Locations_By.TSS
                    If cValue = loci.Location.TSS Then
                        First_Index = i1
                    Else Exit For
                    End If
                Case Locations_By.PAS
                    If cValue = loci.Location.PAS Then
                        First_Index = i1
                    Else Exit For
                    End If
                Case Locations_By.LS
                    If cValue = loci.Location.LocationStart Then
                        First_Index = i1
                    Else Exit For
                    End If
                Case Locations_By.LE
                    If cValue = loci.Location.LocationEnd Then
                        First_Index = i1
                    Else Exit For
                    End If
                Case Else
                    Dim kj As Int16 = 54
            End Select
        Next
        Return First_Index
    End Function
    ''' <summary>
    ''' width 0 means exact match, width is always positive
    ''' </summary>
    ''' <param name="loci"></param>
    ''' <param name="Type"></param>
    ''' <returns></returns>
    ''' 
    Private Function Get_First_Index_equal_or_larger(loci As Basic_Location, type As Locations_By) As Integer

        Dim comp = Get_Comparer(type)
        Dim Index = Get_Index(loci, type, comp)
        If Index < 0 Then
            Return Not (Index) 'index of the first element that is larger than value or size of array
        Else
            Return Get_First_Index(Index, type, loci)
        End If



    End Function
    Private Function Get_Last_Index_equeal_or_smaller(loci As Basic_Location, Type As Locations_By) As Integer

        Dim comp = Get_Comparer(Type)
        Dim Index = Get_Index(loci, Type, comp)
        If Index < 0 Then
            Return (Not (Index)) - 1
        Else
            Return Get_Last_Index(Index, Type, loci)
        End If

    End Function

    Public Function Find_Items_byLoci(loci As Basic_Location, Width As Integer, Type As Locations_By) As List(Of Basic_Location)
        Dim Current As New List(Of Basic_Location)
        Dim F_Loci = Szunyi.BLs.Location.Common.Get_Basic_Location(loci.Location.TSS - Width, loci.Location.PAS - Width, loci.Location.Operator)
        Dim L_Loci = Szunyi.BLs.Location.Common.Get_Basic_Location(loci.Location.TSS + Width, loci.Location.PAS + Width, loci.Location.Operator)

        Dim F_Index = Get_First_Index_equal_or_larger(F_Loci, Type)
        Dim L_Index = Get_Last_Index_equeal_or_smaller(L_Loci, Type)
        Dim l = Get_List(Type)
        For i1 = F_Index To L_Index
            Current.Add(l(i1))
        Next
        Return Current
    End Function
    Public Function Find_Index_byLoci(loci As Basic_Location, Width As Integer, Type As Locations_By, Optional IsComplementer As Object = Nothing) As List(Of Integer)
        Dim Current As New List(Of Integer)
        Dim F_Loci = Szunyi.BLs.Location.Common.Get_Basic_Location(loci.Location, -Width, Type)
        Dim L_Loci = Szunyi.BLs.Location.Common.Get_Basic_Location(loci.Location, Width, Type)

        Dim F_Index = Get_First_Index_equal_or_larger(F_Loci, Type)
        Dim L_Index = Get_Last_Index_equeal_or_smaller(L_Loci, Type)
        Dim l = Get_List(Type)
        If IsNothing(IsComplementer) = True Then
            For i1 = F_Index To L_Index
                Current.Add(i1)
            Next
        Else
            For i1 = F_Index To L_Index
                Dim Item = Get_Item(i1, Type)
                If Item.Location.IsComplementer = IsComplementer Then
                    Current.Add(i1)
                End If
            Next

        End If

        Return Current
    End Function
    Private Function Get_List(Type As Locations_By) As List(Of Basic_Location)
        Return Me.By_Enum(Type)
    End Function


    Public Class Get_Locations_Near
        Public Shared Function Common(sorted As List(Of Basic_Location),
                                           f_Location As Basic_Location,
                                           l_Location As Basic_Location,
                                           c As Object, Loc_By As Locations_By) As List(Of Basic_Location)
            Dim First_Index As Integer
            Dim Index = sorted.BinarySearch(f_Location, c)
            If Index < 0 Then
                First_Index = Not (Index) 'index of the first element that is larger than value or size of array
                If First_Index = sorted.Count Then
                    Return New List(Of Basic_Location) ' 
                End If
            Else
                First_Index = Index
                For i1 = Index To 0 Step -1
                    If sorted(i1).Location.Current_Position(Loc_By) = f_Location.Location.Current_Position(Loc_By) Then
                        First_Index = i1
                    Else
                        Exit For
                    End If
                Next
            End If

            Dim Last_Index As Integer
            Index = sorted.BinarySearch(l_Location, c)

            If Index <= -1 Then
                Last_Index = Not (Index)
                Last_Index -= 1 'index of the first element that is larger than value or size of array

            Else
                For i1 = Index + 1 To sorted.Count - 1
                    If sorted(i1).Location.Current_Position(Loc_By) <= l_Location.Location.Current_Position(Loc_By) Then
                        Last_Index = i1
                    Else
                        Exit For
                    End If
                Next
            End If
            If Last_Index >= First_Index Then
                If First_Index > -1 And Last_Index > -1 Then
                    Return sorted.GetRange(First_Index, Last_Index - First_Index)
                Else
                    Dim kj As Int16 = -43
                End If
            End If
            Return New List(Of Basic_Location)

        End Function
        Public Shared Iterator Function TSS(L_Finder As Basic_Location_Finder, Loci As Basic_Location,
                                       Optional Five_Prime As Integer = 0, Optional Three_Prime As Integer = 0) As IEnumerable(Of Basic_Location)

            Dim Sorted = L_Finder.Get_Sorted(Locations_By.TSS)
            Dim F_Location = Basic_Location_Modifier.Get_Basic_Location(Loci.Location, -Five_Prime, Locations_By.TSS)
            Dim L_Location = Basic_Location_Modifier.Get_Basic_Location(Loci.Location, Three_Prime, Locations_By.TSS)
            Dim c = L_Finder.Get_Comparer(Locations_By.TSS)
            For Each Item In Common(Sorted, F_Location, L_Location, c, Locations_By.LE)
                Yield Item
            Next

        End Function



        Public Shared Iterator Function PAS(L_Finder As Basic_Location_Finder, Loci As Basic_Location,
                                       Optional Five_Prime As Integer = 0, Optional Three_Prime As Integer = 0) As IEnumerable(Of Basic_Location)

            Dim Sorted = L_Finder.Get_Sorted(Locations_By.PAS)
            Dim F_Location = Basic_Location_Modifier.Get_Basic_Location(Loci.Location, -Five_Prime, Locations_By.PAS)
            Dim L_Location = Basic_Location_Modifier.Get_Basic_Location(Loci.Location, Three_Prime, Locations_By.PAS)
            Dim c = L_Finder.Get_Comparer(Locations_By.PAS)
            For Each Item In Common(Sorted, F_Location, L_Location, c, Locations_By.LE)
                Yield Item
            Next

        End Function
        Public Shared Iterator Function LS(L_Finder As Basic_Location_Finder, Loci As Basic_Location,
                                       Optional Five_Prime As Integer = 0, Optional Three_Prime As Integer = 0) As IEnumerable(Of Basic_Location)

            Dim Sorted = L_Finder.Get_Sorted(Locations_By.LS)
            Dim F_Location = Basic_Location_Modifier.Get_Basic_Location(Loci.Location, -Five_Prime, Locations_By.LS)
            Dim L_Location = Basic_Location_Modifier.Get_Basic_Location(Loci.Location, Three_Prime, Locations_By.LS)
            Dim c = L_Finder.Get_Comparer(Locations_By.LS)
            For Each Item In Common(Sorted, F_Location, L_Location, c, Locations_By.LE)
                Yield Item
            Next
        End Function
        Public Shared Iterator Function LE(L_Finder As Basic_Location_Finder, Loci As Basic_Location,
                                       Optional Five_Prime As Integer = 0, Optional Three_Prime As Integer = 0) As IEnumerable(Of Basic_Location)

            Dim Sorted = L_Finder.Get_Sorted(Locations_By.LE)
            Dim F_Location = Basic_Location_Modifier.Get_Basic_Location(Loci.Location, -Five_Prime, Locations_By.LE)
            Dim L_Location = Basic_Location_Modifier.Get_Basic_Location(Loci.Location, Three_Prime, Locations_By.LE)
            Dim c = L_Finder.Get_Comparer(Locations_By.LE)
            For Each Item In Common(Sorted, F_Location, L_Location, c, Locations_By.LE)
                Yield Item
            Next

        End Function
        Public Shared Iterator Function TSS_PAS(L_Finder As Basic_Location_Finder, Loci As Basic_Location,
                                       Optional TSS_Five_Prime As Integer = 0, Optional TSS_Three_Prime As Integer = 0,
                                        Optional PAS_Five_Prime As Integer = 0, Optional PAS_Three_Prime As Integer = 0) As IEnumerable(Of Basic_Location)

            Dim Sorted = L_Finder.Get_Sorted(Locations_By.TSS)
            Dim F_Location = Basic_Location_Modifier.Get_Basic_Location(Loci.Location, TSS_Five_Prime, Locations_By.TSS)
            Dim L_Location = Basic_Location_Modifier.Get_Basic_Location(Loci.Location, TSS_Three_Prime, Locations_By.TSS)
            Dim c = L_Finder.Get_Comparer(Locations_By.TSS)
            Dim TSSs = Common(Sorted, F_Location, L_Location, c, Locations_By.TSS)

            Sorted = L_Finder.Get_Sorted(Locations_By.PAS)
            F_Location = Basic_Location_Modifier.Get_Basic_Location(Loci.Location, PAS_Five_Prime, Locations_By.PAS)
            L_Location = Basic_Location_Modifier.Get_Basic_Location(Loci.Location, PAS_Three_Prime, Locations_By.PAS)
            c = L_Finder.Get_Comparer(Locations_By.PAS)
            Dim PASs = Common(Sorted, F_Location, L_Location, c, Locations_By.PAS)

            For Each Item In TSSs.Intersect(PASs)
                Yield Item
            Next

        End Function
        Public Shared Iterator Function LS_LE(L_Finder As Basic_Location_Finder, Loci As Basic_Location,
                                       Optional LS_Five_Prime As Integer = 0, Optional LS_Three_Prime As Integer = 0,
                                        Optional LE_Five_Prime As Integer = 0, Optional LE_Three_Prime As Integer = 0) As IEnumerable(Of Basic_Location)

            Dim Sorted = L_Finder.Get_Sorted(Locations_By.LS)
            Dim F_Location = Basic_Location_Modifier.Get_Basic_Location(Loci.Location, LS_Five_Prime, Locations_By.LS)
            Dim L_Location = Basic_Location_Modifier.Get_Basic_Location(Loci.Location, LS_Three_Prime, Locations_By.LS)
            Dim c = L_Finder.Get_Comparer(Locations_By.LS)
            Dim LSs = Common(Sorted, F_Location, L_Location, c, Locations_By.LS)

            Sorted = L_Finder.Get_Sorted(Locations_By.LE)
            F_Location = Basic_Location_Modifier.Get_Basic_Location(Loci.Location, LE_Five_Prime, Locations_By.LE)
            L_Location = Basic_Location_Modifier.Get_Basic_Location(Loci.Location, LE_Three_Prime, Locations_By.LE)
            c = L_Finder.Get_Comparer(Locations_By.LE)
            Dim LEs = Common(Sorted, F_Location, L_Location, c, Locations_By.LE)

            For Each Item In LSs.Intersect(LEs)
                Yield Item
            Next

        End Function
    End Class

    Public Class Get_Locations_Nears

        Public Shared Function TSS(L_Finder As Basic_Location_Finder, Locis As List(Of Basic_Location),
                                       Optional Five_Prime As Integer = 0, Optional Three_Prime As Integer = 0) As List(Of List(Of Basic_Location))

            Dim Sorted = L_Finder.Get_Sorted(Locations_By.TSS)
            Dim c = L_Finder.Get_Comparer(Locations_By.TSS)

            Dim out As New List(Of List(Of Basic_Location))

            For Each Loci In Locis
                Dim F_Location = Basic_Location_Modifier.Get_Basic_Location(Loci.Location, Five_Prime, Locations_By.TSS)
                Dim L_Location = Basic_Location_Modifier.Get_Basic_Location(Loci.Location, Three_Prime, Locations_By.TSS)

                out.Add(Get_Locations_Near.Common(Sorted, F_Location, L_Location, c, Locations_By.TSS))
            Next

            Return out
        End Function
        Public Shared Function PAS(L_Finder As Basic_Location_Finder, Locis As List(Of Basic_Location),
                                        Optional Five_Prime As Integer = 0, Optional Three_Prime As Integer = 0) As List(Of List(Of Basic_Location))

            Dim Sorted = L_Finder.Get_Sorted(Locations_By.PAS)
            Dim c = L_Finder.Get_Comparer(Locations_By.PAS)

            Dim out As New List(Of List(Of Basic_Location))

            For Each Loci In Locis
                Dim F_Location = Basic_Location_Modifier.Get_Basic_Location(Loci.Location, Five_Prime, Locations_By.PAS)
                Dim L_Location = Basic_Location_Modifier.Get_Basic_Location(Loci.Location, Three_Prime, Locations_By.PAS)

                out.Add(Get_Locations_Near.Common(Sorted, F_Location, L_Location, c, Locations_By.PAS))
            Next

            Return out
        End Function

        Public Shared Function LS(L_Finder As Basic_Location_Finder, Locis As List(Of Basic_Location),
                                     Optional Five_Prime As Integer = 0, Optional Three_Prime As Integer = 0) As List(Of List(Of Basic_Location))

            Dim Sorted = L_Finder.Get_Sorted(Locations_By.LS)
            Dim c = L_Finder.Get_Comparer(Locations_By.LS)

            Dim out As New List(Of List(Of Basic_Location))

            For Each Loci In Locis
                Dim F_Location = Basic_Location_Modifier.Get_Basic_Location(Loci.Location, Five_Prime, Locations_By.LS)
                Dim L_Location = Basic_Location_Modifier.Get_Basic_Location(Loci.Location, Three_Prime, Locations_By.LS)

                out.Add(Get_Locations_Near.Common(Sorted, F_Location, L_Location, c, Locations_By.LS))
            Next

            Return out
        End Function

        Public Shared Function LE(L_Finder As Basic_Location_Finder, Locis As List(Of Basic_Location),
                                     Optional Five_Prime As Integer = 0, Optional Three_Prime As Integer = 0) As List(Of List(Of Basic_Location))

            Dim Sorted = L_Finder.Get_Sorted(Locations_By.LE)
            Dim c = L_Finder.Get_Comparer(Locations_By.LE)

            Dim out As New List(Of List(Of Basic_Location))

            For Each Loci In Locis
                Dim F_Location = Basic_Location_Modifier.Get_Basic_Location(Loci.Location, Five_Prime, Locations_By.LE)
                Dim L_Location = Basic_Location_Modifier.Get_Basic_Location(Loci.Location, Three_Prime, Locations_By.LE)

                out.Add(Get_Locations_Near.Common(Sorted, F_Location, L_Location, c, Locations_By.LE))
            Next

            Return out
        End Function
    End Class
End Class


