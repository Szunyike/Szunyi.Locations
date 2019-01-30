Imports Bio.Extensions
Imports Bio.IO.GenBank

Public Class Basic_Location
    '  Public Property IsComplementer As Boolean
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

Public Class Convert
    Public Shared Function From_Bio_Location(Loci As Bio.IO.GenBank.Location) As Basic_Location
        Return New Basic_Location(Loci)
    End Function

    Public Shared Iterator Function From_Bio_Locations(Locis As IEnumerable(Of Bio.IO.GenBank.ILocation)) As IEnumerable(Of Basic_Location)
        For Each Loci In Locis
            Yield New Basic_Location(Loci)
        Next
    End Function
    Public Shared Iterator Function From_Bio_Locations(Locis As IEnumerable(Of Bio.IO.GenBank.Location)) As IEnumerable(Of Basic_Location)
        For Each Loci In Locis
            Yield New Basic_Location(Loci)
        Next
    End Function
    Public Shared Iterator Function From_Features_Locations(Feats As IEnumerable(Of FeatureItem)) As IEnumerable(Of Basic_Location)
        For Each Feat In Feats
            Yield From_Feature_Locations(Feat)
        Next
    End Function

    Public Shared Function From_Feature_Locations(Feat As FeatureItem) As Basic_Location
        Return New Basic_Location(Feat.Location, Feat)
    End Function
    Public Shared Function From_ListOf_ListOf_Ilocation(Locis As List(Of List(Of ILocation)), Optional Seq As Bio.ISequence = Nothing, Optional Obj As Object = Nothing) As List(Of Basic_Location)
        Dim out As New List(Of Basic_Location)
        For Each Loci In Locis
            out.Add(New Basic_Location(Loci.First, Seq, Obj))
        Next
        Return out
    End Function

    Public Shared Iterator Function From_Template_Switch(tS As List(Of TemplateSwitch)) As IEnumerable(Of Basic_Location)
        For Each t In tS
            Yield New Basic_Location(t.Loci)
        Next
    End Function
End Class

Public Class Basic_Location_Modifier
    Public Shared Property LociBuilder As New Bio.IO.GenBank.LocationBuilder
    Public Shared Function Get_Basic_Location(Location As Bio.IO.GenBank.Location, range As Integer, ExtendBy As Locations_By) As Basic_Location
        Dim l As New Bio.IO.GenBank.Location
        Select Case ExtendBy

            Case Locations_By.TSS
                l = Modify_Location(Location.TSS + range, Location.PAS, Location.IsComplementer)
            Case Locations_By.LE
                l = Modify_Location(Location.LocationStart, Location.LocationEnd + range, Location.IsComplementer)
            Case Locations_By.PAS & Locations_By.TSS
                l = Modify_Location(Location.LocationStart + range, Location.LocationEnd + range, Location.IsComplementer)
            Case Locations_By.PAS
                l = Modify_Location(Location.TSS, Location.PAS + range, Location.IsComplementer)
            Case Locations_By.LS
                l = Modify_Location(Location.LocationStart + range, Location.LocationEnd, Location.IsComplementer)
        End Select
        Return New Basic_Location(l)

    End Function
    Public Shared Function Modify_Location(st As Integer, endy As Integer, IsC As Boolean) As Bio.IO.GenBank.Location
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

Public Class BLs_Binary_Comparers
    Public Property TSS_wStrand As New _ByTSS_wStrand
    Public Property TSS_woStrand As New _ByTSS_woStrand
    Public Property PAS_wStrand As New _ByPAS_wStrand
    Public Property PAS_woStrand As New _ByPAS_woStrand
    Public Property LS_wStrand As New _ByStart_wStrand
    Public Property LS_woStrand As New _ByStart_woStrand
    Public Property LE_wStrand As New _ByEnd_wStrand
    Public Property LE_woStrand As New _ByEnd_woStrand
    Public Property Gr_ByTSS As New Gr_Basic_Location_ByTSS
    Public Property Gr_ByPAS As New Gr_Basic_Location_ByPAS
    Public Class Gr_Basic_Location_ByTSS
        Implements IComparer(Of List(Of Basic_Location))

        Public Function Compare(x As List(Of Basic_Location), y As List(Of Basic_Location)) As Integer Implements IComparer(Of List(Of Basic_Location)).Compare

            If x.First.Location.IsComplementer <> y.First.Location.IsComplementer Then Return x.First.Location.IsComplementer.CompareTo(y.First.Location.IsComplementer)
            Return x.First.Location.TSS.CompareTo(y.First.Location.TSS)
        End Function


    End Class
    Public Class Gr_Basic_Location_ByPAS
        Implements IComparer(Of List(Of Basic_Location))

        Public Function Compare(x As List(Of Basic_Location), y As List(Of Basic_Location)) As Integer Implements IComparer(Of List(Of Basic_Location)).Compare

            If x.First.Location.IsComplementer <> y.First.Location.IsComplementer Then Return x.First.Location.IsComplementer.CompareTo(y.First.Location.IsComplementer)
            Return x.First.Location.PAS.CompareTo(y.First.Location.PAS)
        End Function
    End Class


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

    Public Class Merging
    Public Class For_Merge_Basic_Locations
        Public GroupSortedByCount As List(Of List(Of Basic_Location))
        Public GroupSortedByStart As List(Of List(Of Basic_Location))
        Public GroupSortedByEnd As List(Of List(Of Basic_Location))
        Public GroupSortedByPAS As List(Of List(Of Basic_Location))
        Public GroupSortedByTSS As List(Of List(Of Basic_Location))

        Public OneLociPerGroupByStart As List(Of Basic_Location)
        Public OneLociPerGroupPAS As List(Of Basic_Location)
        Public OneLociPerGroupTSS As List(Of Basic_Location)
        Public OneLociPerGroupEnd As List(Of Basic_Location)

        Public OneLoci_GroupSortedByStart As List(Of List(Of Basic_Location))
        Public OneLoci_GroupSortedByEnd As List(Of List(Of Basic_Location))
        Public OneLoci_GroupSortedByPAS As List(Of List(Of Basic_Location))
        Public OneLoci_GroupSortedByTSS As List(Of List(Of Basic_Location))
        Public Property LociBinary As New BLs_Binary_Comparers
        Public Sub New(sorted As List(Of List(Of Basic_Location)), pos As Locations_By)
            GroupSortedByCount = (From x In sorted Select x Order By x.Count Descending).ToList
            Select Case pos
                Case Locations_By.TSS Or Locations_By.PAS
                    GroupSortedByStart = (From x In sorted Select x Order By x.First.Location.TSS)
                    OneLociPerGroupByStart = (From x In GroupSortedByStart Select x.First).ToList
                    GroupSortedByEnd = (From x In sorted Select x Order By x.First.Location.LocationEnd)
                    OneLociPerGroupEnd = (From x In GroupSortedByEnd Select x.First).ToList
                Case Locations_By.LE
                    GroupSortedByEnd = sorted
                    OneLociPerGroupEnd = (From x In GroupSortedByEnd Select x.First).ToList

                Case Locations_By.PAS

                    GroupSortedByPAS = sorted
                    GroupSortedByPAS.Sort(LociBinary.Gr_ByPAS)
                    OneLociPerGroupPAS = (From x In GroupSortedByPAS Select x.First).ToList
                    OneLociPerGroupPAS.Sort(LociBinary.PAS_wStrand)

                Case Locations_By.LS
                    GroupSortedByTSS = sorted
                    OneLociPerGroupByStart = (From x In GroupSortedByStart Select x.First).ToList

                Case Locations_By.TSS
                    GroupSortedByTSS = sorted
                    GroupSortedByTSS.Sort(LociBinary.Gr_ByTSS)
                    OneLociPerGroupTSS = (From x In GroupSortedByTSS Select x.First).ToList
                    OneLociPerGroupTSS.Sort(LociBinary.TSS_wStrand)

                Case Else
                    GroupSortedByStart = (From x In sorted Select x Order By x.First.Location.LocationStart).ToList
                    OneLociPerGroupByStart = (From x In GroupSortedByStart Select x.First).ToList
                    GroupSortedByEnd = (From x In sorted Select x Order By x.First.Location.LocationEnd).ToList
                    OneLociPerGroupEnd = (From x In GroupSortedByEnd Select x.First).ToList


            End Select
        End Sub
    End Class
    Public Shared Function MergeLocations(Feats As List(Of Basic_Location),
                                                  Width As Integer, pos As Locations_By, Optional MinNof As Integer = 1) As List(Of List(Of Basic_Location))
        Dim Sorted = GroupBy(Feats, pos, MinNof) ' Exact

        Dim out As New List(Of List(Of Basic_Location))

        Dim M As New Merging.For_Merge_Basic_Locations(Sorted, pos)

        Dim Used As New List(Of List(Of Basic_Location))
        Dim groups As New Dictionary(Of String, List(Of List(Of Basic_Location)))

        For Each Loci In M.GroupSortedByCount
            If Used.Contains(Loci) = False Then
                Dim SameStarts As New List(Of List(Of Basic_Location))

                Select Case pos
                    Case Locations_By.LS
                        SameStarts = Get_Locis_Near._Start(Loci, Width, M.GroupSortedByStart, M.OneLociPerGroupByStart)
                    Case Locations_By.LE
                        SameStarts = Get_Locis_Near._End(Loci, Width, M.GroupSortedByEnd, M.OneLociPerGroupEnd)

                    Case Locations_By.PAS
                        SameStarts = Get_Locis_Near._PAS(Loci, Width, M.GroupSortedByPAS, M.OneLociPerGroupPAS)

                    Case Locations_By.TSS
                        SameStarts = Get_Locis_Near._TSS(Loci, Width, M.GroupSortedByTSS, M.OneLociPerGroupTSS)
                    Case Else
                        '  Case Locations_By.TSS_PAS
                        Dim BinarySameStarts = Get_Locis_Near._Start(Loci, Width, M.GroupSortedByStart, M.OneLociPerGroupByStart)
                        SameStarts = Get_Locis_Near._Start_End(Loci, Width, BinarySameStarts)
                End Select

                Dim cLocis As New List(Of Basic_Location)

                For Each SameStart In SameStarts
                    If Used.Contains(SameStart) = False Then
                        cLocis.AddRange(SameStart)
                        Used.Add(SameStart)
                    End If
                Next
                If cLocis.Count > 0 Then
                    out.Add(cLocis)
                End If

            End If

        Next

        Return out

    End Function
    Public Shared Function GroupBy(Locis As List(Of Basic_Location), pos As Locations_By, MinNof As Integer) As List(Of List(Of Basic_Location))
        Select Case pos

            Case Locations_By.TSS
                Return GroupBy_Start_End_Boths.Group_By_Locis_Start(Locis, MinNof).ToList
            Case Locations_By.LE
                Return GroupBy_Start_End_Boths.Group_By_Locis_End(Locis, MinNof).ToList
            Case Locations_By.TSS Or Locations_By.PAS
                Return GroupBy_Start_End_Boths.Group_By_Locis_Start_End(Locis, MinNof).ToList
            Case Locations_By.TSS
                Return GroupBy_Start_End_Boths.Group_By_Locis_TSS(Locis, MinNof).ToList
            Case Locations_By.PAS
                Return GroupBy_Start_End_Boths.Group_By_Locis_PAS(Locis, MinNof).ToList
            Case Locations_By.TSS Or Locations_By.PAS
                Dim kj As Int16 = 65
            Case Locations_By.LE Or Locations_By.LS
                Return GroupBy_Start_End_Boths.Group_By_Locis_TSS(Locis, MinNof).ToList
        End Select
        Return Nothing
    End Function

End Class


Public Class Get_Locis_Near
    Public Shared Property LociBinary As New BLs_Binary_Comparers
#Region "Basic_Location"
    Public Shared Function _Start_End(Loci As List(Of Basic_Location), width As Integer, sorted As List(Of List(Of Basic_Location))) As List(Of List(Of Basic_Location))
        Dim NearStarts = From x In sorted Where x.First.Location.Operator = Loci.First.Location.Operator And x.First.Location.LocationStart >= Loci.First.Location.LocationStart - width And
                                              x.First.Location.LocationStart <= Loci.First.Location.LocationStart + width _
                                              And x.First.Location.LocationEnd >= Loci.First.Location.LocationEnd - width And
                                              x.First.Location.LocationEnd <= Loci.First.Location.LocationEnd + width


        Dim Out As New List(Of List(Of Basic_Location))

        For Each NearStart In NearStarts

            Out.Add(NearStart)

        Next

        Return Out
    End Function
    Public Shared Function _Start(Loci As List(Of Basic_Location), width As Integer, sorted As List(Of List(Of Basic_Location)),
                                                        Locis As List(Of Basic_Location)) As List(Of List(Of Basic_Location))
        Dim cLoci = Szunyi.BLs.Location.Common.GetLocation(Loci.First.Location.LocationStart - width, Loci.First.Location.LocationStart + width)
        Dim mdLoci = Szunyi.BLs.Convert.From_Bio_Location(cLoci)
        Dim Index = Locis.BinarySearch(Loci.First, LociBinary.LS_wStrand)
        Dim out As New List(Of List(Of Basic_Location))
        If Index > -1 Then
            For i1 = Index To 0 Step -1
                If Locis(i1).Location.LocationStart >= mdLoci.Location.LocationStart AndAlso
                            Locis(i1).Location.LocationStart <= mdLoci.Location.LocationEnd AndAlso Locis(i1).Location.Operator = Loci.First.Location.Operator Then
                    out.Add(sorted(i1))
                ElseIf Locis(i1).Location.LocationStart >= mdLoci.Location.LocationStart AndAlso Locis(i1).Location.LocationStart <= mdLoci.Location.LocationEnd Then

                Else

                    Exit For
                End If
            Next
            For i1 = Index + 1 To Locis.Count - 1
                If Locis(i1).Location.LocationStart >= mdLoci.Location.LocationStart AndAlso
                            Locis(i1).Location.LocationStart <= mdLoci.Location.LocationEnd AndAlso Locis(i1).Location.Operator = Loci.First.Location.Operator Then
                    out.Add(sorted(i1))
                ElseIf Locis(i1).Location.LocationStart >= mdLoci.Location.LocationStart AndAlso Locis(i1).Location.LocationStart <= mdLoci.Location.LocationEnd Then
                Else
                    Exit For
                End If
            Next
        Else
            Dim alf As Int16 = 43
        End If

        Return out
    End Function
    Public Shared Function _TSS(Loci As List(Of Basic_Location), width As Integer, sorted As List(Of List(Of Basic_Location)),
                                                        Locis As List(Of Basic_Location)) As List(Of List(Of Basic_Location))

        Dim cLoci = Szunyi.BLs.Location.Common.GetLocation(Loci.First.Location.LocationStart - width, Loci.First.Location.LocationStart + width)
        Dim mdLoci = Szunyi.BLs.Convert.From_Bio_Location(cLoci)

        Dim d As New BLs_Binary_Comparers._ByTSS_wStrand
        Dim Index = Locis.BinarySearch(Loci.First, d)
        Dim out As New List(Of List(Of Basic_Location))
        If Index < 0 Then
            Dim kj As Int16 = 54
        End If
        If Index > -1 Then
            For i1 = Index To 0 Step -1
                If Locis(i1).Location.TSS >= mdLoci.Location.LocationStart AndAlso
                            Locis(i1).Location.TSS <= mdLoci.Location.LocationEnd AndAlso Locis(i1).Location.Operator = Loci.First.Location.Operator Then
                    out.Add(sorted(i1))
                ElseIf Locis(i1).Location.TSS >= mdLoci.Location.LocationStart AndAlso Locis(i1).Location.TSS <= mdLoci.Location.LocationEnd Then

                Else

                    Exit For
                End If
            Next
            For i1 = Index + 1 To Locis.Count - 1
                If Locis(i1).Location.TSS >= mdLoci.Location.LocationStart AndAlso
                            Locis(i1).Location.TSS <= mdLoci.Location.LocationEnd AndAlso Locis(i1).Location.Operator = Loci.First.Location.Operator Then
                    out.Add(sorted(i1))
                ElseIf Locis(i1).Location.TSS >= mdLoci.Location.LocationStart AndAlso Locis(i1).Location.TSS <= mdLoci.Location.LocationEnd Then
                Else
                    Exit For
                End If
            Next

        End If

        Return out
    End Function
    Public Shared Function _PAS(Loci As List(Of Basic_Location), width As Integer, sorted As List(Of List(Of Basic_Location)),
                                                        Locis As List(Of Basic_Location)) As List(Of List(Of Basic_Location))

        Dim cLoci = Szunyi.BLs.Location.Common.GetLocation(Loci.First.Location.LocationStart - width, Loci.First.Location.LocationStart + width)
        Dim mdLoci = Szunyi.BLs.Convert.From_Bio_Location(cLoci)

        Dim Index = Locis.BinarySearch(Loci.First, LociBinary.PAS_wStrand)
        Dim out As New List(Of List(Of Basic_Location))
        If Index > -1 Then
            For i1 = Index To 0 Step -1
                If Locis(i1).Location.PAS >= mdLoci.Location.LocationStart AndAlso
                            Locis(i1).Location.PAS <= mdLoci.Location.LocationEnd AndAlso Locis(i1).Location.IsComplementer = Loci.First.Location.IsComplementer Then
                    out.Add(sorted(i1))
                Else
                    Exit For
                End If
            Next
            For i1 = Index + 1 To Locis.Count - 1
                If Locis(i1).Location.PAS >= mdLoci.Location.LocationStart AndAlso
                            Locis(i1).Location.PAS <= mdLoci.Location.LocationEnd AndAlso Locis(i1).Location.IsComplementer = Loci.First.Location.IsComplementer Then
                    out.Add(sorted(i1))
                Else
                    Exit For
                End If
            Next
        Else
            Dim kj As Int16 = 54
        End If

        Return out
    End Function
    Public Shared Function _Start_xth(Loci As List(Of Basic_Location), xth As Integer, width As Integer, sorted As List(Of List(Of Basic_Location)),
                                                        Locis As List(Of Basic_Location)) As List(Of List(Of Basic_Location))
        Dim xth_loci = Szunyi.BLs.Location.Exon_Intron.Get_XTH_Exon_Location(Loci.First.Location, xth)
        Dim out As New List(Of List(Of Basic_Location))
        For Each item In sorted
            Dim xth_exon = Szunyi.BLs.Location.Exon_Intron.Get_XTH_Exon_Location(item.First.Location, xth)
            If xth_exon.LocationStart >= xth_loci.LocationStart - width AndAlso xth_exon.LocationStart <= xth_loci.LocationStart + width Then
                out.Add(item)
            End If

        Next
        Return out
    End Function
    Public Shared Function _End_xth(Loci As List(Of Basic_Location), xth As Integer, width As Integer, sorted As List(Of List(Of Basic_Location)), Locis As List(Of Basic_Location)) As List(Of List(Of Basic_Location))
        Dim xth_loci = Szunyi.BLs.Location.Exon_Intron.Get_XTH_Exon_Location(Loci.First.Location, xth)
        Dim out As New List(Of List(Of Basic_Location))
        For Each item In sorted
            Dim xth_exon = Szunyi.BLs.Location.Exon_Intron.Get_XTH_Exon_Location(item.First.Location, xth)
            If xth_exon.LocationEnd >= xth_loci.LocationEnd - width AndAlso xth_exon.LocationEnd <= xth_loci.LocationEnd + width Then
                out.Add(item)
            End If

        Next
        Return out
    End Function
    Public Shared Function _End(Loci As List(Of Basic_Location), width As Integer, sorted As List(Of List(Of Basic_Location))) As List(Of List(Of Basic_Location))
        Dim NearStarts = From x In sorted Where x.First.Location.Operator = Loci.First.Location.Operator And x.First.Location.LocationEnd > Loci.First.Location.LocationEnd - width And x.First.Location.LocationEnd < Loci.First.Location.LocationEnd + width And x.First.Location.LocationEnd > Loci.First.Location.LocationEnd - width And x.First.Location.LocationEnd < Loci.First.Location.LocationEnd + width

        Dim Out As New List(Of List(Of Basic_Location))

        For Each NearStart In NearStarts

            Out.Add(NearStart)

        Next

        Return Out
    End Function
    Public Shared Function _End(Loci As List(Of Basic_Location), width As Integer, sorted As List(Of List(Of Basic_Location)), Locis As List(Of Basic_Location)) As List(Of List(Of Basic_Location))
        Dim cLoci = Szunyi.BLs.Location.Common.GetLocation(Loci.First.Location.LocationStart - width, Loci.First.Location.LocationStart + width)
        Dim mdLoci = Szunyi.BLs.Convert.From_Bio_Location(cLoci)

        Dim Index = Locis.BinarySearch(mdLoci, LociBinary.LE_wStrand)
        Dim out As New List(Of List(Of Basic_Location))
        With mdLoci.Location
            If Index > -1 Then
                For i1 = Index To 0 Step -1
                    If Locis(i1).Location.LocationEnd >= .LocationStart AndAlso Locis(i1).Location.LocationEnd <= .LocationEnd AndAlso Locis(i1).Location.Operator = Loci.First.Location.Operator Then
                        out.Add(sorted(i1))
                    Else
                        Exit For
                    End If
                Next
                For i1 = Index + 1 To Locis.Count - 1
                    If Locis(i1).Location.LocationEnd >= .LocationStart AndAlso Locis(i1).Location.LocationEnd <= .LocationEnd AndAlso Locis(i1).Location.Operator = Loci.First.Location.Operator Then
                        out.Add(sorted(i1))
                    Else
                        Exit For
                    End If
                Next
            Else
                Dim alf As Int16 = 43
            End If
        End With


        Return out
    End Function

    Public Shared Function _Start_End(Loci As Basic_Location, width As Integer, sorted As List(Of List(Of Basic_Location))) As List(Of List(Of Basic_Location))
        Dim NearStarts = From x In sorted Where x.First.Location.Operator = Loci.Location.Operator And x.First.Location.LocationStart > Loci.Location.LocationStart - width And x.First.Location.LocationStart < Loci.Location.LocationStart + width And x.First.Location.LocationEnd > Loci.Location.LocationEnd - width And x.First.Location.LocationEnd < Loci.Location.LocationEnd + width

        Dim Out As New List(Of List(Of Basic_Location))

        For Each NearStart In NearStarts

            Out.Add(NearStart)

        Next

        Return Out
    End Function
    Public Shared Function _Start(Loci As Basic_Location, width As Integer, sorted As List(Of List(Of Basic_Location)),
                                                        Locis As List(Of Basic_Location)) As List(Of List(Of Basic_Location))

        Dim cloci = Szunyi.BLs.Location.Common.GetLocation(Loci.Location.LocationStart - width, Loci.Location.LocationStart + width)
        Dim mdloci = Szunyi.BLs.Convert.From_Bio_Location(cloci)
        Dim Index = Locis.BinarySearch(Loci, LociBinary.LS_wStrand)
        Dim out As New List(Of List(Of Basic_Location))
        If Index > -1 Then
            For i1 = Index To 0 Step -1
                If Locis(i1).Location.LocationStart >= mdLoci.Location.LocationStart AndAlso
                            Locis(i1).Location.LocationStart <= mdLoci.Location.LocationEnd AndAlso Locis(i1).Location.Operator = Loci.Location.Operator Then
                    out.Add(sorted(i1))
                ElseIf Locis(i1).Location.LocationStart >= mdLoci.Location.LocationStart AndAlso Locis(i1).Location.LocationStart <= mdLoci.Location.LocationEnd Then

                Else

                    Exit For
                End If
            Next
            For i1 = Index + 1 To Locis.Count - 1
                If Locis(i1).Location.LocationStart >= mdLoci.Location.LocationStart AndAlso
                            Locis(i1).Location.LocationStart <= mdLoci.Location.LocationEnd AndAlso Locis(i1).Location.Operator = Loci.Location.Operator Then
                    out.Add(sorted(i1))
                ElseIf Locis(i1).Location.LocationStart >= mdLoci.Location.LocationStart AndAlso Locis(i1).Location.LocationStart <= mdLoci.Location.LocationEnd Then
                Else
                    Exit For
                End If
            Next
        Else
            Dim alf As Int16 = 43
        End If

        Return out
    End Function
    Public Shared Function _TSS(Loci As Basic_Location, width As Integer, sorted As List(Of List(Of Basic_Location)),
                                                        Locis As List(Of Basic_Location)) As List(Of List(Of Basic_Location))

        Dim cLoci = Szunyi.BLs.Location.Common.GetLocation(Loci.Location.LocationStart - width, Loci.Location.LocationStart + width)
        Dim mdLoci = Szunyi.BLs.Convert.From_Bio_Location(cLoci)

        Dim Index = Locis.BinarySearch(Loci, LociBinary.TSS_wStrand)
        Dim out As New List(Of List(Of Basic_Location))

        If Index > -1 Then
            For i1 = Index To 0 Step -1
                If Locis(i1).Location.TSS >= mdLoci.Location.LocationStart AndAlso
                            Locis(i1).Location.TSS <= mdLoci.Location.LocationEnd AndAlso Locis(i1).Location.Operator = Loci.Location.Operator Then
                    out.Add(sorted(i1))
                ElseIf Locis(i1).Location.TSS >= mdLoci.Location.LocationStart AndAlso Locis(i1).Location.TSS <= mdLoci.Location.LocationEnd Then

                Else

                    Exit For
                End If
            Next
            For i1 = Index + 1 To Locis.Count - 1
                If Locis(i1).Location.TSS >= mdLoci.Location.LocationStart AndAlso
                            Locis(i1).Location.TSS <= mdLoci.Location.LocationEnd AndAlso Locis(i1).Location.Operator = Loci.Location.Operator Then
                    out.Add(sorted(i1))
                ElseIf Locis(i1).Location.TSS >= mdLoci.Location.LocationStart AndAlso Locis(i1).Location.TSS <= mdLoci.Location.LocationEnd Then
                Else
                    Exit For
                End If
            Next

        End If

        Return out
    End Function
    Public Shared Function _PAS(Loci As Basic_Location, width As Integer, sorted As List(Of List(Of Basic_Location)),
                                                        Locis As List(Of Basic_Location)) As List(Of List(Of Basic_Location))

        Dim cLoci = Szunyi.BLs.Location.Common.GetLocation(Loci.Location.LocationStart - width, Loci.Location.LocationStart + width)
        Dim mdLoci = Szunyi.BLs.Convert.From_Bio_Location(cLoci)
        Dim Index = Locis.BinarySearch(Loci, LociBinary.PAS_wStrand)
        Dim out As New List(Of List(Of Basic_Location))
        If Index > -1 Then
            For i1 = Index To 0 Step -1
                If Locis(i1).Location.PAS >= mdLoci.Location.LocationStart AndAlso
                            Locis(i1).Location.PAS <= mdLoci.Location.LocationEnd AndAlso Locis(i1).Location.IsComplementer = Loci.Location.IsComplementer Then
                    out.Add(sorted(i1))
                Else
                    Exit For
                End If
            Next
            For i1 = Index + 1 To Locis.Count - 1
                If Locis(i1).Location.PAS >= mdLoci.Location.LocationStart AndAlso
                            Locis(i1).Location.PAS <= mdLoci.Location.LocationEnd AndAlso Locis(i1).Location.IsComplementer = Loci.Location.IsComplementer Then
                    out.Add(sorted(i1))
                Else
                    Exit For
                End If
            Next
        Else
            Dim kj As Int16 = 54
        End If

        Return out
    End Function
    Public Shared Function _Start_xth(Loci As Basic_Location, xth As Integer, width As Integer, sorted As List(Of List(Of Basic_Location)),
                                                        Locis As List(Of Basic_Location)) As List(Of List(Of Basic_Location))
        Dim xth_loci = Szunyi.BLs.Location.Exon_Intron.Get_XTH_Exon_Location(Loci.Location, xth)
        Dim out As New List(Of List(Of Basic_Location))
        For Each item In sorted
            Dim xth_exon = Szunyi.BLs.Location.Exon_Intron.Get_XTH_Exon_Location(item.First.Location, xth)
            If xth_exon.LocationStart >= xth_loci.LocationStart - width AndAlso xth_exon.LocationStart <= xth_loci.LocationStart + width Then
                out.Add(item)
            End If

        Next
        Return out
    End Function
    Public Shared Function _End_xth(Loci As Basic_Location, xth As Integer, width As Integer, sorted As List(Of List(Of Basic_Location)), Locis As List(Of Basic_Location)) As List(Of List(Of Basic_Location))
        Dim xth_loci = Szunyi.BLs.Location.Exon_Intron.Get_XTH_Exon_Location(Loci.Location, xth)
        Dim out As New List(Of List(Of Basic_Location))
        For Each item In sorted
            Dim xth_exon = Szunyi.BLs.Location.Exon_Intron.Get_XTH_Exon_Location(item.First.Location, xth)
            If xth_exon.LocationEnd >= xth_loci.LocationEnd - width AndAlso xth_exon.LocationEnd <= xth_loci.LocationEnd + width Then
                out.Add(item)
            End If

        Next
        Return out
    End Function
    Public Shared Function _End(Loci As Basic_Location, width As Integer, sorted As List(Of List(Of Basic_Location))) As List(Of List(Of Basic_Location))
        Dim NearStarts = From x In sorted Where x.First.Location.Operator = Loci.Location.Operator And x.First.Location.LocationEnd > Loci.Location.LocationEnd - width And x.First.Location.LocationEnd < Loci.Location.LocationEnd + width And x.First.Location.LocationEnd > Loci.Location.LocationEnd - width And x.First.Location.LocationEnd < Loci.Location.LocationEnd + width

        Dim Out As New List(Of List(Of Basic_Location))

        For Each NearStart In NearStarts

            Out.Add(NearStart)

        Next

        Return Out
    End Function
    Public Shared Function _End(Loci As Basic_Location, width As Integer, sorted As List(Of List(Of Basic_Location)), Locis As List(Of Basic_Location)) As List(Of List(Of Basic_Location))

        Dim cLoci = Szunyi.BLs.Location.Common.GetLocation(Loci.Location.LocationStart - width, Loci.Location.LocationStart + width)
        Dim mdLoci = Szunyi.BLs.Convert.From_Bio_Location(cLoci)

        Dim Index = Locis.BinarySearch(mdLoci, LociBinary.LE_wStrand)
        Dim out As New List(Of List(Of Basic_Location))
        With mdLoci.Location
            If Index > -1 Then
                For i1 = Index To 0 Step -1
                    If Locis(i1).Location.LocationEnd >= .LocationStart AndAlso Locis(i1).Location.LocationEnd <= .LocationEnd AndAlso Locis(i1).Location.Operator = Loci.Location.Operator Then
                        out.Add(sorted(i1))
                    Else
                        Exit For
                    End If
                Next
                For i1 = Index + 1 To Locis.Count - 1
                    If Locis(i1).Location.LocationEnd >= .LocationStart AndAlso Locis(i1).Location.LocationEnd <= .LocationEnd AndAlso Locis(i1).Location.Operator = Loci.Location.Operator Then
                        out.Add(sorted(i1))
                    Else
                        Exit For
                    End If
                Next
            Else
                Dim alf As Int16 = 43
            End If
        End With


        Return out
    End Function

#End Region

End Class


Public Class GroupBy_Start_End_Boths
#Region "Locis"
    Public Shared Function Group_By_Locis_TSS(locis As List(Of ILocation), minNof As Integer, wOrientation As Boolean) As List(Of List(Of ILocation))
        Dim res As New List(Of List(Of ILocation))
        For Each LociGroup In Iterate.Iterate_By_Locis_TSS(locis, wOrientation)
            If minNof <= LociGroup.Count Then
                res.Add(LociGroup)
                res.Last.AddRange(LociGroup)
            End If
        Next
        Dim t = From x In res Select x Order By x.First.TSS
        Return t.ToList
    End Function
    Public Shared Function Group_By_Locis_PAS(locis As List(Of ILocation), minNof As Integer, wOrientation As Boolean) As List(Of List(Of ILocation))
        Dim res As New List(Of List(Of ILocation))
        For Each LociGroup In Iterate.Iterate_By_Locis_PAS(locis, wOrientation)
            If minNof <= LociGroup.Count Then
                res.Add(LociGroup)
                res.Last.AddRange(LociGroup)
            End If
        Next
        Dim t = From x In res Select x Order By x.First.TSS
        Return t.ToList
    End Function
    Public Shared Function Group_By_Locis_Start(Locis As List(Of Bio.IO.GenBank.ILocation), minNofItem As Integer, wOrientation As Boolean) As List(Of List(Of Bio.IO.GenBank.ILocation))
        Dim res As New List(Of List(Of Bio.IO.GenBank.ILocation))
        For Each LociGroup In Iterate.Iterate_By_Locis_Start(Locis, wOrientation)
            If minNofItem <= LociGroup.Count Then
                res.Add(New List(Of Bio.IO.GenBank.ILocation))
                res.Last.AddRange(LociGroup)
            End If
        Next
        Dim t = From x In res Select x Order By x.First.LocationStart
        Return t.ToList
    End Function
    Public Shared Function Group_By_Locis_Start_Xth(Locis As List(Of Bio.IO.GenBank.ILocation), Xth As Integer, Optional minNofItem As Int16 = 1) As List(Of List(Of Bio.IO.GenBank.ILocation))
        Dim res As New List(Of List(Of Bio.IO.GenBank.ILocation))
        For Each LociGroup In Iterate.Iterate_By_Locis_Start_Xth(Locis, Xth)
            If minNofItem <= LociGroup.Count Then
                res.Add(New List(Of Bio.IO.GenBank.ILocation))
                res.Last.AddRange(LociGroup)
            End If

        Next
        Dim t = From x In res Select x Order By x.First.LocationStart
        Return t.ToList
    End Function
    Public Shared Function Group_By_Locis_End_xth(Locis As List(Of Bio.IO.GenBank.ILocation), xth As Integer, Optional minNofItem As Int16 = 1) As List(Of List(Of Bio.IO.GenBank.ILocation))
        Dim res As New List(Of List(Of Bio.IO.GenBank.ILocation))
        For Each LociGroup In Iterate.Iterate_By_Locis_End_Xth(Locis, xth)
            If minNofItem <= LociGroup.Count Then
                res.Add(New List(Of Bio.IO.GenBank.ILocation))
                res.Last.AddRange(LociGroup)
            End If

        Next
        Dim t = From x In res Select x Order By x.First.LocationEnd
        Return t.ToList
    End Function
    Public Shared Function Group_By_Locis_End(Locis As List(Of Bio.IO.GenBank.ILocation), minNofItem As Integer, wOrientation As Boolean) As List(Of List(Of Bio.IO.GenBank.ILocation))
        Dim res As New List(Of List(Of Bio.IO.GenBank.ILocation))
        For Each LociGroup In Iterate.Iterate_By_Locis_End(Locis, wOrientation)
            If minNofItem <= LociGroup.Count Then
                res.Add(New List(Of Bio.IO.GenBank.ILocation))
                res.Last.AddRange(LociGroup)
            End If

        Next
        Dim t = From x In res Select x Order By x.First.LocationEnd
        Return t.ToList
    End Function
    Public Shared Function Group_By_Locis_Start_End(Locis As List(Of Bio.IO.GenBank.ILocation), minNofItem As Integer, wOrientation As Boolean) As List(Of List(Of Bio.IO.GenBank.ILocation))
        Dim res As New List(Of List(Of Bio.IO.GenBank.ILocation))
        For Each LociGroup In Iterate.Iterate_By_Locis_Start_End(Locis, wOrientation)
            If minNofItem <= LociGroup.Count Then
                res.Add(New List(Of Bio.IO.GenBank.ILocation))
                res.Last.AddRange(LociGroup)
            End If
        Next
        Dim t = OrderBy.OrderByCount(res)
        Return t
    End Function
    Public Shared Function Group_By_Locis_Start_End_xth(Locis As List(Of Bio.IO.GenBank.ILocation), xth As Integer, Optional minNofItem As Int16 = 1) As List(Of List(Of Bio.IO.GenBank.ILocation))
        Dim res As New List(Of List(Of Bio.IO.GenBank.ILocation))
        For Each LociGroup In Iterate.Iterate_By_Locis_Start_End_Xth(Locis, xth)
            If minNofItem <= LociGroup.Count Then
                res.Add(New List(Of Bio.IO.GenBank.ILocation))
                res.Last.AddRange(LociGroup)
            End If
        Next
        Dim t = OrderBy.OrderByCount(res)
        Return t
    End Function

#End Region

#Region "BAsic_Location"
    Public Shared Function Group_By_Locis_Start(Locis As List(Of Basic_Location), Optional minNofItem As Int16 = 1) As List(Of List(Of Basic_Location))
        Dim res As New List(Of List(Of Basic_Location))
        For Each LociGroup In Iterate.Iterate_By_Locis_Start(Locis)
            If minNofItem <= LociGroup.Count Then
                res.Add(New List(Of Basic_Location))
                res.Last.AddRange(LociGroup)
            End If
        Next
        Dim t = From x In res Select x Order By x.First.Location.LocationStart
        Return t.ToList
    End Function
    Public Shared Function Group_By_Locis_Start_Xth(Locis As List(Of Basic_Location), Xth As Integer, Optional minNofItem As Int16 = 1) As List(Of List(Of Basic_Location))
        Dim res As New List(Of List(Of Basic_Location))
        For Each LociGroup In Iterate.Iterate_By_Locis_Start_Xth(Locis, Xth)
            If minNofItem <= LociGroup.Count Then
                res.Add(New List(Of Basic_Location))
                res.Last.AddRange(LociGroup)
            End If

        Next
        Dim t = From x In res Select x Order By x.First.Location.LocationStart
        Return t.ToList
    End Function
    Public Shared Function Group_By_Locis_End_xth(Locis As List(Of Basic_Location), xth As Integer, Optional minNofItem As Int16 = 1) As List(Of List(Of Basic_Location))
        Dim res As New List(Of List(Of Basic_Location))
        For Each LociGroup In Iterate.Iterate_By_Locis_End_Xth(Locis, xth)
            If minNofItem <= LociGroup.Count Then
                res.Add(New List(Of Basic_Location))
                res.Last.AddRange(LociGroup)
            End If

        Next
        Dim t = From x In res Select x Order By x.First.Location.LocationEnd
        Return t.ToList
    End Function
    Public Shared Function Group_By_Locis_End(Locis As List(Of Basic_Location), Optional minNofItem As Int16 = 1) As List(Of List(Of Basic_Location))
        Dim res As New List(Of List(Of Basic_Location))
        For Each LociGroup In Iterate.Iterate_By_Locis_End(Locis)
            If minNofItem <= LociGroup.Count Then
                res.Add(New List(Of Basic_Location))
                res.Last.AddRange(LociGroup)
            End If

        Next
        Dim t = From x In res Select x Order By x.First.Location.LocationEnd
        Return t.ToList
    End Function
    Public Shared Function Group_By_Locis_Start_End(Locis As List(Of Basic_Location), Optional minNofItem As Int16 = 1) As List(Of List(Of Basic_Location))
        Dim res As New List(Of List(Of Basic_Location))
        For Each LociGroup In Iterate.Iterate_By_Locis_Start_End(Locis)
            If minNofItem <= LociGroup.Count Then
                res.Add(New List(Of Basic_Location))
                res.Last.AddRange(LociGroup)
            End If
        Next
        Dim t = OrderBy.OrderByCount(res)
        Return t
    End Function
    Public Shared Function Group_By_Locis_Start_End_xth(Locis As List(Of Basic_Location), xth As Integer, Optional minNofItem As Int16 = 1) As List(Of List(Of Basic_Location))
        Dim res As New List(Of List(Of Basic_Location))
        For Each LociGroup In Iterate.Iterate_By_Locis_Start_End_Xth(Locis, xth)
            If minNofItem <= LociGroup.Count Then
                res.Add(New List(Of Basic_Location))
                res.Last.AddRange(LociGroup)
            End If
        Next
        Dim t = OrderBy.OrderByCount(res)
        Return t
    End Function

    Public Shared Function Group_By_Locis_TSS(locis As List(Of Basic_Location), minNof As Integer) As List(Of List(Of Basic_Location))
        Dim res As New List(Of List(Of Basic_Location))
        For Each LociGroup In Iterate.Iterate_By_Locis_TSS(locis)
            If minNof <= LociGroup.Count Then
                res.Add(New List(Of Basic_Location))
                res.Last.AddRange(LociGroup)
            End If
        Next
        Dim t = From x In res Select x Order By x.First.Location.TSS
        Return t.ToList
    End Function

    Friend Shared Function Group_By_Locis_PAS(locis As List(Of Basic_Location), minNof As Integer) As List(Of List(Of Basic_Location))
        Dim res As New List(Of List(Of Basic_Location))
        For Each LociGroup In Iterate.Iterate_By_Locis_PAS(locis)
            If minNof <= LociGroup.Count Then
                res.Add(New List(Of Basic_Location))
                res.Last.AddRange(LociGroup)
            End If
        Next
        Dim t = From x In res Select x Order By x.First.Location.PAS
        Return t.ToList
    End Function
#End Region


End Class

Public Class OrderBy
    Public Shared Function OrderByStart(x As List(Of List(Of Bio.IO.GenBank.ILocation))) As List(Of List(Of Bio.IO.GenBank.ILocation))
        Dim l = From f In x Order By f.First.LocationStart Ascending

        Return l.ToList


    End Function
    Public Shared Function OrderByCount(x As List(Of List(Of Bio.IO.GenBank.ILocation))) As List(Of List(Of Bio.IO.GenBank.ILocation))
        Dim l = From f In x Order By f.Count Descending

        Return l.ToList

    End Function

#Region "Basic_Location"
    Public Shared Function OrderByStart(x As List(Of List(Of Basic_Location))) As List(Of List(Of Basic_Location))
        Dim l = From f In x Order By f.First.Location.LocationStart Ascending

        Return l.ToList


    End Function
    Public Shared Function OrderByCount(x As List(Of List(Of Basic_Location))) As List(Of List(Of Basic_Location))
        Dim l = From f In x Order By f.Count Descending

        Return l.ToList

    End Function
#End Region
End Class

Public Class Iterate

    Private Shared Function Get_Common_Locations_ByRef(jjj As List(Of List(Of ILocation)), list As List(Of List(Of ILocation))) As List(Of List(Of ILocation))
        Dim out As New List(Of List(Of ILocation))
        For i1 = 0 To jjj.Count - 1
            For i2 = 0 To list.Count - 1
                Dim sg As New List(Of ILocation)
                Dim common = jjj(i1).Intersect(list(i2))
                If common.Count > 0 Then
                    out.Add(common.ToList)
                End If

            Next
        Next
        Return out
    End Function


#Region "Locis"
    Public Shared Iterator Function Iterate_By_Locis_Start_Xth(locis As List(Of ILocation), xth As Integer) As IEnumerable(Of List(Of Bio.IO.GenBank.ILocation))

        Dim x = From t In locis Group By Szunyi.BLs.Location.Exon_Intron.Get_XTH_Exon_Location(t, xth).LocationStart, t.IsComplementer Into Group

    End Function

    Public Shared Iterator Function Iterate_By_Locis_Start(Locis As List(Of Bio.IO.GenBank.ILocation), wOrientation As Boolean) As IEnumerable(Of List(Of Bio.IO.GenBank.ILocation))
        If wOrientation = True Then
            Dim x = From t In Locis Group By t.LocationStart, t.IsComplementer Into Group

            For Each j In x
                Yield j.Group.ToList

            Next
        Else
            Dim x = From t In Locis Group By t.LocationStart Into Group

            For Each j In x
                Yield j.Group.ToList

            Next
        End If


    End Function

    Public Shared Iterator Function Iterate_By_Locis_Start_End(Locis As List(Of Bio.IO.GenBank.ILocation), wOrientation As Boolean) As IEnumerable(Of List(Of Bio.IO.GenBank.ILocation))
        If wOrientation = True Then
            Dim x = From t In Locis Group By t.LocationStart, t.LocationEnd, t.IsComplementer Into Group

            For Each j In x
                Yield j.Group.ToList
            Next
        Else
            Dim x = From t In Locis Group By t.LocationStart, t.LocationEnd Into Group

            For Each j In x
                Yield j.Group.ToList
            Next
        End If
    End Function

    Public Shared Iterator Function Iterate_By_Locis_Start_End_Xth(Locis As List(Of Bio.IO.GenBank.ILocation), xth As Integer) As IEnumerable(Of List(Of Bio.IO.GenBank.ILocation))
        Dim x = From t In Locis Group By Szunyi.BLs.Location.Exon_Intron.Get_XTH_Exon_Location(t, xth).LocationStart, Szunyi.BLs.Location.Exon_Intron.Get_XTH_Exon_Location(t, xth).LocationEnd, t.IsComplementer Into Group

        For Each j In x
            Yield j.Group.ToList
        Next
    End Function
    Public Shared Iterator Function Iterate_By_Locis_End_Xth(locis As List(Of ILocation), xth As Integer) As IEnumerable(Of List(Of Bio.IO.GenBank.ILocation))

        Dim x = From t In locis Group By Szunyi.BLs.Location.Exon_Intron.Get_XTH_Exon_Location(t, xth).LocationEnd, t.IsComplementer Into Group

    End Function
    Public Shared Iterator Function Iterate_By_Locis_End(Locis As List(Of Bio.IO.GenBank.ILocation), wOrientation As Boolean) As IEnumerable(Of List(Of Bio.IO.GenBank.ILocation))
        If wOrientation = True Then
            Dim x = From t In Locis Group By t.LocationEnd, t.IsComplementer Into Group

            For Each j In x
                Yield j.Group.ToList

            Next
        Else
            Dim x = From t In Locis Group By t.LocationEnd Into Group

            For Each j In x
                Yield j.Group.ToList

            Next
        End If

    End Function
    Public Shared Iterator Function Iterate_By_Locis_TSS(locis As List(Of ILocation), wOrientation As Boolean) As IEnumerable(Of List(Of ILocation))
        If wOrientation = True Then
            Dim x = From t In locis Group By t.TSS, t.IsComplementer Into Group

            For Each j In x
                Yield j.Group.ToList

            Next
        Else
            Dim x = From t In locis Group By t.TSS Into Group

            For Each j In x
                Yield j.Group.ToList

            Next
        End If

    End Function

    Public Shared Iterator Function Iterate_By_Locis_PAS(locis As List(Of ILocation), wOrientation As Boolean) As IEnumerable(Of List(Of ILocation))
        If wOrientation = True Then
            Dim x = From t In locis Group By t.PAS, t.IsComplementer Into Group

            For Each j In x
                Yield j.Group.ToList

            Next
        Else
            Dim x = From t In locis Group By t.PAS Into Group

            For Each j In x
                Yield j.Group.ToList

            Next
        End If

    End Function

    Public Shared Iterator Function Iterate_By_LocationString(locis As List(Of ILocation)) As IEnumerable(Of List(Of ILocation))
        Dim x = From t In locis Group By l = Szunyi.BLs.Location.Common.GetLocationString(t) Into Group

        For Each j In x
            Yield j.Group.ToList

        Next
    End Function
#End Region

#Region "Basic_Location"
    Public Shared Iterator Function Iterate_By_Locis_Start_Xth(locis As List(Of Szunyi.BLs.Basic_Location), xth As Integer) As IEnumerable(Of List(Of Szunyi.BLs.Basic_Location))

        Dim x = From t In locis Group By Szunyi.BLs.Location.Exon_Intron.Get_XTH_Exon_Location(t, xth).LocationStart, t.Location.Operator Into Group

    End Function

    Public Shared Iterator Function Iterate_By_Locis_Start(Locis As List(Of Szunyi.BLs.Basic_Location)) As IEnumerable(Of List(Of Szunyi.BLs.Basic_Location))
        Dim x = From t In Locis Group By t.Location.LocationStart, t.Location.IsComplementer Into Group

        For Each j In x
            Yield j.Group.ToList

        Next

    End Function

    Public Shared Iterator Function Iterate_By_Locis_Start_End(Locis As List(Of Szunyi.BLs.Basic_Location)) As IEnumerable(Of List(Of Szunyi.BLs.Basic_Location))
        Dim x = From t In Locis Group By t.Location.LocationStart, t.Location.LocationEnd, t.Location.IsComplementer Into Group

        For Each j In x
            Yield j.Group.ToList
        Next
    End Function


    Public Shared Iterator Function Iterate_By_Locis_Start_End_Xth(Locis As List(Of Szunyi.BLs.Basic_Location), xth As Integer) As IEnumerable(Of List(Of Szunyi.BLs.Basic_Location))
        Dim x = From t In Locis Group By Szunyi.BLs.Location.Exon_Intron.Get_XTH_Exon_Location(t.Location, xth).LocationStart,
                                            Szunyi.BLs.Location.Exon_Intron.Get_XTH_Exon_Location(t, xth).LocationEnd, t.Location.Operator Into Group

        For Each j In x
            Yield j.Group.ToList
        Next
    End Function
    Public Shared Iterator Function Iterate_By_Locis_End_Xth(locis As List(Of Szunyi.BLs.Basic_Location), xth As Integer) As IEnumerable(Of List(Of Szunyi.BLs.Basic_Location))

        Dim x = From t In locis Group By Szunyi.BLs.Location.Exon_Intron.Get_XTH_Exon_Location(t, xth).LocationEnd, t.Location.Operator Into Group

    End Function
    Public Shared Iterator Function Iterate_By_Locis_End(Locis As List(Of Szunyi.BLs.Basic_Location)) As IEnumerable(Of List(Of Szunyi.BLs.Basic_Location))
        Dim x = From t In Locis Group By t.Location.LocationEnd, t.Location.IsComplementer Into Group

        For Each j In x
            Yield j.Group.ToList

        Next

    End Function
    Public Shared Iterator Function Iterate_By_Locis_TSS(locis As List(Of Basic_Location)) As IEnumerable(Of List(Of Basic_Location))
        Dim x = From t In locis Group By t.Location.TSS, t.Location.IsComplementer Into Group

        For Each j In x
            Yield j.Group.ToList

        Next
    End Function

    Public Shared Iterator Function Iterate_By_Locis_PAS(locis As List(Of Basic_Location)) As IEnumerable(Of List(Of Basic_Location))
        Dim x = From t In locis Group By t.Location.PAS, t.Location.IsComplementer Into Group

        For Each j In x
            Yield j.Group.ToList

        Next
    End Function
#End Region



End Class

Public Class TemplateSwitch
    Public Property Loci As Bio.IO.GenBank.ILocation
    Public Property FiveOH As Bio.Sequence
    Public Property Length As Integer
    Public Property DonorSite As String
    Public Property AcceptorSite As String
    Public Property Repeat_First_Part As String
    Public Property Repeat_Second_Part As String
    Public Property Consensus As String
    Public Property Repeat_Length As Integer
    Public Property Count As Integer = 1
    Public Property Sw As New Bio.Algorithms.Alignment.SmithWatermanAligner
    Public Property FirstOffset As Int16
    Public Property LastOffset As Int16
    Public Property Surrounding_Seq_DonorSite As String
    Public Property Surrounding_Seq_AcceptorSite As String

    Public Sub New(Seq As Bio.ISequence, Intron As Bio.IO.GenBank.FeatureItem)
        Me.Loci = Intron.Location
        DoIt(Seq)
    End Sub
    Public Sub New(Seq As Bio.ISequence, Intron As Bio.IO.GenBank.Location)
        Me.Loci = Intron
        DoIt(Seq)
    End Sub
    Public Sub New(Seq As Bio.ISequence, Intron As Bio.IO.GenBank.ILocation)
        Me.Loci = Intron
        DoIt(Seq)
    End Sub
    Public Sub New(Seq As Bio.ISequence, Introns As List(Of Bio.IO.GenBank.ILocation))
        Me.Loci = Introns.First
        Me.Count = Introns.Count
        DoIt(Seq)
    End Sub
    Private Sub DoIt(Seq As Bio.ISequence)
        Sw.SimilarityMatrix = New Bio.SimilarityMatrices.DiagonalSimilarityMatrix(2, -10)
        Dim s11 = Seq.GetSubSequence(Loci.LocationStart - 7, 6 * 2)
        Dim s12 = Seq.GetSubSequence(Loci.LocationEnd - 1 - 5, 6 * 2)
        Me.Surrounding_Seq_DonorSite = s11.ConvertToString
        Me.Surrounding_Seq_AcceptorSite = s12.ConvertToString

        If Loci.IsComplementer = True Then
            Dim s = Seq.GetSubSequence(Loci.LocationStart - 1, 2)
            AcceptorSite = s.GetReverseComplementedSequence.ConvertToString
            Dim sl = Seq.GetSubSequence(Loci.LocationEnd - 2, 2)
            DonorSite = sl.GetReverseComplementedSequence.ConvertToString
        Else
            DonorSite = Seq.GetSubSequence(Loci.LocationStart - 1, 2).ConvertToString
            AcceptorSite = Seq.GetSubSequence(Loci.LocationEnd - 2, 2).ConvertToString

        End If


        For i1 = 5 To 2 Step -1
            Dim s1 = Seq.GetSubSequence(Loci.LocationStart - i1 - 1, i1 * 2)
            Dim s2 = Seq.GetSubSequence(Loci.LocationEnd - 1 - i1 + 1, i1 * 2)

            Dim res = Sw.Align(s1, s2)
            Dim Reps = Get_Overlaps(res.First.AlignedSequences, i1, i1)
            If IsNothing(Reps) = False Then

                Me.Repeat_First_Part = Reps.Sequences.First.ConvertToString
                Me.Repeat_Second_Part = Reps.Sequences.Last.ConvertToString
                Dim b As Bio.Sequence = Reps.Metadata("Consensus")
                Me.Consensus = b.ConvertToString
                Me.Repeat_Length = b.Count
                Dim StartOffSets As List(Of Long) = Reps.Metadata("StartOffsets")
                Me.FirstOffset = StartOffSets.First
                Me.LastOffset = StartOffSets.Last
                If Repeat_Length >= 4 Then
                    Dim kj As Int16 = 43
                End If
                Exit For
            End If
        Next




    End Sub
    Private Function Get_Overlaps(AlignedSequences As List(Of Bio.Algorithms.Alignment.IAlignedSequence), LeftPos As Integer, RightPos As Integer) As Bio.Algorithms.Alignment.IAlignedSequence
        For Each AlSeq In AlignedSequences
            Dim StartOffSets As List(Of Long) = AlSeq.Metadata("StartOffsets")
            Dim EndOffSets As List(Of Long) = AlSeq.Metadata("EndOffsets")
            If StartOffSets.First <= LeftPos AndAlso StartOffSets.Last <= LeftPos AndAlso EndOffSets.First >= RightPos - 1 AndAlso EndOffSets.Last >= RightPos - 1 Then
                Return AlSeq
            End If
        Next
        Return Nothing
    End Function

    Public Overrides Function ToString() As String
        Dim str As New System.Text.StringBuilder
        str.Append(Me.Count).Append(vbTab)
        str.Append(Szunyi.BLs.Location.Common.GetLocationStringTab(Me.Loci)).Append(vbTab)
        str.Append(Szunyi.BLs.Location.Common.Get_Length(Me.Loci)).Append(vbTab)
        str.Append(Me.DonorSite).Append(vbTab)
        str.Append(Me.AcceptorSite).Append(vbTab)
        str.Append(Me.Surrounding_Seq_DonorSite).Append(vbTab)
        str.Append(Me.Surrounding_Seq_AcceptorSite).Append(vbTab)
        str.Append(Me.Repeat_First_Part).Append(vbTab)
        str.Append(Me.Repeat_Second_Part).Append(vbTab)
        str.Append(Me.Consensus).Append(vbTab)
        str.Append(Me.Repeat_Length).Append(vbTab)
        str.Append(Me.FirstOffset).Append(vbTab)
        str.Append(Me.LastOffset).Append(vbTab)
        Return str.ToString
    End Function
End Class

Public Class Introns


    Public Shared Function Get_Overlaps(AlignedSequences As List(Of Bio.Algorithms.Alignment.IAlignedSequence), LeftPos As Integer, RightPos As Integer) As Bio.Algorithms.Alignment.IAlignedSequence
        For Each AlSeq In AlignedSequences
            Dim StartOffSets As List(Of Long) = AlSeq.Metadata("StartOffsets")
            Dim EndOffSets As List(Of Long) = AlSeq.Metadata("EndOffsets")

            If StartOffSets.First = StartOffSets.Last Then
                If StartOffSets.First < LeftPos And EndOffSets.First >= LeftPos Then
                    Return AlSeq
                Else
                    Dim kj As Int16 = 54
                End If

            End If

            'If StartOffSets.First <= LeftPos AndAlso StartOffSets.Last <= LeftPos AndAlso EndOffSets.First >= RightPos - 1 AndAlso EndOffSets.Last >= RightPos - 1 Then
            'Return AlSeq
            ' End If
        Next
        Return Nothing
    End Function
    Public Shared Function TemplateSwichHeader() As String

        Dim str As New System.Text.StringBuilder
        str.Append("strand").Append(vbTab)
        str.Append("start").Append(vbTab)
        str.Append("end").Append(vbTab)
        str.Append("count").Append(vbTab)
        str.Append("NOf diff exp").Append(vbTab)
        str.Append("intron length").Append(vbTab)
        str.Append("Donor Site").Append(vbTab)
        str.Append("Acceptor Site").Append(vbTab)
        str.Append("Repeat First Part").Append(vbTab)
        str.Append("Repeat Second Part").Append(vbTab)
        str.Append("Consensus").Append(vbTab)
        str.Append("Repeat Length")
        Return str.ToString




    End Function
    Public Shared Function TemplateSwitch(Seq As Bio.ISequence,
                                          Intron As Bio.IO.GenBank.FeatureItem,
                                          Sw As Bio.Algorithms.Alignment.SmithWatermanAligner) As String
        Dim str As New System.Text.StringBuilder
        '  str.Append(Szunyi.Location.Common.GetLocationStringTab(Intron)).Append(vbTab)
        '     str.Append(Intron.Qualifiers(Bio.IO.GenBank.StandardQualifierNames.IdentifiedBy).Count).Append(vbTab)
        '       str.Append(Intron.Label).Append(vbTab)
        str.Append(Intron.Location.LocationEnd - Intron.Location.LocationStart).Append(vbTab)
        If Intron.Location.IsComplementer = True Then
            Dim s = Seq.GetSubSequence(Intron.Location.LocationStart - 1, 2)
            s = s.GetReverseComplementedSequence
            Dim sl = Seq.GetSubSequence(Intron.Location.LocationEnd - 2, 2)
            sl = sl.GetReverseComplementedSequence
            str.Append(sl.ConvertToString).Append(vbTab)
            str.Append(s.ConvertToString).Append(vbTab)

        Else
            Dim s = Seq.GetSubSequence(Intron.Location.LocationStart - 1, 2)
            str.Append(s.ConvertToString).Append(vbTab)
            Dim sl = Seq.GetSubSequence(Intron.Location.LocationEnd - 2, 2)
            str.Append(sl.ConvertToString).Append(vbTab)
        End If
        Dim s11 = Seq.GetSubSequence(Intron.Location.LocationStart - 1 - 5, 5 * 2)
        Dim s12 = Seq.GetSubSequence(Intron.Location.LocationEnd - 5, 5 * 2)
        str.Append(s11.ConvertToString).Append(vbTab)
        str.Append(s12.ConvertToString).Append(vbTab)
        For i1 = 5 To 2 Step -1
            Dim s1 = Seq.GetSubSequence(Intron.Location.LocationStart - 1 - i1, i1 * 2)
            Dim s2 = Seq.GetSubSequence(Intron.Location.LocationEnd - i1, i1 * 2)

            Dim res = Sw.Align(s1, s2)
            Dim Reps = Get_Overlaps(res.First.AlignedSequences, i1, i1)
            If IsNothing(Reps) = False Then
                str.Append(Reps.Sequences.First.ConvertToString).Append(vbTab)
                str.Append(Reps.Sequences.Last.ConvertToString).Append(vbTab)
                Dim b As Bio.Sequence = Reps.Metadata("Consensus")
                str.Append(b.ConvertToString).Append(vbTab)
                str.Append(Reps.Sequences.First.Count)
                Exit For
            End If
        Next

        Return str.ToString
    End Function

    Public Shared Function TemplateSwitch(Seq As Bio.ISequence,
                                              Location As Bio.IO.GenBank.ILocation,
                                              Sw As Bio.Algorithms.Alignment.SmithWatermanAligner) As TemplateSwitch
        Dim str As New System.Text.StringBuilder
        str.Append(Szunyi.BLs.Location.Common.GetLocationStringTab(Location)).Append(vbTab)
        str.Append(Location.LocationEnd - Location.LocationStart).Append(vbTab)

        Dim x As New TemplateSwitch(Seq, Location)

        Return x
    End Function
    Public Shared Function TemplateSwitch(Seq As Bio.ISequence,
                                              Locations As List(Of Bio.IO.GenBank.ILocation),
                                              Sw As Bio.Algorithms.Alignment.SmithWatermanAligner) As String
        Dim Location = Locations.First
        Dim str As New System.Text.StringBuilder

        Dim x As New TemplateSwitch(Seq, Locations)

        str.Append(Szunyi.BLs.Location.Common.GetLocationStringTab(Location)).Append(vbTab)
        str.Append(Location.LocationEnd - Location.LocationStart).Append(vbTab)
        If Location.IsComplementer = True Then
            Dim s = Seq.GetSubSequence(Location.LocationStart - 1, 2)
            s = s.GetReverseComplementedSequence
            Dim sl = Seq.GetSubSequence(Location.LocationEnd - 2, 2)
            sl = sl.GetReverseComplementedSequence
            str.Append(sl.ConvertToString).Append(vbTab)
            str.Append(s.ConvertToString).Append(vbTab)
        Else
            Dim s = Seq.GetSubSequence(Location.LocationStart - 1, 2)
            str.Append(s.ConvertToString).Append(vbTab)
            Dim sl = Seq.GetSubSequence(Location.LocationEnd - 2, 2)
            str.Append(sl.ConvertToString).Append(vbTab)
        End If
        Dim s11 = Seq.GetSubSequence(Location.LocationStart - 1 - 5, 5 * 2)
        Dim s12 = Seq.GetSubSequence(Location.LocationEnd - 5, 5 * 2)
        str.Append(s11.ConvertToString).Append(vbTab)
        str.Append(s12.ConvertToString).Append(vbTab)
        For i1 = 5 To 2 Step -1
            Dim s1 = Seq.GetSubSequence(Location.LocationStart - 1 - i1, i1 * 2)
            Dim s2 = Seq.GetSubSequence(Location.LocationEnd - i1, i1 * 2)

            Dim res = Sw.Align(s1, s2)
            Dim Reps = Get_Overlaps(res.First.AlignedSequences, i1, i1)
            If IsNothing(Reps) = False Then
                str.Append(Reps.Sequences.First.ConvertToString).Append(vbTab)
                str.Append(Reps.Sequences.Last.ConvertToString).Append(vbTab)
                Dim b As Bio.Sequence = Reps.Metadata("Consensus")
                str.Append(b.ConvertToString).Append(vbTab)
                str.Append(Reps.Sequences.First.Count)
                Exit For
            End If
        Next

        Return str.ToString
    End Function

    Private Shared Function Get_Repetition_Rev(s1 As Bio.ISequence, s2 As Bio.ISequence) As String
        Dim str As New System.Text.StringBuilder
        For i1 = s1.Count - 1 To 0 Step -1
            If s1(i1) = s2(i1) Then
                str.Append(ChrW(s1(i1)))
            Else
                Exit For
            End If
        Next
        str.Append(vbTab).Append(str.Length - 1)
        Return str.ToString
    End Function
    Private Shared Function Get_Repetition(s1 As Bio.ISequence, s2 As Bio.ISequence) As String
        Dim str As New System.Text.StringBuilder
        For i1 = 0 To s1.Count - 1
            If s1(i1) = s2(i1) Then
                str.Append(ChrW(s1(i1)))
            Else
                Exit For
            End If
        Next
        str.Append(vbTab).Append(str.Length - 1)
        Return str.ToString
    End Function
End Class


