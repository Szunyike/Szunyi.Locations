Imports System.IO
Imports Bio
Imports Bio.IO.GenBank
Imports Szunyi.Common
Public Class Sites
    Public Property posStrand As List(Of ILocation)()
    Public Property negStrand As List(Of ILocation)()

    Public Property Pos As Integer()
    Public Property Neg As Integer()

    Dim x As List(Of ILocation)()
    Dim y As List(Of ILocation)()

    Public sort As Locations_By

    Public Property Width As Integer
    Public Property Local_Width As Integer
    Public Property p_Threshold As Double
#Region "NEW"

    Private Sub Set_Parameters(Length As Integer, sort As Locations_By)
        Me.sort = sort
        ReDim x(Length)
        ReDim y(Length)
        ReDim Pos(Length)
        ReDim Neg(Length)
        ReDim posStrand(Length)
        ReDim negStrand(Length)
    End Sub
    Private Sub Set_Parameters(Seq As Bio.ISequence, sort As Locations_By)
        Me.sort = sort
        ReDim x(Seq.Count)
        ReDim y(Seq.Count)
        ReDim Pos(Seq.Count)
        ReDim Neg(Seq.Count)
        ReDim posStrand(Seq.Count)
        ReDim negStrand(Seq.Count)
    End Sub
    Private Sub Set_Counts()

        For i1 = 0 To posStrand.Count - 1
            If IsNothing(posStrand(i1)) = True Then
                Pos(i1) = 0
            Else
                Pos(i1) = posStrand(i1).Count
            End If
        Next

        For i1 = 0 To negStrand.Count - 1
            If IsNothing(negStrand(i1)) = True Then
                Neg(i1) = 0
            Else
                Neg(i1) = negStrand(i1).Count
            End If
        Next
    End Sub
    Public Sub New(Length As Integer, LoL As List(Of Basic_Location), sort As Locations_By)
        Set_Parameters(Length, sort)
        SortIt(LoL, sort)
        Set_Counts()
    End Sub
    Public Sub SortIt(LoL As List(Of Basic_Location), sort As Locations_By)
        Dim Mappings As New List(Of ILocation)
        For Each l In LoL
            Mappings.Add(l.Location)
        Next

        Select Case sort
            Case Locations_By.TSS
                For Each m In Mappings
                    With m
                        If .IsComplementer = False Then
                            If IsNothing(posStrand(.TSS)) = True Then posStrand(.TSS) = New List(Of ILocation)
                            posStrand(.TSS).Add(m)
                        Else
                            If IsNothing(negStrand(.TSS)) = True Then negStrand(.TSS) = New List(Of ILocation)
                            negStrand(.TSS).Add(m)
                        End If
                    End With

                Next
            Case Locations_By.PAS
                For Each m In Mappings
                    With m
                        If .IsComplementer = False Then
                            If IsNothing(posStrand(.PAS)) = True Then posStrand(.PAS) = New List(Of ILocation)
                            posStrand(.PAS).Add(m)
                        Else
                            If IsNothing(negStrand(.PAS)) = True Then negStrand(.PAS) = New List(Of ILocation)
                            negStrand(.PAS).Add(m)
                        End If
                    End With

                Next
            Case Locations_By.LS
                For Each m In Mappings
                    With m
                        If .IsComplementer = False Then
                            If IsNothing(posStrand(.LocationStart)) = True Then posStrand(.LocationStart) = New List(Of ILocation)
                            posStrand(.LocationStart).Add(m)
                        Else
                            If IsNothing(negStrand(.LocationStart)) = True Then negStrand(.LocationStart) = New List(Of ILocation)
                            negStrand(.LocationStart).Add(m)
                        End If
                    End With

                Next
            Case Locations_By.LE
                For Each m In Mappings
                    With m
                        If .IsComplementer = False Then
                            If IsNothing(posStrand(.LocationEnd)) = True Then posStrand(.LocationEnd) = New List(Of ILocation)
                            posStrand(.LocationEnd).Add(m)
                        Else
                            If IsNothing(negStrand(.LocationEnd)) = True Then negStrand(.LocationEnd) = New List(Of ILocation)
                            negStrand(.LocationEnd).Add(m)
                        End If
                    End With

                Next

            Case Else
                Dim kj As Int16 = 54

        End Select

    End Sub
    Public Sub New(Seq As Bio.ISequence, LoL As List(Of Basic_Location), sort As Locations_By)
        Set_Parameters(Seq, sort)
        SortIt(LoL, sort)
        Set_Counts()
    End Sub

    Public Sub New(Seq As Bio.Sequence, Mappings As List(Of ILocation), Sort As Locations_By, Width As Integer, LocalWidth As Integer, P_Threshold As Double)
        Me.Width = Width
        Me.Local_Width = LocalWidth
        Me.p_Threshold = P_Threshold
        Set_Parameters(Seq, Sort)
        Select Case Sort
            Case Locations_By.TSS
                For Each m In Mappings
                    With m
                        If .IsComplementer = False Then
                            If IsNothing(posStrand(.TSS)) = True Then posStrand(.TSS) = New List(Of ILocation)
                            posStrand(.TSS).Add(m)
                        Else
                            If IsNothing(negStrand(.TSS)) = True Then negStrand(.TSS) = New List(Of ILocation)
                            negStrand(.TSS).Add(m)
                        End If
                    End With

                Next
            Case Locations_By.PAS
                For Each m In Mappings
                    With m
                        If .IsComplementer = False Then
                            If IsNothing(posStrand(.PAS)) = True Then posStrand(.PAS) = New List(Of ILocation)
                            posStrand(.PAS).Add(m)
                        Else
                            If IsNothing(negStrand(.PAS)) = True Then negStrand(.PAS) = New List(Of ILocation)
                            negStrand(.PAS).Add(m)
                        End If
                    End With

                Next
            Case Locations_By.LS
                For Each m In Mappings
                    With m
                        If .IsComplementer = False Then
                            If IsNothing(posStrand(.LocationStart)) = True Then posStrand(.LocationStart) = New List(Of ILocation)
                            posStrand(.LocationStart).Add(m)
                        Else
                            If IsNothing(negStrand(.LocationStart)) = True Then negStrand(.LocationStart) = New List(Of ILocation)
                            negStrand(.LocationStart).Add(m)
                        End If
                    End With

                Next
            Case Locations_By.LE
                For Each m In Mappings
                    With m
                        If .IsComplementer = False Then
                            If IsNothing(posStrand(.LocationEnd)) = True Then posStrand(.LocationEnd) = New List(Of ILocation)
                            posStrand(.LocationEnd).Add(m)
                        Else
                            If IsNothing(negStrand(.LocationEnd)) = True Then negStrand(.LocationEnd) = New List(Of ILocation)
                            negStrand(.LocationEnd).Add(m)
                        End If
                    End With

                Next

            Case Else
                Dim kj As Int16 = 54

        End Select
        Set_Counts()
    End Sub
    Public Sub New(Length As Integer, Mappings As List(Of ILocation), Sort As Locations_By)
        Set_Parameters(Length, Sort)
        Select Case Sort
            Case Locations_By.TSS
                For Each m In Mappings
                    With m
                        If .IsComplementer = False Then
                            If IsNothing(posStrand(.TSS)) = True Then posStrand(.TSS) = New List(Of ILocation)
                            posStrand(.TSS).Add(m)
                        Else
                            If IsNothing(negStrand(.TSS)) = True Then negStrand(.TSS) = New List(Of ILocation)
                            negStrand(.TSS).Add(m)
                        End If
                    End With

                Next
            Case Locations_By.PAS
                For Each m In Mappings
                    With m
                        If .IsComplementer = False Then
                            If IsNothing(posStrand(.PAS)) = True Then posStrand(.PAS) = New List(Of ILocation)
                            posStrand(.PAS).Add(m)
                        Else
                            If IsNothing(negStrand(.PAS)) = True Then negStrand(.PAS) = New List(Of ILocation)
                            negStrand(.PAS).Add(m)
                        End If
                    End With

                Next
            Case Locations_By.TSS
                For Each m In Mappings
                    With m
                        If .IsComplementer = False Then
                            If IsNothing(posStrand(.LocationStart)) = True Then posStrand(.LocationStart) = New List(Of ILocation)
                            posStrand(.LocationStart).Add(m)
                        Else
                            If IsNothing(negStrand(.LocationStart)) = True Then negStrand(.LocationStart) = New List(Of ILocation)
                            negStrand(.LocationStart).Add(m)
                        End If
                    End With

                Next
            Case Locations_By.LE
                For Each m In Mappings
                    With m
                        If .IsComplementer = False Then
                            If IsNothing(posStrand(.LocationEnd)) = True Then posStrand(.LocationEnd) = New List(Of ILocation)
                            posStrand(.LocationEnd).Add(m)
                        Else
                            If IsNothing(negStrand(.LocationEnd)) = True Then negStrand(.LocationEnd) = New List(Of ILocation)
                            negStrand(.LocationEnd).Add(m)
                        End If
                    End With

                Next

            Case Else
                Dim kj As Int16 = 54

        End Select
        Set_Counts()
    End Sub

#End Region

#Region "Get_Distribution"
    Public Function Get_Distribution(Feats As List(Of Basic_Location)) As List(Of Integer)
        Dim out As New List(Of Integer)
        For Each Feat In Feats
            Dim str As New System.Text.StringBuilder

            Dim p = Get_Distribution(Feat.Location, Me.sort)
            out.Add(p.GetText)
        Next
        Return out
    End Function
    Public Function Get_Distribution_Values(Feats As List(Of Basic_Location)) As List(Of List(Of Integer))
        Dim out As New List(Of List(Of Integer))
        For Each Feat In Feats
            Dim str As New System.Text.StringBuilder

            Dim p = Get_Distribution(Feat.Location, Me.sort)
            out.Add(p)
        Next
        Return out
    End Function
    Public Function Get_Distribution(Feats As List(Of FeatureItem)) As List(Of String)
        Dim out As New List(Of String)
        For Each Feat In Feats
            Dim str As New System.Text.StringBuilder

            Dim p = Get_Distribution(Feat.Location, Me.sort)
            out.Add(p.Gettext)
        Next
        Return out
    End Function
    Public Function Get_Distribution_Header() As String
        Dim str As New System.Text.StringBuilder
        For i1 = -Local_Width To Local_Width
            str.Append(i1).Append(vbTab)
        Next
        If str.Length > 0 Then str.Length -= 1
        Return str.ToString
    End Function
    Public Function Get_Distributions_woIndex(index As Integer, isComplementer As Boolean, sort As Locations_By, width As Integer) As List(Of Integer)
        Dim out As New List(Of Integer)
        Dim Basic As Integer = index
        Dim max As Integer = posStrand.Count
        If isComplementer = True Then
            For i1 = Basic - width To Basic + width
                If i1 > -1 And i1 < max And i1 <> index Then
                    If IsNothing(negStrand(i1)) = True Then
                        out.Add(0)
                    Else
                        out.Add(negStrand(i1).Count)
                    End If
                End If
            Next
        Else
            For i1 = Basic - width To Basic + width
                If i1 > -1 And i1 < max And i1 <> index Then
                    If IsNothing(posStrand(i1)) = True Then
                        out.Add(0)
                    Else
                        out.Add(posStrand(i1).Count)
                    End If
                End If
            Next
        End If

        Return out
    End Function

    Public Function Get_Distributions(index As Integer, isComplementer As Boolean, sort As Locations_By, width As Integer) As List(Of Integer)
        Dim out As New List(Of Integer)
        Dim Basic As Integer = index
        Dim max As Integer = posStrand.Count
        If isComplementer = True Then
            For i1 = Basic - width To Basic + width
                If i1 > -1 And i1 < max Then
                    If IsNothing(negStrand(i1)) = True Then
                        out.Add(0)
                    Else
                        out.Add(negStrand(i1).Count)
                    End If
                Else
                    out.Add(0)
                End If
            Next
        Else
            For i1 = Basic - width To Basic + width
                If i1 > -1 And i1 < max Then
                    If IsNothing(posStrand(i1)) = True Then
                        out.Add(0)
                    Else
                        out.Add(posStrand(i1).Count)
                    End If
                Else
                    out.Add(0)
                End If
            Next
        End If

        Return out
    End Function
    Public Function Get_Distribution(Pos As Integer, IsComplementer As Boolean, Sort As Locations_By) As List(Of Integer)
        Dim out As New List(Of Integer)
        Dim Basic As Integer = Pos
        Dim max As Integer = posStrand.Count
        If IsComplementer = True Then
            For i1 = Basic - Local_Width To Basic + Local_Width
                If i1 > -1 And i1 < max Then
                    If IsNothing(negStrand(i1)) = True Then
                        out.Add(0)
                    Else
                        out.Add(negStrand(i1).Count)
                    End If
                Else
                    out.Add(0)
                End If
            Next
        Else
            For i1 = Basic - Local_Width To Basic + Local_Width
                If i1 > -1 And i1 < max Then
                    If IsNothing(posStrand(i1)) = True Then
                        out.Add(0)
                    Else
                        out.Add(posStrand(i1).Count)
                    End If
                Else
                    out.Add(0)
                End If
            Next
        End If

        Return out
    End Function
    Public Function Get_Distribution(first As ILocation, Sort As Locations_By) As List(Of Integer)
        Dim out As New List(Of Integer)
        Dim Basic As Integer = 0
        Dim max As Integer = posStrand.Count
        Select Case Sort
            Case Locations_By.TSS
                Basic = first.LocationStart
            Case Locations_By.LE
                Basic = first.LocationEnd
            Case Locations_By.TSS
                Basic = first.TSS
            Case Locations_By.PAS
                Basic = first.PAS
        End Select
        Return Get_Distribution(Basic, first.IsComplementer, Sort)
    End Function
    Public Function Get_Distribution(first As Basic_Location, Sort As Locations_By) As List(Of Integer)
        Dim out As New List(Of Integer)
        Dim Basic As Integer = 0
        Dim max As Integer = posStrand.Count
        Select Case Sort
            Case Locations_By.TSS
                Basic = first.Location.LocationStart
            Case Locations_By.LE
                Basic = first.Location.LocationEnd
            Case Locations_By.TSS
                Basic = first.Location.TSS
            Case Locations_By.PAS
                Basic = first.Location.PAS
        End Select
        Return Get_Distribution(Basic, first.Location.IsComplementer, Sort)
    End Function

#End Region


    Public Function Get_Mappings(Width As Integer, first As Basic_Location, Sort As Locations_By) As List(Of ILocation)
        Dim out As New List(Of ILocation)
        Dim Basic As Integer = 0
        Dim max As Integer = posStrand.Count
        Select Case Sort
            Case Locations_By.LS
                Basic = first.Location.LocationStart
            Case Locations_By.LE
                Basic = first.Location.LocationEnd
            Case Locations_By.TSS
                Basic = first.Location.TSS
            Case Locations_By.PAS
                Basic = first.Location.PAS
        End Select
        If first.Location.IsComplementer = True Then
            For i1 = Basic - Width To Basic + Width
                If i1 > -1 And i1 < max Then
                    If IsNothing(negStrand(i1)) = False Then
                        out.AddRange(negStrand(i1))
                    End If
                End If
            Next
        Else
            For i1 = Basic - Width To Basic + Width
                If i1 > -1 And i1 < max Then
                    If IsNothing(posStrand(i1)) = False Then
                        out.AddRange(posStrand(i1))
                    End If
                End If
            Next
        End If

        Return out
    End Function

    Public Function Get_Mappings(Width As Integer, loci As ILocation, Sort As Locations_By) As List(Of ILocation)
        Dim out As New List(Of ILocation)
        Dim Basic As Integer = 0
        Dim max As Integer = posStrand.Count
        Select Case Sort
            Case Locations_By.LS
                Basic = loci.LocationStart
            Case Locations_By.LE
                Basic = loci.LocationEnd
            Case Locations_By.TSS
                Basic = loci.TSS
            Case Locations_By.PAS
                Basic = loci.PAS
        End Select
        If loci.IsComplementer = True Then
            For i1 = Basic - Width To Basic + Width
                If i1 > -1 And i1 < max Then
                    If IsNothing(negStrand(i1)) = False Then
                        out.AddRange(negStrand(i1))
                    End If
                End If
            Next
        Else
            For i1 = Basic - Width To Basic + Width
                If i1 > -1 And i1 < max Then
                    If IsNothing(posStrand(i1)) = False Then
                        out.AddRange(posStrand(i1))
                    End If
                End If
            Next
        End If
        Return out
    End Function

    Public Function Get_All_Count() As String
        Dim str As New System.Text.StringBuilder
        str.Append("pos").Append(vbTab)
        str.Append("+").Append(vbTab)
        str.Append("-").AppendLine()
        For i1 = 0 To Me.posStrand.Count - 1
            str.Append(i1).Append(vbTab)
            If IsNothing(posStrand(i1)) = True Then
                str.Append(0).Append(vbTab)
            Else
                str.Append(posStrand(i1).Count).Append(vbTab)
            End If
            If IsNothing(negStrand(i1)) = True Then
                str.Append(0).Append(vbTab)
            Else
                str.Append(negStrand(i1).Count).Append(vbTab)
            End If
            str.AppendLine()
        Next
        Return str.ToString

    End Function

    Public Function Get_Statistic(Merge_Width As Integer, Loci As ILocation, Sort As Locations_By) As Statistic
        Dim x = Get_Distribution(Loci, Sort)
        Dim res As New Statistic
        res.Sum = x.Sum
        res.Avarage = x.Sum / x.Count
        res.Values = x
        ' res.Mappings '= Get_Mappings(Merge_Width, Loci, Sort)
        Return res
    End Function

    Public Function Get_alpha() As Double
        Dim LocalMaxPos = Get_Local_Maximums(False, Me.Width)
        Dim LocalMaxNeg = Get_Local_Maximums(True, Me.Width)
        Return p_Threshold / (Width * 2 + 1) / (LocalMaxPos.Count + LocalMaxNeg.Count)
    End Function
    ''' <summary>
    ''' LocalWidth +- Widht +-
    ''' </summary>
    ''' <param name="LocalWidth"></param>
    ''' <param name="Width"></param>
    ''' <param name="P_Threshold"></param>
    ''' <returns></returns>
    Public Function Poisson(LocalWidth As Integer, Width As Integer, Optional P_Threshold As Double = 0.05) As String
        Dim LocalMaxPos = Get_Local_Maximums(False)
        Dim LocalMaxNeg = Get_Local_Maximums(True)
        Dim str As New System.Text.StringBuilder

        Dim Alpha = Get_alpha()

        Return str.ToString

    End Function



    Private Function Get_Poisson_Value(Width As Integer, Index As Integer, isComplementer As Boolean, Value As Integer) As Double
        Dim Hundrends = Get_Distribution(Index, isComplementer, sort)
        Dim avr = Hundrends.Sum / Hundrends.Count
        '    Dim poi As New Accord.Statistics.Distributions.Univariate.PoissonDistribution(avr)
        'Dim poi As New Accord.Statistics.Distributions.Univariate.GeometricDistribution(avr)

        '     Dim p = 1 - poi.DistributionFunction(Pos(Index), True)
        '  Return p
    End Function
    ''' <summary>
    ''' LocalWidth +- Widht +-
    ''' </summary>
    ''' <param name="LocalWidth"></param>
    ''' <param name="Width"></param>
    ''' <param name="P_Threshold"></param>
    ''' <returns></returns>
    Public Function Poisson(Feats As List(Of FeatureItem), LocalWidth As Integer, Width As Integer, File As FileInfo, Optional P_Threshold As Double = 0.05) As List(Of String)
        Dim LocalMaxPos = Get_Local_Maximums(False)
        Dim LocalMaxNeg = Get_Local_Maximums(True)

        Dim Out As New List(Of String)
        Dim Alpha = P_Threshold / (Width * 2 + 1) / (LocalMaxPos.Count + LocalMaxNeg.Count)

        For Each Feat In Feats
            Dim str As New System.Text.StringBuilder
            str.Append(Feat.Key).Append(vbTab)
            str.Append(Feat.Qualifiers(StandardQualifierNames.Label).First).Append(vbTab)
            str.Append(Feat.Location.Current_Position(Me.sort)).Append(vbTab)
            str.Append(Szunyi.BLs.Location.Common.Get_Strand(Feat.Location)).Append(vbTab)
            Dim Index = Feat.Location.Current_Position(Me.sort)
            Dim p As Poisson
            If Feat.Location.IsComplementer = True Then
                p = New Poisson(Me, Width, Index, True, Neg(Index), File)
            Else
                p = New Poisson(Me, Width, Index, False, Pos(Index), File)
            End If
            str.Append(p.Count).Append(vbTab)
            str.Append(p.Hundrends.Sum).Append(vbTab)
            str.Append(p.p).Append(vbTab)
            If Feat.Location.IsComplementer = False Then
                str.Append(Is_Local_Maximum(Index, LocalWidth, False))
            Else
                str.Append(Is_Local_Maximum(Index, LocalWidth, True))
            End If
            str.Append(vbTab)
            If p.p < Alpha Then
                str.Append("Passed").Append(vbTab)
            Else
                str.Append("Failed").Append(vbTab)
            End If

            Out.Add(str.ToString)
        Next


        Return Out

    End Function

    ''' <summary>
    ''' LocalWidth +- Widht +-
    ''' </summary>
    ''' <param name="LocalWidth"></param>
    ''' <param name="Width"></param>
    ''' <param name="P_Threshold"></param>
    ''' <returns></returns>
    Public Function Poisson(Feats As List(Of Basic_Location), LocalWidth As Integer, Width As Integer, File As FileInfo, Optional P_Threshold As Double = 0.05) As List(Of Poisson)
        Dim LocalMaxPos = Get_Local_Maximums(False)
        Dim LocalMaxNeg = Get_Local_Maximums(True)

        Dim Out As New List(Of Poisson)
        Dim Alpha = P_Threshold / (Width * 2 + 1) / (LocalMaxPos.Count + LocalMaxNeg.Count)

        For Each Feat In Feats
            Dim Index = Feat.Location.Current_Position(Me.sort)
            Dim p As Poisson
            If Feat.Location.IsComplementer = True Then
                p = New Poisson(Me, Width, Index, True, Neg(Index), File)
            Else
                p = New Poisson(Me, Width, Index, False, Pos(Index), File)
            End If

            If Feat.Location.IsComplementer = False Then
                p.Is_Local_Maximum = Is_Local_Maximum(Index, LocalWidth, False)
            Else
                p.Is_Local_Maximum = Is_Local_Maximum(Index, LocalWidth, True)
            End If

            If p.p < Alpha Then
                p.Passed = True
            Else
                p.Passed = False
            End If

            Out.Add(p)
        Next


        Return Out

    End Function

    Public Function Get_Local_Maximums(IsComplemeter As Boolean, Optional Width As Integer = 10) As List(Of Integer)
        Dim out As New List(Of Integer)
        If IsComplemeter = False Then
            For i1 = 0 To Pos.Count - 1
                If Pos(i1) <> 0 Then
                    If Pos(i1) > 1 Then ' One read not read
                        If Is_Local_Maximum(i1, Width, IsComplemeter) = True Then
                            out.Add(i1)
                        End If
                    End If
                End If
            Next
        Else
            For i1 = 0 To Neg.Count - 1
                If Neg(i1) <> 0 Then
                    If Neg(i1) > 1 Then ' One read not read
                        If Is_Local_Maximum(i1, Width, IsComplemeter) = True Then
                            out.Add(i1)
                        End If
                    End If
                End If
            Next
        End If
        out.Sort()
        Return out

    End Function

    Public Function Is_Local_Maximum(Position As Integer, Width As Integer, IsComplementer As Boolean) As Boolean
        Dim max As Integer = posStrand.Count
        Dim ka As Integer = 0
        Dim Value As Integer = 0
        If IsComplementer = True Then
            Value = Me.Neg(Position)
        Else
            Value = Me.Pos(Position)
        End If
        If IsComplementer = True Then
            For i1 = Position - Width To Position + Width
                ka += 1
                If i1 > -1 And i1 < max Then
                    If Neg(i1) > Value Then Return False
                    If Neg(i1) = Value And i1 > Position Then Return False
                End If
            Next
            Return True
        Else
            For i1 = Position - Width To Position + Width
                ka += 1
                If i1 > -1 And i1 < max Then
                    If Pos(i1) > Value Then Return False
                    If Pos(i1) = Value And i1 < Position Then Return False
                End If
            Next
            Return True
        End If
    End Function
End Class

Public Class Statistic
    Public Property Avarage As Double
    Public Property Sum As Integer
    Public Property Values As List(Of Integer)
    Public Property Mappings As List(Of Bio.IO.SAM.SAMAlignedSequence)
    Public Property Mapping_Files As IEnumerable(Of FileInfo)
    Public Property Folder As DirectoryInfo
    Public Sub New(Files As IEnumerable(Of FileInfo), Folder As DirectoryInfo)
        Me.Mapping_Files = Files
        Me.Folder = Folder
    End Sub
    Public Sub New()

    End Sub
    Private Function Get_Header() As String
        Dim str As New System.Text.StringBuilder
        str.Append("FileName").Append(vbTab)
        str.Append("Nof Read").Append(vbTab)
        str.Append("Avr. Length of Reads").Append(vbTab)
        str.Append("SD of Avr. Length of Reads").Append(vbTab)

        str.Append("Avr. Length of Alignemnts").Append(vbTab)
        str.Append("SD of Avr. Length of Alignments").Append(vbTab)

        str.Append("Percent of MM in Alignemnts").Append(vbTab)
        str.Append("SD of MM in Alignments").Append(vbTab)

        str.Append("Percent of InDels in Alignemnts").Append(vbTab)
        str.Append("SD of InDels in Alignments").AppendLine()


        Return str.ToString
    End Function
End Class

Public Class Poisson
    Public Property File As FileInfo ' Later For Merging
    Public Property Hundrends As List(Of Integer)
    Public Property avr As Double
    Public Property p As Double
    Public Property width As Integer
    Public Property Count As Integer
    Public Property Index As Integer
    Public Property IsComplementer As Boolean
    Public Property Passed As Boolean
    Public Property Is_Local_Maximum As Boolean
    Public Property Distribution As List(Of Integer)
    Public Sub New(site As Sites, width As Integer, Index As Integer, isComplementer As Boolean, Value As Integer, FIle As FileInfo)
        Me.width = width
        Me.Count = Value
        Me.Index = Index
        Me.File = FIle
        Me.IsComplementer = isComplementer
        Hundrends = site.Get_Distribution(Index, isComplementer, site.sort)
        avr = Hundrends.Sum / Hundrends.Count


    End Sub
    Public Overrides Function ToString() As String
        Dim str As New System.Text.StringBuilder
        str.Append(Me.Index).Append(vbTab)
        If IsComplementer = True Then
            str.Append("-").Append(vbTab)
        Else
            str.Append("+").Append(vbTab)
        End If

        str.Append(Me.Count).Append(vbTab)
        str.Append(Me.Hundrends.Sum).Append(vbTab)
        str.Append(Me.p).Append(vbTab)
        str.Append(Is_Local_Maximum).Append(vbTab)
        str.Append(Passed).Append(vbTab)
        str.Append(Me.Distribution.GetText)
        Return str.ToString
    End Function
End Class

