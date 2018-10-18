Imports Bio.IO.GenBank

Namespace Location
    Public Class Common
        Public Shared LociBuilder As New Bio.IO.GenBank.LocationBuilder
        Public Class LocationsToString
#Region "Classic"
            Public Shared Function Get_Location_Strings(locis As List(Of List(Of Basic_Location))) As List(Of String)
                Dim out As New List(Of String)
                For Each Loci In locis
                    out.Add("e:" & Loci.Count & " " & GetLocationString(Loci.First))
                Next
                Return out
            End Function
            Public Shared Function Get_Location_Strings(locis As List(Of Basic_Location)) As List(Of String)
                Dim out As New List(Of String)

                For Each Loci In locis
                    If IsNothing(Loci.Location.Accession) = True Then
                        out.Add(vbTab & GetLocationString(Loci.Location))
                    Else
                        out.Add(Loci.Location.Accession & vbTab & GetLocationString(Loci.Location))
                    End If

                Next
                Return out
            End Function
            Public Shared Function Get_Location_Strings(locis As List(Of List(Of ILocation))) As List(Of String)
                Dim out As New List(Of String)
                For Each Loci In locis
                    out.Add("e:" & Loci.Count & " " & GetLocationString(Loci.First))
                Next
                Return out
            End Function
            Public Shared Function Get_Location_Strings(locis As List(Of ILocation)) As List(Of String)
                Dim out As New List(Of String)

                For Each Loci In locis
                    out.Add(Loci.Accession & vbTab & GetLocationString(Loci))
                Next
                Return out
            End Function
            Public Shared Function Get_Location_Strings(locis As List(Of FeatureItem)) As List(Of String)
                Dim out As New List(Of String)

                For Each Freat In locis
                    Dim loci = Freat.Location
                    out.Add(loci.Accession & vbTab & GetLocationString(loci))
                Next
                Return out
            End Function
            Public Shared Function GetLocationString(loci As Bio.IO.GenBank.ILocation) As String
                Return LociBuilder.GetLocationString(loci)
            End Function

#End Region
#Region "Tab"
            Public Shared Function GetLocationStringTab(Feat As Bio.IO.GenBank.FeatureItem) As String
                If Feat.Location.IsComplementer = True Then

                    Return "-" & vbTab & Feat.Location.LocationStart & vbTab & Feat.Location.LocationEnd
                Else
                    Return "+" & vbTab & Feat.Location.LocationStart & vbTab & Feat.Location.LocationEnd
                End If
            End Function
            Public Shared Function GetLocationStringTab(Feat As Basic_Location, sort As Locations_By) As String
                Return GetLocationStringTab(Feat.Location, sort)
            End Function
            Public Shared Function GetLocationStringTab(Feat As Bio.IO.GenBank.ILocation, sort As Locations_By) As String
                Dim str As New System.Text.StringBuilder
                If Feat.IsComplementer = True Then
                    str.Append("-").Append(vbTab)
                Else
                    str.Append("+").Append(vbTab)
                End If

                Select Case sort
                    Case Locations_By.TSS & Locations_By.PAS
                        str.Append(Feat.LocationStart).Append(vbTab).Append(Feat.LocationEnd)
                    Case Locations_By.LE
                        str.Append(Feat.LocationEnd)
                    Case Locations_By.PAS
                        str.Append(Feat.PAS)
                    Case Locations_By.TSS
                        str.Append(Feat.LocationStart)
                    Case Locations_By.TSS
                        str.Append(Feat.TSS)

                End Select





                Return str.ToString
            End Function
            Public Shared Function GetLocationStringTab(Feat As Bio.IO.GenBank.Location, sort As Locations_By) As String
                Dim str As New System.Text.StringBuilder
                If Feat.IsComplementer = True Then
                    str.Append("-").Append(vbTab)
                Else
                    str.Append("+").Append(vbTab)
                End If

                Select Case sort
                    Case Locations_By.TSS & Locations_By.PAS
                        str.Append(Feat.LocationStart).Append(vbTab).Append(Feat.LocationEnd).Append(vbTab)
                    Case Locations_By.LE
                        str.Append(Feat.LocationEnd).Append(vbTab)
                    Case Locations_By.PAS
                        str.Append(Feat.PAS).Append(vbTab)
                    Case Locations_By.TSS
                        str.Append(Feat.LocationStart).Append(vbTab)
                    Case Locations_By.TSS
                        str.Append(Feat.TSS).Append(vbTab)

                End Select





                Return str.ToString
            End Function
            Public Shared Function GetLocationStringTab(Feat As Bio.IO.GenBank.FeatureItem, sort As Locations_By) As String
                Dim str As New System.Text.StringBuilder
                If Feat.Location.IsComplementer = True Then
                    str.Append("-").Append(vbTab)
                Else
                    str.Append("+").Append(vbTab)
                End If

                Select Case sort
                    Case Locations_By.TSS & Locations_By.PAS
                        str.Append(Feat.Location.LocationStart).Append(vbTab).Append(Feat.Location.LocationEnd).Append(vbTab)
                    Case Locations_By.LE
                        str.Append(Feat.Location.LocationEnd).Append(vbTab)
                    Case Locations_By.PAS
                        str.Append(Feat.Location.PAS).Append(vbTab)
                    Case Locations_By.TSS
                        str.Append(Feat.Location.LocationStart).Append(vbTab)
                    Case Locations_By.TSS
                        str.Append(Feat.Location.TSS).Append(vbTab)

                End Select

                Return str.ToString
            End Function

#End Region
        End Class
#Region "String"

        Public Shared Function Get_LocationString_LSLE(Feat As Bio.IO.GenBank.ILocation) As String
            Return Feat.LocationStart & vbTab & Feat.LocationEnd

        End Function
        Public Shared Function GetLocationStringTab(Feat As Bio.IO.GenBank.ILocation) As String
            If Feat.IsComplementer = True Then

                Return "-" & vbTab & Feat.LocationStart & vbTab & Feat.LocationEnd
            Else
                Return "+" & vbTab & Feat.LocationStart & vbTab & Feat.LocationEnd
            End If
        End Function
        Public Shared Function GetLocationStringPASTab(Feat As Bio.IO.GenBank.ILocation) As String
            If Feat.IsComplementer = True Then

                Return "-" & vbTab & Feat.PAS
            Else
                Return "+" & vbTab & Feat.PAS
            End If
        End Function
        Public Shared Function GetLocationStringTSSTab(Feat As Bio.IO.GenBank.ILocation) As String
            If Feat.IsComplementer = True Then

                Return "-" & vbTab & Feat.TSS & vbTab & Feat.LocationEnd
            Else
                Return "+" & vbTab & Feat.TSS
            End If
        End Function
        Public Shared Function GetLocationStringTSS_PAS_Strand_Tab(Feat As Bio.IO.GenBank.ILocation) As String
            If Feat.IsComplementer = True Then

                Return "-" & vbTab & Feat.TSS & vbTab & Feat.PAS
            Else
                Return "+" & vbTab & Feat.TSS & vbTab & Feat.PAS
            End If
        End Function
        Public Shared Function GetLocationString(Feat As Bio.IO.GenBank.FeatureItem) As String
            Return LociBuilder.GetLocationString(Feat.Location)
        End Function
        Public Shared Function GetLocationString(Loci As Bio.IO.GenBank.Location) As String
            Return LociBuilder.GetLocationString(Loci)
        End Function
        Public Shared Function GetLocationString(Loci As Bio.IO.GenBank.ILocation) As String
            Return LociBuilder.GetLocationString(Loci)
        End Function
#End Region
#Region "Lengths"
        Public Shared Function Get_Length(Feats As List(Of ILocation)) As List(Of Integer)
            Dim out As New List(Of Integer)
            For Each f In Feats
                out.Add(Get_Length(f))
            Next
            Return out
        End Function

        Public Shared Function Get_Length(f As ILocation) As Integer
            Dim Exons = Get_All_Exon_Location(f)
            Dim res = (From x In Exons Select x.LocationEnd - x.LocationStart + 1).Sum
            Return res
        End Function

        Public Shared Function Get_Length(Feats As List(Of FeatureItem)) As List(Of Integer)
            Dim out As New List(Of Integer)
            For Each f In Feats
                out.Add(Get_Length(f))
            Next
            Return out
        End Function

        Public Shared Function Get_Smaller(i1 As Integer, i2 As Integer) As Integer
            If i1 < i2 Then Return i1
            Return i2
        End Function
        Public Shared Function Get_Bigger(i1 As Integer, i2 As Integer) As Integer
            If i1 > i2 Then Return i1
            Return i2
        End Function
#End Region
#Region "Get_Locations "
        Public Shared Function Get_All_Intron_Location(Loci As ILocation) As List(Of ILocation)
            Dim exons = Get_All_Exon_Location(Loci)
            If exons.Count = 0 Then Return New List(Of ILocation)
            Dim out As New List(Of ILocation)
            For i1 = 0 To exons.Count - 2
                If exons(i1).LocationEnd > exons(i1 + 1).LocationStart Then
                    Dim ald As Int16 = 54
                End If
                If exons(i1).Operator = LocationOperator.Complement Then
                    out.Add(LociBuilder.GetLocation("complement(" & exons(i1).LocationEnd + 1 & ".." & exons(i1 + 1).LocationStart - 1 & ")"))
                    out.Last.Accession = exons.First.Accession
                Else
                    out.Add(LociBuilder.GetLocation(exons(i1).LocationEnd + 1 & ".." & exons(i1 + 1).LocationStart - 1))
                    out.Last.Accession = exons.First.Accession
                End If

            Next

            Return out
        End Function

        Public Shared Function Set_Direction(Locis As List(Of ILocation), ForPositive As Boolean) As List(Of ILocation)
            Dim out As New List(Of ILocation)
            If ForPositive = True Then
                For Each loci In Locis
                    If loci.IsComplementer = True Then
                        loci = Szunyi.BLs.Location.Common.Change_Strand(loci)
                    End If
                    out.Add(loci)
                Next
            Else
                For Each loci In Locis
                    If loci.IsComplementer = False Then
                        loci = Szunyi.BLs.Location.Common.Change_Strand(loci)
                    End If

                    out.Add(loci)
                Next
            End If
        End Function

        Public Shared Function Get_All_Exons_Location(Locis As List(Of Bio.IO.GenBank.ILocation)) As List(Of Bio.IO.GenBank.ILocation)
            Dim out As New List(Of Bio.IO.GenBank.ILocation)
            For Each Loci In Locis
                out.AddRange(Get_All_Exon_Location(Loci))
            Next
            Return out
        End Function
        Public Shared Function Get_All_Exons_Location(Locis As List(Of Bio.IO.GenBank.Location)) As List(Of Bio.IO.GenBank.ILocation)
            Dim out As New List(Of Bio.IO.GenBank.ILocation)
            For Each Loci In Locis
                out.AddRange(Get_All_Exon_Location(Loci))
            Next
            Return out
        End Function
        Public Shared Function Get_Smallest_Exon_Location(Loci As Bio.IO.GenBank.ILocation) As Bio.IO.GenBank.ILocation
            Dim Exons = Get_All_Exon_Location(Loci)
            Dim es = From x In Exons Order By x.LocationEnd - x.LocationStart Ascending

            Return es.First
        End Function
        Public Shared Function Get_Biggest_Exon_Location(Loci As Bio.IO.GenBank.ILocation) As Bio.IO.GenBank.ILocation
            Dim Exons = Get_All_Exon_Location(Loci)
            Dim es = From x In Exons Order By x.LocationEnd - x.LocationStart Ascending

            Return es.Last
        End Function
        Public Shared Function Get_Biggest_Exon_Length(Loci As Bio.IO.GenBank.ILocation) As Integer
            Dim Exons = Get_All_Exon_Location(Loci)
            Dim es = From x In Exons Order By x.LocationEnd - x.LocationStart Ascending

            Return Get_Length(es.Last)
        End Function
        Public Shared Function Get_Biggest_Intron_Location(Loci As Bio.IO.GenBank.ILocation) As Bio.IO.GenBank.ILocation
            Dim Exons = Get_All_Intron_Location(Loci)
            If Exons.Count = 0 Then Return Nothing
            Dim es = From x In Exons Order By x.LocationEnd - x.LocationStart Ascending

            Return es.Last
        End Function
        Public Shared Function Get_Biggest_Intron_Length(Loci As Bio.IO.GenBank.ILocation) As Integer
            Dim Exons = Get_All_Intron_Location(Loci)
            If Exons.Count = 0 Then Return 0
            Dim es = From x In Exons Order By x.LocationEnd - x.LocationStart Ascending

            Return Get_Length(es.Last)
        End Function
        Public Shared Function Get_Smallest_Intron_Location(Loci As Bio.IO.GenBank.ILocation) As Bio.IO.GenBank.ILocation
            Dim Exons = Get_All_Intron_Location(Loci)
            If Exons.Count = 0 Then Return Nothing
            Dim es = From x In Exons Order By x.LocationEnd - x.LocationStart Ascending

            Return es.First
        End Function
        Public Shared Function Get_All_Exon_Location(Loci As Bio.IO.GenBank.ILocation) As List(Of Bio.IO.GenBank.ILocation)
            If IsNothing(Loci) = True Then Return Nothing
            Dim out As New List(Of ILocation)
            If Loci.SubLocations.Count = 0 Then ' No complement no join
                out.Add(Loci)
                out.Last.Accession = Loci.Accession
                Return out
            End If
            If Loci.Operator <> LocationOperator.Complement Then ' no complement join
                For Each subL In Loci.SubLocations
                    subL.Accession = Loci.Accession
                Next
                Return Loci.SubLocations
            End If
            If Loci.SubLocations.First.Operator = LocationOperator.Join Then ' complement join
                For Each subloci As Bio.IO.GenBank.Location In Loci.SubLocations.First.SubLocations
                    Dim s = "complement(" & LociBuilder.GetLocationString(subloci) & ")"
                    out.Add(LociBuilder.GetLocation(s))
                    out.Last.Accession = Loci.Accession
                Next
                Return out
            Else ' complement no join
                out.Add(Loci)
                out.Last.Accession = Loci.Accession
                Return out
            End If

        End Function
        Public Shared Function Get_All_Exon_Location(Loci As Bio.IO.GenBank.Location) As List(Of Bio.IO.GenBank.ILocation)
            If IsNothing(Loci) = True Then Return Nothing
            Dim out As New List(Of ILocation)
            If Loci.SubLocations.Count = 0 Then ' No complement no join
                out.Add(Loci)
                out.Last.Accession = Loci.Accession
                Return out
            End If
            If Loci.Operator <> LocationOperator.Complement Then ' no complement join
                For Each subL In Loci.SubLocations
                    subL.Accession = Loci.Accession
                Next
                Return Loci.SubLocations
            End If
            If Loci.SubLocations.First.Operator = LocationOperator.Join Then ' complement join
                For Each subloci As Bio.IO.GenBank.Location In Loci.SubLocations.First.SubLocations
                    Dim s = "complement(" & LociBuilder.GetLocationString(subloci) & ")"
                    out.Add(LociBuilder.GetLocation(s))
                    out.Last.Accession = Loci.Accession
                Next
                Return out
            Else ' complement no join
                out.Add(Loci)
                out.Last.Accession = Loci.Accession
                Return out
            End If

        End Function


        Public Shared Function Get_Full_Location(loci As ILocation) As ILocation
            If loci.Operator = LocationOperator.Join Then
                Dim x = Get_Location(loci.LocationStart & ".." & loci.LocationEnd)
                Return x
            ElseIf loci.Operator = LocationOperator.Complement Then
                Dim x = Get_Location("complement(" & loci.LocationStart & ".." & loci.LocationEnd & ")")
                Return x
            Else
                Dim nLoci = loci.Clone

                Return nLoci
            End If

        End Function
        Public Shared Function Get_Location(loci As String) As ILocation
            If loci = "" Then Return Nothing
            Return LociBuilder.GetLocation(loci)
        End Function

        ''' <summary>
        ''' start, end, strand (+,-) Correct start and end
        ''' </summary>
        ''' <param name="st"></param>
        ''' <param name="endy"></param>
        ''' <param name="strand"></param>
        ''' <returns></returns>
        Public Shared Function GetLocation(st As String, endy As String, strand As String) As ILocation
            Dim s As Integer = st
            Dim e As Integer = endy
            If e < s Then
                Dim tmp = e
                e = s
                s = tmp
            End If
            If strand = "+" Then
                Return Get_Location(s & ".." & e)
            Else
                Return Get_Location("complement(" & s & ".." & e & ")")
            End If

        End Function
        Public Shared Function GetLocation(st As Integer, endy As Integer, strand As String) As ILocation
            Dim s As Integer = st
            Dim e As Integer = endy
            If e < s Then
                Dim tmp = e
                e = s
                s = tmp
            End If
            If strand = "+" Then
                Return Get_Location(s & ".." & e)
            Else
                Return Get_Location("complement(" & s & ".." & e & ")")
            End If

        End Function
        Public Shared Function GetLocation(st As Integer, endy As Integer, IsC As Boolean) As ILocation
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
                Return Get_Location("complement(" & s & ".." & e & ")")
            Else
                Return Get_Location(s & ".." & e)
            End If
        End Function
        Public Shared Function GetLocation(st As Integer, endy As Integer, Op As Bio.IO.GenBank.LocationOperator) As ILocation
            Dim s As Integer = st
            Dim e As Integer = endy
            If e < s Then
                Dim tmp = e
                e = s
                s = tmp
            End If
            If Op = LocationOperator.Complement Then
                Return Get_Location("complement(" & s & ".." & e & ")")

            Else
                Return Get_Location(s & ".." & e)
            End If

        End Function
        Public Shared Function GetLocation(position As String, orientation As String) As ILocation
            Dim pos As Integer = position
            If orientation <> "+" Then
                Return GetLocation(pos, pos - 1, True)
            Else
                Return GetLocation(pos, pos + 1, False)
            End If

        End Function
        Public Shared Function GetLocation(pos As Integer, isCommplementer As Boolean) As ILocation
            If isCommplementer = True Then
                Return GetLocation(pos, pos - 1, isCommplementer)
            Else
                Return GetLocation(pos, pos + 1, isCommplementer)
            End If

        End Function
        Public Shared Function GetLocation(Tss_pos As Integer, PAS_POS As Integer) As ILocation
            If Tss_pos < PAS_POS Then
                Return GetLocation(Tss_pos, PAS_POS, False)
            Else
                Return GetLocation(Tss_pos, PAS_POS, True)
            End If
        End Function
        Public Shared Function GetLocation(locis As List(Of ILocation), Op As LocationOperator) As ILocation
            Dim Sorted = From x In locis Order By x.LocationStart
            Dim str As New System.Text.StringBuilder
            For Each s In Sorted
                str.Append(s.LocationStart & ".." & s.LocationEnd & ",")
            Next
            str.Length -= 1
            If Op = LocationOperator.Complement Then
                If locis.Count > 1 Then
                    Return Get_Location("complement(join(" & str.ToString & "))")
                Else
                    Return Get_Location("complement(" & str.ToString & ")")
                End If
            Else
                If locis.Count > 1 Then
                    Return Get_Location("join(" & str.ToString & ")")
                Else
                    Return Get_Location(str.ToString)
                End If
            End If
        End Function

        Public Shared Function Get_Location(start As Integer, endy As Integer, strand As String) As ILocation
            If start > endy Then
                If strand = "+" Then
                    Return LociBuilder.GetLocation(endy & ".." & start)
                Else
                    Return LociBuilder.GetLocation("complement(" & endy & ".." & start & ")")
                End If
            Else
                If strand = "+" Then
                    Return LociBuilder.GetLocation(start & ".." & endy)
                Else
                    Return LociBuilder.GetLocation("complement(" & start & ".." & endy & ")")
                End If
            End If
        End Function

#End Region
#Region "Accession"
        Public Shared Function Get_Locis_Accesions(locis As List(Of List(Of ILocation))) As List(Of String)
            Dim out As New List(Of String)
            For Each Loci In locis
                For Each Locz In Loci
                    out.Add(Locz.Accession)
                Next
            Next
            Return out
        End Function
        Public Shared Function Get_Locis_Accesions(locis As List(Of ILocation)) As List(Of String)
            Dim out As New List(Of String)

            For Each Loci In locis

                out.Add(Loci.Accession)

            Next
            Return out
        End Function

#End Region
#Region "Counts"
        Public Shared Function Get_Count(Locis As List(Of List(Of ILocation))) As Integer
            Dim c As Integer = 0
            For Each Loci In Locis
                c += Loci.Count
            Next
            Return c
        End Function
        Public Shared Function Get_Count(locis As List(Of ILocation)) As Integer
            Return locis.Count
        End Function
#End Region
#Region "Start End"

        ''' <summary>
        ''' Always + strand
        ''' </summary>
        ''' <param name="locis"></param>
        ''' <returns></returns>
        Public Shared Function Get_Start_Location(locis As List(Of ILocation)) As List(Of ILocation)
            Dim out As New List(Of ILocation)
            For Each loci In locis
                out.Add(LociBuilder.GetLocation(loci.LocationStart & ".." & loci.LocationStart))
            Next
            Return out
        End Function
        ''' <summary>
        ''' Always + strand
        ''' </summary>
        ''' <param name="locis"></param>
        ''' <returns></returns>
        Public Shared Function Get_End_Location(locis As List(Of ILocation)) As List(Of ILocation)
            Dim out As New List(Of ILocation)
            For Each loci In locis
                out.Add(LociBuilder.GetLocation(loci.LocationEnd & ".." & loci.LocationEnd))
            Next
            Return out
        End Function


        Public Shared Function Get_No_Complement_Locations(locis As List(Of ILocation)) As List(Of ILocation)
            Dim out As New List(Of ILocation)
            For Each Loci In locis
                out.Add(Get_No_Complement_Locations(Loci))
            Next
            Return out
        End Function
        Public Shared Function Get_No_Complement_Locations(loci As ILocation) As ILocation
            If IsNothing(loci.Accession) = True OrElse loci.Accession = "" Then
                Dim alf As Int16 = 54
            End If
            If loci.Operator = LocationOperator.Complement Then
                loci.SubLocations.First.Accession = loci.Accession
                Return loci.SubLocations.First
            Else
                Return loci
            End If

        End Function
        Public Shared Function Get_Correct_Location(locis As List(Of ILocation)) As List(Of ILocation)
            Dim out As New List(Of ILocation)
            For Each Loci In locis
                out.Add(Get_Correct_Location(Loci))
            Next
            Return out
        End Function
        Public Shared Function Create_Biggest(Locis As List(Of ILocation)) As ILocation
            Dim s = (From x In Locis Select x.LocationStart).Min
            Dim e = (From x In Locis Select x.LocationEnd).Max
            Dim l As Bio.IO.GenBank.Location = Get_Location(s & ".." & e)
            If Locis.First.Operator = LocationOperator.Complement Then
                l = Change_Strand(l)
            End If

            If l.LocationEnd - l.LocationStart > 10000 Then
                Dim alf As Int16 = 43
            End If
            Return l
        End Function

        Public Shared Function Change_Strand(gexons As Bio.IO.GenBank.Location) As Bio.IO.GenBank.Location
            Dim s = LociBuilder.GetLocationString(gexons)
            If s.Contains("complement") Then
                s = s.Replace("complement", "")
                s = Mid(s, 2, s.Length - 2)
                Dim jk = LociBuilder.GetLocation(s)
                jk.Accession = gexons.Accession
                Return jk
            Else
                If gexons.SubLocations.Count > 0 AndAlso gexons.Operator = LocationOperator.None Then gexons.Operator = LocationOperator.Join
                s = LociBuilder.GetLocationString(gexons)
                s = "complement(" & s & ")"
            End If
            Dim nLOC = LociBuilder.GetLocation(s)
            nLOC.Accession = gexons.Accession
            Return nLOC
        End Function

        Public Shared Function IsOverLapped(gExon1 As ILocation, gExon2 As ILocation) As Boolean
            If gExon1.LocationEnd >= gExon2.LocationStart And gExon1.LocationEnd <= gExon2.LocationEnd Then
                Return True
            End If
            If gExon1.LocationStart >= gExon2.LocationStart And gExon1.LocationStart <= gExon2.LocationEnd Then
                Return True
            End If
            Return False
        End Function

        Public Shared Function Get_Basic_Location(start As Integer, endy As Integer, Strand As String) As Basic_Location
            Dim l = GetLocation(start, endy, Strand)
            Dim x As New Basic_Location(l)
            Return x

        End Function
        Public Shared Function Get_Basic_Location(start As Integer, endy As Integer, Strand As Bio.IO.GenBank.LocationOperator) As Basic_Location
            If Strand = LocationOperator.Complement Then
                Dim l = GetLocation(start, endy, "-")
                Dim x As New Basic_Location(l)
                Return x
            Else
                Dim l = GetLocation(start, endy, "+")
                Dim x As New Basic_Location(l)
                Return x
            End If


        End Function
        Public Shared Function Get_Basic_Location(loci As ILocation, range As Integer, ExtendBy As Locations_By) As Basic_Location
            Dim l As ILocation
            Select Case ExtendBy

                Case Locations_By.TSS
                    l = GetLocation(loci.TSS + range, loci.PAS, loci.IsComplementer)
                Case Locations_By.LE
                    l = GetLocation(loci.LocationStart, loci.LocationEnd + range, loci.IsComplementer)
                Case Locations_By.TSS & Locations_By.PAS
                    l = GetLocation(loci.LocationStart + range, loci.LocationEnd + range, loci.IsComplementer)
                Case Locations_By.PAS
                    l = GetLocation(loci.TSS, loci.PAS + range, loci.IsComplementer)
                Case Locations_By.LS
                    l = GetLocation(loci.LocationStart + range, loci.LocationEnd, loci.IsComplementer)
            End Select


            Return New Basic_Location(l)
        End Function





        ''' <summary>
        ''' Return + or -
        ''' </summary>
        ''' <param name="location"></param>
        ''' <returns></returns>
        Public Shared Function Get_Strand(location As ILocation) As String
            If location.IsComplementer = True Then
                Return "-"
            Else
                Return "+"
            End If
        End Function
        ''' <summary>
        ''' Return + or -
        ''' </summary>
        ''' <param name="isComplementer"></param>
        ''' <returns></returns>
        Public Shared Function Get_Strand(isComplementer As Boolean) As String
            If isComplementer = True Then
                Return "-"
            Else
                Return "+"
            End If
        End Function


        Public Shared Function Get_LocationString_wIntron(location As ILocation) As String
            Dim str As New System.Text.StringBuilder
            str.Append(GetLocationStringTab(location)).Append(vbTab)
            Dim Introns = Get_All_Intron_Location(location)
            For Each I In Introns
                str.Append(vbTab).Append(Get_LocationString_LSLE(I))
            Next
            Return str.ToString
        End Function
#End Region
        Public Shared Function modify(Loci As Bio.IO.GenBank.Location, m As Integer) As Bio.IO.GenBank.Location

            Dim Others As New List(Of String)
            Dim l = GetLocationString(Loci)
            Dim IsDigit As Boolean = Char.IsNumber(l.First)
            Dim curr As String = ""
            For Each c As Char In l
                If [Char].IsNumber(c) Then
                    If IsDigit = True Then
                        curr = curr & c
                    Else
                        IsDigit = True
                        Others.Add(curr)
                        curr = c
                    End If
                Else
                    If IsDigit = False Then
                        curr = curr & c
                    Else
                        Others.Add(curr)
                        curr = c
                        IsDigit = False
                    End If
                End If
            Next
            Others.Add(curr)
            Dim sb As New System.Text.StringBuilder
            For Each Item In Others
                Dim res As Integer
                If Integer.TryParse(Item, res) = True Then
                    sb.Append(res + m)
                Else
                    sb.Append(Item)
                End If
            Next
            Return Get_Location(sb.ToString)
        End Function


    End Class

    Public Class Sites
        Public Shared Function Get_Sites(Mappings As IEnumerable(Of Basic_Location), Seq As Bio.Sequence, Sort As Locations_By) As Dictionary(Of String, Integer())
            Dim t(Seq.Count) As Integer
            Dim t1(Seq.Count) As Integer
            Dim res As New Dictionary(Of String, Integer())
            res.Add("+", t)
            res.Add("-", t1)
            Select Case Sort
                Case Locations_By.TSS
                    For Each m In Mappings
                        If m.Location.IsComplementer = False Then
                            res("+")(m.Location.TSS) += 1
                        Else
                            res("-")(m.Location.TSS) += 1
                        End If
                    Next
                Case Locations_By.PAS
                    For Each m In Mappings
                        If m.Location.IsComplementer = False Then
                            res("+")(m.Location.PAS) += 1
                        Else
                            res("-")(m.Location.PAS) += 1
                        End If
                    Next
                Case Else
                    Dim kj As Int16 = 54

            End Select

            Return res




        End Function

        Public Shared Function Convert_ToString(sites As Dictionary(Of String, Integer())) As Object

            Dim str As New System.Text.StringBuilder
            str.Append("pos").Append(vbTab)
            str.Append("+").Append(vbTab)
            str.Append("-").AppendLine()
            For i1 = 0 To sites.First.Value.Count - 1
                str.Append(i1).Append(vbTab)
                str.Append(sites("+")(i1)).Append(vbTab)
                str.Append(sites("-")(i1)).AppendLine()
            Next
            Return str.ToString
        End Function

    End Class

    Public Class Exon_Intron
        Public Shared Property LociBuilder As New Bio.IO.GenBank.LocationBuilder

        Public Shared Function Get_All_Introns_Location(locis As List(Of Bio.IO.GenBank.ILocation)) As List(Of ILocation)
            Dim out As New List(Of Bio.IO.GenBank.ILocation)
            For Each Loci In locis
                Dim Exons = Get_All_Exon_Location(Loci)
                out.AddRange(Get_All_Intron_Location(Exons))
            Next
            Return out
        End Function
        Public Shared Function Get_All_Introns_Location(loci As Bio.IO.GenBank.ILocation) As List(Of ILocation)
            Dim Exons = Get_All_Exon_Location(loci)
            Return Get_All_Intron_Location(Exons)

        End Function
        Public Shared Function Get_All_Intron_Location(Exons As List(Of ILocation)) As List(Of ILocation)
            If Exons.Count = 0 Then Return New List(Of ILocation)
            Dim out As New List(Of ILocation)
            For i1 = 0 To Exons.Count - 2
                If Exons(i1).LocationEnd > Exons(i1 + 1).LocationStart Then
                    Dim ald As Int16 = 54
                End If
                If Exons(i1).Operator = LocationOperator.Complement Then
                    out.Add(LociBuilder.GetLocation("complement(" & Exons(i1).LocationEnd + 1 & ".." & Exons(i1 + 1).LocationStart - 1 & ")"))
                    out.Last.Accession = Exons.First.Accession
                Else
                    out.Add(LociBuilder.GetLocation(Exons(i1).LocationEnd + 1 & ".." & Exons(i1 + 1).LocationStart - 1))
                    out.Last.Accession = Exons.First.Accession
                End If

            Next
            Return out
        End Function
        Public Shared Function Get_All_Exons_Location(Locis As List(Of Bio.IO.GenBank.ILocation)) As List(Of Bio.IO.GenBank.ILocation)
            Dim out As New List(Of Bio.IO.GenBank.ILocation)
            For Each Loci In Locis
                out.AddRange(Get_All_Exon_Location(Loci))
            Next
            Return out
        End Function
        Public Shared Function Get_All_Exon_Location(Loci As Bio.IO.GenBank.ILocation) As List(Of Bio.IO.GenBank.ILocation)
            If IsNothing(Loci) = True Then Return Nothing
            Dim out As New List(Of ILocation)
            If Loci.SubLocations.Count = 0 Then ' No complement no join
                out.Add(Loci)
                out.Last.Accession = Loci.Accession
                Return out
            End If
            If Loci.Operator <> LocationOperator.Complement Then ' no complement join
                For Each subL In Loci.SubLocations
                    subL.Accession = Loci.Accession
                Next
                Return Loci.SubLocations
            End If
            If Loci.SubLocations.First.Operator = LocationOperator.Join Then ' complement join
                For Each subloci As Bio.IO.GenBank.Location In Loci.SubLocations.First.SubLocations
                    Dim s = "complement(" & LociBuilder.GetLocationString(subloci) & ")"
                    out.Add(LociBuilder.GetLocation(s))
                    out.Last.Accession = Loci.Accession
                Next
                Return out
            Else ' complement no join
                out.Add(Loci)
                out.Last.Accession = Loci.Accession
                Return out
            End If

        End Function
        Public Shared Function Get_All_Exon_Location(Feat As Bio.IO.GenBank.FeatureItem) As List(Of Bio.IO.GenBank.ILocation)
            If IsNothing(Feat) = True Then Return Nothing
            Dim loci = Feat.Location
            Dim out As New List(Of ILocation)
            If loci.SubLocations.Count = 0 Then ' No complement no join
                out.Add(loci)
                out.Last.Accession = loci.Accession
                Return out
            End If
            If loci.Operator <> LocationOperator.Complement Then ' no complement join
                For Each subL In loci.SubLocations
                    subL.Accession = loci.Accession
                Next
                Return loci.SubLocations
            End If
            If loci.SubLocations.First.Operator = LocationOperator.Join Then ' complement join
                For Each subloci As Bio.IO.GenBank.Location In loci.SubLocations.First.SubLocations
                    Dim s = "complement(" & LociBuilder.GetLocationString(subloci) & ")"
                    out.Add(LociBuilder.GetLocation(s))
                    out.Last.Accession = loci.Accession
                Next
                Return out
            Else ' complement no join
                out.Add(loci)
                out.Last.Accession = loci.Accession
                Return out
            End If

        End Function
        Public Shared Function GetCDSExonsLocations(Feat As FeatureItem) As List(Of Bio.IO.GenBank.ILocation)
            Dim out As New List(Of ILocation)
            If Feat.Location.SubLocations.Count = 0 Then ' No complement no join
                out.Add(Feat.Location)
                Return out
            End If
            If Feat.Location.Operator <> LocationOperator.Complement Then ' no complement join
                Return Feat.Location.SubLocations
            End If
            If Feat.Location.SubLocations.First.Operator = LocationOperator.Join Then ' complement join
                For Each loci As Bio.IO.GenBank.Location In Feat.Location.SubLocations.First.SubLocations
                    Dim s = "complement(" & LociBuilder.GetLocationString(loci) & ")"
                    out.Add(LociBuilder.GetLocation(s))
                Next
                Return out
            Else ' complement no join
                out.Add(Feat.Location)
                Return out
            End If


        End Function
        Public Shared Function Get_Last_Exons_Location(locis As List(Of ILocation)) As List(Of ILocation)
            Dim out As New List(Of Bio.IO.GenBank.ILocation)
            For Each Loci In locis
                out.Add(Get_Last_Exon_Location(Loci))
            Next
            Return out
        End Function
        Public Shared Function Get_xth_Exons_Location(locis As List(Of ILocation), xth As Integer) As List(Of ILocation)
            Dim out As New List(Of Bio.IO.GenBank.ILocation)
            For Each Loci In locis
                out.Add(Get_XTH_Exon_Location(Loci, xth))
            Next
            Return out
        End Function
        Public Shared Function Get_XTH_Exon_Location(loci As ILocation, xth As Integer) As ILocation
            If loci.Operator = LocationOperator.Complement Then
                Return loci.SubLocations.First.SubLocations(xth)
            ElseIf loci.Operator = LocationOperator.Join Then
                Return loci.SubLocations(xth)

            Else 'no complement join
                Return loci.SubLocations(xth)
            End If
        End Function
        Public Shared Function Get_Last_Exon_Location(loci As ILocation) As ILocation
            Dim out As New List(Of Bio.IO.GenBank.ILocation)
            If loci.SubLocations.Count = 0 Then ' No complement no join
                Return loci
            ElseIf loci.Operator = LocationOperator.Complement Then
                If loci.SubLocations.First.Operator <> LocationOperator.Join Then
                    Dim t = loci.Clone
                    t.Operator = LocationOperator.Complement
                    Return t
                Else
                    Dim t = loci.SubLocations.First.SubLocations.First.Clone
                    t.Operator = LocationOperator.Complement
                    Return t
                End If

            Else 'no complement join
                Return loci.SubLocations.Last
            End If
            Return out
        End Function

        ''' <summary>
        ''' Depends on location operators
        ''' </summary>
        ''' <param name="Locis"></param>
        ''' <returns></returns>
        Public Shared Function Get_First_Exons_Location(Locis As List(Of Bio.IO.GenBank.ILocation)) As List(Of Bio.IO.GenBank.ILocation)
            Dim out As New List(Of Bio.IO.GenBank.ILocation)
            For Each Loci In Locis
                out.Add(Get_First_Exon_Location(Loci))
            Next
            Return out
        End Function
        ''' <summary>
        ''' Depends on location operator
        ''' </summary>
        ''' <param name="loci"></param>
        ''' <returns></returns>
        Public Shared Function Get_First_Exon_Location(loci As Bio.IO.GenBank.Location) As Bio.IO.GenBank.Location
            If loci.SubLocations.Count = 0 Then ' No complement no join
                Return loci
            ElseIf loci.Operator = LocationOperator.Complement Then
                If loci.SubLocations.First.Operator <> LocationOperator.Join Then
                    Dim t = loci.Clone
                    t.Operator = LocationOperator.Complement
                    Return t
                Else
                    Dim t = loci.SubLocations.First.SubLocations.Last.Clone
                    t.Operator = LocationOperator.Complement
                    t.Accession = loci.Accession
                    Return t
                End If

            Else 'no complement join
                Dim t = loci.SubLocations.First.Clone
                t.Accession = loci.Accession
                Return t
            End If
        End Function
        Public Shared Function GetCDSExonsLocations(Loci As Bio.IO.GenBank.Location) As List(Of Bio.IO.GenBank.ILocation)
            Dim out As New List(Of ILocation)
            If Loci.SubLocations.Count = 0 Then ' No complement no join
                out.Add(Loci)
                Return out
            End If
            If Loci.Operator <> LocationOperator.Complement Then ' no complement join
                Return Loci.SubLocations
            End If
            If Loci.SubLocations.First.Operator = LocationOperator.Join Then ' complement join
                For Each subloci As Bio.IO.GenBank.Location In Loci.SubLocations.First.SubLocations
                    Dim s = "complement(" & LociBuilder.GetLocationString(subloci) & ")"
                    out.Add(LociBuilder.GetLocation(s))
                Next
                Return out
            Else ' complement no join
                out.Add(Loci)
                Return out
            End If


        End Function
        Public Shared Function GetIntronLocations(Feat As FeatureItem) As List(Of ILocation)
            Dim Exons = GetCDSExonsLocations(Feat)
            Return GetIntronLocationsFromExonLOcations(Exons)
        End Function

        Public Shared Function GetIntronLocationsFromExonLOcations(Ls As List(Of Bio.IO.GenBank.ILocation)) As List(Of Bio.IO.GenBank.ILocation)
            If Ls.Count = 0 Then Return New List(Of ILocation)
            Dim out As New List(Of ILocation)
            For i1 = 0 To Ls.Count - 2
                If Ls(i1).LocationEnd > Ls(i1 + 1).LocationStart Then
                    out.Add(LociBuilder.GetLocation(Ls(i1 + 1).LocationEnd + 1 & ".." & Ls(i1).LocationStart - 1))
                Else
                    out.Add(LociBuilder.GetLocation(Ls(i1).LocationEnd + 1 & ".." & Ls(i1 + 1).LocationStart - 1))
                End If
                If Ls.First.IsComplementer = True Then
                    out(out.Count - 1) = Szunyi.BLs.Location.Common.Change_Strand(out.Last)
                End If
            Next
            Return out
        End Function

    End Class


End Namespace



