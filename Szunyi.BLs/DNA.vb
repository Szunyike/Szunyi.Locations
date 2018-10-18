Imports Bio
Imports Bio.Extensions
Imports Bio.IO.GenBank

Public Class DNA

    Public Class PolyA_Signal_Sites
        Public PolyA_Signal_Sites As New Dictionary(Of String, List(Of Integer))
        Public Property Basic_Location As Basic_Location
        Public Property Signal_Sites As List(Of PolyA_Signal_Site)
        Public Property InvestigatedSequence As Bio.ISequence
        Public Function Get_Best() As PolyA_Signal_Site
            ' First AATAAA
            ' Second From All Pos
            Dim x = From a In Signal_Sites Where a.Sequence = "AATAAA"

            Select Case x.Count
                Case = 0
                    Dim x1 = From a In Signal_Sites Order By a.Distance Ascending

                    If x1.Count > 0 Then Return x1.First

                    Return New PolyA_Signal_Site
                Case 1
                    Return x.First
                Case Else
                    Dim x1 = From a In x Order By a.Distance Ascending

                    Return x1.First
            End Select
        End Function
        Public Function Get_All()
            Dim x = From a In Me.Signal_Sites Order By a.Distance Ascending

            If x.Count > 0 Then
                Return x.ToList
            Else
                Return New List(Of PolyA_Signal_Site)
            End If
        End Function
        Public Sub New(loci As Basic_Location, The_Seq As Bio.ISequence, PolyAs As List(Of PolyA_Signal_Site))
            Me.Basic_Location = loci
            Me.InvestigatedSequence = The_Seq
            Me.Signal_Sites = PolyAs
        End Sub
        Public Sub New(loci As ILocation, The_Seq As Bio.ISequence, PolyAs As List(Of PolyA_Signal_Site))
            Me.Basic_Location = Szunyi.BLs.Convert.From_Bio_Location(loci)
            Me.InvestigatedSequence = The_Seq
            Me.Signal_Sites = PolyAs
        End Sub
    End Class
    Public Class PolyA_Signal_Site
        Public Property Sequence As String
        Public Property Position As Integer
        Public Property Distance As Integer
        Public Sub New()

        End Sub
        Public Sub New(Seq As String, Pos As Integer, Best_Position As Integer)
            Me.Sequence = Seq
            Me.Position = Pos
            Me.Distance = System.Math.Abs(Pos + Best_Position)
        End Sub
        Public Overrides Function ToString() As String
            Return Me.Sequence & vbTab & Me.Position & vbTab & Me.Distance
        End Function
    End Class
    Public Class Common
        Public Shared Function Get_AT_Percent(Seq As Bio.ISequence) As Double
            Return (((From x In Seq Where x = Bio.Alphabets.DNA.A).Count + (From x In Seq Where x = Bio.Alphabets.DNA.T).Count) / Seq.Count) * 100


        End Function
        Public Shared Function Get_AT_Percent(Seq As Bio.ISequence, isComplenter As Boolean) As Double
            If isComplenter = False Then
                Return ((From x In Seq Where x = Bio.Alphabets.DNA.A).Count / Seq.Count) * 100

            Else
                Return (((From x In Seq Where x = Bio.Alphabets.DNA.T).Count) / Seq.Count) * 100

            End If


        End Function
        Public Shared Function Get_Percents(Seq As Bio.Sequence) As String
            Dim nofA = (From x In Seq Where x = Bio.Alphabets.DNA.A).Count

            Dim nofC = (From x In Seq Where x = Bio.Alphabets.DNA.C).Count

            Dim nofG = (From x In Seq Where x = Bio.Alphabets.DNA.G).Count

            Dim nofT = (From x In Seq Where x = Bio.Alphabets.DNA.T).Count

            Dim str As New System.Text.StringBuilder
            Return (nofC + nofG) / (nofA + nofC + nofG + nofT) * 100

        End Function
        Public Shared Function Get_Max_Consequitve(Seq As Bio.Sequence, b As Byte) As Integer
            Dim currByte = b
            Dim count = 0
            Dim maxCount As Integer = 0
            For i1 = 0 To Seq.Count - 1
                If Seq(i1) = currByte Then
                    count += 1
                Else
                    If maxCount >= count Then
                        count = 0
                    Else
                        maxCount = count
                        count = 0
                    End If
                End If
            Next
            Return maxCount
        End Function
        Public Shared Function Get_NofA(Seq As Bio.ISequence)
            Return (From x In Seq Where x = Bio.Alphabets.DNA.A).Count
        End Function
        Public Shared Function Get_NofT(Seq As Bio.ISequence)
            Return (From x In Seq Where x = Bio.Alphabets.DNA.T).Count
        End Function
        Public Shared Function Get_Percents_All_Header() As String
            Dim str As New System.Text.StringBuilder
            str.Append("NofA").Append(vbTab)
            str.Append("NofC").Append(vbTab)
            str.Append("NofG").Append(vbTab)
            str.Append("NofT").Append(vbTab)
            str.Append("Percent A").Append(vbTab)
            str.Append("Percent C").Append(vbTab)
            str.Append("Percent G").Append(vbTab)
            str.Append("Percent T").Append(vbTab)
            Return str.ToString
        End Function
        Public Shared Function Get_Percents_All(Seq As Bio.Sequence) As String
            Dim nofA = (From x In Seq Where x = Bio.Alphabets.DNA.A).Count

            Dim nofC = (From x In Seq Where x = Bio.Alphabets.DNA.C).Count

            Dim nofG = (From x In Seq Where x = Bio.Alphabets.DNA.G).Count

            Dim nofT = (From x In Seq Where x = Bio.Alphabets.DNA.T).Count

            Dim str As New System.Text.StringBuilder
            str.Append(nofA).Append(vbTab)
            str.Append(nofC).Append(vbTab)
            str.Append(nofG).Append(vbTab)
            str.Append(nofT).Append(vbTab)

            str.Append(nofA / Seq.Count * 100).Append(vbTab)
            str.Append(nofC / Seq.Count * 100).Append(vbTab)
            str.Append(nofG / Seq.Count * 100).Append(vbTab)
            str.Append(nofT / Seq.Count * 100).Append(vbTab)

            Return str.ToString
        End Function
        Public Shared Function Get_Percents_GC(Seq As Bio.Sequence) As Double
            Dim nofC = (From x In Seq Where x = Bio.Alphabets.DNA.C).Count

            Dim nofG = (From x In Seq Where x = Bio.Alphabets.DNA.G).Count

            Return (nofC + nofG) * Seq.Count * 100

        End Function
    End Class

    Public Enum StartCodons
        ATG = 0
        ATG_GTG = 1
        Any = 2
    End Enum
    Public Enum Frames
        fr1 = 1
        fr2 = 2
        fr3 = 3
        frm1 = -1
        frm2 = -2
        frm3 = -3
        frm = -4
        fr = 0
    End Enum
    Public Class ORF
        Public Property Frame As Integer
        Public Property Pos As Integer
        Public Property Endy As Integer
        Public Property ParentSeq As Bio.ISequence
        Public Property NASeq As Bio.ISequence
        Public Property AASeq As Bio.ISequence
        Public Property IsInner As Boolean = False
        Public Property HasStopCodon As Boolean = True
        Public Property Parent_CDS As FeatureItem
        Public Property Parent_mRNSs As New List(Of FeatureItem)
        Public Property ID As String
        Public Property loci As Bio.IO.GenBank.Location
        Public Sub New(pSeq As Bio.ISequence, nSeq As Bio.Sequence, st As Integer, en As Integer)
            Me.ParentSeq = pSeq
            Me.NASeq = nSeq
            Me.AASeq = DNA.Translate.Translate(nSeq)
            Me.Pos = st
            Me.Endy = en
            Dim fm = st Mod 3
            Select Case fm
                Case 0
                    Me.Frame = 3
                Case 1
                    Me.Frame = 1
                Case 2
                    Me.Frame = 2
            End Select
        End Sub
    End Class

    Public Class ORF_Manipulation

        Public Shared Sub Set_Names(ORFs As List(Of ORF), Desc As String)
            For i1 = 0 To ORFs.Count - 1
                Dim orf = ORFs(i1)
                orf.NASeq.ID = Desc & "_" & i1
                orf.AASeq.ID = Desc & "_" & i1
                orf.ParentSeq.ID = Desc
            Next

        End Sub

        Public Shared Iterator Function ORF_By_Frame(ORFs As List(Of ORF)) As IEnumerable(Of List(Of ORF))
            Dim gr = From x In ORFs Group By x.Frame Into Group

            For Each g In gr
                Dim res = From t In g.Group Order By t.AASeq.Count Descending
                Yield res.ToList

            Next
        End Function

        ''' <summary>
        ''' Return Longest ORF By AASeq.COunt
        ''' </summary>
        ''' <param name="oRFs"></param>
        ''' <returns></returns>
        Public Shared Function Get_Longest(oRFs As List(Of ORF)) As ORF
            Dim res = From x In oRFs Order By x.AASeq.Count Descending

            If res.Count > 0 Then Return res.First
            Return Nothing
        End Function

        ''' <summary>
        ''' First By Pos
        ''' </summary>
        ''' <param name="oRFs"></param>
        ''' <returns></returns>
        Public Shared Function Get_First(oRFs As List(Of ORF)) As ORF
            Dim res = From x In oRFs Order By x.Pos Ascending

            If res.Count > 0 Then Return res.First
            Return Nothing
        End Function

        ''' <summary>
        ''' IsIneer = False
        ''' </summary>
        ''' <param name="oRFs"></param>
        ''' <returns></returns>
        Public Shared Function Get_Fulls(oRFs As List(Of ORF)) As List(Of ORF)
            Dim res = From x In oRFs Where x.IsInner = False
            If res.Count = 0 Then Return New List(Of ORF)
            Return res.ToList

        End Function

        Public Shared Function Get_Inners(oRFs As List(Of ORF)) As List(Of ORF)
            Dim res = From x In oRFs Where x.IsInner = True
            If res.Count = 0 Then Return New List(Of ORF)
            Return res.ToList
        End Function

        Public Shared Function Get_As_TSV(orf As ORF) As String
            If IsNothing(orf) = True Then Return String.Empty
            Dim str As New System.Text.StringBuilder
            str.Append(orf.Frame).Append(vbTab)
            str.Append(orf.NASeq.ConvertToString).Append(vbTab)
            str.Append(orf.AASeq.ConvertToString).Append(vbTab)
            str.Append(orf.AASeq.Count).Append(vbTab)
            str.Append(orf.IsInner).Append(vbTab)
            str.Append(orf.HasStopCodon)
            Return str.ToString
        End Function

        Public Shared Function Get_AAs(ORFs As List(Of ORF)) As List(Of Bio.ISequence)
            Dim res = From x In ORFs Select x.AASeq

            If res.Count > 0 Then Return res.ToList

            Return New List(Of Bio.ISequence)
        End Function
        Public Shared Function Get_NAs(ORFs As List(Of ORF)) As List(Of Bio.ISequence)
            Dim res = From x In ORFs Select x.NASeq

            If res.Count > 0 Then Return res.ToList

            Return New List(Of Bio.ISequence)
        End Function

        Public Shared Function Merge(ORFs As List(Of ORF)) As List(Of ORF)

            Dim out As New List(Of ORF)
            For Each ByLabel In By_Label(ORFs)
                Dim Index As Integer = 0
                For Each item In By_TSS_wOrientation(ByLabel)
                    For i1 = 1 To item.Count - 1
                        item(0).Parent_mRNSs.Add(item(i1).Parent_mRNSs.First)
                        'Szunyi.Features.FeatureManipulation.GenBankMetaDataManipulation.AddFeature(cSeq, Ordered(i1 - 1))
                    Next
                    Index += 1
                    item.First.ID = Index & " " & item.First.Parent_CDS.Label
                    out.Add(item.First)
                Next

            Next
            Return out
        End Function

        Private Shared Iterator Function By_Label(oRFs As List(Of ORF)) As IEnumerable(Of List(Of ORF))
            Dim res = From x In oRFs Group By x.Parent_CDS.Label Into Group

            For Each gr In res
                Yield gr.Group.ToList
            Next
        End Function
        Private Shared Iterator Function By_TSS_wOrientation(Feats As List(Of ORF)) As IEnumerable(Of List(Of ORF))
            If Feats.First.Parent_CDS.Location.IsComplementer = False Then
                Dim j = From x In Feats Group By x.Pos Into Group
                Dim kj = From x In j Order By x.Pos Ascending
                For Each jh In kj
                    Yield jh.Group.ToList
                Next
            Else
                Dim j = From x In Feats Group By x.Pos Into Group
                Dim kj = From x In j Order By x.Pos Descending
                For Each jh In kj
                    Yield jh.Group.ToList
                Next
            End If
        End Function

        Public Shared Function Stat_By_CDS(merged_ORFs As List(Of ORF)) As String
            Dim str As New System.Text.StringBuilder
            Dim gr = From x In merged_ORFs Group By x.Parent_CDS Into Group

            For Each g In gr
                str.AppendLine()
                str.Append(g.Parent_CDS.Label).Append(vbTab).Append(Szunyi.BLs.Location.Common.GetLocationStringTab(g.Parent_CDS)).Append(vbTab).Append(g.Group.Count)
                For Each Item In g.Group
                    str.AppendLine(vbTab)
                    str.Append(Item.ID).Append(vbTab).Append(Item.AASeq.ConvertToString).Append(vbTab).Append(Item.AASeq.Count - 1).Append(vbTab)
                    str.Append(Szunyi.BLs.Location.Common.GetLocationStringTab(Item.loci))
                Next
            Next
            Return str.ToString
        End Function

        Public Shared Function Stat_By_ORF(merged_ORFs As List(Of ORF)) As String
            Dim str As New System.Text.StringBuilder
            For Each Item In merged_ORFs
                str.AppendLine()
                str.Append(Item.ID).Append(vbTab).Append(Szunyi.BLs.Location.Common.GetLocationStringTab(Item.loci)).Append(vbTab).Append(Item.Parent_mRNSs.Count)
                For Each si In Item.Parent_mRNSs
                    str.AppendLine()
                    str.Append(si.Label)
                Next
            Next
            Return str.ToString
        End Function

        Public Shared Function Stat_By_mRNA(merged_ORFs As List(Of ORF), mRNAs As List(Of FeatureItem)) As String
            Dim str As New System.Text.StringBuilder
            Dim has As Integer = 0
            For Each f In mRNAs
                Dim cORFs = From x In merged_ORFs Where x.Parent_mRNSs.Contains(f)
                str.Append(f.Label).Append(vbTab).Append(Szunyi.BLs.Location.Common.GetLocationStringTab(f)).Append(vbTab).Append(cORFs.Count)
                If cORFs.Count > 0 Then has += 1
                For Each item In cORFs
                    str.AppendLine()
                    str.Append(item.ID).Append(vbTab).Append(item.AASeq.ConvertToString).Append(vbTab).Append(item.AASeq.Count - 1).Append(vbTab)
                Next

            Next
            Return str.ToString
        End Function
    End Class

    Public Class ORF_Finding
        Public Shared AllStopCodons = Split("TAA,TAG,TGA", ",")

        Public Shared Function IsTerminalCodon(Seq As Bio.ISequence, Position As Integer, op As Bio.IO.GenBank.LocationOperator)
            If op <> LocationOperator.Complement Then

                If Seq.Item(Position) = 84 Then
                    If Seq.Item(Position + 1) = 65 AndAlso Seq.Item(Position + 2) = 65 Then 'TAA
                        Return True
                    ElseIf Seq.Item(Position + 1) = 65 AndAlso Seq.Item(Position + 2) = 71 Then 'TAG
                        Return True
                    ElseIf Seq.Item(Position + 1) = 71 AndAlso Seq.Item(Position + 2) = 65 Then 'TGA
                        Return True
                    End If
                End If
            Else
                If Seq.Item(Position) = 65 Then
                    If Seq.Item(Position - 1) = 84 AndAlso Seq.Item(Position - 2) = 84 Then 'TAA Rev = ATT
                        Return True
                    ElseIf Seq.Item(Position - 1) = 84 AndAlso Seq.Item(Position - 2) = 67 Then 'TAG rev = ATC
                        Return True
                    ElseIf Seq.Item(Position - 1) = 67 AndAlso Seq.Item(Position - 2) = 84 Then 'TGA rev = ACT
                        Return True
                    End If
                End If
            End If

            Return False
        End Function
        ''' <summary>
        ''' 'Return Location Or Nothing
        ''' </summary>
        ''' <param name="Seq"></param>
        ''' <param name="Start"></param>
        ''' <param name="IsReverse"></param>
        ''' <param name="AlloweedStart"></param>
        ''' <returns></returns>
        Public Shared Function Get_Location_From_Start_Position(Seq As Bio.ISequence, Start As Integer, op As Bio.IO.GenBank.LocationOperator, AlloweedStart As StartCodons) As Bio.IO.GenBank.Location
            If op <> LocationOperator.Complement Then
                Dim StartToEnd = Seq.GetSubSequence(Start - 1, Seq.Count - Start + 1)
                Select Case AlloweedStart
                    Case StartCodons.ATG
                        If Seq.Item(Start - 1) = 65 AndAlso Seq.Item(Start) = 84 AndAlso Seq.Item(Start + 1) = 71 Then
                            Dim alf As Int16 = 54
                            For i1 As Integer = Start - 1 To Seq.Count - 2 Step 3
                                If IsTerminalCodon(Seq, i1, op) Then
                                    Dim loci = Szunyi.BLs.Location.Common.GetLocation(Start, i1 + 3)

                                    Return loci
                                End If
                            Next
                        Else
                            Return Nothing
                        End If

                    Case StartCodons.ATG_GTG
                        If (StartToEnd.Item(Start - 1) = 65 Or StartToEnd.Item(Start - 1) = 71) AndAlso StartToEnd.Item(Start) = 84 AndAlso StartToEnd.Item(Start + 1) = 71 Then
                            For i1 As Integer = Start To Seq.Count - 2 Step 3
                                If IsTerminalCodon(Seq, i1, op) Then
                                    Return Szunyi.BLs.Location.Common.GetLocation(Start, i1 + 3)
                                End If
                            Next
                        Else
                            Return Nothing
                        End If
                    Case Else

                End Select
            Else
                Dim StartToEnd = Seq.GetSubSequence(0, Start)
                StartToEnd = StartToEnd.GetReverseComplementedSequence
                Select Case AlloweedStart
                    Case StartCodons.ATG
                        If StartToEnd.Item(0) = 65 AndAlso StartToEnd.Item(1) = 84 AndAlso StartToEnd.Item(2) = 71 Then
                            Dim alf As Int16 = 54
                            For i1 = Start - 1 To 2 Step -3
                                If IsTerminalCodon(Seq, i1, op) Then
                                    Dim loci = Szunyi.BLs.Location.Common.Get_Location("complement(" & i1 - 1 & ".." & Start & ")")

                                    Return loci
                                End If
                            Next
                        Else
                            Return Nothing
                        End If

                    Case StartCodons.ATG_GTG
                        If (StartToEnd.Item(0) = 61 Or StartToEnd.Item(0) = 65) AndAlso StartToEnd.Item(1) = 81 AndAlso StartToEnd.Item(2) = 65 Then
                            For i1 As Integer = Start To Seq.Count - 2 Step 3
                                If IsTerminalCodon(Seq, i1, op) Then
                                    Return Szunyi.BLs.Location.Common.GetLocation(Start, i1 + 3)
                                End If
                            Next
                        Else
                            Return Nothing
                        End If
                    Case Else

                End Select
            End If
        End Function
        Public Shared Function Get_uORFs(ORFs As List(Of ORF), fUTR As FeatureItem) As List(Of ORF)
            If fUTR.Location.IsComplementer = False Then
                Return (From x In ORFs Where x.Pos < Szunyi.BLs.Location.Common.Get_Length(fUTR) Order By x.AASeq.Count Descending).ToList
            Else
                Return (From x In ORFs Where x.Pos < Szunyi.BLs.Location.Common.Get_Length(fUTR) Order By x.AASeq.Count Descending).ToList
            End If

        End Function
        Public Shared Function Get_Longest(ORFs As List(Of ORF)) As List(Of ORF)
            Dim gr = From x In ORFs Group By x.Endy Into Group

            Dim out As New List(Of ORF)
            For Each g In gr
                Dim r = From x In g.Group Order By x.AASeq.Count Descending

                out.Add(r.First)
            Next
            Return out
        End Function
        ''' <summary>
        ''' Return Get_All_ORFs depends on location.iscomplementer
        ''' </summary>
        ''' <param name="Seq"></param>
        ''' <param name="Feat"></param>
        ''' <returns></returns>
        Public Shared Function Get_All_ORFs_ByOrientation(Seq As Bio.Sequence, Feat As FeatureItem) As List(Of ORF)
            If Feat.Location.IsComplementer = False Then
                Return DNA.ORF_Finding.Get_All_ORFs(Seq, Feat, DNA.Frames.fr, True, True)
            Else
                Return DNA.ORF_Finding.Get_All_ORFs(Seq, Feat, DNA.Frames.frm, True, True)
            End If
        End Function
        Public Shared Function Get_All_ORFs(Seq As Bio.Sequence, Feat As FeatureItem, frame As Frames, MustStartwATG As Boolean, MustEndwStopCodon As Boolean) As List(Of ORF)
            Dim TheSeq = Feat.GetSubSequence(Seq)

            If Feat.Location.IsComplementer = True Then
                TheSeq = TheSeq.GetReversedSequence

            End If

            Dim StartCodons = Get_Codons_Position(TheSeq, "ATG", frame)
            Dim StopCodons = Get_Codons_Positions(TheSeq, AllStopCodons, frame)
            Dim StartCodonsf = Get_Codons_Position_By_Frame(TheSeq, "ATG", frame, MustStartwATG)
            Dim StopCodonsf = Get_Codons_Positions_By_Frame(TheSeq, AllStopCodons, frame)
            Dim AllORFs = ORFs_From_Codons(StartCodonsf, StopCodonsf, TheSeq, MustEndwStopCodon)
            Return AllORFs
        End Function
        Public Shared Function Get_All_ORFs(Seq As Bio.Sequence, frame As Frames, MustStartwATG As Boolean, MustEndwStopCodon As Boolean) As List(Of ORF)

            Dim TheSeq = Seq
            Dim StartCodons = Get_Codons_Position(TheSeq, "ATG", frame)
            Dim StopCodons = Get_Codons_Positions(TheSeq, AllStopCodons, frame)
            Dim StartCodonsf = Get_Codons_Position_By_Frame(TheSeq, "ATG", frame, MustStartwATG)
            Dim StopCodonsf = Get_Codons_Positions_By_Frame(TheSeq, AllStopCodons, frame)
            Dim AllORFs = ORFs_From_Codons(StartCodonsf, StopCodonsf, TheSeq, MustEndwStopCodon)

            Return AllORFs
        End Function
        Private Shared Function ORFs_From_Codons(StartCodonsf As Dictionary(Of Integer, List(Of Integer)),
                                                 StopCodonsf As Dictionary(Of Integer, List(Of Integer)),
                                                 Seq As Bio.ISequence,
                                                 MustEndwStopCodon As Boolean) As List(Of ORF)
            Dim out As New List(Of ORF)
            For i1 = 0 To 2
                If StartCodonsf.ContainsKey(i1) And StopCodonsf.ContainsKey(i1) Then
                    For i2 = 0 To StartCodonsf(i1).Count - 1
                        Dim TheStopCodon = From x In StopCodonsf(i1) Order By x Ascending Where x > StartCodonsf(i1)(i2)

                        If TheStopCodon.Count > 0 Then
                            Dim tmpSeq = Seq.GetSubSequence(StartCodonsf(i1)(i2), TheStopCodon.First - StartCodonsf(i1)(i2) + 3)
                            Dim x As New ORF(Seq, tmpSeq, StartCodonsf(i1)(i2), TheStopCodon.First + 3)
                            out.Add(x)

                        ElseIf MustEndwStopCodon = False Then
                            Try
                                Dim tmpSeq = Seq.GetSubSequence(StartCodonsf(i1)(i2), Seq.Count - StartCodonsf(i1)(i2))
                                Dim x As New ORF(Seq, tmpSeq, StartCodonsf(i1)(i2), Seq.Count)
                                x.HasStopCodon = False
                                out.Add(x)
                            Catch ex As Exception
                                Dim kj As Int16 = 43
                            End Try

                        End If
                    Next
                ElseIf StartCodonsf.ContainsKey(i1) AndAlso MustEndwStopCodon = False Then
                    For i2 = 0 To StartCodonsf(i1).Count - 1
                        Try
                            Dim tmpSeq = Seq.GetSubSequence(StartCodonsf(i1)(i2), Seq.Count - StartCodonsf(i1)(i2))
                            Dim x As New ORF(Seq, tmpSeq, StartCodonsf(i1)(i2), Seq.Count)
                            x.HasStopCodon = False
                            out.Add(x)
                        Catch ex As Exception
                            Dim kj As Int16 = 43
                        End Try

                    Next

                End If
            Next
            Return out
        End Function


        Private Shared Function Get_Codons_Positions(theSeq As ISequence, Codons() As String, frame As Frames) As List(Of Integer)


            Dim Seq = theSeq.ConvertToString
            Dim fm As Integer = 0
            Dim out As New List(Of Integer)
            For Each codon In Codons
                Do
                    fm = Seq.IndexOf(codon, fm)
                    If fm = -1 Then Exit For
                    out.Add(fm)
                    fm += 1
                Loop
            Next
            Return out
        End Function

        Private Shared Function Get_Codons_Position(theSeq As ISequence, Codon As String, frame As Frames) As List(Of Integer)

            Dim Seq = theSeq.ConvertToString
            Dim fm As Integer = 0
            Dim out As New List(Of Integer)
            Do
                fm = Seq.IndexOf(Codon, fm)
                If fm = -1 Then Return out
                out.Add(fm)
                fm += 1
            Loop
        End Function
        Private Shared Function Get_Codons_Positions_By_Frame(theSeq As ISequence, Codons() As String, frame As Frames) As Dictionary(Of Integer, List(Of Integer))

            Dim Seq = theSeq.ConvertToString
            Dim out As New Dictionary(Of Integer, List(Of Integer))
            For Each Codon In Codons
                Dim fm As Integer = 0

                Do
                    fm = Seq.IndexOf(Codon, fm)
                    If fm = -1 Then Exit Do
                    Dim m = fm Mod 3
                    If out.ContainsKey(m) = False Then out.Add(m, New List(Of Integer))
                    out(m).Add(fm)
                    fm += 1
                Loop
            Next
            Return out
        End Function
        Private Shared Function Get_Codons_Position_By_Frame(theSeq As ISequence,
                                                             Codon As String,
                                                             frame As Frames,
                                                           Optional MustStartwATG As Boolean = True) As Dictionary(Of Integer, List(Of Integer))

            Dim Seq = theSeq.ConvertToString
            Dim fm As Integer = 0
            Dim out As New Dictionary(Of Integer, List(Of Integer))
            Do
                fm = Seq.IndexOf(Codon, fm)
                If fm = -1 Then Return out
                Dim m = fm Mod 3
                If out.ContainsKey(m) = False Then out.Add(m, New List(Of Integer))
                out(m).Add(fm)
                fm += 1
            Loop
        End Function
    End Class

    Public Class Translate
        Public Shared Function Translate(TheSeq As Bio.ISequence, Optional lociOperator As LocationOperator = LocationOperator.None) As Bio.ISequence
            If lociOperator = LocationOperator.Complement Then TheSeq = TheSeq.GetReverseComplementedSequence

            Dim RNASeq = Bio.Algorithms.Translation.Transcription.Transcribe(TheSeq)
            Dim AASeq As Bio.Sequence = Bio.Algorithms.Translation.ProteinTranslation.Translate(RNASeq)
            Return AASeq
        End Function
        Public Shared Function TranslateFrom(Seq As Bio.ISequence, StartPos As Integer, Optional Reverse As Boolean = False) As Bio.ISequence
            If Reverse = False Then

            End If

        End Function
        Public Shared Function TranaslateToString(Feat As FeatureItem, Seq As Bio.ISequence) As String
            Dim TheSeq As Bio.Sequence = Feat.GetSubSequence(Seq)

            If Feat.Location.Operator = LocationOperator.Complement Then TheSeq = TheSeq.GetReversedSequence

            Dim RNASeq = Bio.Algorithms.Translation.Transcription.Transcribe(TheSeq)
            Dim AASeq As Bio.Sequence = Bio.Algorithms.Translation.ProteinTranslation.Translate(RNASeq)
            Return AASeq.ConvertToString(0, AASeq.Count)

        End Function
        Public Shared Function TranaslateToString(TheSeq As Bio.ISequence, Optional lociOperator As LocationOperator = LocationOperator.None) As String

            If lociOperator = LocationOperator.Complement Then TheSeq = TheSeq.GetReversedSequence

            Dim RNASeq = Bio.Algorithms.Translation.Transcription.Transcribe(TheSeq)
            Dim AASeq As Bio.Sequence = Bio.Algorithms.Translation.ProteinTranslation.Translate(RNASeq)
            Return AASeq.ConvertToString(0, AASeq.Count)

        End Function
        Public Shared Function Tranaslate_By_Operator(TheSeq As Bio.Sequence, lociOperator As LocationOperator) As String

            If lociOperator = LocationOperator.Complement Then TheSeq = TheSeq.GetReverseComplementedSequence

            Dim RNASeq = Bio.Algorithms.Translation.Transcription.Transcribe(TheSeq)
            Dim AASeq As Bio.Sequence = Bio.Algorithms.Translation.ProteinTranslation.Translate(RNASeq)
            Return AASeq.ConvertToString(0, AASeq.Count)

        End Function

        Public Shared Function To_String(Seq As Bio.Sequence)
            Try

                Dim s = Tranaslate_By_Operator(Seq, LocationOperator.None)
                Return s
            Catch ex As Exception
                Return String.Empty

            End Try


        End Function
    End Class
End Class
