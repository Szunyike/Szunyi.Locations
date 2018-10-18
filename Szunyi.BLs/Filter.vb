Imports Bio.Extensions
Imports Bio.IO.GenBank

Public Class Filter
    Public Shared Function Get_Most_Abundants(Mappings As List(Of Bio.IO.GenBank.ILocation), Sort As Locations_By) As List(Of Bio.IO.GenBank.ILocation)
        Select Case Sort
            Case Locations_By.TSS

                Dim x = From t In Mappings Group By t.TSS, t.IsComplementer Into Group Order By Group.Count Descending

                Return x.First.Group.ToList

            Case Locations_By.PAS
                Dim x = From t In Mappings Group By t.PAS, t.IsComplementer Into Group Order By Group.Count Descending

                Return x.First.Group.ToList
            Case Locations_By.TSS & Locations_By.PAS
                Dim x = From t In Mappings Group By t.LocationStart, t.LocationEnd, t.IsComplementer Into Group Order By Group.Count Descending

                Return x.First.Group.ToList
            Case Locations_By.LE
                Dim x = From t In Mappings Group By t.LocationEnd, t.IsComplementer Into Group Order By Group.Count Descending

                Return x.First.Group.ToList
            Case Locations_By.TSS
                Dim x = From t In Mappings Group By t.LocationStart, t.IsComplementer Into Group Order By Group.Count Descending

                Return x.First.Group.ToList
        End Select
        Return New List(Of ILocation)
    End Function

    Public Shared Function Get_Most_Abundants_tss_pas(Mappings As List(Of Bio.IO.GenBank.ILocation)) As Bio.IO.GenBank.ILocation
        Dim x = From t In Mappings Group By t.TSS, t.IsComplementer Into Group Order By Group.Count Descending

        Dim TSS_Pos = x.First.Group.First.TSS

        Dim x1 = From t In Mappings Group By t.PAS, t.IsComplementer Into Group Order By Group.Count Descending

        Dim PAS_Pos = x.First.Group.First.PAS

        Dim loci = Location.Common.GetLocation(TSS_Pos, PAS_Pos)

        Return loci
    End Function

    Public Shared Function Get_Most_Abundants(BLs As List(Of Basic_Location), Sort As Locations_By) As List(Of Basic_Location)

        Select Case Sort
            Case Locations_By.TSS

                Dim x = From t In BLs Group By t.Location.TSS, t.Location.IsComplementer Into Group Order By Group.Count Descending

                Return x.First.Group.ToList

            Case Locations_By.PAS
                Dim x = From t In BLs Group By t.Location.PAS, t.Location.IsComplementer Into Group Order By Group.Count Descending

                Return x.First.Group.ToList
            Case Locations_By.TSS = Locations_By.PAS
                Dim x = From t In BLs Group By t.Location.LocationStart, t.Location.LocationEnd, t.Location.IsComplementer Into Group Order By Group.Count Descending

                Return x.First.Group.ToList
            Case Locations_By.LE
                Dim x = From t In BLs Group By t.Location.LocationEnd, t.Location.IsComplementer Into Group Order By Group.Count Descending

                Return x.First.Group.ToList
            Case Locations_By.TSS
                Dim x = From t In BLs Group By t.Location.LocationStart, t.Location.IsComplementer Into Group Order By Group.Count Descending

                Return x.First.Group.ToList
        End Select
        Return New List(Of Basic_Location)
    End Function

End Class

