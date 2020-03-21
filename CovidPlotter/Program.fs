open System
open System.IO
open FSharp.Data
open XPlot.Plotly

let dataDir = "../../../../COVID-19/csse_covid_19_data/csse_covid_19_daily_reports/"

type DailyData = CsvProvider<"../COVID-19/csse_covid_19_data/csse_covid_19_daily_reports/02-01-2020.csv">

let filterNonMonotonic (s: ('a * int) seq) =
    let maxVals = 
        [0..Seq.length s - 1]
        |> Seq.map(fun i -> s |> Seq.take(i+1) |> Seq.map(fun (t, _) -> t) |> Seq.max)

    [
        for ((t, num), maxSoFar) in Seq.zip s maxVals do
            if t = maxSoFar
            then yield (t, num)
    ]

[<EntryPoint>]
let main argv =
    let data = 
        Directory.EnumerateFiles(dataDir)
        |> Seq.where(fun x -> Path.GetFileName(x).StartsWith("01") = false && x.EndsWith(".csv"))
        |> Seq.map(fun x ->
            printfn "%A" x
            x)
        |> Seq.map(DailyData.Load)
        |> Seq.collect(fun x -> x.Rows)
        |> Seq.toArray

    let dataByLocation = [
        ("New York", data |> Seq.where(fun x -> x.``Province/State`` = "New York"))
        ("New Jersey", data |> Seq.where(fun x -> x.``Province/State`` = "New Jersey"))
        ("North Carolina", data |> Seq.where(fun x -> x.``Province/State`` = "North Carolina"))
        ("Italy", data |> Seq.where(fun x -> x.``Country/Region`` = "Italy"))
        ("Germany", data |> Seq.where(fun x -> x.``Country/Region`` = "Germany"))
        ("Spain", data |> Seq.where(fun x -> x.``Country/Region`` = "Spain"))
        ("Iran", data |> Seq.where(fun x -> x.``Country/Region`` = "Iran"))
        ("United Kingdom", data |> Seq.where(fun x -> x.``Province/State`` = "United Kingdom"))
    ]

    let buildPlot (data: (float * int) seq) =
        Scatter(
            x = (data |> Seq.map(fun (x, y) -> (float)x)),
            y = (data |> Seq.map(fun (x, y) -> (float)y)),
            mode = "lines"
        )

    let firstDayPast100 (x: DailyData.Row seq) = x |> Seq.find(fun x -> x.Confirmed > 100) |> (fun x -> x.``Last Update``)
    
    let casesSince100DayXY (data: DailyData.Row seq) =
        let dayOf100 = firstDayPast100 data
        data
        |> Seq.map(fun x -> ((float)(x.``Last Update`` - dayOf100).TotalHours / 24.0, x.Confirmed))
        |> Seq.where(fun (t, _) -> t >= 0.0)
        |> filterNonMonotonic
        |> buildPlot

    let layout =
        Layout(
            yaxis =
                Yaxis(
                    ``type`` = "log",
                    autorange = true
                )
        )

    dataByLocation
    |> List.map(fun (loc, data) -> data)
    |> List.map(casesSince100DayXY)
    |> Chart.Plot
    |> Chart.WithLabels(dataByLocation |> List.map(fun (loc, data) -> loc))
    |> Chart.WithYTitle("Cases")
    |> Chart.WithOptions(layout)
    |> Chart.WithXTitle("Days since cases exceeded 100")
    |> Chart.WithYTitle("Confirmed Cases")
    |> Chart.Show

    0 // return an integer exit code
