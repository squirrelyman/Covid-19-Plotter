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

    let ny = data |> Seq.where(fun x -> x.``Province/State`` = "New York")
    let nj = data |> Seq.where(fun x -> x.``Province/State`` = "New Jersey")
    let nc = data |> Seq.where(fun x -> x.``Province/State`` = "North Carolina")
    let it = data |> Seq.where(fun x -> x.``Country/Region`` = "Italy")
    let de = data |> Seq.where(fun x -> x.``Country/Region`` = "Germany")
    let sp = data |> Seq.where(fun x -> x.``Country/Region`` = "Spain")
    let ir = data |> Seq.where(fun x -> x.``Country/Region`` = "Iran")
    let uk = data |> Seq.where(fun x -> x.``Province/State`` = "United Kingdom")

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

    [ny; nj; nc; it; de; sp; ir; uk]
    |> List.map(casesSince100DayXY)
    |> Chart.Plot
    |> Chart.WithLabels(["New York"; "New Jersey"; "North Carolina"; "Italy"; "Germany"; "Spain"; "Iran"; "United Kingdom"])
    |> Chart.WithYTitle("Cases")
    |> Chart.WithOptions(layout)
    |> Chart.Show

    0 // return an integer exit code
