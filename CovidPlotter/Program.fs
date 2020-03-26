﻿open System
open System.IO
open FSharp.Data
open XPlot.Plotly
open MathNet.Numerics.LinearAlgebra
open MathNet.Numerics.LinearAlgebra.Solvers

let dataDir = "../../../../COVID-19/csse_covid_19_data/csse_covid_19_daily_reports/"

type CsvDailyData1 = CsvProvider<"../COVID-19/csse_covid_19_data/csse_covid_19_daily_reports/02-01-2020.csv">
type CsvDailyData2 = CsvProvider<"../COVID-19/csse_covid_19_data/csse_covid_19_daily_reports/03-22-2020.csv">

type DailyData =
    {
        county: string
        province_state: string
        country_region: string
        last_update: DateTime
        confirmed: int
        deaths: int
        recovered: int
    }

let fitLog2ndOrder (data: (float * float) seq) =
    let mat = matrix ( data |> Seq.map(fun (t, x) -> [1.0; t; t*t]))
    let vec = vector (data |> Seq.map(fun(t, x) -> Math.Log x))
    [yield! mat.Solve(vec)]

let fitLog1stOrder (data: (float * float) seq) =
    let mat = matrix ( data |> Seq.map(fun (t, x) -> [1.0; t]))
    let vec = vector (data |> Seq.map(fun(t, x) -> Math.Log x))
    let res = new Double.DenseVector(Seq.length data)
    [yield! mat.Solve(vec); 0.0]

let fitLog2ndOrderConcaveDown data =
    match data |> fitLog2ndOrder with
    | [_; _; x] when x > 0.0 ->  data |> fitLog1stOrder
    | x -> x

let layoutLog() =
    Layout(
        yaxis =
            Yaxis(
                ``type`` = "log",
                autorange = true
            )
    )

let layoutLogLog() =
    Layout(
        xaxis =
            Xaxis(
                ``type`` = "log",
                autorange = true
            ),
        yaxis =
            Yaxis(
                ``type`` = "log",
                autorange = true
            )
    )

[<EntryPoint>]
let main argv =
    let mapFileData (f: string) : DailyData seq =
        match DateTime.Parse(Path.GetFileNameWithoutExtension(f)) with
        | dt when dt < DateTime.Parse("06:00:00 02-01-2020") -> Seq.empty
        | dt when dt < DateTime.Parse("06:00:00 03-21-2020") ->
            (CsvDailyData1.Load f).Rows
            |> Seq.map(fun row ->
                {
                    county = "total"
                    province_state = row.``Province/State``
                    country_region = row.``Country/Region``
                    last_update = DateTime.Parse(Path.GetFileNameWithoutExtension(f)) + TimeSpan.FromDays(1.0)
                    confirmed = row.Confirmed
                    deaths = row.Deaths
                    recovered = row.Recovered
                })
        | _ ->
            (CsvDailyData2.Load f).Rows
            |> Seq.map(fun row ->
                {
                    county = row.Admin2
                    province_state = row.``Province_State``
                    country_region = row.``Country_Region``
                    last_update = DateTime.Parse(Path.GetFileNameWithoutExtension(f)) + TimeSpan.FromDays(1.0)
                    confirmed = row.Confirmed
                    deaths = row.Deaths
                    recovered = row.Recovered
                })

    let combineCountyData fileData =
        fileData
        |> Seq.where(fun x -> x.province_state <> "")
        |> Seq.map(fun x -> x.province_state)
        |> Seq.distinct
        |> Seq.map(fun state ->
            let stateData = fileData |> Seq.where(fun x -> x.province_state = state)
            {
                county = "total"
                province_state = state
                country_region = (Seq.head stateData).country_region
                last_update = (Seq.head stateData).last_update
                confirmed = stateData |> Seq.sumBy(fun x -> x.confirmed)
                deaths = stateData |> Seq.sumBy(fun x -> x.deaths)
                recovered = stateData |> Seq.sumBy(fun x -> x.recovered)
            }
        )

    let data = 
        Directory.EnumerateFiles(dataDir)
        |> Seq.where(fun x -> Path.GetExtension(x) = ".csv")
        |> Seq.map(fun x ->
            printfn "%A" x
            x)
        |> Seq.map(mapFileData)
        |> Seq.collect(fun fileData -> [
            yield! fileData
            yield! combineCountyData fileData
        ])
        |> Seq.toArray

    let dataByLocation = [
        ("New York", data |> Seq.where(fun x -> x.province_state = "New York" && x.county = "total"))
        ("New Jersey", data |> Seq.where(fun x -> x.province_state = "New Jersey" && x.county = "total"))
        ("North Carolina", data |> Seq.where(fun x -> x.province_state = "North Carolina" && x.county = "total"))
        ("Italy", data |> Seq.where(fun x -> x.country_region = "Italy"))
        ("Germany", data |> Seq.where(fun x -> x.country_region = "Germany"))
        ("Spain", data |> Seq.where(fun x -> x.country_region = "Spain"))
        ("Iran", data |> Seq.where(fun x -> x.country_region = "Iran"))
        ("United Kingdom", data |> Seq.where(fun x -> (x.province_state = "United Kingdom" || x.province_state = "") && x.country_region = "United Kingdom"))
        ("India", data |> Seq.where(fun x -> x.country_region = "India"))
        ("South Korea", data |> Seq.where(fun x -> x.country_region = "South Korea" || x.country_region = "Korea, South"))
    ]

    let buildPlot (data: (float * float) seq) =
        Scatter(
            x = (data |> Seq.map(fun (x, y) -> (float)x)),
            y = (data |> Seq.map(fun (x, y) -> (float)y)),
            mode = "lines"
        )

    let firstDayPast100 (x: DailyData seq) = x |> Seq.find(fun x -> x.confirmed > 100) |> (fun x -> x.last_update)
    
    let casesSince100DayXY (data: DailyData seq) =
        let dayOf100 = firstDayPast100 data
        data
        |> Seq.map(fun x -> ((float)(x.last_update - dayOf100).TotalHours / 24.0, (float)x.confirmed))
        |> Seq.where(fun (t, _) -> t >= 0.0)
        |> buildPlot

    let deathsSince100DayXY (data: DailyData seq) =
        let dayOf100 = firstDayPast100 data
        data
        |> Seq.map(fun x -> ((float)(x.last_update - dayOf100).TotalHours / 24.0, (float)x.deaths))
        |> Seq.where(fun (t, _) -> t >= 0.0)
        |> Seq.where(fun (_, x) -> x > 0.0)
        |> buildPlot

    let deathsVsConfirmedXY (data: DailyData seq) =
        data
        |> Seq.map(fun x -> ((float)x.confirmed, (float)x.deaths))
        |> Seq.where(fun (c, d) -> c > 100.0 && d > 10.0)
        |> buildPlot

    [
        dataByLocation
        |> List.map(fun (loc, data) -> casesSince100DayXY data)
        |> Chart.Plot
        |> Chart.WithLabels(dataByLocation |> List.map(fun (loc, data) -> loc))
        |> Chart.WithLayout(layoutLog())
        |> Chart.WithXTitle("Days since cases exceeded 100")
        |> Chart.WithYTitle("Confirmed Cases")

        dataByLocation
        |> List.map(fun (loc, data) -> deathsSince100DayXY data)
        |> Chart.Plot
        |> Chart.WithLabels(dataByLocation |> List.map(fun (loc, data) -> loc))
        |> Chart.WithLayout(layoutLog())
        |> Chart.WithXTitle("Days since cases exceeded 100")
        |> Chart.WithYTitle("Deaths")

        dataByLocation
        |> List.map(fun (loc, data) -> deathsVsConfirmedXY data)
        |> Chart.Plot
        |> Chart.WithLabels(dataByLocation |> List.map(fun (loc, data) -> loc))
        |> Chart.WithLayout(layoutLogLog())
        |> Chart.WithXTitle("Confirmed Cases")
        |> Chart.WithYTitle("Deaths")
    ] |> Chart.ShowAll

    dataByLocation
    |> Seq.map(fun (loc, data) ->
        let casesVsTime =
            data
            |> Seq.map(fun x -> ((float)(x.last_update - DateTime.Now).TotalHours / 24.0, (float)x.confirmed))
            |> Seq.where(fun (t, x) -> x > 50.0)
            |> Seq.toList

        //include nth point n times to bias fit to match more recent data
        let casesToFit = casesVsTime |> List.mapi(fun i x -> List.replicate(i) x) |> List.concat

        let casesFit = fitLog2ndOrderConcaveDown casesToFit        
        let predictedCases = Math.Exp casesFit.[0]
        let actualCases = casesVsTime |> Seq.map(fun (t, x) -> x) |> Seq.max
        let dailyGrowth = Math.Exp casesFit.[1] - 1.0
        let tDouble = Math.Log(2.0) / casesFit.[1]

        printfn "\n\n%s---------------------------------------" loc
        printfn "Cases = e^((%f)t^2 + (%f)t + %f)" casesFit.[2] casesFit.[1] casesFit.[0]
        printfn "Daily Growth: %f%% (doubles every %f days)" (dailyGrowth * 100.0) tDouble
        printfn "Current Predicted: %f Actual: %f" predictedCases actualCases

        let earliestInSeries = casesVsTime |> Seq.map(fun (t, _) -> t) |> Seq.min
        let predictedCasesVsTime = [int earliestInSeries-1..7] |> List.map(float) |> List.map(fun t ->
            (t, Math.Exp(casesFit.[2] * t * t + casesFit.[1] * t + casesFit.[0]))
        )

        [casesVsTime; predictedCasesVsTime]
        |> Seq.map(buildPlot)
        |> Chart.Plot
        |> Chart.WithLabels(["Actual"; "Fit"])
        |> Chart.WithOptions(layoutLog())
        |> Chart.WithYTitle("Confirmed Cases")
        |> Chart.WithXTitle("time from present (days)")
        |> Chart.WithTitle(loc)
    ) |> Chart.ShowAll

    0 // return an integer exit code
