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

    let analyzeAndPlotLocationDataVsTime (inputData: (float * float) List) loc propertyName =
        //include nth point n times to bias fit to match more recent data
        let casesToFit = inputData |> List.mapi(fun i x -> List.replicate(i+1) x) |> List.concat

        let fit = fitLog2ndOrderConcaveDown casesToFit        
        let dailyGrowth = Math.Exp fit.[1] - 1.0
        let tDouble = Math.Log(2.0) / fit.[1]

        printfn "\n\n%s %s---------------------------------------" loc propertyName
        printfn "y = e^((%f)t^2 + (%f)t + %f)" fit.[2] fit.[1] fit.[0]
        printfn "Daily Growth: %f%% (doubles every %f days)" (dailyGrowth * 100.0) tDouble

        let earliestInSeries = inputData |> Seq.map(fun (t, _) -> t) |> Seq.min
        let predictedData = [int earliestInSeries-1..7] |> List.map(float) |> List.map(fun t ->
            (t, Math.Exp(fit.[2] * t * t + fit.[1] * t + fit.[0]))
        )

        [inputData; predictedData]
        |> Seq.map(buildPlot)
        |> Chart.Plot
        |> Chart.WithLabels(["Actual"; "Fit"])
        |> Chart.WithOptions(layoutLog())
        |> Chart.WithYTitle(propertyName)
        |> Chart.WithXTitle("Time from present (days)")
        |> Chart.WithTitle(loc)

    dataByLocation |> Seq.map(fun (loc, data) ->
        let casesVsTime =
            data
            |> Seq.map(fun x -> ((float)(x.last_update - DateTime.Now).TotalHours / 24.0, (float)x.confirmed))
            |> Seq.where(fun (t, x) -> x > 50.0)
            |> Seq.toList

        analyzeAndPlotLocationDataVsTime casesVsTime loc "Confirmed Cases"
    ) |> Chart.ShowAll

    dataByLocation |> Seq.map(fun (loc, data) ->
        let casesVsTime =
            data
            |> Seq.map(fun x -> ((float)(x.last_update - DateTime.Now).TotalHours / 24.0, float x.confirmed - float x.recovered - float x.deaths))
            |> Seq.where(fun (t, x) -> x > 50.0)
            |> Seq.toList

        analyzeAndPlotLocationDataVsTime casesVsTime loc "Active Cases"
    ) |> Chart.ShowAll

    dataByLocation
    |> Seq.where(fun (_, x) -> x |> Seq.where(fun row -> row.deaths > 5) |> Seq.length > 4)
    |> Seq.map(fun (loc, data) ->
        let casesVsTime =
            data
            |> Seq.map(fun x -> ((float)(x.last_update - DateTime.Now).TotalHours / 24.0, float x.deaths))
            |> Seq.where(fun (t, x) -> x > 5.0)
            |> Seq.toList

        analyzeAndPlotLocationDataVsTime casesVsTime loc "Deaths"
    ) |> Chart.ShowAll

    0 // return an integer exit code
