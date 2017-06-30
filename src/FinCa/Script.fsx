// Learn more about F# at http://fsharp.org. See the 'F# Tutorial' project
// for more guidance on F# programming.

#load "Library.fs"
#r "../../packages/FSharp.Data/lib/net40/FSharp.Data.dll"

open FinCa.Library
open FSharp.Data
open FSharp.Data.CsvExtensions
open System

//let  file =  "/report.csv"
type DataSet =  
  CsvProvider<
    "c:\\\\report.csv",
    HasHeaders = true ,
    Schema = "Name,Amount (decimal),Direction,Schedule,Importance"
    >

let report = DataSet.Load("report.csv")

let bills =
  report.Rows 
   |>  Seq.map(fun row ->
       { 
         Name = row.Name
         Amount = Money row.Amount
         Direction =  Direction.fromString row.Direction
         Schedule =  Schedule.fromString row.Schedule
         Importance =  Importance.fromString row.Importance
         Subject = None
       }
      )
   |> Array.ofSeq

let adjustment = { 
  Name = "paycheck" 
  Schedule = Some Weekly
  Direction = Some Direction.In
  Importance = None
  Subject = None
  Amount = Money 0.0m
}
let billsObj = { Bills =  billAdjust bills [ adjustment ]   }
//let worth = calculateNetWorth billsObj
let daysPeriod =  Yearly  |> Some |> toDays
let monthWorth = getRecomendation billsObj daysPeriod