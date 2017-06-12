// Learn more about F# at http://fsharp.org. See the 'F# Tutorial' project
// for more guidance on F# programming.

#load "Library.fs"
#r "../../packages/FSharp.Data/lib/net40/FSharp.Data.dll"
open FinCa.Library
open FSharp.Data
open FSharp.Data.CsvExtensions

let report = CsvFile.Load(__SOURCE_DIRECTORY__ + "/report.csv")

// Print the prices in the HLOC format
for row in report.Rows do
  printfn "HLOC: (%s, %s, %s)" 
    (row.GetColumn "High") (row.GetColumn "Low") (row.GetColumn "Date")


