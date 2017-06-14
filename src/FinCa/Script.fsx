// Learn more about F# at http://fsharp.org. See the 'F# Tutorial' project
// for more guidance on F# programming.

#load "Library.fs"
#r "../../packages/FSharp.Data/lib/net40/FSharp.Data.dll"

open FinCa.Library
open FSharp.Data
open FSharp.Data.CsvExtensions

let report = CsvFile.Load(__SOURCE_DIRECTORY__ + "/report.csv")

// Print the prices in the HLOC format

let r= 
  report.Rows 
   |> Seq.map(fun row -> 
     (row.GetColumn "Name"),
     (row.GetColumn "Amount"),
     (row.GetColumn "Direction"),
     (row.GetColumn "Schedule"),
     (row.GetColumn "Importance")
    )
   |> Seq.map(fun (a,b,c,d,e) -> 
        printfn "HLOC: (%s, %s, %s, %s, %s)" a b c d e
        (a ,b :?> float ,c :?> Direction ,d :?> Schedule ,e :?> Importance)
     )
   |> Array.ofSeq


