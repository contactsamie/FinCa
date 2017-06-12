module FinCa.Tests

System.IO.Directory.SetCurrentDirectory (__SOURCE_DIRECTORY__)

open FinCa
open NUnit.Framework
open FinCa.Library
open FsCheck
open FsCheck.NUnit

[<Property( Verbose = true )>]
let ``calculate Net Worth`` (owing:float,payCheck:float) =

  let compositeBill = {
    Bills =
      [| 
        { emptyBill with Amount = owing; Direction = Out }
        { emptyBill with Amount = payCheck; Direction = In }
      |] 
  }
  let netWorth = calculateNetWorth compositeBill
  Assert.AreEqual(payCheck - owing,netWorth)
