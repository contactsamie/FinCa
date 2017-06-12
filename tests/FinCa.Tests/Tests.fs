module FinCa.Tests

System.IO.Directory.SetCurrentDirectory (__SOURCE_DIRECTORY__)

open FinCa
open NUnit.Framework
open FinCa.Library
open FsCheck

[<Test>]
let ``calculate Net Worth`` () =
  let owing = 10.0
  let payCheck = 20.0
  
  let compositeBill = {
    Bills =
      [| 
        { emptyBill with Amount = owing; Direction = Out }
        { emptyBill with Amount = payCheck; Direction = In }
      |] 
  }
  let netWorth = calculateNetWorth compositeBill
  Assert.AreEqual(payCheck - owing,netWorth)
