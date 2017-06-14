module FinCa.Tests

System.IO.Directory.SetCurrentDirectory (__SOURCE_DIRECTORY__)

open FinCa
open NUnit.Framework
open FinCa.Library
open FsCheck
open FsCheck.NUnit

type BillsInOut = BillsInOut of Bill * Bill

type BillsInOutGen =
  static member BillsInOut () =
    { 
     new Arbitrary<BillsInOut>() with
      override x.Generator =
        let credit =
          Arb.generate<Bill>
          |> Gen.map (fun dt -> { dt with Direction = In })
        let debit =
          Arb.generate<Bill>
          |> Gen.map (fun dt -> { dt with Direction = Out })        
        Gen.zip credit debit |> Gen.map BillsInOut 
    }


[]
type ``when analysing bills``() =

  [<SetUp>]
  member x.SetUp() = 
   Arb.register<BillsInOutGen>() |> ignore

  [<Property( Verbose = true )>]
  member x.``it should calculate net worth`` (BillsInOut (payCheck, owing)) =
    let compositeBill = {
      Bills = [| owing ; payCheck |] 
    }
    let netWorth = calculateNetWorth compositeBill
    Assert.AreEqual(payCheck.Amount - owing.Amount,netWorth)

  [<Property( Verbose = true )>]
  member x.``it should calculate net worth 2`` (owing : Bill, payCheck : Bill) = 
    let bills = [| 
        { owing with Direction = Direction.Out }  
        { payCheck with Direction = Direction.In } 
      |]
    let compositeBill = { Bills = bills }
    let netWorth = calculateNetWorth compositeBill
    Assert.AreEqual(payCheck.Amount - owing.Amount,netWorth)