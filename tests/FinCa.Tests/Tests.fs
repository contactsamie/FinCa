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
          |> Gen.map (fun dt -> 
              { 
                 dt with 
                   Direction = Some In 
                   Amount = if dt.Amount > 0m then dt.Amount else dt.Amount * -1
                   Name = System.Guid.NewGuid().ToString()
              })
        let debit =
          Arb.generate<Bill>
          |> Gen.map (fun dt -> 
              { 
                  dt with 
                    Direction = Some Out
                    Amount = if dt.Amount > 0m then dt.Amount else dt.Amount * -1
                    Name = System.Guid.NewGuid().ToString()
                           
              })     
        Gen.zip credit debit |> Gen.map BillsInOut 
    }


[]
type ``when analysing bills``() =

  [<SetUp>]
  member x.SetUp() = 
   Arb.register<BillsInOutGen>() |> ignore

  [<Property( 
   Verbose = true 
   //MaxTest=1000 
   )>]

  member x.``it should calculate simple net worth`` (BillsInOut (payCheck, owing)) =
    let compositeBill = {
      Bills = [| owing ; payCheck |] 
    }
    let netWorth = calculateNetWorth compositeBill
    Assert.AreEqual(payCheck.Amount - owing.Amount,netWorth)

  [<Property( 
   Verbose = true 
   //MaxTest=1000 
   )>]
  member x.``it should calculate net worth over a period of days`` (BillsInOut (payCheck, owing))  (days:uint8) =
    let compositeBill = {
      Bills = 
      [| 
         { owing with Schedule = Some Weekly ; Amount = Money 250.0m } 
         { payCheck with Schedule = Some BiWeekly ; Amount = Money 500.0m } 
         { owing with Schedule =  Some Monthly ; Amount = Money (decimal days) } 
         { payCheck with Schedule = Some Monthly ; Amount = Money (decimal days)} 
      |] 
    }
    let netWorth = calculateNetWorthOverPeriodDays compositeBill (int days)
    Assert.AreEqual(netWorth,Money 0.0m)
    
  [<Property( 
   Verbose = true 
   //MaxTest=1000 
   )>]
  member x.``it should calculate net worth over a period of days 2`` (BillsInOut (payCheck, owing))  =
    let compositeBill = {
      Bills = 
      [| 
         { owing with Schedule = Some Weekly ; Amount = Money 251.0m } 
         { payCheck with Schedule = Some BiWeekly ; Amount = Money 500.0m } 
         { owing with Schedule = Some Weekly ; Amount = Money 250.0m } 
         { payCheck with Schedule = Some BiWeekly ; Amount = Money 502.0m }  
      |] 
    }
    let netWorth = calculateNetWorthOverPeriodDays compositeBill 28
    Assert.AreEqual(netWorth,Money 0.0m)

  [<Property( 
   Verbose = true 
   //MaxTest=1000 
   )>]
  member x.``it should calculate net worth over a period of days 3`` (BillsInOut (payCheck, owing))  =
    let compositeBill = {
      Bills = 
      [| 
         { owing with Schedule = Some Weekly ; Amount = Money 251.0m } 
         { payCheck with Schedule = Some BiWeekly ; Amount = Money 500.0m } 
         { owing with Schedule = Some Weekly ; Amount = Money 251.0m } 
         { payCheck with Schedule = Some BiWeekly ; Amount = Money 500.0m }  
      |] 
    }
    let netWorth = calculateNetWorthOverPeriodDays compositeBill 28
    Assert.AreEqual(netWorth,Money -8.0m)
    
  [<Property( 
   Verbose = true 
   //MaxTest=1000 
   )>]
  member x.``it should calculate net worth with adjustments on credit`` (BillsInOut (payCheck, owing))  =
    let newMoney = Money 5.0m  
    let adjustment = { 
      Name = payCheck.Name 
      Schedule = Some Weekly
      Direction = Some Direction.In
      Importance = None
      Subject = None
      Amount = newMoney
    }
   
    let bills = [| owing ; payCheck |] 
    let compositeBill = { Bills =  billAdjust bills [ adjustment ]   }
    let netWorth = calculateNetWorth compositeBill
    Assert.AreEqual(newMoney - owing.Amount,netWorth)


  [<Property( 
   Verbose = true 
   //MaxTest=1000 
   )>]
  member x.``it should calculate net worth with adjustments on credit 2`` (BillsInOut (payCheck, owing))  =
    let newMoney = Money 5.0m  
    let adjustment = { 
      Name = payCheck.Name 
      Schedule = Some Weekly
      Direction = Some Direction.In
      Importance = None
      Subject = None
      Amount = newMoney
    }
   
    let bills = [| owing ; payCheck |] 
    let compositeBill = { Bills =  billAdjust bills [ adjustment; adjustment ]   }
    let netWorth = calculateNetWorth compositeBill
    Assert.AreEqual(newMoney  - owing.Amount,netWorth)

  [<Property( 
   Verbose = true 
   //MaxTest=1000 
   )>]
  member x.``it should calculate net worth with adjustments on debit`` (BillsInOut (payCheck, owing))  =
    let newMoney = Money 5.0m  
    let adjustment = { 
      Name = owing.Name 
      Schedule = Some Weekly
      Direction = Some Direction.Out
      Importance = None
      Subject = None
      Amount = newMoney
    }
   
    let bills = [| owing ; payCheck |] 
    let compositeBill = { Bills =  billAdjust bills [ adjustment ]   }
    let netWorth = calculateNetWorth compositeBill
    Assert.AreEqual( payCheck.Amount - newMoney,netWorth)


  [<Property( 
   Verbose = true 
   //MaxTest=1000 
   )>]
  member x.``it should calculate net worth with adjustments on debit 2`` (BillsInOut (payCheck, owing))  =
    let newMoney = Money 5.0m  
    let adjustment = { 
      Name = owing.Name 
      Schedule = Some Weekly
      Direction = Some Direction.Out
      Importance = None
      Subject = None
      Amount = newMoney
    }
   
    let bills = [| owing ; payCheck |] 
    let compositeBill = { Bills =  billAdjust bills [ adjustment; adjustment ]   }
    let netWorth = calculateNetWorth compositeBill
    Assert.AreEqual( payCheck.Amount - newMoney ,netWorth)

  [<Property( 
   Verbose = true 
   //MaxTest=1000 
   )>]
  member x.``it should calculate net worth with adjustments on debit and credit`` (BillsInOut (payCheck, owing))  =
    let newMoney = Money 5.0m     
    let bills = [| owing ; payCheck |] 
    let compositeBill = 
     { 
       Bills =  
         billAdjust bills [  
                            { owing with Amount = newMoney } 
                            { payCheck with Amount = newMoney } 
                          ]   
      }
    let netWorth = calculateNetWorth compositeBill
    Assert.AreEqual( Money 0m ,netWorth)

  [<Property( 
   Verbose = true 
   //MaxTest=1000 
   )>]
  member x.``it should calculate net worth with adjustments on debit and credit 2`` (BillsInOut (payCheck, owing))  =
    let newMoney = Money 5.0m 
    let paycheckAdjustment =  Money 10.0m
    let bills = [| owing ; payCheck |] 
    let compositeBill = 
     { 
       Bills =  
         billAdjust bills [  
                            { owing  with  Amount = newMoney } 
                            { payCheck with Amount = paycheckAdjustment } 
                          ]   
      }
    let netWorth = calculateNetWorth compositeBill
    Assert.AreEqual( paycheckAdjustment - newMoney ,netWorth)