namespace FinCa

module Library = 
  
  type Direction = 
    | UnknownDirection 
    | In  
    | Out    

  type Importance = 
    | UnKnownImportance
    | Critical 
    | Ok
    | Casual 

  type Schedule =
    | UnknownSchedule
    | Monthly
    | Weekly
    | BiWeekly
    | EveryNthDayOfMonth
    | Yearly
    | Constant

  type Bill = {
     Name : string
     Amount : float
     Subject : string
     Direction : Direction
     Importance : Importance
     Schedule : Schedule
  }

  type CompositeBill = {
    Bills : Bill [] 
  }

  let UNKNOWN = "Unknown"
  
  let emptyBill = {
     Name = UNKNOWN
     Amount = 0.0
     Subject = UNKNOWN
     Direction = UnknownDirection
     Importance = UnKnownImportance
     Schedule = UnknownSchedule
  }

  let calculateNetWorth bills = 
    let credit =
      bills.Bills 
      |> Array.filter(fun o -> o.Direction = In)
      |> Array.map(fun o -> o.Amount)
      |> Array.reduce(+)
    let debit =
      bills.Bills 
      |> Array.filter(fun o -> o.Direction = Out)
      |> Array.map(fun o -> o.Amount)
      |> Array.reduce(+)    
    credit - debit