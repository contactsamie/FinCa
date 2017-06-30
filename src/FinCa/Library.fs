namespace FinCa
open Microsoft.FSharp.Reflection
module Library = 
 
  let toString (x:'a) = 
    match FSharpValue.GetUnionFields(x, typeof<'a>) with
    | case, _ -> case.Name

  let fromString<'a> (s:string) =
    match FSharpType.GetUnionCases typeof<'a> |> Array.filter (fun case -> case.Name = s) with
    |[|case|] -> Some(FSharpValue.MakeUnion(case,[||]) :?> 'a)
    |_ -> None

  type Direction = 
    | In  
    | Out 
    with
    override this.ToString() = toString this
    static member fromString s = fromString<Direction> s

  type Importance = 
    | Critical 
    | Ok
    | Casual    
    with
    override this.ToString() = toString this
    static member fromString s = fromString<Importance> s

  type Month = 
    | January
    | February
    | March
    | April
    | May
    | June
    | July
    | August
    | September
    | October
    | November
    | December

  type Schedule =
    | Monthly 
    | Weekly
    | BiWeekly
    | EveryNthDayOfMonth of int
    | Yearly
    | Constant of int
    | EveryMonthOf of Month
    | Daily
    with
    override this.ToString() = toString this
    static member fromString s = fromString<Schedule> s

  
  let toDays (s:Schedule option) =
    match s with
    | Some(EveryMonthOf x) -> 
       match x with
       | January -> 31
       | February -> 28
       | March -> 31
       | April -> 30
       | May -> 31
       | June -> 30
       | July -> 31
       | August -> 31
       | September -> 30
       | October -> 31
       | November -> 30
       | December -> 31       
    | Some Weekly -> 7
    | Some BiWeekly -> 14
    | Some Yearly  -> 28 *12
    | Some (Constant x)-> x
    | Some (EveryNthDayOfMonth x) -> x
    | Some Monthly -> 28
    | Some Daily -> 1
    | None -> 0

  let inline constrained predicate errormessage value =
    if not (predicate value)
    then invalidArg "value" errormessage
    else value

  let positive =
    constrained (fun x -> x > 0m) "Value must be positive"

  //Define it as a single case union type
  type Money = Money of decimal

  let (+) (Money x) (Money y) = Money ( x + y )
  let (-) (Money x) (Money y) = Money ( x - y )
  let (*) (Money x) (y:int) = Money ( x * decimal y )
  let (/) (Money x) y = Money ( x / y )
  let (>) (Money x) y =  x >= y 



  let toDecimal (Money x) = x

  type Bill = {
    Name : string
    Amount : Money
    Subject : string option
    Direction : Direction option
    Importance : Importance option
    Schedule : Schedule option
  }

  type CompositeBill = {
    Bills : Bill [] 
  }

  let UNKNOWN = "Unknown"
  
  let emptyBill = {
     Name = UNKNOWN
     Amount = Money 0.0m
     Subject = None
     Direction = None
     Importance = None
     Schedule = None
  }

  let BillPerDay bill = 
    let days = toDays bill.Schedule
    printfn "%A" days
    let bpd = bill.Amount / decimal days
    bpd


(*##################################################################################################################*)

  let getAmountOfBill (bills: Bill []) =
    if bills.Length <> 0 then
     bills
      |> Array.map(fun o -> o.Amount)
      |> Array.reduce(+)
    else
     Money 0.0m

  let getCreditBills bills =
     bills
      |> Array.filter(fun o -> o.Direction = Some In)

  let getDebitBills bills =
     bills
      |> Array.filter(fun o -> o.Direction = Some Out)

  let getAmountOfBillOverPeriod (period:int) (bills: Bill [])  =
     let result = 
      if bills.Length <> 0 then
       bills
       |> Array.map(fun b -> { b with Amount = (BillPerDay b) * period })
      else
       [||]
     getAmountOfBill result


  let calculateNetWorthOverPeriodDays bills period = 
    let credit =
       getCreditBills bills.Bills |> getAmountOfBillOverPeriod period
    let debit =
       getDebitBills bills.Bills |> getAmountOfBillOverPeriod period
    credit - debit


  let getRecomendation  bills period  =
    let worth = calculateNetWorthOverPeriodDays bills period
    if worth > 0.0m then
     "You are doing ok for now"
    else
     let unitAMount = worth / decimal period
     sprintf "You will need to make %A every day for %A days to be ok over this period"  unitAMount period

  let calculateNetWorth bills = 
    let credit =
      bills.Bills 
      |> Array.filter(fun o -> o.Direction = Some In)
      |> Array.map(fun o -> o.Amount)
      |> Array.reduce(+)
    let debit =
      bills.Bills 
      |> Array.filter(fun o -> o.Direction = Some  Out)
      |> Array.map(fun o -> o.Amount)
      |> Array.reduce(+)   
    credit - debit


  let billAdjust bills (adjustments:Bill list) =       
      bills 
      |> Array.map(fun b -> 
         if adjustments.IsEmpty then 
           b 
         else
           let items = adjustments |> List.filter(fun o -> o.Name=b.Name) 
           if  items.IsEmpty then
            b
           else
            items.Head
        )