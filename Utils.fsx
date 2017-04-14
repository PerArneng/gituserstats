
open System

module Utils =

    // week of the year
    let weekOfYear year dayOfYear =
        dayOfYear |> float 
                  |> (fun x -> (x/7.0)) 
                  |> Math.Ceiling
                  |> int
                  |> (fun week -> if week > 52 then 52 else week)
                  |> (fun week -> (year-2000)*100+week)
                  |> sprintf "Week %d"

    // month of the year
    let monthOfYear year month =
        (year-2000)*100+month |> sprintf "Month %d"

