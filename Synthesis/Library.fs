module Synthesis

open System.Text
open System.Text

let abelar a =
    (a<3097)&&(a>12)&&(a%12=0) 
     

let area b h =
    match (h >= 0.0) && (b >= 0.0) with
    |true -> (b / 2.0) * h 
    |_ -> failwith "Cannot have any negative value"

let zollo a =
    match a < 0 with 
    |true -> a * -1
    |_ -> a * 2

let min a b =
    match a < b with
    | true -> a
    | false -> b 

let max a b =
        match a > b with
    | true -> a
    | false -> b 

let ofTime a b c =
    (a * 60 * 60) + (b * 60) + (c)

let toTime a =
    match a <= 0 with
    | true -> 0,0,0
    | false -> let h = a / 3600
               let m = (a % 3600) / 60
               let s = (a - (h * 3600) - ( m * 60)) 
               h,m,s

let digits a =
    let rec cntDigits x nDigits=
        match x = 0 && nDigits <> 0 with
        | true -> nDigits
        | _ -> cntDigits (x / 10) (nDigits + 1)
    cntDigits a 0

let minmax (a,b,c,d) =
    let minF = min (min a b ) (min c d)
    let maxF = max  (max a b) ( max c d) 
    minF, maxF

let isLeap year =
    match year < 1582 with
    |true -> failwith "Requires larger number"
    |_ -> match ( (year % 4 = 0) && not (year % 100 = 0) ) || (year % 400 = 0)  with
          | true -> true
          | false -> false

let month a =
    match a with |1 -> "January", 31
                 |2 -> "February", 28
                 |3 -> "March", 31
                 |4 -> "April", 30
                 |5 -> "May", 31
                 |6 -> "June", 30
                 |7 -> "July", 31
                 |8 -> "August", 31
                 |9 -> "September", 30
                 |10 -> "October", 31
                 |11 -> "November", 30
                 |12 -> "December", 31
                 |_ -> failwith "Unacceptable month"
    

let toBinary num =
    let rec mBin v bin =
        match v<0 with
        |true -> failwith "No negatives"
        |_ ->    match v<=1 with
                |true-> match v with
                    |0->"0" + bin
                    |_->"1" + bin
                |_-> match v % 2 with
                    |1 -> mBin (v / 2) ("1" + bin )
                    |_ -> mBin (v / 2) ("0" + bin ) 
    mBin num ""

let bizFuzz final =
    let rec cnt n (t,f,tf) = 
        match n > final || n<1 with
        | true -> (t,f,tf) 
        |_ -> match (n % 3 = 0, n % 5 = 0, (n % 3 = 0) && (n % 5 = 0) ) with
              |true, true, true -> cnt (n + 1) (t + 1, f + 1, tf + 1)
              |true, _, _ -> cnt (n + 1) (t + 1, f, tf)
              |_, true, _ -> cnt (n + 1) (t, f + 1, tf)
              |_ ,_ ,_ -> cnt (n + 1) (t, f , tf)
    cnt 1 (0,0,0)

let monthDay day year =
    let rec myMonth d y monthCnt total=
        let a,b = month monthCnt 
        let total = total + b
        match d <= 0 || y < 1581 || isLeap y = false && d>=366 with
            | true -> failwith "Invalid day"
            | _ -> match isLeap y = true with 
                   |true ->  match day <= total, monthCnt = 2 with
                             |true, _ -> a      
                             |_, true -> myMonth day year (monthCnt + 1) (total - 1)
                             |_, _ -> myMonth day year (monthCnt + 1) total
                   |false -> match day <= total with
                             |true -> a
                             |_ -> myMonth day year (monthCnt + 1) total
    myMonth day year 1 0 
let coord _ =
    failwith "Not implemented"