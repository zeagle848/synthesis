module Synthesis

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

let bizFuzz _ =
    failwith "Not implemented"

let monthDay _ _ =
    failwith "Not implemented"

let coord _ =
    failwith "Not implemented"