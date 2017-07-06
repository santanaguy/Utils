//Learn more about F# at http://fsharp.org

open System
open FParsec
open System.Collections.Generic
open System

type UserState = unit // doesn't have to be unit, of course
type Parser<'t> = Parser<'t, UserState>

type Op = | ConvertBase of baseFrom:int * baseTo:int * value:string
          | ReverseWord of string
          | Fail of string

let replaceHexPrefix (s:string) = if s.StartsWith("0x") then s.Substring(2) else s
let removeSpaces = String.filter (fun c -> c <> ' ')
let toUpper (s:string)= s.ToUpper();
let parseOp str = 
    let intws = pint32 .>> spaces
    let parsebase = pstring "convert base " >>. pipe3 intws intws (restOfLine false |>> replaceHexPrefix |>> removeSpaces |>> toUpper) (fun a b c -> ConvertBase(a,b,c))
    let parseReverseWord = pstring "text reverse " >>. restOfLine false |>> ReverseWord
    
    match run (parsebase <|> parseReverseWord) str with 
    | Success(op,_,_) -> op
    | Failure(message,_,_) -> Fail message

let reverse (str:string) = Array.ofSeq str |> Array.rev |> System.String

let toBaseTen fromBase (number:string) = 
    let numberFromChar = 
        function 
        | c when c >= 'A' && c <= 'Z' -> 
            printfn "%c" c |> ignore
            (int c - int 'A') + 10 
        | c -> 
            printfn "%c" c |> ignore
            Char.ToString(c) |> Int32.Parse
    let numbers = number |> reverse |> Array.ofSeq |> Array.map numberFromChar
    let inBase10 fromBase n w = n * (Math.Pow(fromBase |> float, w |> float) |> int)
    if Array.isEmpty numbers || numbers |> Array.exists (fun n -> n > fromBase - 1) then 
        None
    else 
        Some (numbers |> Array.mapi (fun i n-> inBase10 fromBase n i) |> Array.reduce (+))

let fromBaseTen (toBase:int) number = 
    let numberToValue n = if n >= 10 then 15 + int 'A' - 10 |> char |> string else n |> string
    let rec inner nmb = 
        if (nmb = 0) then [""] else  inner (nmb / toBase) @ [numberToValue (nmb % toBase)]
    inner number |> List.skipWhile (fun n -> n = "0") |> List.reduce (+)

let executeCommand cmd = 
    let command = parseOp cmd
    match command with
    | ConvertBase (from, t, value) -> (toBaseTen from value |> Option.map (fromBaseTen t))
    | ReverseWord word -> Some (reverse word)
    | Fail str -> Some str

// printfn "%O" (executeCommand "text reverse coise")
// let numberFromChar c = function | c when c >= 'A' && c <= 'Z' -> (int c - int 'A') + 10 | c -> Char.ToString(c) |> Int32.Parse
// numberFromChar 'Z'

[<EntryPoint>]
let main argv =
     printfn "%O" (executeCommand "convert base 16 10 ‭10111101‬")
    
