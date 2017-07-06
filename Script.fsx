#r "E:/Projects/FeedMe/packages/FParsec/lib/portable-net45+netcore45+wpa81+wp8/FParsecCS.dll"
#r "E:/Projects/FeedMe/packages/FParsec/lib/portable-net45+netcore45+wpa81+wp8/FParsec.dll"
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
    let another = pstring "convert base " >>. pipe3 intws intws (manyChars anyChar) 
                    (fun a b c -> printfn "str: '%s' count: %d" c (c |> String.length))

    run another str |> ignore
    match run (parsebase <|> parseReverseWord) str with 
    | Success(op,_,_) -> op
    | Failure(message,_,_) -> Fail message

let reverse (str:string) = List.ofSeq str |> List.rev |> Array.ofList |> System.String

let toBaseTen fromBase (number:string) = 
    let numberFromChar = function | c when c >= 'A' && c <= 'Z' -> (int c - int 'A') + 10 | c -> Char.ToString(c) |> Int32.Parse
    let numbers = number |> reverse |> List.ofSeq |> List.map numberFromChar
    let inBase10 fromBase n w = n * (Math.Pow(fromBase |> float, w |> float) |> int)
    if List.isEmpty numbers || numbers |> List.exists (fun n -> n > fromBase - 1) then 
        None
    else 
        Some (numbers |> List.mapi (fun i n-> inBase10 fromBase n i) |> List.reduce (+))

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

printfn "%A" (parseOp "convert base 16 10 10111101")