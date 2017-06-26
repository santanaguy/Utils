#r "E:/Projects/FParsec/packages/FParsec/lib/portable-net45+netcore45+wpa81+wp8/FParsecCS.dll"
#r "E:/Projects/FParsec/packages/FParsec/lib/portable-net45+netcore45+wpa81+wp8/FParsec.dll"
open FParsec
open System.Collections.Generic
type UserState = unit // doesn't have to be unit, of course
type Parser<'t> = Parser<'t, UserState>

let test p str = 
    match run p str with 
    | Success(result, _, _) -> printfn "Success: %A" result
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg
 
let str = pstring
let floatBetweenBrackets : Parser<_> = str "[" >>. pfloat .>> str "]"
let psep : Parser<_> = sepBy pfloat (pstring ",")

let identifier : Parser<_> = 
    let isIdentifierFirstChar c = isDigit c || c = '_'
    let isIdentifierChar c = isLetter c || c = '_'
    many1Satisfy2L isIdentifierFirstChar isIdentifierChar "cenas" .>> spaces 

test (many1Satisfy2 isDigit (fun c -> isLetter c || c = ' ')) "1ab ac"
test (anyOf "\\\n\r\t\"") "\"cd"

let mp: Parser<_> = pipe2 pfloat (str "€") (fun a b -> string (a - 10.0) + b)

test mp "1.25€"

let pt: Parser<_> = pfloat .>>. (str "€")

let testStrinReturn : Parser<_> = (stringReturn "ca" true) <|> (stringReturn "co" false)

test testStrinReturn "cnp"
type Element = Text of string
let t : Parser<_> = many1Satisfy (isNoneOf "<>'\"\\") |>> Text

test t "teste<coiso/><cenas2></cenas2>"
//era fixe poder fazer convert frombase 2 tobase 3

type Op = | ConvertBase of baseFrom:int * baseTo:int * value:int
          | ReverseWord of string

let next : Parser<_> = 
    let intws = pint32 .>> spaces
    let parsebase = pstring "convert base " >>. pipe3 intws intws intws (fun a b c -> ConvertBase(a,b,c))
    let parseReverseWord = pstring "text reverse " >>. restOfLine false |>> ReverseWord
    parsebase <|> parseReverseWord

let p str = run next str 

p "text reverse coisas"

