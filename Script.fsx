#r "E:/Projects/FParsec/packages/FParsec/lib/portable-net45+netcore45+wpa81+wp8/FParsecCS.dll"
#r "E:/Projects/FParsec/packages/FParsec/lib/portable-net45+netcore45+wpa81+wp8/FParsec.dll"
open FParsec
type UserState = unit // doesn't have to be unit, of course
type Parser<'t> = Parser<'t, UserState>

let test p str = 
    match run p str with 
    | Success(result, _, _) -> printfn "Success: %A" result
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg
 
let str s = pstring s
let floatBetweenBrackets : Parser<_> = str "[" >>. pfloat .>> str "]"