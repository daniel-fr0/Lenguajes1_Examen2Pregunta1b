type Church =
    | Cero
    | Suc of Church

let rec suma a b = 
    match b with
    | Cero -> a
    | Suc b' -> Suc (suma a b')

let rec mult a b =
    match b with
    | Cero -> Cero
    | Suc b' -> suma a (mult a b')

let rec toInt church =
    match church with
    | Cero -> 0
    | Suc x -> 1 + toInt x

// Aliases para utilizar notación infija
let (<+>) a b = suma a b
let (<*>) a b = mult a b


// Ejemplos
let cuatro = Cero |> Suc |> Suc |> Suc |> Suc
let cinco  = Cero |> Suc |> Suc |> Suc |> Suc |> Suc


printfn "%d + %d es %d" (cuatro |> toInt) (cinco |> toInt) (cuatro <+> cinco |> toInt)
printfn "%d + %d es %d" (cinco |> toInt) (cuatro |> toInt) (cinco <+> cuatro |> toInt)

printfn ""

printfn "%d * %d es %d" (cuatro |> toInt) (cinco |> toInt) (cuatro <*> cinco |> toInt)
printfn "%d * %d es %d" (cinco |> toInt) (cuatro |> toInt) (cinco <*> cuatro |> toInt)

printfn ""

printfn "%d + %d es %d" (cuatro |> toInt) (Cero |> toInt) (cuatro <+> Cero |> toInt)
printfn "%d + %d es %d" (Cero |> toInt) (cuatro |> toInt) (Cero <+> cuatro |> toInt)

printfn ""

printfn "%d * %d es %d" (cinco |> toInt) (Cero |> toInt) (cinco <*> Cero |> toInt)
printfn "%d * %d es %d" (Cero |> toInt) (cinco |> toInt) (Cero <*> cinco |> toInt)

printfn ""

printfn "%d * %d es %d" (Suc Cero |> toInt) (cinco |> toInt) (Suc Cero <*> cinco |> toInt)
printfn "%d * %d es %d" (cinco |> toInt) (Suc Cero |> toInt) (cinco <*> Suc Cero |> toInt)
