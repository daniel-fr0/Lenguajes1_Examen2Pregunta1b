type Arbol<'a> =
    | Hoja of 'a
    | Rama of 'a * Arbol<'a> * Arbol<'a>

let rec esMaxHeap arbol =
    match arbol with
    | Hoja x -> true
    | Rama (x, izq, der) ->
        match izq, der with
        | Hoja y, Hoja z -> x >= y && x >= z
        | Hoja y, Rama (z, _, _) -> x >= y && x >= z && esMaxHeap der
        | Rama (y, _, _), Hoja z -> x >= y && x >= z && esMaxHeap izq
        | Rama (y, _, _), Rama (z, _, _) -> x >= y && x >= z && esMaxHeap izq && esMaxHeap der

let rec preorder arbol =
    match arbol with
    | Hoja x -> [x]
    | Rama (x, izq, der) -> [x] @ (preorder izq) @ (preorder der)

let rec postorder arbol =
    match arbol with
    | Hoja x -> [x]
    | Rama (x, izq, der) -> (postorder izq) @ (postorder der) @ [x]

let rec esSimetrico arbol =
    preorder arbol = postorder arbol

let esMaxHeapSimetrico arbol = esMaxHeap arbol && esSimetrico arbol





// Ejemplos
let A = Rama (5, Rama (4, Hoja 3, Hoja 1), Hoja 2)

printfn "Preorder: %A" (preorder A)
printfn "Postorder: %A" (postorder A)
printfn "Max-heap: %b" (esMaxHeap A)
printfn "Simetrico: %b" (esSimetrico A)

if esMaxHeapSimetrico A then
    printfn "Es max-heap simetrico"
else
    printfn "No es max-heap simetrico"

printfn "" // Salto de linea

let B = Rama (1, Hoja 1, Hoja 1)

printfn "Preorder: %A" (preorder B)
printfn "Postorder: %A" (postorder B)
printfn "Max-heap: %b" (esMaxHeap B)
printfn "Simetrico: %b" (esSimetrico B)

if esMaxHeapSimetrico B then
    printfn "Es max-heap simetrico"
else
    printfn "No es max-heap simetrico"

printfn "" // Salto de linea

let C = Hoja 1

printfn "Preorder: %A" (preorder C)
printfn "Postorder: %A" (postorder C)
printfn "Max-heap: %b" (esMaxHeap C)
printfn "Simetrico: %b" (esSimetrico C)

if esMaxHeapSimetrico C then
    printfn "Es max-heap simetrico"
else
    printfn "No es max-heap simetrico"

printfn "" // Salto de linea

let D = Rama (1, Rama (2, Hoja 3, Hoja 4), Rama (5, Hoja 6, Hoja 7))

printfn "Preorder: %A" (preorder D)
printfn "Postorder: %A" (postorder D)
printfn "Max-heap: %b" (esMaxHeap D)
printfn "Simetrico: %b" (esSimetrico D)

if esMaxHeapSimetrico D then
    printfn "Es max-heap simetrico"
else
    printfn "No es max-heap simetrico"
