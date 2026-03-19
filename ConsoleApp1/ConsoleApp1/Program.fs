open System

type Tree =
    | Empty                          
    | Node of float * Tree * Tree    

let rnd = Random()

let randomValue minNumber maxNumber = 
    minNumber + (maxNumber - minNumber) * rnd.NextDouble()

let rec insert number tree =
    match tree with
    | Empty -> Node(number, Empty, Empty)
    | Node(v, left, right) ->
        if number < v then
            Node(v, insert number left, right)
        else
            Node(v, left, insert number right)

let rec generateTree nodeCount minNumber maxNumber =
    if nodeCount <= 0 then Empty
    else
        let number = randomValue minNumber maxNumber
        insert number (generateTree (nodeCount - 1) minNumber maxNumber)


let rec map f tree =
    match tree with
    | Empty -> Empty
    | Node(v, left, right) ->
        Node(f v, map f left, map f right)

let rec print indent tree =
    match tree with
    | Empty -> ()
    | Node(v, left, right) ->
        printfn "%s%.2f" indent v
        print (indent + "  ") left
        print (indent + "  ") right

[<EntryPoint>]
let main argv =
    let nodeCount = 8        
    let minNumber = -50.0       
    let maxNumber = 50.0        
    
    printfn "Create tree in %d random number in %.0f to %.0f" nodeCount minNumber maxNumber
    let tree = generateTree nodeCount minNumber maxNumber
    
    printfn "\nBefore tree:"
    print "" tree          
    
    let rounded = map Math.Round tree
    
    printfn "\nAfter tree:"
    print "" rounded
    
    0