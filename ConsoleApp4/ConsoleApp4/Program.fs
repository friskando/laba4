open System

type Tree =
    | Empty                          
    | Node of string * Tree * Tree    

let rnd = Random()

let randomValue minNumber maxNumber = 
    rnd.Next(minNumber, maxNumber + 1)

let radnomChar len = 
    let chars = "zxc123"
    String(Array.init len (fun _ ->chars.[rnd.Next(chars.Length)]))

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
        let len = randomValue minNumber maxNumber
        let number = radnomChar len 
        insert number (generateTree (nodeCount - 1) minNumber maxNumber)

let rec foldTree folder state tree =
    match tree with
    | Empty -> state
    | Node(v, left, right) ->
        let stateLeft = foldTree folder state left
        let stateMiddle = folder stateLeft v
        let stateRight = foldTree folder stateMiddle right
        stateRight

let countCome target tree =
    foldTree (fun count value -> 
        if value = target then
            count + 1 else count
    ) 0 tree


//let rec countCome target tree =
//    match tree with
//    | Empty -> 0
//    | Node(v, left, right) ->
//        let countInLeft = countCome target left
//        let countInRight = countCome target right
//        if v = target then
//            1 + countInLeft + countInRight
//        else
//            countInLeft + countInRight

//let rec countComeOpt target tree =
//    match tree with
//    | Empty -> 0
//    | Node(v, left, right) ->
//        if target = v then
//            1 + countComeOpt target left + countComeOpt target right
//        elif target < v then
//            countComeOpt target left 
//        else
//            countComeOpt target right




let rec print indent tree =
    match tree with
    | Empty -> ()
    | Node(v, left, right) ->
        printfn "%s%s" indent v
        print (indent + "  ") left
        print (indent + "  ") right

[<EntryPoint>]
let main argv =
    let nodeCount = 15        
    let minNumber = 1       
    let maxNumber = 4        
    
    printfn "Создано дерево из %d : "  nodeCount
    let tree = generateTree nodeCount minNumber maxNumber
    
    printfn "\nДерево:"
    print "" tree
    
    printfn "\nВведите число для поиска:"
    let target = Console.ReadLine()
    
    let count = countCome target tree
    
    if count = 0 then
        printfn "\nЧисло %s не найдено в дереве" target
    else
        printfn "\nЧисло %s входит в дереве %d раз" target count
    
    0