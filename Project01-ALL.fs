#light

module Project01

open System

// helper functions for testing/implementing

let withinTolerance num target tolerance =
    (
     (num > (target-tolerance)) 
     && 
     (num < (target+tolerance))
    )

let matchTuples a b =
    match a,b with
    | ((x1,y1),(x2,y2)),((ansX1,ansY1),(ansX2,ansY2)) -> 
        ((x1=ansX1) && (x2=ansX2) && (y1=ansY1) && (y2=ansY2))
            ||
        ((x1=ansX2) && (x2=ansX1) && (y1=ansY2) && (y2=ansY1))

let head L =
    match L with
    | []      -> []
    | e::rest -> e

let rev L =
    let rec revHelp x = function
        | []           -> x
        | h::t -> revHelp (h::x) t
    revHelp [] L


// Returns the length of the list passed in as a parameter

let rec length L =
    match L with
    | []   -> 0
    | e::rest -> 1 + (length rest)


// The function max returns the value of the maximum element (as determined by the > operator)

let rec maxHelp curMax L =
    match L with
    | [] -> curMax
    | l_head::l_tail when l_head > curMax -> maxHelp l_head l_tail
    | _::l_tail -> maxHelp curMax l_tail

let max L =
    match L with
    | [] -> raise (System.ArgumentException("The input sequence was empty."))
    | l_head::l_tail -> maxHelp l_head l_tail


// The function min returns the value of the minimum element (as determined by the < operator)

let rec minHelp curMax L =
    match L with
    | [] -> curMax
    | l_head::l_tail when l_head < curMax -> minHelp l_head l_tail
    | _::l_tail -> minHelp curMax l_tail

let min L =
    match L with
    | [] -> raise (System.ArgumentException("The input sequence was empty."))
    | l_head::l_tail -> minHelp l_head l_tail


// The function nth returns the value of the element at index n of the list.

let rec nth L n =
    if n = 0 
        then List.head L
        else nth (List.tail L) (n - 1)


// The function map returns a new list created from the elements of the list passed in (L), after they have been transformed
// by the application of function F.  This function does not modify the original list, creating a brand new list one element
// at a time. 


let rec map F L =
    match L with
    | [] -> []
    | hd::tl -> 
        let hd2 = (F hd)
        hd2::(map F tl)


// The function iter behaves like map, but instead of building a new list from the elements of L, applies F and then throws
// away the result.  This is generally used only for functions which have side-effects, such as printing functions which do
// not change memory, but have the side effect of outputting to the screen. 


let rec iter F L =
    match L with
    | [] -> []
    | hd::tl -> 
        let hd2 = (F hd)
        hd2::(iter F tl)

// The function reduce takes a binary function, and pairwise applies the function to the elements of the list.
// The first application of the function takes the first and second elements as parameters, each following application uses
// the result of the previous application as the first parameter, and the next element of the list as the second parameter.

let rec reduceHelp F total list = 
    match list with
    | [] -> total
    | hd::tl ->
        if tl = [] then
            total
        else
            reduceHelp F (F total tl.Head) tl
        

let rec reduce F L =
    match L with
    | [] -> raise (System.ArgumentException("The input list was empty."))
    | hd::tl -> 
        if tl = [] then
            hd
        else
            (reduceHelp F (F hd tl.Head) tl)


// The function fold applies a function f to each element of the collection by threading an accumulator argument through the
// computation.  What this means is that the first parameter of the function initially has the valuepassed in to the accumulator,
// and then the result of each application of the function between the accumulator and an element of the list becomes the
// new accumulator for the next step.  These “updates” to the accumulator are accomplished via new values to the parameter
// for each function invocation

let rec fold F start L =
    match L with
    | [] -> start
    | hd::tl ->
        (fold F (F start hd) tl)


// The function flatten takes a list of lists, and puts the elements all together into a single list.

let rec flatHelp L Y output =
    match L with
    | [] -> 
        if Y = [] then
            output
        else
            (flatHelp (head Y) Y.Tail output)
    | hd::tail -> 
        let L2 = output @ [hd]
        (flatHelp tail Y L2)
            
let flatten L =
    match L with
    | [] -> []
    | hd::tail -> 
        (flatHelp hd tail [])


// The function zip combines the elements of two listspairwise, resulting in a list of tuples, where the first tuple in the list
// contains the first element of L1 as the first element, and the first element of L2 as the second element

let zip L1 L2 = 
  let rec zipHelp output L1 L2 =
    match L1, L2 with 
    | [],[]         -> output
    | h1::t1, h2::t2 -> zipHelp ((h1, h2)::output) t1 t2
    | _ ->  raise (System.ArgumentException("Different lengths"))
  rev(zipHelp [] L1 L2)
 

// The function unzip takes a list of pairs (tuples of size 2) and splits them apart into two lists.
// Since two lists are returned by this function, the return value is a tuple containing two lists.

let rec unzipHelp L output1 output2 =
    match L with
    | [] -> (rev(output1), rev(output2))
    | h1::rest -> 
        let(a, b) = h1
        unzipHelp rest (a::output1) (b::output2)

let unzip L =
    let output1, output2 = [], []
    unzipHelp L output1 output2


// The function range creates a list of integers from 0 to the stop value.  The list should not include the stop value,
// but instead contain stopelements (0 to stop-1). 

let rec rh x L = 
    match x with
    | 0 -> L
    | var1->
        let newList = (x-1)::L
        rh (x-1) newList

let rec range stop  =
    rh stop []


// The function range2 creates a list of integers from startto stop.  The list should include start but not stop,
// having(stop-start) elements.Called with a 0 as the value of start, should behave the same as the single parameter version
// of range.

let rec rangeHelp start stop L = 
    if stop = start then
        let newList = start::L
        L
    else
        let newList = (stop-1)::L
        rangeHelp start (stop-1) newList

let range2 start stop =
    rangeHelp start stop []


// The function range3 creates a list of integers from start to stop moving step values at a time.  The list should include
// start but not stop, having floor((stop-start)/step) elements.  Called with a 0 as the value of step, should behave the same
// as the two parameter version of range.

let rec rangeHelp2 start stop step L neg= 
    if neg = 0 then 
        match L with
        | [] -> 
            if(start+step) < stop then
                let thisList = start::L
                rangeHelp2 start stop step thisList 0
            else
                L

        | h1::rest ->
            if (start + step) >= stop then
                L
            else
                let newNum = (start + step)
                let newList = newNum::L
                rangeHelp2 newNum stop step newList 0
    else
        match L with
        | [] -> 
            if (start+step) > stop then
                let thisList = start::L
                rangeHelp2 start stop step thisList 1
            else
                L
        | h1::rest -> 
            if (start + step) <= stop then
                L
            else
                let newNum = (start + step)
                let newList = newNum::L
                rangeHelp2 newNum stop step newList 1



let range3 start stop step =
    if step < 0 then
        rev(rangeHelp2 start stop step [] 1)
    else
       rev(rangeHelp2 start stop step [] 0)
    

// The function slice returns a slice of the list using the limits provided

let rec sliceHelp L start stop acc output =
    match L with
    | [] -> output
    | h1::rest -> 
        if acc >= start && acc < stop then
            let newList = h1::output
            sliceHelp rest start stop (acc+1) newList
        else
            sliceHelp rest start stop (acc + 1) output

let slice L start stop =
    rev(sliceHelp L start stop 0 [])


// The function filter applies the predicate function F to
// the elements of list L and returns a list containing only the elements
// satisfying the predicate function.  

let rec filterHelp F L output = 
    match L with
    | [] -> output
    | h::rest ->
        if (F h) then
            let newList = h::output
            filterHelp F rest newList
        else
            filterHelp F rest output
            

let rec filter F L =
    match L with
    | [] -> []
    | h::rest ->
        rev(filterHelp F L [])



// Produce a list of pairs from a list of values.

let rec pairHelp L output = 
    match L with
    | [] -> output
    | h1::rest -> 
        match rest with
        | [] -> output
        | h2::rest2 ->
            let newList = (h1, h2)::output
            pairHelp rest2 newList

let pairUp L =
    rev(pairHelp L [])


// Finds the distance of tuple T from the origin.

let inline square x y =
    sqrt((float(x) + float(y)))


let inline distanceFromOrigin (x,y) =
    let x1, y1 = (x,y)
    let x2 = x1*x1
    let y2 = y1*y1
    square x2 y2


// Transforms a list of tuples into a list of distances from origin.

let rec lengthHelp L output = 
    match L with
    | [] -> output
    | h::rest ->
        let x,y = h
        let newList = (sqrt((x*x)+(y*y)))::output
        lengthHelp rest newList

let lengthsFromOrigin L =
    rev(lengthHelp L [])


// furthestPoints L

let rec furthHelpHelp h L dist pair = 
    match L with
    | [] -> pair
    | head::rest ->
        let x1, y1 = h
        let x2, y2 = head
        let newValue = (sqrt(((x2-x1)*(x2-x1))+((y2-y1)*(y2-y1))))
        if newValue >= dist then
            furthHelpHelp h rest newValue (h, head, newValue)
        else
            furthHelpHelp h rest dist pair


let rec furthHelp L dist output =
    match L with
    | [] -> 
        output
    | h::rest ->
        let tmp1, tmp2 = output
        let p1, p2, newDist = (furthHelpHelp h rest dist (tmp1, tmp2, dist))
        if newDist >= dist then
            furthHelp rest newDist (p1, p2)
        else
            furthHelp rest dist output


let furthestPoints L =
    match L with
    | [] -> raise (new ArgumentException("furthestPoints requires at least two points"))
    | _::[] -> raise (new ArgumentException("furthestPoints requires at least two points"))
    | _ -> furthHelp L 0.0 ((0.0,0.0), (0.0,0.0))








