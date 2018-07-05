module SegmentModule

type Coordinate = (int * int) // x, y coordinate of a pixel
type Colour = byte list       // one entry for each colour band, typically: [red, green and blue]

type Segment = 
    | Pixel of Coordinate * Colour
    | Parent of Segment * Segment 


//Conver segment into  Colour list
let rec convertSegments (segment:Segment) =
    match segment with
    |Pixel(_,colour) ->[ colour]
    |Parent(seg1,seg2) ->  (convertSegments seg1) @ (convertSegments seg2 )


//helper function square
let sqr x = x * x


// COnvert Byte to float
let rec convertByteToFloat list =
    match list with
    | [] -> []
    | head:: tail -> [List.map (fun x -> float x) head] @ (convertByteToFloat tail)

   
 //helper function ,calculate the standard deviation of a list
let stddevList numList =
    let mean = numList |> List.average
    let variance = numList |> List.averageBy (fun x -> sqr(x - mean))
    sqrt(variance)
// get all first column of a list of list
let getFirstColumnItems list =
    list |> List.map List.head


//remove first item in a list
let  rexmoveFirstItemList list =
    match list with 
    |[] -> []
    |head :: tail -> tail
// remove all first item in a list  of list
let removeFirstItemListofList list =
    list |> List.map rexmoveFirstItemList
// get all the first columns of a list of list, and then get all the second columns,...
let rec extractFirstColumn (list : 'T list list) =
    if list.[0] = [] then [] 
    else [getFirstColumnItems list] @  (list|>removeFirstItemListofList|>extractFirstColumn)

// extract the colourband of a segment
let extractColourBands (list: Colour List) =
    list |> extractFirstColumn




// return a list of the standard deviations of the pixel colours in the given segment
// the list contains one entry for each colour band, typically: [red, green and blue]
let stddev (segment: Segment) : float list =
    segment
    |> convertSegments 
    |>extractColourBands
    |>convertByteToFloat
    |>List.map stddevList

// Sum all elements in a float list and return a float value
let sumList (values: float list) =

    let rec sumList values accum =

        match values with
        | [] -> accum
        | head :: tail -> sumList tail (accum + head)

    sumList values 0.0

// count number of Pixels function
let rec countPixel segment :float =
    match segment with
    | Pixel(_,_) -> 1.0
    | Parent(seg1, seg2) -> (countPixel seg1) + (countPixel seg2)


// determine the cost of merging the given segments: 
// equal to the standard deviation of the combined the segments minus the sum of the standard deviations of the individual segments, 
// weighted by their respective sizes and summed over all colour bands
let mergeCost segment1 segment2 : float = 
    let combinedSegment = Parent(segment1,segment2)
    //standard dev of the segment1
    let sumStddev1 = segment1|>stddev|> sumList
     //standard dev of the segment2
    let sumStddev2 = segment2|>stddev|> sumList
     //standard dev of the combinedSegment
    let sumCombinedStddev = combinedSegment|>stddev|> sumList
    //find the number of pixels in the each segment
    let npixels1 = countPixel segment1
    let npixels2 = countPixel segment2
    let ncombined = countPixel combinedSegment
    //calculate the merge cost
    let cost = (sumCombinedStddev * ncombined) - (sumStddev1 * npixels1) - (sumStddev2 * npixels2)
    cost

   




     

