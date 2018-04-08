module SegmentationModule

open SegmentModule
open System.Runtime.InteropServices.ComTypes

// Maps segments to their immediate parent segment that they are contained within (if any) 
type Segmentation = Map<Segment, Segment>
// find the list of coordinates which make up the segment
let  rec findListCoordinates (segment:Segment) =
    match segment with
    |Pixel(coord,_) -> [coord]
    |Parent(seg1, seg2) -> findListCoordinates seg1 @ findListCoordinates seg2
// find the list of neighbour coordinates of a given coordinates
let findNeigbourCoords ((x,y):Coordinate) =
    [(x+1,y);(x-1,y);(x,y+1);(x,y-1)]

// find the list of all neighbour coordinates of the coordinates of a given segment
let findListOfNeighbourCoordinates (segment:Segment)=
    segment |> findListCoordinates |> List.map findNeigbourCoords |> List.concat
//filter the list of of neighbour coordinates to match the top corner only
let coordinatesisFilter list n =
    list |> List.filter (fun (x,y) -> x <=  ( pown 2 n )/2 ) |> List.filter (fun(x,y) -> y>=0 )|> List.filter (fun(x,y) -> x  >= 0 )|> List.filter (fun(x,y) -> y <= ( pown 2 n  )/2 ) 
// find the minimun value of a set. Return 0 if the set is empty ( cannot use minElement for empty set , mergecost cannot be negative)
let findBestMergeCost set =
    if (Set.isEmpty set) then 0.0 else Set.minElement set




// Find the largest/top level segment that the given segment is a part of (based on the current segmentation)
let rec findRoot (segmentation: Segmentation) segment : Segment =
    match segmentation.TryFind segment with
    | None -> segment
    | Some value -> findRoot segmentation value

// Initially, every pixel/coordinate in the image is a separate Segment
// Note: this is a higher order function which given an image, 
// returns a function which maps each coordinate to its corresponding (initial) Segment (of kind Pixel)
let createPixelMap (image:TiffModule.Image) : (Coordinate -> Segment) =

    let pixelMap ((x,y) : Coordinate)  = Pixel( (x,y) , TiffModule.getColourBands image (x,y) )
    pixelMap


// Find the neighbouring segments of the given segment (assuming we are only segmenting the top corner of the image of size 2^N x 2^N)
// Note: this is a higher order function which given a pixelMap function and a size N, 
// returns a function which given a current segmentation, returns the set of Segments which are neighbours of a given segment
let createNeighboursFunction (pixelMap:Coordinate->Segment) (N:int) : (Segmentation -> Segment -> Set<Segment>) =
    let neighbours (segmentation :Segmentation) (segment:Segment) =
        //find the neighbours of the current segment
        let listOfNeigbourCoords =  findListOfNeighbourCoordinates segment
        // filter the list of neighbours coordinates to match the top-left corner
        let filterListOfNeighbourCoords = coordinatesisFilter listOfNeigbourCoords N 
        // Map the neighbours coords to the corresponding segment
        let neighbourPixels =  filterListOfNeighbourCoords |> List.map pixelMap
        // Find the root segment of the neighbours segment
        let neighbourRootMap = neighbourPixels |> List.map (fun x -> (findRoot segmentation x ) )
        //turn the segments into set to remove duplicate
        let setOfNeighbours = Set.ofList neighbourRootMap
        // remove the initial segment
        Set.remove segment setOfNeighbours
    neighbours
 

    // Fixme: add implementation here


// The following are also higher order functions, which given some inputs, return a function which ...


 // Find the neighbour(s) of the given segment that has the (equal) best merge cost
 // (exclude neighbours if their merge cost is greater than the threshold)
let createBestNeighbourFunction (neighbours:Segmentation->Segment->Set<Segment>) (threshold:float) : (Segmentation->Segment->Set<Segment>) =
    let bestNeighbours (segmentation:Segmentation) (segment:Segment) =
        let setOfNeighbours = neighbours segmentation segment
        let bestMergeCost = setOfNeighbours|> Set.map (fun seg -> (mergeCost seg segment) ) |> findBestMergeCost
        let filterSetOfNeighbours = setOfNeighbours |> Set.filter (fun x -> ( (mergeCost x segment) = bestMergeCost ) )|> Set.filter (fun x -> ( (mergeCost x segment) <= threshold ) ) 
        filterSetOfNeighbours
    bestNeighbours
      
  
    // Fixme: add implementation here



// Try to find a neighbouring segmentB such that:
//     1) segmentB is one of the best neighbours of segment A, and 
//     2) segmentA is one of the best neighbours of segment B
// if such a mutally optimal neighbour exists then merge them,
// otherwise, choose one of segmentA's best neighbours (if any) and try to grow it instead (gradient descent)
let createTryGrowOneSegmentFunction (bestNeighbours:Segmentation->Segment->Set<Segment>) (pixelMap:Coordinate->Segment) : (Segmentation->Coordinate->Segmentation) =    
    let rec growSegment (segmentation:Segmentation) (coord :Coordinate) =
        let currentSegment = pixelMap coord
        let rootOfCurrentSegment = findRoot segmentation currentSegment
        let bestNeighbourOfSegmentSet = bestNeighbours segmentation rootOfCurrentSegment
        let mutuallyBestNeighbours = bestNeighbourOfSegmentSet |> Set.filter (fun seg -> (Set.contains rootOfCurrentSegment (bestNeighbours segmentation seg) ) )
        if (Set.isEmpty mutuallyBestNeighbours) then
             if (Set.isEmpty bestNeighbourOfSegmentSet) then
                segmentation
             else
                let newSegment = bestNeighbourOfSegmentSet |> Set.toList |> List.head
                let newCoord = newSegment |> findListCoordinates |> List.head
                growSegment segmentation newCoord
           
           

        else
            let bestNeighbourSegment = mutuallyBestNeighbours |> Set.toList |> List.head
            let newSegmentation1 = segmentation.Add( bestNeighbourSegment , Parent(rootOfCurrentSegment, bestNeighbourSegment))
            let newSegmentation2 = newSegmentation1.Add(rootOfCurrentSegment, Parent(rootOfCurrentSegment, bestNeighbourSegment) )
           
            newSegmentation2
    growSegment

    
    // Fixme: add implementation here


// Try to grow the segments corresponding to every pixel on the image in turn 
// (considering pixel coordinates in special dither order)
let createTryGrowAllCoordinatesFunction (tryGrowPixel:Segmentation->Coordinate->Segmentation) (N:int) : (Segmentation->Segmentation) =
    let rec tryGrowAllCoordinates (segmentation:Segmentation) =
        let seqOfGrowth = DitherModule.coordinates N
        let listSeqOfGrowth = Seq.toList seqOfGrowth
        let rec findfinalSegmentation (list :Coordinate list) (segmentation1 : Segmentation) =
            match list with 
            |[] -> segmentation1
            | head :: tail -> findfinalSegmentation tail (tryGrowPixel segmentation1 head)
        let finalSegmentation = findfinalSegmentation listSeqOfGrowth segmentation
        finalSegmentation

        
    tryGrowAllCoordinates
   
    // Fixme: add implementation here


// Keep growing segments as above until no further merging is possible
let createGrowUntilNoChangeFunction (tryGrowAllCoordinates:Segmentation->Segmentation) : (Segmentation->Segmentation) =
    let rec growUntilNoChange (segmentation : Segmentation) =
        let growthSegmentation = tryGrowAllCoordinates segmentation
        if ( growthSegmentation = segmentation) then
            growthSegmentation
        else
            growUntilNoChange growthSegmentation
        
    growUntilNoChange
    // Fixme: add implementation here


// Segment the given image based on the given merge cost threshold, but only for the top left corner of the image of size (2^N x 2^N)
let segment (image:TiffModule.Image) (N: int) (threshold:float)  : (Coordinate -> Segment) =
    let segmentTheImage (coord:Coordinate) =
        //create the Pixelmap of the image
        let pixelMap = createPixelMap image
        let neighbours = createNeighboursFunction pixelMap N
        let bestNeighbours = createBestNeighbourFunction neighbours threshold
        let tryGrowPixel = createTryGrowOneSegmentFunction bestNeighbours pixelMap
        let tryGrowAllCoordinates = createTryGrowAllCoordinatesFunction tryGrowPixel N
        let growUntilNoChange = createGrowUntilNoChangeFunction tryGrowAllCoordinates
        // retrieve the inital segment 
        let currentSegment = pixelMap coord
        //get the final segmentation, grow from an emty segmenetation
        let finalSegmentation = growUntilNoChange Map.empty
        
        let resultSegment = findRoot finalSegmentation currentSegment
        resultSegment
    segmentTheImage
    
    // Fixme: use the functions above to help implement this function