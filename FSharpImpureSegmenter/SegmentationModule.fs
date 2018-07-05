module SegmentationModule


open SegmentModule
open System.Runtime.InteropServices.ComTypes
open System.Collections.Generic
open System.Xml.Linq
open System.Xml.Linq

// Maps segments to their immediate parent segment that they are contained within (if any) 
// Use Dictionary instead of Map, which is a mutable data type to increase the efficient
// The efficient is greatly increased from 5 minutes for  n=5 to only 15 seconds
type Segmentation = Map<Segment, Segment>

// find the list of coordinates which make up the segment
let  rec findListCoordinates (segment:Segment) =
    match segment with
    |Pixel(coord,_) -> [coord]
    |Parent(seg1, seg2) -> findListCoordinates seg1 @ findListCoordinates seg2
// create a list that contains all the Pixels correspond to the available coordinates

// Impure Implementation, i
let createPixelsList (image:TiffModule.Image) (size:int) =
    let imageSize = pown 2 size
    let listOfPixels = new List<Segment>()

    for x= 0 to  (imageSize-1)   do
        for y =0 to (imageSize-1) do
            listOfPixels.Add( Pixel( (x,y) , TiffModule.getColourBands image (x,y) )) |> ignore
    listOfPixels
      
        
    

// Find the largest/top level segment that the given segment is a part of (based on the current segmentation)
let rec findRoot (segmentation: Segmentation) segment : Segment =
    match segmentation.TryFind segment with
    | None -> segment
    | Some value -> findRoot segmentation value

// Initially, every pixel/coordinate in the image is a separate Segment
// Note: this is a higher order function which given an image, 
// returns a function which maps each coordinate to its corresponding (initial) Segment (of kind Pixel)
let createPixelMap (N:int) (pixelsList : List<Segment>) : (Coordinate -> Segment) =
    let size = (pown 2 N)
    let pixelMap ((x,y) : Coordinate)  = 
        let currentPosition = size * x + y
        pixelsList.[ currentPosition ]
    pixelMap


// Find the neighbouring segments of the given segment (assuming we are only segmenting the top corner of the image of size 2^N x 2^N)
// Note: this is a higher order function which given a pixelMap function and a size N, 
// returns a function which given a current segmentation, returns the set of Segments which are neighbours of a given segment
let createNeighboursFunction (pixelMap:Coordinate->Segment) (N:int) : (Segmentation -> Segment -> Set<Segment>) =
    // find the list of neighbour coordinates of a given coordinates
    let findNeigbourCoords ((x,y):Coordinate) =
        [(x+1,y);(x-1,y);(x,y+1);(x,y-1)]

    // find the list of all neighbour coordinates of the coordinates of a given segment
    let findListOfNeighbourCoordinates (segment:Segment)=
        segment |> findListCoordinates |> List.map findNeigbourCoords |> List.concat    
    //filter the list of of neighbour coordinates to match the top corner only
    let coordinatesisFilter n list =
        list |> List.filter (fun (x,y) -> (x < ( pown 2 n )) && (y>=0 ) && (x  >= 0 ) && ( y < ( pown 2 n  ) ) ) 

    //the require neighbours function
    
    let neighbours (segmentation :Segmentation) (segment:Segment) =
        //find the neighbours of the current segment
        segment 
        |> findListOfNeighbourCoordinates
        // filter the list of neighbours coordinates to match the top-left corner
        |> coordinatesisFilter N 
        // Map the neighbours coords to the corresponding segment
        |> List.map pixelMap
        // Find the root segment of the neighbours segment
        |> List.map (fun x -> (findRoot segmentation x ) )
        //turn the list into set to remove duplicate
        |> Set.ofList
        // remove the initial segment
        |> Set.remove segment 
    neighbours
 

    // Fixme: add implementation here


// The following are also higher order functions, which given some inputs, return a function which ...


 // Find the neighbour(s) of the given segment that has the (equal) best merge cost
 // (exclude neighbours if their merge cost is greater than the threshold)
let createBestNeighbourFunction (neighbours:Segmentation->Segment->Set<Segment>) (threshold:float) : (Segmentation->Segment->Set<Segment>) =
   
    // find the minimun value of a set. Return 0 if the set is empty 
    //( cannot use minElement for empty set , mergecost cannot be negative)
    let findBestMergeCost set =
        if (Set.isEmpty set) then 0.0 else Set.minElement set

    let bestNeighbours (segmentation:Segmentation) (segment:Segment) =
        let setOfNeighbours = neighbours segmentation segment
        let bestMergeCost = setOfNeighbours|> Set.map (fun seg -> (mergeCost seg segment) ) |> findBestMergeCost
        let filterSetOfNeighbours = setOfNeighbours |> Set.filter (fun x -> ( (mergeCost x segment) <= threshold ) && ( (mergeCost x segment) = bestMergeCost ) )
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
        let mutuallyBestNeighbours = bestNeighbourOfSegmentSet |> Set.filter (fun seg -> (Set.contains rootOfCurrentSegment (bestNeighbours segmentation (findRoot segmentation seg)) ) )
        // Check if the Mutually best neighbours is empty . If yes, check if the best Neighbours is empty. If also yes, return the current segmentation, else, gradient descent.
        if (Set.isEmpty mutuallyBestNeighbours) then
             if (Set.isEmpty bestNeighbourOfSegmentSet) then
                segmentation
             else
                let newSegment = bestNeighbourOfSegmentSet |> Set.toList |> List.head
                let newCoord = newSegment |> findListCoordinates |> List.head
                growSegment segmentation newCoord
           
         // if mutually best Neighbours is not empty , grow the first mutually best neighbour  
        else
            let bestNeighbourSegment = mutuallyBestNeighbours |> Set.toList |> List.head
            let newSegmentation1 = segmentation.Add( bestNeighbourSegment , Parent(rootOfCurrentSegment, bestNeighbourSegment))
            let newSegmentation2 = newSegmentation1.Add(rootOfCurrentSegment, Parent(rootOfCurrentSegment, bestNeighbourSegment) )
           
            newSegmentation2
    growSegment



// Try to grow the segments corresponding to every pixel on the image in turn 
// (considering pixel coordinates in special dither order)
let createTryGrowAllCoordinatesFunction (tryGrowPixel:Segmentation->Coordinate->Segmentation) (N:int) : (Segmentation->Segmentation) =
    let seqOfGrowth = DitherModule.coordinates N
    let tryGrowAllCoordinates (segmentation:Segmentation) =
        // check if the sequence of growth is empty or no
        //if yes,return the current segmentation
        // grow the segmentation using the first coord, apply the next coord to the result segmentation
        if (Seq.isEmpty seqOfGrowth) then
            segmentation
        else
            let finalSegmentation = Seq.fold (fun seg -> tryGrowPixel seg) segmentation seqOfGrowth
            finalSegmentation
     
    tryGrowAllCoordinates
   


// Keep growing segments as above until no further merging is possible
let createGrowUntilNoChangeFunction (tryGrowAllCoordinates:Segmentation->Segmentation) : (Segmentation->Segmentation) =
    let rec growUntilNoChange (segmentation : Segmentation) =
        let growthSegmentation = tryGrowAllCoordinates segmentation
        // check if the new Segmenetation is the same or differnt from the previous segment
        //if same return the segmentation, esle continue to grow the segmentation
        if ( growthSegmentation = segmentation) then
            growthSegmentation
        else
            growUntilNoChange growthSegmentation
        
    growUntilNoChange



let segment (image:TiffModule.Image) (N: int) (threshold:float)  : (Coordinate -> Segment) =
    //try to create the growUntilnoChangeFunction by using the paramters
    let listOfPixels = createPixelsList image N
    let pixelMap = createPixelMap N listOfPixels
    let neighbours = createNeighboursFunction pixelMap N
    let bestNeighbours = createBestNeighbourFunction neighbours threshold
    let tryGrowPixel = createTryGrowOneSegmentFunction bestNeighbours pixelMap
        
    let tryGrowAllCoordinates = createTryGrowAllCoordinatesFunction tryGrowPixel  N
    let  growUntilNoChange = createGrowUntilNoChangeFunction tryGrowAllCoordinates

  
    //get the final segmentation, grow from an emty segmenetation
    let segmentation = Map.empty
    let finalSegmentation = growUntilNoChange segmentation 

    
    let segmentTheImage (coord:Coordinate) =
        //get the current Segment using the coord
        let currentSegment = pixelMap coord
        // return the segment of the image
        findRoot finalSegmentation currentSegment
        
    segmentTheImage
    

