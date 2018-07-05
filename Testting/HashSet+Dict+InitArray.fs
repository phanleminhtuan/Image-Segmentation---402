module HashSet_Dict_InitArray


open SegmentModule
open System.Runtime.InteropServices.ComTypes
open System.Collections.Generic
open System.Xml.Linq
open System.Linq
open System.Diagnostics

// Maps segments to their immediate parent segment that they are contained within (if any) 
// Use Dictionary instead of Map, which is a mutable data type to increase the efficient
// The efficient is greatly increased from 5 minutes for  n=5 to only 15 seconds
type Segmentation = Dictionary<Segment, Segment>

// find the list of coordinates which make up the segment
let  rec findListCoordinates (segment:Segment) =
    match segment with
    |Pixel(coord,_) -> [coord]
    |Parent(seg1, seg2) -> findListCoordinates seg1 @ findListCoordinates seg2
// create a list that contains all the Pixels correspond to the available coordinates
let createPixelsArray (image:TiffModule.Image) (size:int) =
    let imageSize = pown 2 size
    let listOfPixels = Array.zeroCreate<Segment> (imageSize * imageSize)
    let mutable currentPosition =0
    for x= 0 to  (imageSize-1)   do
        for y =0 to (imageSize-1) do
            currentPosition <- ( y + (imageSize * x) ) 
            listOfPixels.[currentPosition] <- Pixel( (x,y) , TiffModule.getColourBands image (x,y) )
    listOfPixels
      
        
    

// Find the largest/top level segment that the given segment is a part of (based on the current segmentation)
let rec findRoot (segmentation: Segmentation) segment : Segment =
    if ( segmentation.ContainsKey segment) then
        findRoot segmentation segmentation.[segment]
    else
        segment

// Initially, every pixel/coordinate in the image is a separate Segment
// Note: this is a higher order function which given an image, 
// returns a function which maps each coordinate to its corresponding (initial) Segment (of kind Pixel)
let createPixelMap (N:int) (pixelsList : Segment[]) : (Coordinate -> Segment) =
    let pixelMap ((x,y) : Coordinate)  = 
        let currentPosition = y + x *(pown 2 N)
        pixelsList.[ currentPosition ]
    pixelMap


// Find the neighbouring segments of the given segment (assuming we are only segmenting the top corner of the image of size 2^N x 2^N)
// Note: this is a higher order function which given a pixelMap function and a size N, 
// returns a function which given a current segmentation, returns the set of Segments which are neighbours of a given segment
let createNeighboursFunction (pixelMap:Coordinate->Segment) (N:int) : (Segmentation -> Segment -> HashSet<Segment>) =
    // find the list of neighbour coordinates of a given coordinates
 

    
    let neighbours (segmentation :Segmentation) (segment:Segment)  =
        //find the neighbours of the current segment
        let coordinates = findListCoordinates segment
        let listOfNeighbourCoords = new List<Coordinate>()
        // Map the neighbours coords to the corresponding segment
        let listOfNeighbourSegment = new List<Segment>()
        for ( (x,y) :Coordinate) in coordinates do
            listOfNeighbourCoords.Add( (x-1,y))
            listOfNeighbourCoords.Add( (x+1,y))
            listOfNeighbourCoords.Add( (x,y-1))
            listOfNeighbourCoords.Add( (x,y+1))
        let filterNeighbourCoords = listOfNeighbourCoords.Where(fun (x,y) -> x>=0 && y>=0 && x <(pown 2 N) && y <(pown 2 N) )
        for neighbourCoord in filterNeighbourCoords do  
            listOfNeighbourSegment.Add( pixelMap neighbourCoord |> findRoot segmentation)

        let neighboursSet = new HashSet<Segment> (listOfNeighbourSegment)
        neighboursSet.Remove(segment) |>ignore
        neighboursSet
         
    neighbours
 


// The following are also higher order functions, which given some inputs, return a function which ...


 // Find the neighbour(s) of the given segment that has the (equal) best merge cost
 // (exclude neighbours if their merge cost is greater than the threshold)
let createBestNeighbourFunction (neighbours:Segmentation->Segment->HashSet<Segment>) (threshold:float) : (Segmentation->Segment->HashSet<Segment>) =
   
    // find the minimun value of a set. Return 0 if the set is empty 
    //( cannot use minElement for empty set , mergecost cannot be negative)
    let findBestMergeCost (segmentSet:HashSet<Segment>) (segment:Segment) =
        let mutable bestCost = 0.0
        if ( segmentSet.Count <> 0) then
            let mutable firstPosition = true
            for seg in segmentSet do
                if (firstPosition) then
                   bestCost <- (mergeCost seg segment)
                   firstPosition <- false
                let currentMergeCost = (mergeCost seg segment)
                if (currentMergeCost < bestCost) then bestCost <- currentMergeCost
        bestCost

    //the require functions
    let bestNeighbours (segmentation:Segmentation) (segment:Segment) =
        let setOfNeighbours = neighbours segmentation segment
        let bestMergeCost =  findBestMergeCost setOfNeighbours segment
        let bestNeighboursSet = new HashSet<Segment>()
        for neighbourSeg in setOfNeighbours do 
            let currentMergeCost = (mergeCost neighbourSeg segment)
            if  ( (currentMergeCost = bestMergeCost) && (currentMergeCost <= threshold) ) then
                bestNeighboursSet.Add( neighbourSeg) |> ignore
        bestNeighboursSet           
    bestNeighbours
      
  

// Try to find a neighbouring segmentB such that:
//     1) segmentB is one of the best neighbours of segment A, and 
//     2) segmentA is one of the best neighbours of segment B
// if such a mutally optimal neighbour exists then merge them,
// otherwise, choose one of segmentA's best neighbours (if any) and try to grow it instead (gradient descent)
let createTryGrowOneSegmentFunction (bestNeighbours:Segmentation->Segment->HashSet<Segment>) (pixelMap:Coordinate->Segment) : (Segmentation->Coordinate->Segmentation) =    
    let rec growSegment (segmentation:Segmentation) (coord :Coordinate) =
        let currentSegment = pixelMap coord
        let rootOfCurrentSegment = findRoot segmentation currentSegment
        let bestNeighbourOfSegmentSet = bestNeighbours segmentation rootOfCurrentSegment
        let mutuallyBestNeighbours = new List<Segment>()
        if (bestNeighbourOfSegmentSet.Count <> 0) then
            for neighboursSeg in bestNeighbourOfSegmentSet do
                let neighbourRoot = findRoot segmentation neighboursSeg
                let bestNeighboursOfCurrentNeighbours = bestNeighbours segmentation neighbourRoot
                
                if (bestNeighboursOfCurrentNeighbours.Contains(rootOfCurrentSegment)) then
                    mutuallyBestNeighbours.Add(neighboursSeg) |> ignore
        
       //// Check if the Mutually best neighbours is empty . If yes, check if the best Neighbours is empty. If also yes, return the current segmentation, else, gradient descent.
        if (mutuallyBestNeighbours.Count = 0) then
             if (bestNeighbourOfSegmentSet.Count <> 0 ) then
                let newSegment = bestNeighbourOfSegmentSet.First()
                let newCoord = newSegment |> findListCoordinates
                let x = newCoord.[0]
                growSegment segmentation x  |> ignore
           
        // if mutually best Neighbours is not empty , grow the first mutually best neighbour  
        else
            let bestNeighbourSegment = mutuallyBestNeighbours.[0]
            let parentSeg = Parent(rootOfCurrentSegment, bestNeighbourSegment)
            segmentation.Add( bestNeighbourSegment , parentSeg) |>ignore
            segmentation.Add(rootOfCurrentSegment, parentSeg) |>ignore
        segmentation
           
        
    growSegment



// Try to grow the segments corresponding to every pixel on the image in turn 
// (considering pixel coordinates in special dither order)
let createTryGrowAllCoordinatesFunction (tryGrowPixel:Segmentation->Coordinate->Segmentation) (N:int) : (Segmentation->Segmentation) =
    let seqOfGrowth = DitherModule.coordinates N
    let tryGrowAllCoordinates (segmentation:Segmentation) =
            Seq.fold (fun seg -> tryGrowPixel seg) segmentation seqOfGrowth |> ignore
            segmentation
            
     
    tryGrowAllCoordinates
   


// Keep growing segments as above until no further merging is possible
let createGrowUntilNoChangeFunction (tryGrowAllCoordinates:Segmentation->Segmentation) : (Segmentation->Segmentation) =
    let rec growUntilNoChange (segmentation : Segmentation) =
        let currentCount  =  segmentation.Count
        tryGrowAllCoordinates segmentation |> ignore
        // check if the new Segmenetation is the same or differnt from the previous segment
        //if same return the segmentation, esle continue to grow the segmentation
        if ( currentCount = segmentation.Count) then
            segmentation
        else
            growUntilNoChange segmentation
        
    growUntilNoChange



let segment (image:TiffModule.Image) (N: int) (threshold:float)  : (Coordinate -> Segment) =
    //try to create the growUntilnoChangeFunction by using the paramters
    let listOfPixels = createPixelsArray image N
    let pixelMap = createPixelMap N listOfPixels
    let neighbours = createNeighboursFunction pixelMap N
    let bestNeighbours = createBestNeighbourFunction neighbours threshold
    let tryGrowPixel = createTryGrowOneSegmentFunction bestNeighbours pixelMap
        
    let tryGrowAllCoordinates = createTryGrowAllCoordinatesFunction tryGrowPixel  N
    let  growUntilNoChange = createGrowUntilNoChangeFunction tryGrowAllCoordinates
  
    //get the final segmentation, grow from an emty segmenetation
    let segmentation = new Dictionary<Segment,Segment>()
    let finalSegmentation = growUntilNoChange segmentation 

    
    let segmentTheImage (coord:Coordinate) =
        //get the current Segment using the coord
        let currentSegment = pixelMap coord
        // return the segment of the image
        findRoot finalSegmentation currentSegment
        
    segmentTheImage
    


