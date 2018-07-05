using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace CSharpSegmenter {
    class SegmentationModule {

        /*/ Segmentation module, acept an Image , a size for outcome and the threshhold as paremeter/*/
        
        private int size = 3;// default size
        private TiffImage image;
        private float currentThreshold = 800; //default value
        private Segment[] pixelArray;
        private Dictionary<Segment,Segment> moduleSegmentation;

        //The Constructor for the Module, acept image, the size of the image and the threshhold as parameters
        public SegmentationModule(TiffImage image, int size, float threshold) {
            if(size > 0) {
                this.size = size;   
            }
            this.image = image;
            this.currentThreshold = threshold;
            this.pixelArray = CreatePixelArray(this.size);
            moduleSegmentation = new Dictionary<Segment, Segment>();
        }

        // Find the largest/top level segment that the given segment is a part of (based on the current segmentation)
        private Segment FindRoot( Segment segment) {
            Segment rootSegment;
            if (moduleSegmentation.ContainsKey(segment)) {
                rootSegment = FindRoot(moduleSegmentation[segment]);
            } else {
                rootSegment = segment;
            }
            return rootSegment;
        }

        //matche the Coordinate to the correspond Pixel
        private Segment PixelMap(Coordinate coord) {
            int currentPosition = (int) (Math.Pow(2, size) * coord.xCoord + coord.yCoord);
            Segment pixel = pixelArray[currentPosition];
            return pixel;
        }
        // returns the set of Segments which are neighbours of a given segment
        public HashSet<Segment> NeighboursSegment( Segment segment) {
            List <Coordinate> filterNeighboursCoords = FilterNeighboursCoords(segment);
            List<Segment> correspondPixelList = new List<Segment>();
            List<Segment> rootSegmentList = new List<Segment>();
            foreach (Coordinate coord in filterNeighboursCoords){
                correspondPixelList.Add(PixelMap(coord));
            }
            foreach (Segment seg in correspondPixelList) {
                Segment rootSegment = FindRoot( seg);
                rootSegmentList.Add(rootSegment);
            }
            HashSet<Segment> setOfRootSegments = new HashSet<Segment>(rootSegmentList);
            setOfRootSegments.Remove(segment);
            return setOfRootSegments;

        }
        //Find the neighbour(s) of the given segment that has the (equal) best merge cost
        // (exclude neighbours if their merge cost is greater than the threshold)
        private HashSet<Segment> FindBestNeighbours(Segment inputSegment) {
            HashSet<Segment> setOfNeighbours = NeighboursSegment(inputSegment);
            HashSet<Segment> setOfBestNeighbours = new HashSet<Segment>();
            float bestMergeCost = FindBestMergeCost(setOfNeighbours,inputSegment);
            if (setOfNeighbours.Count != 0) {
                foreach (Segment neighbourSeg in setOfNeighbours) {
                    float currentMergeCost = SegmentModule.CalculateMergeCost(inputSegment, neighbourSeg);
                    if ( (currentMergeCost == bestMergeCost) && (currentMergeCost <= this.currentThreshold)) {
                        setOfBestNeighbours.Add(neighbourSeg);
                    } 
                }

            }

            return setOfBestNeighbours;
        }

        // Try to find a neighbouring segmentB such that:
        //     1) segmentB is one of the best neighbours of segment A, and 
        //     2) segmentA is one of the best neighbours of segment B
        // if such a mutally optimal neighbour exists then merge them,
        // otherwise, choose one of segmentA's best neighbours (if any) and try to grow it instead (gradient descent)
        private void GrowOneSegment(Coordinate coord) {
            Segment currentSegment = PixelMap(coord);
            Segment currentSegmentRoot = FindRoot(currentSegment);
            HashSet<Segment> bestNeighbours = FindBestNeighbours(currentSegmentRoot);
            HashSet<Segment> mutuallyBestNeighbours = new HashSet<Segment>();
            if (bestNeighbours.Count != 0) {
                foreach (Segment bestNeighbour in bestNeighbours) {
                    Segment neighbourRoot = FindRoot( bestNeighbour);
                    HashSet<Segment> bestNeighbourBestNeighbours = FindBestNeighbours(neighbourRoot);
                    if( bestNeighbourBestNeighbours.Contains(currentSegmentRoot)) {
                        mutuallyBestNeighbours.Add(bestNeighbour);
                    }
                }
            }

            if (mutuallyBestNeighbours.Count ==0) {
                if(bestNeighbours.Count != 0) {
                    Segment newSegment = bestNeighbours.First();
                    Coordinate newCoord = newSegment.GetSegmentCoordinates()[0];
                    GrowOneSegment(newCoord);
                }
            } else {
                Segment bestNeighbourSegment = mutuallyBestNeighbours.First();
                Segment mergeSegment = new Parent(currentSegmentRoot, bestNeighbourSegment);
                moduleSegmentation.Add(bestNeighbourSegment, mergeSegment);
                moduleSegmentation.Add(currentSegmentRoot, mergeSegment);
            }


        }

        // Try to grow the segments corresponding to every pixel on the image in turn 
        // (considering pixel coordinates in special dither order)
        public void GrowAllCoordinates() {
            var seqOfGrowth = DitherModule.Coordinates(size);
            if (seqOfGrowth.Count != 0) {
                foreach (Coordinate coord in seqOfGrowth) {
                    GrowOneSegment(coord);
                }
            }
        }
        // Keep growing segments as above until no further merging is possible
        public void GrowUntilNoChange() {
            var currentItemCount = moduleSegmentation.Count;
            GrowAllCoordinates();
            if (currentItemCount != moduleSegmentation.Count) {
                GrowUntilNoChange();
            }
        }

        
        public Segment Segment(int x, int y) {
            Coordinate currentCoord = new Coordinate(x, y);
            Segment newSegment = PixelMap(currentCoord);
            Segment returnSegment = FindRoot(newSegment);
            return returnSegment;
            
        }
        // get the module segmentation
        private Dictionary<Segment,Segment> RetrieveSegmentation() {
            return moduleSegmentation;
        }
        // find Best MergeCost of a list helper function
        private float FindBestMergeCost(HashSet<Segment> segmentSet, Segment segment) {
            float bestMergeCost = 0;
            if (segmentSet.Count != 0) {
                bool firstPosition = true;
                foreach (Segment neighbourSeg in segmentSet) {
                    if (firstPosition) {
                        bestMergeCost = SegmentModule.CalculateMergeCost(segment, neighbourSeg);
                        firstPosition = false;
                    }
                    float currentMergeCost = SegmentModule.CalculateMergeCost(segment, neighbourSeg);
                    if (currentMergeCost < bestMergeCost) {
                        bestMergeCost = currentMergeCost;
                    }

                }
            }
            return bestMergeCost;


        }



        //return a list of neighbour coordinates of a given Segment
        private List<Coordinate> FindListNeighbourCoords(Segment segment) {
            List<Coordinate> segmentCoords = segment.GetSegmentCoordinates();
            List<Coordinate> neighbourCoords = new List<Coordinate>();
            foreach (Coordinate coord in segmentCoords) {
                int currentXCoord = coord.xCoord;
                int currentYCoord = coord.yCoord;
                neighbourCoords.Add(new Coordinate(currentXCoord + 1, currentYCoord));
                neighbourCoords.Add(new Coordinate(currentXCoord - 1, currentYCoord));
                neighbourCoords.Add(new Coordinate(currentXCoord, currentYCoord + 1));
                neighbourCoords.Add(new Coordinate(currentXCoord, currentYCoord - 1));
            }
            return neighbourCoords;

        }
        // Filter the list of NeighbourCoordinares to fits the image
        private List<Coordinate> FilterNeighboursCoords(Segment segment) {
            List<Coordinate> neighbourCoord = FindListNeighbourCoords(segment);
            List<Coordinate> filterNeighbourCoords = new List<Coordinate>();
            foreach (Coordinate coord in neighbourCoord) {
                int currentXCoord = coord.xCoord;
                int currentYCoord = coord.yCoord;
                if ((currentXCoord >= 0) && (currentYCoord >= 0) && (currentXCoord < Math.Pow(2, size)) && (currentYCoord < Math.Pow(2, size))) {
                    filterNeighbourCoords.Add(coord);
                }
            }
            return filterNeighbourCoords;
        }


        /// <summary>
        ///  Create every array of pixel that matches the coordinates of the outcome . 
        ///  To find the pixel array position xCoord * Math.Pow(2,size) + yCoord
        /// </summary>
        /// <param name="size"> size of the image </param>
        /// <returns> the array that contains all the Pixels represents a coordinate in the picture</returns>
        private Segment[] CreatePixelArray(int size) {
            int width = (int)Math.Pow(2, size);
            int height = (int)Math.Pow(2, size);

            Segment[] pixelArray = new Segment[width * height];
            for (int x =0; x < Math.Pow(2,size); x++) {
                for (int y = 0; y <Math.Pow(2,size); y++) {
                    Coordinate currentCoord = new Coordinate(x, y);
                    Colour currentColour = new Colour( new List<byte>(this.image.getColourBands(currentCoord) ) );
                    int position = (int) (x * Math.Pow(2, size) + y);
                    pixelArray[position] = new Pixel(currentCoord, currentColour);
                    
                }
            }
            return pixelArray;
        }
    }

}
