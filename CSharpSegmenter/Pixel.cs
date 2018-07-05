using System;
using System.Collections.Generic;
using System.Text;

namespace CSharpSegmenter {
    /*/ Implement the Pixel Class based on the Segment interface /*/
    class Pixel: Segment {
        private int numOfPixel =1;
        private Coordinate coordinate;
        private Colour colour;
        public Pixel(Coordinate coordinate , Colour colour) {
            this.coordinate = coordinate;
            this.colour = colour;
        }

        public List<Coordinate> GetSegmentCoordinates() {
            List<Coordinate> coordList = new List<Coordinate>();
            coordList.Add(coordinate);
            return coordList;
        }
        public List<Colour> GetSegmentColours() {
            List<Colour> colourList = new List<Colour>();
            colourList.Add(colour);
            return colourList;
        }
        public int FindNumberOfPixels() {
            return numOfPixel;
        }
        
    }
}
