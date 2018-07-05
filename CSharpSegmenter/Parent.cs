using System;
using System.Collections.Generic;
using System.Text;

namespace CSharpSegmenter{
    /*/ The Parent Class, implemented based on the Segment interface /*/
    class Parent:Segment {
        private int numOfPixels;
        private List<Coordinate> segmentCoordinates;
        private List<Colour> segmentColours;
        private Segment segment1;
        private Segment segment2;
        public Parent(Segment segment1,Segment segment2) {
            this.segment1 = segment1;
            this.segment2 = segment2;
            numOfPixels = segment1.FindNumberOfPixels() + segment2.FindNumberOfPixels();

            segmentCoordinates = new List<Coordinate>();
            segmentColours = new List<Colour>();
            //add the list of coordinates of the segments
            segmentCoordinates.AddRange(segment1.GetSegmentCoordinates());
            segmentCoordinates.AddRange(segment2.GetSegmentCoordinates());
            //add the list of colours of the segments
            segmentColours.AddRange(segment1.GetSegmentColours());
            segmentColours.AddRange(segment2.GetSegmentColours());

        }

        public List<Colour> GetSegmentColours() {
            return segmentColours;
        }
        public List<Coordinate> GetSegmentCoordinates() {
            return segmentCoordinates;
        }
        public int FindNumberOfPixels() {
            return numOfPixels;
        }

    }
}
