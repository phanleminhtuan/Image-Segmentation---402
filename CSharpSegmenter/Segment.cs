using System;
using System.Collections.Generic;
using System.Text;

namespace CSharpSegmenter {
    /*/ The interface for the segment /*/
    interface Segment {
        List<Colour> GetSegmentColours();
        List<Coordinate> GetSegmentCoordinates();
        int FindNumberOfPixels();

    }
}
