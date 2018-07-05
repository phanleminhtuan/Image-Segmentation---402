using System;
using System.Collections.Generic;
using System.Text;


namespace CSharpSegmenter {
    /*/ static class to interact with segments
     * Use the class to calculate the standard deviation of a segment
     * also calculate the merge cost between 2 segments/*/
    static class SegmentModule {
        // extract each colour and put into a list in a list of list
        public static List<List<byte>> ExtractColourBand(Segment segment) {
            List<Colour> currentColours = segment.GetSegmentColours();
            int numberOfColours = currentColours[0].colours.Count;
            //create the list of list to keep each colour
            List<List<Byte>> segmentColourBand = new List<List<byte>>();
            //create the item in the list to store each colour
            for(int i = 0; i < numberOfColours; i++) {
                segmentColourBand.Add(new List<byte>() ) ;
            }
                
            foreach(Colour colour in currentColours) {
                int currentPosition = 0;
                foreach (byte colourByte in colour.colours){
                    segmentColourBand[currentPosition].Add(colourByte);
                    currentPosition++;
                }
                
            }
            return segmentColourBand;
        }

        //Calculate the sum standard deviation of all colour of a segment
        public static float CalculateStandardDeviation(Segment segment) {
            float sumStandardDev = 0;
            List < List<Byte>> colourbands = ExtractColourBand(segment);
            foreach (List<Byte> distinctColour in colourbands) {
                sumStandardDev += FindStddev(distinctColour);
            }

            return sumStandardDev;
        }


        // Find the mean value of a  byte list
        public static float FindMeanList(List<byte> list) {
            float mean ;
            int numberOfElements = list.Count;
            if (numberOfElements == 0) {
                mean = 0;
            } else {
                float sumOfElement = 0;
                foreach (Byte value in list) {
                    sumOfElement += value;
                }
                mean = sumOfElement / numberOfElements;
            }
            return mean;
        }
        // find the variance of a byte list
        public static float FindListVariance(List<byte> list) {
            float meanOfList = FindMeanList(list);
            float varianceOfList = 0;
            int numberOfElements = list.Count;
            if (numberOfElements == 0) {
                varianceOfList = 0;
            } else {
                foreach (byte value in list) {
                    varianceOfList += (float)Math.Pow((value - meanOfList), 2);
                }
                varianceOfList = varianceOfList / numberOfElements;
            }
            return varianceOfList;
        }
        // find the sttdev of a byte list
        public static float FindStddev(List<Byte> list) {
            float variance = FindListVariance(list);
            float stddev = (float)Math.Sqrt(variance);
            return stddev;

        }
        // calculate the merge Cost of 2 segment
        public static float CalculateMergeCost(Segment segment1, Segment segment2) {
            float stddevSeg1 = CalculateStandardDeviation(segment1);
            float stddevSeg2 = CalculateStandardDeviation(segment2);
            Segment parentSegement = new Parent(segment1, segment2);
            float stddevParent = CalculateStandardDeviation(parentSegement);
            int numberOfPixelsSeg1 = segment1.FindNumberOfPixels();
            int numberOfPixelsSeg2 = segment2.FindNumberOfPixels();
            int numberOfPixelsParent = parentSegement.FindNumberOfPixels();
            float mergeCost = stddevParent * numberOfPixelsParent - stddevSeg1 * numberOfPixelsSeg1 - stddevSeg2 * numberOfPixelsSeg2;
            return mergeCost;
                

        }
    }
}
