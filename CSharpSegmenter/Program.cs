using System;
using System.Collections;
using System.Collections.Generic;
using System.Drawing;

namespace CSharpSegmenter {
    class Program {
        static void Main(string[] args) {
            // the size for the image
            int size = 6;
            TiffImage image =  new TiffImage("D:/SegmentationSkeleton/TestImages/L15-3662E-1902N-Q4.tif");
            //passing the image, the size (2*size x 2*size image ) and the threshhol into the segmentation module
            SegmentationModule module1 = new SegmentationModule(image, size, 800);
            //grow the Segmentation in the module from an emty dictionary            
            module1.GrowUntilNoChange();
            // determine the segmentation for the (top left corner of the) image (2^N x 2^N) pixels
            Segmentation segmentation = new Segmentation(module1.Segment);

            // draw the (top left corner of the) original image but with the segment boundaries overlayed in blue
            image.overlaySegmentation("C#output.tif", size, segmentation);
            // Notify the Console when the output is finished.
            Console.WriteLine("finish");

            Console.ReadKey();


        }

        // Fixme: add implementation here
    }
}

