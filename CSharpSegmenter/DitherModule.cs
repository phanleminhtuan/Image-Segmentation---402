using System;
using System.Collections.Generic;
using System.Text;
using System.Collections;
using System.Linq;
using System.Drawing;

namespace CSharpSegmenter {
    public class DitherModule {
        // check if the bit as position pos of number v is set
        static bool IsBitSet(int v, int pos) {
            return (v & (1 << pos)) != 0;
        }

        // reverse the bits in the representation of number v, up to the given bit position
        static int ReverseBits(int v, int length) {
            var r = 0;
            for (var i = 0; i < length; i++)
                if (IsBitSet(v, i))
                    r |= (1 << (length - i - 1));
            return r;
        }

        static int evenBits(int x) {
            int r = 0;
            for (int i = 0; i < sizeof(int) * 4; i++)
                if (IsBitSet(x, i * 2))
                    r |= (1 << i);
            return r;
        }

        static int OddBits(int x) {
            int r = 0;
            for (int i = 0; i < sizeof(int) * 4; i++)
                if (IsBitSet(x, 1 + i * 2))
                    r |= (1 << i);
            return r;
        }
        // returns the coordinates of all pixels in the image in special dither order
        static public List<Coordinate> Coordinates(int N) {
            List<Coordinate> ditherOrder = new List<Coordinate>();
            var width = 1 << N;
            var height = 1 << N;

            for (var i = 0; i < width * height; i++) {
                var r = ReverseBits(i, 2 * N);
                var xCoord = OddBits(r);
                var yCoord = xCoord ^ evenBits(r);
                Coordinate currentCoordinates = new Coordinate(xCoord, yCoord);
                ditherOrder.Add(currentCoordinates);
            }
            return ditherOrder;
        }



    }
}
