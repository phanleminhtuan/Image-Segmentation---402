using System;
using System.Collections.Generic;
using System.Text;

namespace CSharpSegmenter {
    /*/Coordinate class for easier parameter /*/
    public class Coordinate {
        public int xCoord { get; private set; }
        public int yCoord { get; private set; }

        public Coordinate(int xCoord, int yCoord) {
            this.xCoord = xCoord;
            this.yCoord = yCoord;

                
        }
        public KeyValuePair<int,int> RetrieveFullCoord() {
            return new KeyValuePair<int, int>(xCoord,yCoord);
        }
    }
}
