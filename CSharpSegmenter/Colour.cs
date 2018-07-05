using System;
using System.Collections.Generic;
using System.Text;

namespace CSharpSegmenter{
    /*/ THe Colours class for easier parameter /*/
    public class Colour{
        public List<Byte> colours { get; private set; }
        public Colour (List<byte> colours) {
            this.colours = colours;
        }

    }
}
