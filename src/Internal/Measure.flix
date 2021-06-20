/*
 * Copyright 2021 Stephen Tetley
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */


namespace Text/PPrint/Internal/Measure {

    type alias Measure = 
        { height: Int32
        , lastWidth: Int32
        , maxWidth: Int32
        }
    
    pub def hcat(x: Measure, y: Measure): Measure = 
        { height = x.height + y.height
        , lastWidth = x.lastWidth + y.lastWidth
        , maxWidth = Int32.max(x.maxWidth, x.lastWidth + y.maxWidth)
        }

    pub def text(s: String): Measure = 
        { height = 0
        , lastWidth = String.length(s)
        , maxWidth = String.length(s)
        }

    pub def flush(x: Measure): Measure = 
        { height = x.height + 1
        , lastWidth = 0
        , maxWidth = x.maxWidth
        }

    pub def vcat(x: Measure, y: Measure): Measure = 
        flush(x) `hcat` y


    /// Order of compare - height, lastWidth, maxWidth
    pub def compare(x: Measure, y: Measure): Comparison = 
        match Order.compare(x.height, y.height) { 
            case EqualTo => match Order.compare(x.lastWidth, y.lastWidth) {
                case EqualTo => Order.compare(x.maxWidth, y.maxWidth)
                case z2 => z2
            }
            case z1 => z1
        }


    pub def precedes(x: Measure, y: Measure): Bool = 
        x.height <= y.height and x.maxWidth <= y.maxWidth and x.lastWidth <= y.lastWidth



    pub def valid(pageWidth: Int32, x: Measure): Bool = 
        x.maxWidth <= pageWidth
}

