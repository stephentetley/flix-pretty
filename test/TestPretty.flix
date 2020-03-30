/*
 * Copyright 2019 Stephen Tetley
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

namespace TestPretty {

/////////////////////////////////////////////////////////////////////////////
// empty                                                                   //
/////////////////////////////////////////////////////////////////////////////

@test
def empty01(): Bool = 
    Pretty.prettyPrint(1000, Pretty.empty()) == ""

/////////////////////////////////////////////////////////////////////////////
// text                                                                   //
/////////////////////////////////////////////////////////////////////////////

@test
def text01(): Bool =
    Pretty.prettyPrint(1000, Pretty.text("")) == ""

@test
def text02(): Bool =
    Pretty.prettyPrint(1000, Pretty.text("A")) == "A"

@test
def text03(): Bool =
    Pretty.prettyPrint(1000, Pretty.text("AB")) == "AB"

@test
def text04(): Bool =
    Pretty.prettyPrint(1000, Pretty.text("A B")) == "A B"

}