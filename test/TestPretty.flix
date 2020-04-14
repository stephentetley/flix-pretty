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
    Pretty.toString(Pretty.empty()) == ""

/////////////////////////////////////////////////////////////////////////////
// text                                                                    //
/////////////////////////////////////////////////////////////////////////////

@test
def text01(): Bool =
    Pretty.toString(Pretty.text("")) == ""

@test
def text02(): Bool =
    Pretty.toString(Pretty.text("A")) == "A"

@test
def text03(): Bool =
    Pretty.toString(Pretty.text("AB")) == "AB"

@test
def text04(): Bool =
    Pretty.toString(Pretty.text("A B")) == "A B"

/////////////////////////////////////////////////////////////////////////////
// Punctuate                                                               //
/////////////////////////////////////////////////////////////////////////////

@test
pub def punctuate01(): Bool =
    let ds = Pretty.punctuate(Pretty.comma(), Nil);
    List.map(Pretty.toString, ds) == Nil

@test
pub def punctuate02(): Bool =
    let ds = Pretty.punctuate(Pretty.comma(), Pretty.text("abc") :: Nil);
    List.map(Pretty.toString, ds) == "abc" :: Nil

@test
pub def punctuate03(): Bool =
    let ds = Pretty.punctuate(Pretty.comma(), Pretty.text("abc") :: Pretty.text("def") :: Nil);
    List.map(Pretty.toString, ds) == "abc," :: "def" :: Nil

@test
pub def punctuate04(): Bool =
    let ds = Pretty.punctuate(Pretty.comma(), Pretty.text("abc") :: Pretty.text("def") :: Pretty.text("ghi") :: Nil);
    List.map(Pretty.toString, ds) == "abc," :: "def," :: "ghi" :: Nil


}