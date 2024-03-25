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


mod TestPrettyPrint {


    /////////////////////////////////////////////////////////////////////////////
    // empty                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def empty01(): Bool = 
        use Text.PrettyPrint.{empty};
        ToString.toString(empty()) == ""

    /////////////////////////////////////////////////////////////////////////////
    // text                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def text01(): Bool =
        use Text.PrettyPrint.{text};
        ToString.toString(text("")) == ""

    @test
    def text02(): Bool =
        use Text.PrettyPrint.{text};
        ToString.toString(text("A")) == "A"

    @test
    def text03(): Bool =
        use Text.PrettyPrint.{text};
        ToString.toString(text("AB")) == "AB"

    @test
    def text04(): Bool =
        use Text.PrettyPrint.{text};
        ToString.toString(text("A B")) == "A B"

    /////////////////////////////////////////////////////////////////////////////
    // Punctuate                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    pub def punctuate01(): Bool =
        use Text.PrettyPrint.{punctuate, comma};
        let ds = punctuate(comma(), Nil);
        List.map(ToString.toString, ds) == Nil

    @test
    pub def punctuate02(): Bool =
        use Text.PrettyPrint.{punctuate, comma, text};
        let ds = punctuate(comma(), text("abc") :: Nil);
        List.map(ToString.toString, ds) == "abc" :: Nil

    @test
    pub def punctuate03(): Bool =
        use Text.PrettyPrint.{punctuate, comma, text};
        let ds = punctuate(comma(), text("abc") :: text("def") :: Nil);
        List.map(ToString.toString, ds) == "abc," :: "def" :: Nil

    @test
    pub def punctuate04(): Bool =
        use Text.PrettyPrint.{punctuate, comma, text};
        let ds = punctuate(comma(), text("abc") :: text("def") :: text("ghi") :: Nil);
        List.map(ToString.toString, ds) == "abc," :: "def," :: "ghi" :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // besideSoftLine                                                          //
    /////////////////////////////////////////////////////////////////////////////

    @test
    pub def besideSoftLine01(): Bool =
        use Text.PrettyPrint.{besideSoftLine, text};
        let doc1 = besideSoftLine(besideSoftLine(text("abc"), text(",")), text("def"));
        ToString.toString(doc1) == "abc , def"

    /////////////////////////////////////////////////////////////////////////////
    // besideSoftSpace                                                         //
    /////////////////////////////////////////////////////////////////////////////

    @test
    pub def besideSoftSpace01(): Bool =
        use Text.PrettyPrint.{besideSoftSpace, parens, text};
        let doc1 = parens(besideSoftSpace(text("hello"), text("world")));
        ToString.toString(doc1) == "(hello world)"

    @test
    pub def besideSoftSpace02(): Bool =
        use Text.PrettyPrint.{besideSoftSpace, parens, text, empty};
        let doc1 = parens(besideSoftSpace(empty(), empty()));
        ToString.toString(doc1) == "()"

    @test
    pub def besideSoftSpace03(): Bool =
        use Text.PrettyPrint.{besideSoftSpace, parens, text, empty};
        let doc1 = parens(besideSoftSpace(text("hello"), empty()));
        ToString.toString(doc1) == "(hello)"

    @test
    pub def besideSoftSpace04(): Bool =
        use Text.PrettyPrint.{besideSoftSpace, parens, text, empty};
        let doc1 = parens(besideSoftSpace(empty(), text("world")));
        ToString.toString(doc1) == "(world)"

    /////////////////////////////////////////////////////////////////////////////
    // symAmpersandAmpersand                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    pub def symAmpersandAmpersand01(): Bool =
        use Text.PrettyPrint.{<&&>, text};
        let doc1 = text("abc") <&&> text(",") <&&> text("def");
        String.lines(ToString.toString(doc1)) == "abc" :: "," :: "def" :: Nil
}