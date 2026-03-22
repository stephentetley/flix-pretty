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

    use Assert.{assertEq}
    use Fs.FileSystem
    use Fs.FileRead
    use Fs.FileWrite
    use Fs.ReadFile
    use Fs.WriteFile

    use FlixPretty.{<&&>, besideSoftLine, besideSoftSpace, parens, text, 
        empty, hsep, tupled, optionalTupled, removeEmpties, comma, punctuate}

    /////////////////////////////////////////////////////////////////////////////
    // empty                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def empty01(): Unit \ Assert =
        assertEq(expected = "", ToString.toString(empty()))

    /////////////////////////////////////////////////////////////////////////////
    // text                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def text01(): Unit \ Assert =
        assertEq(expected = "", ToString.toString(text("")))

    @Test
    def text02(): Unit \ Assert =
        assertEq(expected = "A", ToString.toString(text("A")))

    @Test
    def text03(): Unit \ Assert =
        assertEq(expected = "AB", ToString.toString(text("AB")))

    @Test
    def text04(): Unit \ Assert =
        assertEq(expected = "A B", ToString.toString(text("A B")))

    /////////////////////////////////////////////////////////////////////////////
    // Punctuate                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    pub def punctuate01(): Unit \ Assert =
        let ds = punctuate(comma(), Nil);
        assertEq(expected = Nil, List.map(ToString.toString, ds))

    @Test
    pub def punctuate02(): Unit \ Assert =
        let ds = punctuate(comma(), text("abc") :: Nil);
        assertEq(expected = "abc" :: Nil, List.map(ToString.toString, ds))

    @Test
    pub def punctuate03(): Unit \ Assert =
        use FlixPretty.{punctuate, comma, text};
        let ds = punctuate(comma(), text("abc") :: text("def") :: Nil);
        assertEq(expected = "abc," :: "def" :: Nil, List.map(ToString.toString, ds))

    @Test
    pub def punctuate04(): Unit \ Assert =
        let ds = punctuate(comma(), text("abc") :: text("def") :: text("ghi") :: Nil);
        assertEq(expected = "abc," :: "def," :: "ghi" :: Nil, List.map(ToString.toString, ds))

    /////////////////////////////////////////////////////////////////////////////
    // besideSoftLine                                                          //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    pub def besideSoftLine01(): Unit \ Assert =
        let doc1 = besideSoftLine(besideSoftLine(text("abc"), text(",")), text("def"));
        assertEq(expected = "abc , def", ToString.toString(doc1))

    /////////////////////////////////////////////////////////////////////////////
    // besideSoftSpace                                                         //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    pub def besideSoftSpace01(): Unit \ Assert =
        let doc1 = parens(besideSoftSpace(text("hello"), text("world")));
        assertEq(expected = "(hello world)", ToString.toString(doc1))

    @Test
    pub def besideSoftSpace02(): Unit \ Assert =
        let doc1 = parens(besideSoftSpace(empty(), empty()));
        assertEq(expected = "()", ToString.toString(doc1))

    @Test
    pub def besideSoftSpace03(): Unit \ Assert =
        let doc1 = parens(besideSoftSpace(text("hello"), empty()));
        assertEq(expected = "(hello)", ToString.toString(doc1))

    @Test
    pub def besideSoftSpace04(): Unit \ Assert =
        let doc1 = parens(besideSoftSpace(empty(), text("world")));
        assertEq(expected = "(world)", ToString.toString(doc1))

    /////////////////////////////////////////////////////////////////////////////
    // symAmpersandAmpersand                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    pub def symAmpersandAmpersand01(): Unit \ Assert =
        let doc1 = text("abc") <&&> text(",") <&&> text("def");
        assertEq(expected = "abc" :: "," :: "def" :: Nil, String.lines(ToString.toString(doc1)))

    /////////////////////////////////////////////////////////////////////////////
    // removeEmpties                                                           //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    pub def removeEmpties01(): Unit \ Assert =
        let doc1 = hsep(removeEmpties(List#{empty(), text("A"), empty()}));
        assertEq(expected = "A", ToString.toString(doc1))

    @Test
    pub def removeEmpties02(): Unit \ Assert =
        let doc1 = hsep(removeEmpties(List#{empty(), text("A"), empty(), text("B")}));
        assertEq(expected = "A B", ToString.toString(doc1))

    /////////////////////////////////////////////////////////////////////////////
    // tupled                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    pub def tupled01(): Unit \ Assert =
        let doc1 = tupled(Nil);
        assertEq(expected = "()", ToString.toString(doc1))

    @Test
    pub def tupled02(): Unit \ Assert =
        let doc1 = tupled(List#{empty()});
        assertEq(expected = "()", ToString.toString(doc1))

    @Test
    pub def tupled03(): Unit \ Assert =
        let doc1 = tupled(List#{text("A"), text("B")});
        assertEq(expected = "(A, B)", ToString.toString(doc1))

    @Test
    pub def tupled04(): Unit \ Assert =
        let doc1 = tupled(List#{text("A"), empty()});
        assertEq(expected = "(A,)", ToString.toString(doc1))

    /////////////////////////////////////////////////////////////////////////////
    // optionalTupled                                                          //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    pub def optionalTupled01(): Unit \ Assert =
        let doc1 = optionalTupled(Nil);
        assertEq(expected = "", ToString.toString(doc1))

    @Test
    pub def optionalTupled02(): Unit \ Assert =
        let doc1 = optionalTupled(List#{empty()});
        assertEq(expected = "", ToString.toString(doc1))

    @Test
    pub def optionalTupled03(): Unit \ Assert =
        let doc1 = optionalTupled(List#{text("A"), empty()});
        assertEq(expected = "(A)", ToString.toString(doc1))

    @Test
    pub def optionalTupled04(): Unit \ Assert =
        let doc1 = optionalTupled(List#{text("A"), empty(), text("B")});
        assertEq(expected = "(A, B)", ToString.toString(doc1))


    /////////////////////////////////////////////////////////////////////////////
    // writePretty                                                             //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    pub def writePretty01(): Unit \ { FileSystem, FileWrite, FileRead, Assert } =
        let doc1 = text("hello");
        let result = 
            run {
                let path = "output.txt";
                discard FileWrite.write({ str = "XXXXXXXXXXXXXX" }, path);
                discard FlixPretty.writePretty(doc1, 120, path);
                FileRead.read(path)
            } with FileSystem.withBaseDir("/tmp/flix-pretty-test")
              with WriteFile.runWithFileWrite
              with ReadFile.runWithFileRead
        ;
        assertEq(expected = Ok("hello"), result)

}
