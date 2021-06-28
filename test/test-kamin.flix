use Text/PrettyPrint/Kamin.{pretty, text, line, tab};
use Text/PrettyPrint/Kamin.{<>, <<>>, <!>};

def main(_args: Array[String]): Int32 & Impure = 
    Console.printLine("Running...");
    test01();
    test02();
    test03();
    test04();
    test05();
    test06();
    0

def test01(): Unit & Impure = 
    let doc = text("hello");
    pretty(80, doc) |> println

def test02(): Unit & Impure = 
    let doc = text("hello") <<>> text("world");
    pretty(80, doc) |> println  

def test03(): Unit & Impure = 
    let doc = text("hello") <<>> text("world");
    pretty(8, doc) |> println  

def test04(): Unit & Impure = 
    let doc1 = text("hello") <<>> text("world");
    let doc2 = text("hello") <> line() <> text("world");
    let doc = doc1 <!> doc2;
    pretty(8, doc) |> println  

def test05(): Unit & Impure = 
    let doc1 = text("hello") <<>> text("world");
    let doc2 = text("hello") <> line() <> text("world");
    let doc = doc1 <!> doc2;
    pretty(16, doc) |> println    

def test06(): Unit & Impure = 
    let doc = text("hello") <<>> tab(text("world") <> line() <> text("!"));
    pretty(80, doc) |> println 