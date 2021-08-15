use Text/PrettyPrint.{Doc};
use Text/PrettyPrint.{render, text, group, nest, line, break};
use Text/PrettyPrint.{<>, <!>, <<>>};

def main(_args: Array[String]): Int32 & Impure = 
    Console.printLine("Running...");
    render(32, doc1()) |> println;
    render(15, doc1()) |> println;
    render(10, doc1()) |> println;
    render(8, doc1()) |> println;
    render(7, doc1()) |> println;
    render(6, doc1()) |> println;
    0


def binop(left: String, op: String, right: String): Doc = 
    group(nest(2, (group(text(left) <!> text(op)) <!> text(right))))

def cond(): Doc = binop("a", "==", "b")
def expr1(): Doc = binop("a", "<<", "2")
def expr2(): Doc = binop("a", "+", "b")

def ifthen(c: Doc, e1: Doc, e2: Doc): Doc = 
    group(  group(nest(2, text("if")   <!> c)) 
        <!> group(nest(2, text("then") <!> e1))
        <!> group(nest(2, text("else") <!> e2)))

def doc1(): Doc = ifthen(cond(), expr1(), expr2())
