
use Text/PrettyPrint.{Doc};
use Text/PrettyPrint.{brackets, text, hang, prettyPrint, vlinebreak, vcat};

def node(name: String, kids: Array[Doc]): Doc & Impure = 
    match Array.toList(kids) {
        case Nil => brackets(text(name))
        case ks => hang(4, brackets(vlinebreak(text(name), vcat(ks))))
    }

def tree1(): Doc & Impure = 
    node("factx-fsharp",
        [ node("src",
            [ node("bin", [])
            , node("FactX",
                [ node("Common.fs", [])
                , node("FactOutput.fs", [])
                , node("FactWriter.fs", [])
                , node("Pretty.fs", [])
                , node("Syntax.fs", [])
                ])
            ])
        , node("test",
            [ node("FactXTest.fsproj", [])
            , node("Test01.fsx", [])
            ])
        ])

@test
def test01(): String & Impure = 
    prettyPrint(99, tree1())
