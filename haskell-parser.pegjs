{
  var lines = [];

  function concatFoldl (acc, value) { return acc + value };
  function getCachedContext() {
     if (lines.length >= 1) {
       return lines
     } else {
       lines = text().split('\n')
       return lines
     }
  }
}

top = Head body:Body { return body; }

Word "word" = value:([a-zA-Z_0-9+*/^<>=&|#-]+)
              { return value.reduce(concatFoldl, "") }

Symbol "symbol" = [\"!\\#$%&'()*+,-./:;<=>?@\[\]^_`{|}]

Number "Number" = value:[0-9]+
                  { return value.reduce(concatFoldl, "") }

_ "whitespace" = [ \t]+ { return "" }

nl "linebreak" = [\r\n]+ { return "\n" }

BlockCommentStart = "{-" { return "" }
BlockCommentEnd = "-}" { return "" }
LPragma = "{-#" _ "LANGUAGE" _ { return "" }
RPragma = "#-}" { return "" }
DCol = "::" { return "" }

BlockComment = BlockCommentStart text:(Word / _ / nl)* BlockCommentEnd
               { return "" }

PragmaDefinition = LPragma _ pragma:Word "#-}" { return ""  }

HaddockTag = "@package" _ name:(Word/Symbol)+ nl { return "" }
           / "@version" _ version:[0-9.]+ nl { return "" }

Comment  = "--" _* text:(Word / Symbol / [ \t])* nl
           { return text.reduce(concatFoldl, "") + "\n" }

SimpleCommentBlock = Comment+ { return "" }

DocBlock = "-- |" head:(Word / Symbol / " "+)* nl tail:Comment*
           {
               return ( head.reduce(concatFoldl, "")
                      + "\n"
                      + tail.reduce(concatFoldl, "")).trim() + "\n"
           }

Module = "module" _ name:Word _? nl { return "" }

Head = (PragmaDefinition nl)*
       (SimpleCommentBlock / BlockComment / nl / HaddockTag)* Module
       { getCachedContext()
         return "" }

Id "identifier" = value:Word
                  { return { value: value
                           , location: location()
                           }
                  }
// data IO a :: * -> *
DataKW = doc:(DocBlock)? "data" _ id:Id _? args:(Word " ")*
         tail:(DCol Type)? nl
         {
             args = args.map(
                 (arg) => {
                     return arg.reduce(concatFoldl, "")
                 }
             ).reduce(concatFoldl, "");
             if (!tail) {
                 tail = "";
             }
             return { type: "data"
                    , value: id.value
                    , args: args
                    , tail: tail
                    , location: id.location
                    , doc: doc ? doc : ""
                    }
         }

Constraint = Word _ "=> "  {return ""}

// HasCallStack => Number -> Number -> Number
Type = Constraint? val:[a-zA-Z ->\]\[=]+
       {
           return val.reduce(concatFoldl, "")
       }

// type Number = Int
TypeKW = doc:(DocBlock)? "type" _ id:Id _? "=" _? tail:Type nl
         { return { type: "type"
                  , value: id.value
                  , location: id.location
                  , tail: tail
                  , doc: doc ? doc : ""
                  }
         }

// (+) :: Number -> Number -> Number
InfDecl  = doc:(DocBlock)? "(" id:Id ")" _ DCol  _? tail:Type
           { return { type: "funcDecl"
                    , value: id.value
                    , declaration: "(" + id.value + ")" + " :: " + tail
                    , tail: tail
                    , location: id.location
                    , doc: doc ? doc : ""
                    }
           }

// max :: (Number, Number) -> Number
PrefDecl = doc:(DocBlock)? patt:("pattern ")? id:Id args:(_ Word)* _
           DCol _ tail:Type
           {
               args = args ? args.reduce(concatFoldl, "") : "";
               let location = id.location;
               if (patt) {
                   location.end.column -= patt.length;
                   location.start.column -= patt.length;
               }
               return { type: "funcDecl"
                      , value: id.value
                      , declaration: id.value + args + " :: " + tail
                      , tail: tail
                      , location: location
                      , doc: doc ? doc : ""}
           }

FDeclaration = decl:InfDecl nl { return decl }
             / decl:PrefDecl nl { return decl; }

InfixOp = ("infixl"/ "infixr" / "infix") _ Number _ (Word / Symbol+) nl
          { return "" }

Body = entries:( DataKW
               / TypeKW
               / FDeclaration
               / InfixOp
               / BlockComment
               / SimpleCommentBlock)+
       {
           let result = {};

           entries.forEach((entry) => {
               if (entry === "") {
                   return
               } else if (entry.type === "data") {
                   let tail = entry.tail ? " " + entry.args + " :: " +
                              entry.tail : "",
                       name = entry.value,
                       symbolEnd = entry.location.end.column - 1,
                       symbolStart = entry.location.start.column - 1;
                   if (result[entry.value]) {
                       return
                   }

                   result[name] = { declaration: ("data " + name +
                                                  " " + tail).trim()
                                  , doc: entry.doc
                                  , symbolEnd: symbolEnd
                                  , symbolStart: symbolStart
                                  }
               } else if (entry.type === "type") {
                   if (result[entry.value]) {
                       return
                   }
                   let symbolEnd = entry.location.end.column - 1,
                       symbolStart = entry.location.start.column - 1;


                   result[entry.value] = { declaration: "type " +
                                                        entry.value +
                                                        " = " + entry.tail
                                         , doc: entry.doc
                                         , symbolEnd: symbolEnd
                                         , symbolStart: symbolStart
                                         }
               } else if (entry.type === "funcDecl") {
                   if (result[entry.value]) {
                       return
                   }
                   let declaration = entry.declaration.replace(
                           /(\s)!([A-Za-z([])/g, '$1$2'),
                       symbolEnd = entry.location.end.column - 1,
                       symbolStart = entry.location.start.column - 1;
                   result[entry.value] = { declaration: declaration
                                         , doc: entry.doc
                                         , symbolEnd: symbolEnd
                                         , symbolStart: symbolStart
                                         }
               }
           })
           return result
       }
