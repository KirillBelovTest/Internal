(* ::Package:: *)

BeginPackage["KirillBelov`Internal`Console`"]; 


ConsolePrint::usage = 
"ConsolePrint[text, style] prints styled text to console."; 


ConsoleEcho::usage = 
"ConsoleEcho[message, name] prints mesage with specific name.";


ConsolePrintMessage::usage = 
"ConsolePrintError[message, args] print mesage for the symbol."; 


Begin["`Private`"]; 


ConsolePrint[text_String, style_String] := 
WriteString[$Output, style <> text <> DEFAULT]; 


ConsolePrint[text_String] := 
WriteString[$Output, text]; 


ConsolePrint[expr_, rest___] := 
ConsolePrint[ImportString[ToString[expr], "Text"], rest]; 


ConsoleEcho[message_, name_String] := (
    ConsolePrint["\:276f " <> DateString[] <> " \:276f " <> ToUpperCase[name] <> " \:276f ", YELLOWDARK]; 
    ConsolePrint["\n"]; 
    ConsolePrint[message]; 
    ConsolePrint["\n"]; 
    ConsolePrint["\n"]; 
    message
); 


ConsoleEcho[message_Association?AssociationQ, name_String] := 
(ConsoleEcho[AssocToConsoleString[message], name]; message); 


ConsoleEcho[name_String][msg_] := 
ConsoleEcho[msg, name]; 


SetAttributes[ConsolePrintMessage, HoldFirst]; 


ConsolePrintMessage[MessageName[symbol_Symbol, messageName_String], args___] := 
Module[{symbolName = SymbolName[Unevaluated[symbol]]}, 
    ConsolePrint[symbolName, BOLD <> RED]; 
    ConsolePrint["::", BOLD]; 
    ConsolePrint[messageName, BOLD <> RED]; 
    ConsolePrint[" ", DEFAULT]; 
    ConsolePrint[StringTemplate[MessageName[symbol, messageName]][args], RED]; 
    ConsolePrint["\n", DEFAULT]; 
]; 



DEFAULT = "\033[0m";
BOLD = "\033[1m";
ITALIC = "\033[3m";
UNDERLINE = "\033[4m";
UNDERLINETHICK = "\033[21m";
HIGHLIGHTED = "\033[7m";
HIGHLIGHTEDBLACK = "\033[40m";
HIGHLIGHTEDRED = "\033[41m";
HIGHLIGHTEDGREEN = "\033[42m";
HIGHLIGHTEDYELLOW = "\033[43m";
HIGHLIGHTEDBLUE = "\033[44m";
HIGHLIGHTEDPURPLE = "\033[45m";
HIGHLIGHTEDCYAN = "\033[46m";
HIGHLIGHTEDGREY = "\033[47m";

HIGHLIGHTEDGREYLIGHT = "\033[100m";
HIGHLIGHTEDREDLIGHT = "\033[101m";
HIGHLIGHTEDGREENLIGHT = "\033[102m";
HIGHLIGHTEDYELLOWLIGHT = "\033[103m";
HIGHLIGHTEDBLUELIGHT = "\033[104m";
HIGHLIGHTEDPURPLELIGHT = "\033[105m";
HIGHLIGHTEDCYANLIGHT = "\033[106m";
HIGHLIGHTEDWHITELIGHT = "\033[107m";

STRIKETHROUGH = "\033[9m";
MARGIN1 = "\033[51m";
MARGIN2 = "\033[52m";

BLACK = "\033[30m";
REDDARK = "\033[31m";
GREENDARK = "\033[32m";
YELLOWDARK = "\033[33m";
BLUEDARK = "\033[34m";
PURPLEDARK = "\033[35m";
CYANDARK = "\033[36m";
GREYDARK = "\033[37m";

BLACKLIGHT = "\033[90m";
RED = "\033[91m";
GREEN = "\033[92m";
YELLOW = "\033[93m";
BLUE = "\033[94m";
PURPLE = "\033[95m";
CYAN = "\033[96m";
WHITE = "\033[97m"; 


$PrintAssocIndent = ""; 
$PrintAssocBracketColors = {BLUEDARK, PURPLEDARK, CYANDARK, GREENDARK, GREEN, CYAN, BLUE, RED, PURPLE, REDDARK}; 
$PrintAssocBracketColorIndex = 1; 


AssocToConsoleString[assoc_Association] := 
Module[{$result = ""}, 
    $result = 
        $result <> 
        $PrintAssocIndent <> 
        $PrintAssocBracketColors[[$PrintAssocBracketColorIndex]] <> 
        "<|" <> 
        DEFAULT <> 
        If[Length[assoc] > 0, "\n", ""];
    
    If[Length[assoc] > 0, 

        $result = $result <> Block[{
            $PrintAssocIndent = $PrintAssocIndent <> "  ", 
            $PrintAssocBracketColorIndex = $PrintAssocBracketColorIndex + 1
        }, 
            $PrintAssocIndent <> StringRiffle[KeyValueMap[StringTrim[PairToConsoleString[#1, #2]]&, assoc], ",\n" <> $PrintAssocIndent] <> "\n"
        ];
    ]; 
    
    $result = $result <>  
        If[Length[assoc] > 0, $PrintAssocIndent, ""] <> 
        $PrintAssocBracketColors[[$PrintAssocBracketColorIndex]] <> 
        "|>" <> 
        DEFAULT <> 
        "\n";
        
    Return[$result]
];


PairToConsoleString[key_, value_] := 
$PrintAssocIndent <> ToKey[key] <> " -> " <> ToValue[value];


ToKey[s_String] := CYANDARK <> "\"" <>  s <> "\"" <> DEFAULT;


ToKey[n_?NumericQ] := BLUEDARK <> ToString[n] <> DEFAULT;


ToValue[n_?NumericQ] := BLUEDARK <> ToString[n] <> DEFAULT;


ToValue[s_String] := "\"" <>  s <> "\"";


ToValue[b_?BooleanQ] := If[b, GREENDARK, REDDARK] <> ToString[b] <> DEFAULT;


ToValue[a_?AssociationQ] := StringTrim[AssocToConsoleString[a]];


ToValue[a_?ListQ] := 
Module[{}, 
    If[Length[a] > 0, 
        $PrintAssocBracketColors[[$PrintAssocBracketColorIndex]] <> 
        "{\n" <> 
        DEFAULT <> 
        Block[{$PrintAssocIndent = $PrintAssocIndent <> "  ", $PrintAssocBracketColorIndex = $PrintAssocBracketColorIndex + 1}, 
            $PrintAssocIndent <> StringRiffle[Map[ToValue, a], ",\n" <> $PrintAssocIndent] <> "\n"
        ] <> 
        $PrintAssocIndent <> 
        $PrintAssocBracketColors[[$PrintAssocBracketColorIndex]] <> 
        "}" <> 
        DEFAULT, 
        $PrintAssocBracketColors[[$PrintAssocBracketColorIndex]] <> "{}" <> DEFAULT
    ]
];


ToValue[date_?DateObjectQ] := 
PURPLE <> DateString[date] <> DEFAULT; 


ToValue[expr_] := 
ToString[expr];


End[(*`Private`*)]; 


EndPackage[(*KirillBelov`Internal`Console`*)]; 
