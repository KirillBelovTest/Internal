(* :Package: *)

BeginPackage["KirillBelov`Internal`Console`"]; 


ConsolePrint::usage = 
"ConsolePrint[text, style] prints styled text to console."; 


ConsolePrintMessage::usage = 
"ConsolePrintError[message, args] print mesage for the symbol."; 


Begin["`Private`"]; 


ConsolePrint[text_String, style_String] := 
WriteString[$Output, style <> text <> DEFAULT]; 


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


End[(*`Private`*)]; 


EndPackage[(*KirillBelov`Internal`Console`*)]; 