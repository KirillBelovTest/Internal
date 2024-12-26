(* :Package: *)

BeginPackage["KirillBelov`Internal`Alpha`"]; 


WolframAlphaXML::usage = 
"WolframAlphaXML[query] returns xml data from wolfram alpha API."; 


WolframAlphaLLM::usage = 
"WolframAlphaLLM[query] returns formatted answer optimazed for LLMs."; 


WolframAlphaMD::usage = 
"WolframAlphaMD[query] returns list of markdown blocks."; 


Begin["`Private`"]; 


WolframAlphaXML[query_String] := 
URLRead[Internal`HouseKeep[$xmlEndpoint, {"input" -> URLEncode[query]}]]["Body"]; 


WolframAlphaLLM[query_String] := 
URLRead[Internal`HouseKeep[$llmEndpoint, {"input" -> URLEncode[query]}]]["Body"]; 


$xmlEndpoint = "https://api.wolframalpha.com/v1/query.jsp"; 


$llmEndpoint = StringReplace[$xmlEndpoint, "query.jsp" -> "llm-api"]; 


End[]; 


EndPackage[]; 