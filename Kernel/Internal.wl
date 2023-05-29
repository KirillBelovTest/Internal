(*:Package:*)

BeginPackage["KirillBelov`Internal`"]; 


ClearAll["`*"]; 


PreCompile::usage = 
"PreCompile[name, func] load if library exists and compliling if not"; 


ConditionApply::usage = 
"ConditionApply[pairs, default][args] select rule if key[args] is True and resturn value[args]"; 


Cache::usage = 
"Cache[expr] cache expr for one minute
Cache[expr, period] cache expr for specific period"; 


BytesPosition::usage = 
"BytesPosition[data, sep, n] n position of sep in data"; 


BytesSplit::usage = 
"BytesSplit[data, sep -> n] works like Map[StringJoin, TakeDrop[StringSplit[text, sep], n]]"; 


AssocMatchQ::usage = 
"AssocMatchQ[assoc, pattern] match assoc with pattern
AssocMatchQ[assoc, key, valuePattern] check key from assoc
AssocMatchQ[pattern] - function"; 


Begin["`Private`"];


ConditionApply[conditionAndFunctions_Association: <||>, defalut_: Function[Null], ___] := 
Function[Last[SelectFirst[conditionAndFunctions, Function[cf, First[cf][##]], {defalut}]][##]]; 


SetAttributes[Cache, HoldFirst]; 


Cache[expr_, period_Integer: 60] := 
Module[{roundNow = Floor[AbsoluteTime[], period]}, 
	If[IntegerQ[Cache[expr, "Date"]] && Cache[expr, "Date"] == roundNow, 
		Cache[expr, "Value"], 
	(*Else*)
		Cache[expr, "Date"] = roundNow; 
		Cache[expr, "Value"] = expr
	]
]; 


BytesPosition[data_ByteArray, bytes_ByteArray, n_Integer: 1] := 
bytesPosition[data, bytes, {n}]; 


BytesPosition[byteArray_ByteArray, subByteArray_ByteArray, n_List] := 
bytesPosition[byteArray, subByteArray, n]; 


BytesPosition[data_ByteArray, bytes_ByteArray, Span[n_Integer, All]] := 
bytesPosition[data, bytes, Range[n, Round[Length[data] / Length[bytes]]]]; 


BytesSplit[data_ByteArray, separator_ByteArray -> n_Integer?Positive] := 
Module[{position}, 
	position = BytesPosition[data, separator, n]; 
	If[Length[position] > 0, 
		{data[[ ;; position[[1, 1]] - 1]], data[[position[[1, 2]] + 1 ;; ]]}, 
	(*Else*)
		{data}
	]
]; 


AssocMatchQ[assoc_Association, pattern_Association] := 
Apply[And, KeyValueMap[AssocMatchQ[assoc, #1, #2]&, pattern]]; 


AssocMatchQ[pattern_Association][assoc_Association] := 
AssocMatchQ[assoc, pattern]; 


AssocMatchQ[request_Association, key__String, test: _String | _StringExpression] := 
StringMatchQ[request[key], test, IgnoreCase -> True]; 


AssocMatchQ[request_Association, key__String, test: _Association] := 
AssocMatchQ[request[key], test]; 


AssocMatchQ[request_Association, key: _String | {__String}, test: _Function | _Symbol | _[___]] := 
test[request[key]]; 


SetAttributes[PreCompile, HoldRest]; 


PreCompile[name_String, func_FunctionCompile] := 
Module[{lib = FileNameJoin[{$lLibraryResources, name <> "." <> Internal`DynamicLibraryExtension[]}]}, 
	If[
		FileExistsQ[lib], 
			LibraryFunctionLoad[lib], 
		(*Else*)
			If[!FileExistsQ[$lLibraryResources], CreateDirectory[$lLibraryResources]];
			LibraryFunctionLoad[FunctionCompileExportLibrary[lib, func]]
	]
]; 


(*Internal*)


$indent = ""; 


$lLibraryResources = FileNameJoin[{
	DirectoryName[$InputFileName, 2], 
	"LibraryResources", 
	$SystemID
}]; 


bytesPosition := bytesPosition = PreCompile["bytesPosition", 
	FunctionCompile[Function[{
		Typed[byteArray, "NumericArray"::["UnsignedInteger8", 1]], 
		Typed[subByteArray, "NumericArray"::["UnsignedInteger8", 1]], 
		Typed[n, "PackedArray"::["MachineInteger", 1]]
	}, 
		Module[{m = 0, j = 1, len = Length[subByteArray], positions = {}},
			Do[
				If[
					byteArray[[i ;; i + len - 1]] === subByteArray, 
						m++; 
						If[m === n[[j]], 
							j++; 
							positions = Append[positions, {i, i + len - 1}];  
							If[j > Length[n], Break[]]
						]
				], 
				{i, 1, Length[byteArray] - len + 1}
			]; 

			(*Return: {{_Integer, _Integer}.., }*)
			positions
		]
	]]
]; 


(*End private*)


End[]; 


(*End package*)


EndPackage[];
