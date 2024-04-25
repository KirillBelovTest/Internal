(*:Package:*)

BeginPackage["KirillBelov`Internal`", {
	"CCompilerDriver`"
}]; 


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


AssocBlock::usage = 
"AssocBlock[assoc, vars, expr] assoc converts to local variables for block."; 


AssocFunction


WolframAlphaTextPod


$LibraryLinkVersion


Begin["`Private`"]; 


$LibraryLinkVersion := $LibraryLinkVersion = 
getLibraryLinkVersion[]; 


AssocFunction[func_Symbol][assoc_Association?AssociationQ] := 
Module[{params}, 
	Evaluate[params = 
	Map[Function[ReleaseHold[# //. {
		Verbatim[HoldPattern][func[$a___]] :> Hold[{$a}], 
		Verbatim[PatternTest][Verbatim[Pattern][$n_, $h_], $t_] :> <|
			"Name" -> ToString[$n], 
			"Head" -> $h, 
			"Test" -> $t
        |>, 
		Verbatim[Pattern][$n_, $h_] :> <|
			"Name" -> ToString[$n], 
			"Head" -> $h
		|>
	}]]] @ DownValues[func][[All, 1]];

	With[{targetParams = 
    	Query[All, {"Name" -> Slot}] @ 
		SelectFirst[params,
			Map[ToLowerCase, Sort[#[[All, "Name"]]]] == 
			Map[ToLowerCase, Sort[Keys[assoc]]] &
		]
	},
	
		With[{target = Table[
			With[{$h = param["Head"], $n = param["Name"], $t = param["Test"]}, 
				Which[
					$t === VectorQ, Hold[Map[ToExpression] @ StringSplit[$n, ","]], 
					$h === _Real, Hold[N[ToExpression[$n]]], 
					$h === _Integer, Hold[Round[ToExpression[$n]]], 
					$h === _List, Hold[StringSplit[$n, ","]],
					$t === NumberQ, Hold[ToExpression[$n]], 
					True, Hold[$n]
				]
			], 
			{param, targetParams}
		]}, 

			With[{$func = func @@ target & //. {Hold[$a_] :> $a}}, 
				$func[assoc]
			]
		]
	]]
]; 


JSONFunction[func_Symbol][jsonText_String] := 
JSONFunction[func][ImportString[jsonText, "RawJSON"]]; 


SetAttributes[AssocBlock, HoldRest]; 


AssocBlock[assoc_Association, vars___Set, expr_] := 
Module[{x1, x2}, 
	x1 = Normal @ assoc /. Rule[k_String, v_] :> Rule[ToExpression[k, StandardForm, Hold], v]; 
	x2 = Apply[List, Hold[vars] /. Verbatim[Set][a_, b_] :> (Hold[a] -> b)]; 
	With[{x3 = Join[x1, x2]}, 
		With[{x4 = Hold[x3, expr] /. Rule[Hold[s_], v_] :>  Hold[s, v]}, 
			Apply[Block , x4  /. Hold[s_Symbol, v_] :> Set[s, v]]
		]
	]
]; 


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


PreCompile[{directory_String, name_String}, func_FunctionCompile] := 
Module[{libraryResources, lib}, 
	libraryResources = getLibraryResourcesDirectory[directory]; 
	lib = FileNameJoin[{libraryResources, name <> "." <> Internal`DynamicLibraryExtension[]}]; 
	If[
		FileExistsQ[lib], 
			LibraryFunctionLoad[lib], 
		(*Else*)
		With[{},
			If[!FileExistsQ[libraryResources], CreateDirectory[libraryResources]]; 
			LibraryFunctionLoad[FunctionCompileExportLibrary[lib, func]]
		]
	]
]; 


PreCompile[{directory_String, name_String}, path_] := 
Module[{libraryResources, lib}, 
	libraryResources = getLibraryResourcesDirectory[directory]; 
	lib = FileNameJoin[{libraryResources, name <> "." <> Internal`DynamicLibraryExtension[]}]; 
	If[
		FileExistsQ[lib], 
			LibraryFunctionLoad[lib], 
		(*Else*)
		If[!(path // ListQ),
			With[{func = Import[path]},
				If[!FileExistsQ[libraryResources], CreateDirectory[libraryResources]]; 
				LibraryFunctionLoad[FunctionCompileExportLibrary[lib, func]]
			]
			(*Else*)
		,
			With[{func = Import[Which@@Flatten[(path/.Rule->List)]]},
				If[!FileExistsQ[libraryResources], CreateDirectory[libraryResources]]; 
				LibraryFunctionLoad[FunctionCompileExportLibrary[lib, func]]
			]		
		]
	]
]; 


(*Internal*)


$directory = 
DirectoryName[$InputFileName, 2]; 


getLibraryLinkVersion[] := 
Module[{source, lib, version, getLibraryLinkVerionFunc}, 
	source = "#include \"WolframLibrary.h\"

DLLEXPORT mint WolframLibrary_getVersion() {
	return WolframLibraryVersion;
}

DLLEXPORT int WolframLibrary_initialize(WolframLibraryData libData) {
	return 0;
}

DLLEXPORT int getLibraryLinkVersion(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res){
	MArgument_setInteger(Res, WolframLibrary_getVersion()); 
	return LIBRARY_NO_ERROR; 
}"; 
	lib = CreateLibrary[source, "getLibraryLinkVersion", 
		"TargetDirectory" -> Directory[], 
		"Debug" -> False
	]; 

	getLibraryLinkVerionFunc = LibraryFunctionLoad[lib, "getLibraryLinkVersion", {}, Integer]; 
	version = getLibraryLinkVerionFunc[]; 

	LibraryFunctionUnload[getLibraryLinkVerionFunc]; 

	DeleteFile[lib]; 

	version
]; 


getLibraryResourcesDirectory[directory_String] := 
FileNameJoin[{
	directory, 
	"LibraryResources", 
	$SystemID
}]; 


versionQ[n_] := $VersionNumber >= n; 


testBytePositions[func_] := 
If[func[ByteArray[{0,2,1,4}], ByteArray[{1}], {1}] === {{3,3}}, True, False]; 


bytesPosition := bytesPosition = 
If[versionQ[13.2], 
	With[{compiled = PreCompile[{$directory, "bytesPosition"}, {
		versionQ[13.2] -> FileNameJoin[{$directory, "Kernel", "bytesPosition.wl"}],
		True -> FileNameJoin[{$directory, "Kernel", "bytesPosition-legacy.wl"}]
		}]
	},
		If[TrueQ[testBytePositions[compiled]],
			compiled,
			Get[FileNameJoin[{$directory, "Kernel", "bytesPosition-uncompiled.wl"}]]
		]
	],
(*Else*)
	Get[FileNameJoin[{$directory, "Kernel", "bytesPosition-uncompiled.wl"}]]
]; 


End[]; 


EndPackage[];

(*
	КОТ=16
	РОТ=18
	АУ=14

	2АУ+КОТ+РОТ=28+16+18
	2АУ+К+Р+2ОТ=48
	
*)