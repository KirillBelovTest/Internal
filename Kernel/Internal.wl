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


ByteMask::usage = 
"ByteMask[mask, numericArray] returns masked numeric array."; 


AssocMatchQ::usage = 
"AssocMatchQ[assoc, pattern] match assoc with pattern
AssocMatchQ[assoc, key, valuePattern] check key from assoc
AssocMatchQ[pattern] - function"; 


AssocBlock::usage = 
"AssocBlock[assoc, vars, expr] assoc converts to local variables for block."; 


AssocFunction


CreateBackgroundTask::usage = 
"CreateBackgroundTask[expr, {timeSpec, runCount}] run expr in background like in ScheduledTask."; 


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


AssocMatchQ[assoc_Association, keys__, stringPattern_?StringPattern`StringPatternQ] := 
StringMatchQ[assoc[[keys]], stringPattern, IgnoreCase -> True]; 


AssocMatchQ[assoc_Association, keys__, func_Function] := 
func[assoc[[key]]]; 


AssocMatchQ[assoc_Association, keys__, assocPattern_Association?AssociationQ] := 
AssocMatchQ[assoc[[keys]], assocPattern]; 


AssocMatchQ[assoc_Association, keys__, pattern_] := 
MatchQ[assoc[[keys]], pattern]; 


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
	Echo["\033[1;44mPre compilation was initiated. Please wait...\033[1;0m"];
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


SetAttributes[CreateBackgroundTask, HoldFirst]; 


toEvent[task_, event_, {n_, runCount_, id_}] := 
<|
	"task" :> task, 
	"n" :> n, 
	"runCount" :> runCount, 
	"id" :> id
|>; 


CreateBackgroundTask[expr_, {interval_?NumericQ, count_Integer}] := 
With[{id = $id++}, 
	$backgroundTasks[id] := expr; 
	$backgroundTaskCounts[id] = count; 

	Internal`CreateAsynchronousTask[
		startBackgroundTask, 
		{id, Round[interval * 1000], count}, 
		runBackgroundTask[toEvent[##]]&
	]
]; 


If[!IntegerQ[$id], $id = 1]; 


runBackgroundTask = Function[
	If[#runCount < #n, 
		stopBackgroundTask[#task[[2]]], 

	(*Else*)
		$backgroundTasks[#id]
	]
]; 


If[!AssociationQ[$backgroundTasks], $backgroundTasks = <||>]; 


If[!AssociationQ[$backgroundTasks], $backgroundTaskCounts = <||>]; 


$backgroundTaskLibrary = 
FileNameJoin[{getLibraryResourcesDirectory[$directory] <> "-v7", "backgroundTask." <> Internal`DynamicLibraryExtension[]}]; 


startBackgroundTask = LibraryFunctionLoad[$backgroundTaskLibrary, "startBackgroundTask", {Integer, Integer, Integer}, Integer]; 


stopBackgroundTask = LibraryFunctionLoad[$backgroundTaskLibrary, "stopBackgroundTask", {Integer}, Integer]; 


End[]; 


EndPackage[]; 
