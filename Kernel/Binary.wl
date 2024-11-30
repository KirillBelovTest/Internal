(*:Package:*)

BeginPackage["KirillBelov`Internal`Binary`", {
	"KirillBelov`Internal`Compilation`"
}]; 


ClearAll["`*"]; 


BytesPosition::usage = 
"BytesPosition[data, sep, n] n position of sep in data"; 


BytesSplit::usage = 
"BytesSplit[data, sep -> n] works like Map[StringJoin, TakeDrop[StringSplit[text, sep], n]]"; 


ByteMask::usage = 
"ByteMask[mask, numericArray] returns masked numeric array."; 


Begin["`Private`"]; 


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


$directory = 
DirectoryName[$InputFileName, 2]; 


versionQ[n_] := 
$VersionNumber >= n; 


bytesPosition := bytesPosition = 
If[versionQ[$VersionNumber > 13.2], 
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
