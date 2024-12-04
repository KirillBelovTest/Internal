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
Select[bytesPosition[data, Length[data], bytes, Length[bytes], n], #>0&]; 


BytesSplit[data_ByteArray, separator_ByteArray -> n_Integer?Positive] := 
Module[{positions, dataLen = Length[data], sepLen = Length[separator]}, 
	positions = BytesPosition[data, separator, n]; 
	If[Length[position] === n && 1 < positions[[-1]] < dataLen, 
		{data[[ ;; positions[[-1]]]], data[[positions[[-1]] + 1 ;; ]]}, 
	(*Else*)
		{data}
	]
]; 


ByteMask[mask_ByteArray, data_ByteArray] := 
byteMask[mask, Length[mask], data, Length[data]]; 


$directory = 
DirectoryName[$InputFileName, 2]; 


$binaryLibrary = 
LibraryResource[$directory, "binary"]; 


bytesPosition = 
LibraryFunctionLoad[$binaryLibrary, "bytesPosition", {{"ByteArray", "Shared"}, _Integer, {"ByteArray", "Shared"}, _Integer, _Integer}, {_Integer, 1}]; 


byteMask = 
LibraryFunctionLoad[$binaryLibrary, "byteMask", {{"ByteArray", "Shared"}, _Integer, {"ByteArray", "Shared"}, _Integer}, "ByteArray"]; 


End[]; 


EndPackage[]; 
