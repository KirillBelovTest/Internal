#!/usr/bin/env wolframscript
(* ::Package:: *)

Get["CCompilerDriver`"]; 
Get["LibraryLink`"]; 


$libraryLinkVersion = 
LibraryVersionInformation[FindLibrary["demo"]]["WolframLibraryVersion"]; 


Block[{$directory, $libSrc, $libDir, $linkerOptions}, 
	$directory = DirectoryName[If[$InputFileName == "", 
		NotebookFileName[], 
		$InputFileName
	], 2]; 

	$libSrc = File[FileNameJoin[{
		$directory, 
		"Source", 
		"internal.c"
	}]]; 

	$libDir = FileNameJoin[{
		$directory, 
		"LibraryResources", 
		$SystemID <> "-v" <> ToString[$libraryLinkVersion]
	}]; 


	If[!FileExistsQ[$libDir], CreateDirectory[]];

	CreateLibrary[$libSrc, "internal", 
		"TargetDirectory" -> $libDir, 
		"Debug" -> False
	]
]; 
