#!/usr/bin/env wolframscript
(* ::Package:: *)

Get["CCompilerDriver`"]; 
Get["LibraryLink`"]; 


$libraryLinkVersion = 
LibraryVersionInformation[FindLibrary["demo"]]["WolframLibraryVersion"]; 


build[lib_String, opts: OptionsPattern[CreateLibrary]] := 
Block[{$directory, $libSrc, $libDir, $linkerOptions}, 
    $directory = DirectoryName[If[$InputFileName == "", 
        NotebookFileName[], 
        $InputFileName
    ], 2]; 

    $libSrc = File[FileNameJoin[{
        $directory, 
        "Source", 
        lib <> ".c"
    }]]; 

    $libDir = FileNameJoin[{
        $directory, 
        "LibraryResources", 
        $SystemID <> "-v" <> ToString[$libraryLinkVersion]
    }]; 


    If[!FileExistsQ[$libDir], CreateDirectory[]];

    CreateLibrary[$libSrc, lib, 
        "TargetDirectory" -> $libDir, 
        "Debug" -> False, 
        opts
    ]
]; 

build["internal"]; 


build["backgroundAsyncTask"]; 