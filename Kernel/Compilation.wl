(*:Package:*)

BeginPackage["KirillBelov`Internal`Compilation`", {
    "CCompilerDriver`", 
    "LibraryLink`"
}]; 


ClearAll["`*"]; 


PreCompile::usage = 
"PreCompile[name, func] load if library exists and compliling if not."; 


LibraryResourcesDirectory::usage = 
"LibraryResourcesDirectory[path] returns directory with compiled binaries where path is paclet dirctory or $InputFileName."; 


LibraryResource::usage = 
"LibraryResource[path, name] returns full path to dynamic library according with operation system and LibraryLink version."; 


$LibraryLinkVersion::usage = 
"Current version of the LibraryLink package."; 


Begin["`Private`"]; 


$LibraryLinkVersion := $LibraryLinkVersion = 
getLibraryLinkVersion[]; 


SetAttributes[PreCompile, HoldRest]; 


PreCompile[{directory_String, name_String}, func_FunctionCompile] := 
Module[{libraryResources, lib}, 
    libraryResources = LibraryResourcesDirectory[directory]; 
    lib = FileNameJoin[{libraryResources, name <> "." <> Internal`DynamicLibraryExtension[]}]; 
    
    If[FileExistsQ[lib], 
        LibraryFunctionLoad[lib], 
    (*Else*)
        If[!FileExistsQ[libraryResources], CreateDirectory[libraryResources]]; 
        LibraryFunctionLoad[FunctionCompileExportLibrary[lib, func]]
    ]
]; 


LibraryResourcesDirectory[directory_String?DirectoryQ] := 
FileNameJoin[{
    directory, 
    "LibraryResources", 
    $SystemID <> "-v" <> ToString[$LibraryLinkVersion]
}]; 


LibraryResourcesDirectory[file_String?(!DirectoryQ[#] && FileExistsQ[#] && standardPackageFileQ[#]&)] := 
FileNameJoin[{
    ParentDirectory[DirectoryName[file]], 
    "LibraryResources", 
    $SystemID <> "-v" <> ToString[$LibraryLinkVersion]
}]; 


LibraryResource[path_, name_String] := 
FileNameJoin[{LibraryResourcesDirectory[path], name <> "." <> Internal`DynamicLibraryExtension[]}]


$directory = 
DirectoryName[$InputFileName, 2]; 


getLibraryLinkVersion[] := 
Which[
    $VersionNumber >= 14.1, 
        LibraryVersionInformation[FindLibrary["demo"]]["WolframLibraryVersion"], 
    $VersionNumber >= 13.1, 
        7, 
    $VersionNumber >= 12.1, 
        6, 
    $VersionNumber >= 12.0, 
        5, 
    $VersionNumber >= 11.2, 
        4, 
    $VersionNumber >= 10.0, 
        3, 
    $VersionNumber >= 9.0, 
        2, 
    True, 
        1
]; 


standardPackageFileQ[file_] := 
MatchQ[Map[ToLowerCase, FileNameSplit[file]], {__, "kernel" | "scripts" | "tests", _}] && 
FileExistsQ[FileNameJoin[Flatten[FileNameSplit[file][[ ;; -3]], "PacletInfo.wl"]]]; 


End[]; 


EndPackage[]; 