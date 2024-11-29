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


$directory = 
DirectoryName[$InputFileName, 2]; 


getLibraryLinkVersion[] := 
Module[{source, lib, version, getLibraryLinkVerionFunc}, 
    source = "\
#include \"WolframLibrary.h\"

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


standardPackageFileQ[file_] := 
MatchQ[Map[ToLowerCase, FileNameSplit[file]], {__, "kernel" | "scripts" | "tests", _}] && 
FileExistsQ[FileNameJoin[Flatten[FileNameSplit[file][[ ;; -3]], "PacletInfo.wl"]]]; 


End[]; 


EndPackage[]; 