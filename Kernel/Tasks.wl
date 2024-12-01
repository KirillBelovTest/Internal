(*:Package:*)

BeginPackage["KirillBelov`Internal`Tasks`", {
    "LibraryLink`", 
    "KirillBelov`Internal`Compilation`"
}]; 


ClearAll["`*"]; 


CreateBackgroundTask::usage = 
"CreateBackgroundTask[expr, {interval, count}] run expr in background like in ScheduledTask where interval in seconds."; 


Begin["`Private`"]; 


SetAttributes[CreateBackgroundTask, HoldFirst]; 


CreateBackgroundTask[expr_, {interval_?NumericQ, count_Integer?Positive}] /; interval > 0.001 := 
With[{
	intervalMs = Round[interval * 1000]
},     
    Internal`CreateAsynchronousTask[
        startBackgroundTask, 
        {intervalMs, count}, 
        PreemptProtect[expr]&
    ]
]; 


$directory = 
DirectoryName[$InputFileName, 2]; 


$backgroundTaskLibrary = 
LibraryResource[$directory, "backgroundTask"]; 


startBackgroundTask = LibraryFunctionLoad[$backgroundTaskLibrary, "startBackgroundTask", {Integer, Integer}, Integer]; 


stopBackgroundTask = LibraryFunctionLoad[$backgroundTaskLibrary, "stopBackgroundTask", {Integer}, Integer]; 


End[]; 


EndPackage[]; 