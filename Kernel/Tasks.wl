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
	id = $id++, 
	ms = Round[interval * 1000]
}, 
    $backgroundTasks[id] := expr; 
    $backgroundTaskCounts[id] = count; 

    Internal`CreateAsynchronousTask[
        startBackgroundTask, 
        {"BackgroundTask", Round[interval * 1000], count}, 
        PreemptProtect[runBackgroundTask[toEvent[##]]]&
    ]
]; 


toEvent[task_, event_, {n_, count_, id_}] := 
<|
    "task" :> task, 
    "n" :> n, 
    "count" :> runCount, 
    "id" :> id
|>; 


If[!IntegerQ[$id], $id = 1]; 


$directory = 
DirectoryName[$InputFileName, 2]; 


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
LibraryResource[$directory, "backgroundTask"]; 


startBackgroundTask = LibraryFunctionLoad[$backgroundTaskLibrary, "startBackgroundTask", {String, Integer, Integer}, Integer]; 


stopBackgroundTask = LibraryFunctionLoad[$backgroundTaskLibrary, "stopBackgroundTask", {Integer}, Integer]; 


End[]; 


EndPackage[]; 