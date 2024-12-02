(*:Package:*)

BeginPackage["KirillBelov`Internal`Tasks`", {
    "LibraryLink`", 
    "Parallel`Developer`", 
    "KirillBelov`Internal`Compilation`"
}]; 


ClearAll["`*"]; 


CreateBackgroundTask::usage = 
"CreateBackgroundTask[expr, {interval, count}] run expr in background like in ScheduledTask where interval in seconds."; 


ParallelAsyncEvaluate::usage = 
"ParallelAsyncEvaluate[expr] evaluates expression on parallel kernel."; 


$BackgroundTask::usage = 
"Current background task."; 


Begin["`Private`"]; 


SetAttributes[CreateBackgroundTask, HoldFirst]; 


Options[CreateBackgroundTask] = {
    "StopCondition" -> Function[False]
};


CreateBackgroundTask[expr_, {interval_?NumericQ, count_Integer?Positive}, OptionsPattern[]] /; interval >= 0.001 := 
With[{
	intervalMs = Round[interval * 1000], 
    stopCondition = OptionValue["StopCondition"]
},     
    Internal`CreateAsynchronousTask[
        startBackgroundTask, 
        {intervalMs, count}, 
        (
            PreemptProtect[
                $BackgroundTask = {##};
                expr; 

                If[stopCondition[##], 
                    stopBackgroundTask[#1[[2]]]
                ];
            ]; 
        )&
    ]
]; 


Options[ParallelAsyncEvaluate] := {
    "LaunchKernels" -> 2, 
    "CheckInterval" -> 0.1, 
    "TimeConstrained" -> 10, 
    "ResultHandler" -> Function[Null], 
    "DistributeDefinitions" -> {}
}; 


SetAttributes[ParallelAsyncEvaluate, HoldFirst]; 


ParallelAsyncEvaluate[expr_, OptionsPattern[]] := 
With[{
    resultHandler = OptionValue["ResultHandler"], 
    checkInterval = OptionValue["CheckInterval"], 
    timeConstrained = OptionValue["TimeConstrained"]
},
    If[Length[Kernels[]] < OptionValue["LaunchKernels"], 
        LaunchKernels[OptionValue["LaunchKernels"]]
    ];
  
    Map[DistributeDefinitions, OptionValue["DistributeDefinitions"]]; 
  
    With[{task = ParallelSubmit[expr]},     
        CreateBackgroundTask[
            Parallel`Developer`QueueRun[]; 

            If[Parallel`Developer`DoneQ[task], 
                resultHandler[ReleaseHold[task["Result"]]]; 
            ], 

            {checkInterval, Round[timeConstrained / checkInterval]}, 

            "StopCondition" -> Function[Parallel`Developer`DoneQ[task]]
        ]
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