(*:Package:*)

BeginPackage["KirillBelov`Internal`Tasks`", {
    "LibraryLink`", 
    "Parallel`Developer`", 
    "KirillBelov`Internal`Compilation`"
}]; 


ClearAll["`*"]; 


CreateBackgroundTask::usage = 
"CreateBackgroundTask[expr, {interval, count}] run expr in background like in ScheduledTask where interval in seconds."; 


AsyncEvaluate::usage = 
"AsyncEvaluate[expr, finish] evaluates expression on parallel kernel and call finish[result] function on master kernel."; 


$BackgroundEvent::usage = 
"Current background task event."; 


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
                $BackgroundEvent = {##};
                expr; 

                If[stopCondition[##], 
                    StopAsynchronousTask[#1]; 
                    RemoveAsynchronousTask[#1]; 
                ];
            ]; 
        )&
    ]
]; 


Options[AsyncEvaluate] := {
    "LaunchKernels" :> All, 
    "CheckInterval" :> 0.01, 
    "TimeConstrained" :> 25, 
    "DistributeDefinitions" -> {}
}; 


SetAttributes[AsyncEvaluate, HoldFirst]; 


AsyncEvaluate[expr_, finish_, OptionsPattern[]] := 
With[{
    checkInterval = OptionValue["CheckInterval"], 
    timeConstrained = OptionValue["TimeConstrained"]
},
    If[Length[Kernels[]] < OptionValue["LaunchKernels"], 
        If[OptionValue["LaunchKernels"] === All, 
            LaunchKernels[]; 
        (*Else*)
            LaunchKernels[OptionValue["LaunchKernels"]]
        ];
    ];
    
    With[{task = With[{$$init = Map[Language`ExtendedFullDefinition, OptionValue["DistributeDefinitions"]]}, 
        ParallelSubmit[
            Once[Map[(Language`ExtendedFullDefinition[] = #)&, $$init]]; 
            expr
        ]
    ]}, 
        CreateBackgroundTask[
            Parallel`Developer`QueueRun[]; 

            If[Parallel`Developer`DoneQ[task], 
                finish[ReleaseHold[task["Result"]]]; 
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


startBackgroundTask = 
LibraryFunctionLoad[$backgroundTaskLibrary, "startBackgroundTask", {Integer, Integer}, Integer]; 


End[]; 


EndPackage[]; 