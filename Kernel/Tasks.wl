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


URLReadAsync::usage = 
"URLReadAsync[request, func] execute request in async mode and call func on response."; 


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
    "Once" -> False
}; 


SetAttributes[AsyncEvaluate, HoldFirst]; 


AsyncEvaluate[expr_, handler_, OptionsPattern[]] := 
With[{id = If[#, Hash[Hold[expr]], Hash[CreateUUID[]]]& @ OptionValue["Once"]}, 
    initAsyncTools[]; 
    
    If[!KeyExistsQ[$asyncTasks, id], $asyncTasks[id] = <|
        "Task" -> ParallelSubmit[expr], 
        "Id" -> id, 
        "Handler" -> handler
    |>]; 

    AsyncTask[id]
]; 


If[!ValueQ[$asyncToolsNeedInit], $asyncToolsNeedInit = True]; 


initAsyncTools[] := 
If[$asyncToolsNeedInit, 
    $asyncTasks = <||>; 

    $asyncWatcher = CreateBackgroundTask[checkAsyncTasks[], {0.001, 10^10}]; 

    $asyncToolsNeedInit = False; 
]; 


checkAsyncTasks[] := 
If[Length[$asyncTasks] > 0, 
    Parallel`Developer`QueueRun[]; 
    Map[If[Parallel`Developer`DoneQ[#Task], 
        KeyDropFrom[$asyncTasks, #Id]; 
        #Handler[ReleaseHold[#Task["Result"]]]
    ]&, $asyncTasks]
]; 


AsyncEvaluate[expr_, finish_, OptionsPattern[]] := 
With[{
    checkInterval = OptionValue["CheckInterval"], 
    timeConstrained = OptionValue["TimeConstrained"], 
    once = OptionValue["Once"], 
    launchKernels = OptionValue["LaunchKernels"], 
    hash = Hash[Hold[expr]]
},
    Which[
        Kernels[] === {}, LaunchKernels[], 
        IntegerQ[launchKernels] && Length[Kernels[]] < launchKernels, LaunchKernels[launchKernels]
    ]; 

    If[once && KeyExistsQ[$asyncTasks, hash], Return[Null]]; 
    
    With[{
        task = With[{
            $$init = Map[Language`ExtendedFullDefinition, OptionValue["DistributeDefinitions"]]
        }, 
            ParallelSubmit[
                Once[Map[(Language`ExtendedFullDefinition[] = #)&, $$init]]; 
                expr
            ]
        ]
    }, 
        $asyncTasks[hash] = CreateBackgroundTask[
            Parallel`Developer`QueueRun[]; 

            If[Parallel`Developer`DoneQ[task], 
                KeyDropFrom[$asyncTasks, hash]; 
                finish[ReleaseHold[task["Result"]]]; 
            ], 

            {checkInterval, Round[timeConstrained / checkInterval]}, 
            "StopCondition" -> Function[Parallel`Developer`DoneQ[task]]
        ]
    ]; 
]; 


URLReadAsync[request_HTTPRequest, func_, "Stream"] := 
AsyncEvaluate[
    Module[{
        stream = URLRead[request, "Stream"], part = "", response = ""
    }, 
        While[True, 
            part = ReadString[stream]; 
            If[part === EndOfFile, Break[]]; 
            response = response <> part; 
        ]; 

        Close[stream]; 

        response
    ], 
    func, 
    "Once" -> True
]; 


URLReadAsync[request_HTTPRequest, func_] := 
AsyncEvaluate[
    URLRead[request], 
    func, 
    "Once" -> True
]; 


$asyncTasks = 
<||>; 


$directory = 
DirectoryName[$InputFileName, 2]; 


$backgroundTaskLibrary = 
LibraryResource[$directory, "backgroundTask"]; 


startBackgroundTask = 
LibraryFunctionLoad[$backgroundTaskLibrary, "startBackgroundTask", {Integer, Integer}, Integer]; 


End[]; 


EndPackage[]; 