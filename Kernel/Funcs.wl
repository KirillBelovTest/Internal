(*:Package:*)

BeginPackage["KirillBelov`Internal`Funcs`"]; 


ClearAll["`*"]; 


ConditionApply::usage = 
"ConditionApply[pairs, default][args] select rule if key[args] is True and resturn value[args]"; 


Cache::usage = 
"Cache[expr] cache expr for one minute
Cache[expr, period] cache expr for specific period"; 


AssocMatchQ::usage = 
"AssocMatchQ[assoc, pattern] match assoc with pattern
AssocMatchQ[assoc, key, valuePattern] check key from assoc
AssocMatchQ[pattern] - function"; 


AssocBlock::usage = 
"AssocBlock[assoc, vars, expr] assoc converts to local variables for block."; 


AssocFunction


Begin["`Private`"]; 


AssocFunction[func_Symbol][assoc_Association?AssociationQ] := 
Module[{params}, 
    Evaluate[params = 
    Map[Function[ReleaseHold[# //. {
        Verbatim[HoldPattern][func[$a___]] :> Hold[{$a}], 
        Verbatim[PatternTest][Verbatim[Pattern][$n_, $h_], $t_] :> <|
            "Name" -> ToString[$n], 
            "Head" -> $h, 
            "Test" -> $t
        |>, 
        Verbatim[Pattern][$n_, $h_] :> <|
            "Name" -> ToString[$n], 
            "Head" -> $h
        |>
    }]]] @ DownValues[func][[All, 1]];

    With[{targetParams = 
        Query[All, {"Name" -> Slot}] @ 
        SelectFirst[params,
            Map[ToLowerCase, Sort[#[[All, "Name"]]]] == 
            Map[ToLowerCase, Sort[Keys[assoc]]] &
        ]
    },
    
        With[{target = Table[
            With[{$h = param["Head"], $n = param["Name"], $t = param["Test"]}, 
                Which[
                    $t === VectorQ, Hold[Map[ToExpression] @ StringSplit[$n, ","]], 
                    $h === _Real, Hold[N[ToExpression[$n]]], 
                    $h === _Integer, Hold[Round[ToExpression[$n]]], 
                    $h === _List, Hold[StringSplit[$n, ","]],
                    $t === NumberQ, Hold[ToExpression[$n]], 
                    True, Hold[$n]
                ]
            ], 
            {param, targetParams}
        ]}, 

            With[{$func = func @@ target & //. {Hold[$a_] :> $a}}, 
                $func[assoc]
            ]
        ]
    ]]
]; 


JSONFunction[func_Symbol][jsonText_String] := 
JSONFunction[func][ImportString[jsonText, "RawJSON"]]; 


SetAttributes[AssocBlock, HoldRest]; 


AssocBlock[assoc_Association, vars___Set, expr_] := 
Module[{x1, x2}, 
    x1 = Normal @ assoc /. Rule[k_String, v_] :> Rule[ToExpression[k, StandardForm, Hold], v]; 
    x2 = Apply[List, Hold[vars] /. Verbatim[Set][a_, b_] :> (Hold[a] -> b)]; 
    With[{x3 = Join[x1, x2]}, 
        With[{x4 = Hold[x3, expr] /. Rule[Hold[s_], v_] :>  Hold[s, v]}, 
            Apply[Block , x4  /. Hold[s_Symbol, v_] :> Set[s, v]]
        ]
    ]
]; 


ConditionApply[conditionAndFunctions_Association: <||>, defalut_: Function[Null], ___] := 
Function[Last[SelectFirst[conditionAndFunctions, Function[cf, First[cf][##]], {defalut}]][##]]; 


SetAttributes[Cache, HoldFirst]; 


Cache[expr_, period_Integer: 60] := 
Module[{roundNow = Floor[AbsoluteTime[], period]}, 
    If[IntegerQ[Cache[expr, "Date"]] && Cache[expr, "Date"] == roundNow, 
        Cache[expr, "Value"], 
    (*Else*)
        Cache[expr, "Date"] = roundNow; 
        Cache[expr, "Value"] = expr
    ]
]; 


AssocMatchQ[assoc_Association, pattern_Association] := 
Apply[And, KeyValueMap[AssocMatchQ[assoc, #1, #2]&, pattern]]; 


AssocMatchQ[pattern_Association][assoc_Association] := 
AssocMatchQ[assoc, pattern]; 


AssocMatchQ[assoc_Association, keys__, stringPattern_?StringPattern`StringPatternQ] := 
StringMatchQ[assoc[[keys]], stringPattern, IgnoreCase -> True]; 


AssocMatchQ[assoc_Association, keys__, func_Function] := 
func[assoc[[key]]]; 


AssocMatchQ[assoc_Association, keys__, assocPattern_Association?AssociationQ] := 
AssocMatchQ[assoc[[keys]], assocPattern]; 


AssocMatchQ[assoc_Association, keys__, pattern_] := 
MatchQ[assoc[[keys]], pattern]; 


$directory = 
DirectoryName[$InputFileName, 2]; 


End[]; 


EndPackage[]; 