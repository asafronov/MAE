(* Mathematica package *)

BeginPackage["MultiAgent`"]
(* Exported symbols added here with SymbolName::usage *) 

Begin["`Private`"]

Binary[arr_] := 
	arr /. x_ /; (x != 0) -> 1;

Round2[num_] :=
	NumberForm[num, {6, 2}]

NotNeg[arg_] := 
	If[ arg > 0, arg, 0 ]

evaluateWithOpts[func_, list_, arg___] :=
    func[ arg, Sequence@FilterRules[list, Options[func][[All, 1]] ] ]
    
filterStrategy[w_, strats___] :=
    Select[strats, MatchQ[#, {w, _}] &][[All, 2]];    

makeTitle[name1_, name2_] :=
    StringJoin[ #[[1]], "/", #[[2]] ] &@ 
    	( If[ Length@# >= 3, #[[1 ;; 3]], # ] &/@ Characters /@ {name1, name2} )
    	
makeDaySesPairs[] :=
	(ToString@#[[1]] <> "/" <> ToString@#[[2]]) &/@ Flatten[ Outer[ List, Range@dayN, Range@sesN ], 1 ];

cRandom[] :=
    (Random[] - 0.5) * 2 (*centered Random[] -> (-1..1)*)

(* not used *)

recurseMap::usage = "[func, list]: recursively @ f to elements of list if it's not List.\n recursMap[Sqrt,[{1,2},4}] = {{1,4},8}";
recurseMap[func_, list_] :=
    If[ ListQ@list,
        recurseMap[func, #] &/@ list,
        func@list
    ]

replaceElemsByTest[l_, t_, e_] :=
    Module[ {p, m, n}, (*replace elements by test in list of list*)
        p = Position[l, _?t];
        If[ Length@p == 0,
            Return[l]
        ];
        m = Last /@ p // Min;
        n = Length /@ l // First;
        Join[#[[1 ;; m - 1]], ConstantArray[0, n - m + 1]] & /@ l
    ]

End[]

EndPackage[]