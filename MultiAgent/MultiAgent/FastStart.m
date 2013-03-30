(* Mathematica package *)

BeginPackage["MultiAgent`"]
(* Exported symbols added here with SymbolName::usage *) 

initMultiAgentExchange::usage = "Function for initialising Multi Agent system (Mono/Multi) ";

Options[initMultiAgentExchange] =
    Join[
        { FromFile -> DefaultTestFile, SystemType -> MoneySystem }, 
     	Options[testCurrencyMarket],
     	Options[createAgents]
	];

Begin["`Private`"]

initMultiAgentExchange[OptionsPattern[]] :=
    Module[ {opts, fself = initMultiAgentExchange, file = OptionValue[FromFile], newMarkets, MakeMarketsFunc, newAgents, defTestFile = "test.conf.m"},
        (* opts = #->StringTrim[OptionValue@#, RegularExpression["^(" <> String@Context@# <> ")?*"]] &/@ Options[fself][[All, 1]]; *)
        opts = #->OptionValue@# &/@ Options[fself] [[All, 1]];
        
        Switch[file,
 
        	DefaultTestFile, (* read parameters from "test.conf.m" *)
            	ReadList[ DirectoryName@FindFile["MultiAgent`"] <> FileNameJoin[{"..", "Scripts", defTestFile}] ];
        		opts = First /@ GatherBy[PARAMS ~Join~ opts, First];
        		Print[opts]; ,
        		
        	_, (* defalt *)
            	ReadList[ DirectoryName@FindFile@file ];
            	Print["System is not inited !!!"];
            	True
        ];
        
        initHistoryVars[];
        initExchange[];
        initAgents[];
        initMarkets[];
        
        evaluateWithOpts[createProducts, opts];
        
        newAgents = evaluateWithOpts[createAgents, opts];
        addAgent /@ newAgents;
        
        MakeMarketsFunc = If[OptionValue[SystemType] == "Mono", monoCurrencyMarkets, multiCurrencyMarkets]; 
        newMarkets = evaluateWithOpts[MakeMarketsFunc, opts];
        addMarket /@ newMarkets;
        
        setAgentsParams[];
        
        initAgentsGoods[];        
    ]
    
End[]

EndPackage[]