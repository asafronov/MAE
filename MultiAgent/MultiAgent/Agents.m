(* Mathematica package *)

BeginPackage["MultiAgent`"]
(* Exported symbols added here with SymbolName::usage *) 

(* METHODS *)
initAgents			::usage = "free variables associated with Agents part of Exchange";
createAgent			::usage = "function to create one agent";
createAgents		::usage = "creates agent population";
addAgent			::usage = "added created agent to agents array";
makeAgentDead		::usage = "func to kill agent";
makeAgentAlive		::usage = "func to alive agent";
aliveAgentsNums		::usage = "gets a list of agents numbers, which are still alive";
initAgentsGoods		::usage = "generates agents' start portfolios";
setAgentsGoods		::usage = "set all agents' start portfolios";
setAgentsParams		::usage = "set parameters {consumption, producing} for agents ";
getAgentCapital		::usage = "estimates agent capitalization as sum of it's property costs by last market price";
printAgentParams	::usage = "prints all agent parameters: id, prod/cons, norm, prodfunc, etc.";

(* VARS *)
agents			::usage = "agents started options";
agentsNums		::usage = "indexes of agents";
agentsN			::usage = "amount of agents";
agentsParams	::usage = "parameters like coefs of prod funcs, norm";
agentsProds		::usage = "produced goods";
agentsCons		::usage = "consumed goods";

Options[createAgents] := {
	AgentsAmount -> 0, StartMoney -> 0, ProdFuncInd -> 0, StratPeriod -> 0
};

Begin["`Private`"]

initAgents[] :=
    Module[ {},
        agents =.;
        agentsNums = {};
        agentsN = 0;
        agentsId = 0;
        agentsAlive =.;
        
        agentsProds = .;
        agentsCons = .;
        
        agentsParams =.;
    ];

Options[createAgent] :=
{
	ProdFuncInd -> 0
}

createAgent[strats___, OptionsPattern[]] :=
    Module[ {AN, st = List@strats},
        AN = ++agentsId;
        {
        	Number -> AN,
        	ProdFuncInd -> OptionValue[ProdFuncInd],
			CreateActions -> filterStrategy[CreateAction, st /. AgentNUM -> AN],
			Learning -> filterStrategy[Learn, st /. AgentNUM -> AN],
			Prognostication -> filterStrategy[Forecast, st /. AgentNUM -> AN],
			TradeStrategies -> filterStrategy[Strategy, st /. AgentNUM -> AN],
			Production -> filterStrategy[Produce, st /. AgentNUM -> AN],
			Consumption -> filterStrategy[Consume, st /. AgentNUM -> AN]
    	}
    ];
    
createAgents[OptionsPattern[]] :=
    Module[ {agents, money, agCount, prodF, period, defstr = {}},
        agents = {};
        money = OptionValue[StartMoney];
        agCount = OptionValue[AgentsAmount];
        prodF = OptionValue[ProdFuncInd];
        period = OptionValue[StratPeriod];
            	    	       		       	
        defstr = {
            {CreateAction, Hold@strProduceByConstant[AgentNUM, 1, money]},
            {Learn, Hold@strLearnConsAndDeals[AgentNUM, period]},
            (*{Forecast, Hold@strRandomPrognose[AgentNUM]},*)
            {Forecast, Hold@strAdaptivePrognose[AgentNUM]},
            {Strategy, Hold@strTradeAllNeeded[AgentNUM]},
            {Produce, Hold@strProduceByRandom[AgentNUM]},
            {Produce, Hold@strProduceByFunc[AgentNUM]}, 	
            {Consume, Hold@strConsumeAll[AgentNUM]}
    	};
		
		(AppendTo[ agents, createAgent[ReplacePart[defstr, 0 -> Sequence] , ProdFuncInd -> prodF ]]) &/@ Range@agCount;
		agents
    ];        

addAgent[a_] :=
    Module[ {n = OptionValue[a, Number]},
        agents[n] = Rest@a;
        makeAgentAlive[n];
        initAgentProducts[n];
        AppendTo[agentsNums, n];
        agentsN++;       
    ];

makeAgentAlive[n_] :=
    agentsAlive[n] = True;
    
makeAgentDead[n_] :=
    agentsAlive[n] = False;
    
aliveAgentsNums[] :=
    agentsNums[[ Flatten@Position[agentsAlive /@ agentsNums, True] ]];

initAgentsGoods[dest_:CQuantGoods] :=
    Module[ {avals},
		hAgentsGoodsStart = (RandomVariate[dest] &/@ productsNums) &/@ agentsNums;
		
		Function[an,
    		avals = 0 &/@ productsNums;
    		avals[[1]] = 1;
    		avals[[ agentsProds[an] ]] = 1;
    		avals[[ agentsCons[an] ]] = 1;
    		
    		Switch[agentsParams[an, prodFunc],
    			1, avals += agentsParams[an, cLinear],
    			2, avals += agentsParams[an, cMin],
    			3, avals += agentsParams[an, cCD]
    		];
    		
    		If[avals[[#]] == 0, hAgentsGoodsStart[[an, #]] = 0]& /@ productsNums;
    		 	
    	] /@ agentsNums;
    ];

(* TODO: REDO =) *)
setAgentsGoods[] :=
	Module[{},
		Function[an,
			Function[pn,
				goods[an, pn] = hAgentsGoodsStart[[an, pn]];
			] /@ productsNums;
		] /@ agentsNums;
	];    

setAgentsParams[] :=
	Module[ {producedProd, consumedProd, linearCoefs, minCoefs, cdCoefs, temp},
		Function[an,
					
			producedProd = RandomInteger[productsN - 1] + 1;
	        consumedProd = RandomInteger[productsN - 2] + 2;  (* 1 - not consumed *)
	        While[consumedProd == producedProd, consumedProd = RandomInteger[productsN - 1] + 1 ];
	        
	        agentsProds[an] = producedProd;
	        agentsCons[an] = consumedProd;
			    
            agentsParams[an, cRegen] = NotNeg /@ RandomVariate[CRegen, productsN];

(*			linearCoefs = NotNeg /@ RandomVariate[CLinearProduce, productsN];
*)			minCoefs = NotNeg /@ RandomVariate[CMinProduce, productsN];
			cdCoefs = NotNeg /@ RandomVariate[CCDProduce, productsN];
			
			linearCoefs = 0 &/@ productsNums;
			temp = agentsProds[an];
			While[temp == agentsProds[an], temp = RandomInteger[productsN - 1] + 1];
			linearCoefs[[ temp ]] = Abs@RandomVariate[CLinearProduce];
			
			linearCoefs[[ agentsProds[an] ]] = 0;
			minCoefs[[ agentsProds[an] ]] = 0;
			cdCoefs[[ agentsProds[an] ]] = 0;
			
			agentsParams[an, prodFunc] = OptionValue[agents[an], ProdFuncInd];
			agentsParams[an, cLinear] = linearCoefs;
			agentsParams[an, cMin] = minCoefs;
			agentsParams[an, cCD] = cdCoefs;
			agentsParams[an, cCDTech] = RandomVariate[CCDTech];

			agentsParams[an, lambda] = RandomReal[CLambdaRange];
			agentsParams[an, norm] = RandomReal[CNormRange];		
		] /@ agentsNums;
	];

 
getAgentCapital[an_, cur_:1] :=
    Module[{gds, rest, prs, ams},
    	gds = agentGoods[an];
    	rest = Delete[productsNums, cur];
    	prs = {cur, #}& /@ rest;
    	ams = gds[[ rest ]];
        Total[(ams * (marketLastPrice[#[[1]], #[[2]]]& /@ prs)) ~Join~ { gds[[ cur ]] }]
    ];
    
printAgentParams[an_] :=
	Module[{consGood = agentsCons[an], prodGood = agentsProds[an], prodFuncInd, paramName},
		Print["***** [", an , "] Agent Parameters *****"];
		Print["Produces good: " , prodGood , ", " , productsNames[[prodGood]] ];
		Print["Consumed good: " , consGood , ", " , productsNames[[consGood]] ];
		
		prodFuncInd = agentsParams[an, prodFunc];
		paramName = Switch[prodFuncInd, 1, cLinear, 2, cMin, 3, cCD, _, False];
		Print["Production function: ", prodFuncInd];
		Print["Func coefs : " , Round2@agentsParams[an, paramName] ];
		
		Print["Daily regen: ", Round2@agentsParams[an, cRegen] ];
		Print["Start store: ", Round2@hAgentsGoodsStart[[an]]];
		Print["Consume norm: ", Round2@agentsParams[an, norm] ];
		Print["Lambda coef: ", Round2@agentsParams[an, lambda] ];
		Print[""];
	]   

End[]

EndPackage[]