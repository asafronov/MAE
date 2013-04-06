(* Mathematica package *)

BeginPackage["MultiAgent`"]
(* Exported symbols added here with Symbolamne::usage *) 

Begin["`Private`"]
(* Implementation of the package *)

strProduceByConstant[an_, res_, c_] :=
	Module[ {},
		If[ agentsAlive[an],
			goods[an, res] += c;			
		]
	]
	
strConsumeByConstant[an_, res_, c_] :=
    Module[ {},
        If[ agentsAlive[an],
            goods[an, res] -= c;
                       
            If[ goods[an, res] < 0,
                makeAgentDead[an]
            ]
        ]
    ]
    

strProduceByRandom[an_] :=
    Module[ {r = agentsParams[an, cRegen]},
        MapThread[(goods[an, #1] += #2)&,
    		{productsNums, r}
        ] /; agentsAlive[an];
    ]


strConsumeByNormStupid[an_] :=
	Module[ {n = agentsParams[an, norm], consInd = agentsCons[an], consSize},
		If[ agentsAlive[an],
			consSize = goods[an, consInd] * n;
			goods[an, consInd] -= consSize;
			dailyCons[[an, consInd]] += consSize;
		]
	]
	
strConsumeAll[an_] :=
	Module[ {consInd = agentsCons[an], consSize},
		If[ agentsAlive[an],
			consSize = goods[an, consInd];
			goods[an, consInd] -= consSize;
			dailyCons[[an, consInd]] += consSize;
		]
	]


strProduceByFunc[an_] :=
	Module[ {},
		Switch[OptionValue[agents[an], ProdFuncInd],
			1, produceLinear[an],
			2, produceMin[an],
			3, produceCD[an],
			_, True
		];
	];   
   
produceLinear[an_] :=
	Module[ {coefs = agentsParams[an, cLinear], prod = agentsProds[an], canmake},
		canmake = Sum[ (agentGoods[an][[i]] * coefs[[i]]), {i, productsN} ] ;
		
		If[agentsAlive[an] && canmake > 0,
			goods[an, prod] += canmake;			
			dailyProds[[an, prod]] += canmake;
			
			If[coefs[[#]] != 0, goods[an, #] = 0] &/@ productsNums;
		]
	]

(* ? *)
produceMin[an_] :=
	Module[ {coefs = agentsParams[an, cMin], prod = agentsProds[an], canmake},
		canmake = Min[ 
			agentGoods[an][[#]] * ( coefs[[#]] /. 0 -> 100500 )& /@ productsNums 
		] ;
		
		If[agentsAlive[an] && canmake > 0,
			goods[an, prod] += canmake;
			dailyProds[[an, prod]] += canmake;
			
			If[coefs[[#]] != 0, goods[an, #] -= canmake / coefs[[#]] ] &/@ productsNums;		
		]
	]
	
	
produceCD[an_] :=
	Module[ {coefs = agentsParams[an, cCD], tech = agentsParams[an, cCDTech], prod = agentsProds[an], canmake},	
		canmake = tech * Product[ (agentGoods[an][[i]] ^ coefs[[i]]), {i, productsN} ] ;
		
		If[agentsAlive[an] && canmake > 0,
			goods[an, prod] += canmake;
			dailyProds[[an, prod]] += canmake;
			
			( If[coefs[[#]] != 0, goods[an, #] = 0] ) &/@ productsNums;
		]
	]

(* strategy function specific for market, send without Hold *)

strTradeAllNeeded[an_] :=
	Module[ {cur, money, norma, prodGood, prodGoodSize, consGood, paramName, compGoods, compN, c1, c2, c3, compProd},		
		cur = mainCurrencies[[an]];
		money = goods[an, cur];
		norma = agentsParams[an, norm];
		
		prodGood = agentsProds[an];
		consGood = agentsCons[an];
		paramName = Switch[agentsParams[an, prodFunc], 1, cLinear, 2, cMin, 3, cCD, _, False];
		compGoods = Binary@agentsParams[an, paramName];
		
		If[DebugTrade, Print["-- D/S: " <> ToString@dayNumber <> "/" <> ToString@sessionNumber <> " --"] ];
		
		prodGoodSize = goods[an, prodGood];
		c1 = (cur != prodGood) && (prodGoodSize > CEpsAmount);
		If[c1,
			tryToSellByLastPrognose[getMarketNumber[cur, prodGood], an, prodGoodSize, True],
				If[DebugTrade, Print[an, "-agent don't sell prod good ", prodGood, ", has it ", prodGoodSize, ", cur=", cur] ],
					Print["ERROR C1"];
		];
		
		c2 = (cur != consGood) && ((money * norma) > CEpsAmount);
		If[c2,
			tryToBuyAllOnCash[getMarketNumber[cur, consGood], an, money * norma],
				If[DebugTrade, Print[an, "-agent don't buy cons good ", consGood, ", has money ", money, ", cur=", cur] ],
					Print["ERROR C2", CEpsAmount];
		];
		
		compN = Count[compGoods, 1];		
		If[compN == 0, Print["DANGER!!! compN = 0"]];
					
		For[i = 1, i <= Length@compGoods, i++, 
			If[compGoods[[i]] == 1,
				compProd = productsNums[[i]];
				c3 = (cur != compProd) && ((money * (1 - norma)) > CEpsAmount);
				If[c3, 
					tryToBuyAllOnCash[getMarketNumber[cur, compProd], an, money * (1 - norma) / compN],
						If[DebugTrade, Print[an, "-agent don't buy comp good ", compProd, "; has money ", money, ", cur=", cur] ],
							Print["ERROR C3"];
				];
			];
		];
	]
    
strRandomPrognose[an_] :=
    Module[ {lastPrc, prognosePrc},
        Function[mn,
        	lastPrc = marketLastPrice[mn];
            (*prognosePrc = lastPrc + lastPrc * 0.5 * cRandom[];*)
            prognosePrc = lastPrc + (-1)^an * 0.1 * lastPrc;
            AppendTo[hMarketsPrognoses[[mn, an]], prognosePrc];
        ] /@ marketsNums;
    ];
    
strAdaptivePrognose[an_] :=
	Module[ {dn, lastPrc, lastPrognose, newPrognose},
		Function[mn,
			dn = dayNumber - 1;
			lastPrc = marketLastPrice[mn];
			
			newPrognose = If[dn == 0,
							lastPrc * (1 +  0.1 * RandomInteger[]),
								lastPrognose = hMarketsPrognoses[[mn, an, dn]];
								lastPrognose + agentsParams[an, lambda] * (marketMeanPrice[mn, dn] - lastPrognose)	
						];	
			AppendTo[hMarketsPrognoses[[mn, an]], newPrognose];
				
		] /@ marketsNums;
	];
	
estimateDiscount[mn_, an_] :=
	Module[{curSize,  allDeals, anDeals, lastTraded, discount},
		If[dayNumber == 1 && sessionNumber == 1, Return [0]];
		
		curSize = goods[an, marketGoods[mn][[2]] ];
		allDeals = If[Length@Last@hMarketsDeals[[mn]] == 0, {}, Last@Last@(hMarketsDeals[[mn]]) ];
		anDeals = Select[allDeals, #[[1]] == an&];
		lastTraded = Total@anDeals[[All, 2, 2]];
		
		discount = Max[0.0, CMaxDiscount * (curSize - lastTraded) / curSize ];
		
		If[discount < 0 || discoint > 0.1,
			Print["Big discount!", discount];
		];
		
		discount
	];
	
estimate 

End[]

EndPackage[]