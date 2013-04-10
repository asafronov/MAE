(* Mathematica package *)

BeginPackage["MultiAgent`"]
(* Exported symbols added here with SymbolName::usage *) 

initExchange::usage = "init variables associated with exchange";
runExchange::usage = "main function that runs MAExchange";

mainCurrencies	::usage = "main currency, which is designated with creating markets";
agentsWeights	::usage = "[[an, pn]]: weight";

dailyProds::usage = "";
dailyCons::usage = "";

consMetrics::usage = "";
dealMetrics::usage = "";

dayN::usage = "";
sesN::usage = "";

daySesStr::usage = "";
dayStr::usage = "";

Options[startExchange] :=
    {Days -> 1};

Begin["`Private`"]
(* Implementation of the package *)

initExchange[OptionsPattern[]] :=
    Module[ {},
    	dayN = 0;
    	sesN = 0;
    	
		dayNumber = 0;
        sessionNumber = 0;      
        firstSession = True;
		
		mainCurrencies = {};
		agentsWeights = {};
        
        dailyProds = {};
        dailyCons = {};
    ];

runExchange[] :=
	evaluateWithOpts[startExchange, PARAMS];

startExchange[OptionsPattern[]] :=
    Module[ {r, wts},
    	
    	setAgentsGoods[];
        initHistory[];
		
		dayN = OptionValue[Days];
       	sesN = OptionValue[markets[1], SessionsInDay];
       	
       	daySesStr = makeDaySesPairs[];
       	r = Range@dayN;
       	dayStr = ToString /@ (PrependTo[r, 0]);

        If[ firstSession,       	
			executeActions[#, CreateActions] &/@ agentsNums;
			hMarketsPricesStart = RandomReal[ OptionValue[markets[#], FirstPrice] ] &/@ marketsNums;
			wts = (1 / productsN) &/@ productsNums;
			AppendTo[agentsWeights, wts] &/@ agentsNums;
			
			firstSession = False;
        ];
        
        While[++dayNumber <= dayN,
        	prepareMarketHistory /@ marketsNums;
        	
			learnDay[];
			prognoseDay[];
			produceDay[];	 
			tradeDay[];
			consumeDay[];
			 
			updateGoodHistory[];
        ];
    ];
    
executeActions[na_, actionP_, replace___] :=
    Module[ {fs, r = List@replace},
        If[ agentsAlive@na,
        	fs = OptionValue[agents[na], actionP] /. r;
        	ReleaseHold /@ fs;
        ]
    ];

learnDay[] :=
    Module[ {},
        executeActions[#, Learning] &/@ agentsNums
    ];

prognoseDay[] :=
    Module[ {},
        executeActions[#, Prognostication] &/@ agentsNums
    ];

produceDay[] :=
    Module[ {},
        executeActions[#, Production] &/@ agentsNums
    ];
    
consumeDay[] :=
    Module[ {},	
        executeActions[#, Consumption] &/@ agentsNums
    ];
    
tradeDay[] :=
    Module[ {asks, bids, price},
     	      	      	
        Function[sn,
        	sessionNumber = sn;
        	
        	Function[mn, 
	    		marketsAsks[mn] = {};
		        marketsBids[mn] = {};
	        ] /@ marketsNums;
	         
	        executeActions[#, TradeStrategies] &/@ agentsNums; (* make asks/bids *)
	        
	        Function[mn,
	        	deals = {0, 0} &/@ agentsNums;
	        	        	 
	            asks = marketsAsks[mn];
	            bids = marketsBids[mn];	                                    
	            updateHistoryOfBooks[mn, dayNumber, sessionNumber, asks, bids];
	            
                price = handleOrderBookDA[mn, asks, bids];
				updateMarketHistory[mn, dayNumber, sessionNumber, price];
								
	        ] /@ marketsNums;         
        ] /@ Range@sesN;
    ];

prepareMarketHistory[mn_] :=
	Module[{},			
		While[Length@hMarketsBooks[[mn]] < dayNumber,
    		AppendTo[hMarketsBooks[[mn]], {}]
    	];
    	While[Length@hMarketsPrices[[mn]] < dayNumber,
    		AppendTo[hMarketsPrices[[mn]], {}]
    	];
    	While[Length@hMarketsDeals[[mn]] < dayNumber,
    		AppendTo[hMarketsDeals[[mn]], {}]
    	];
	];

updateGoodHistory[] :=
    Module[ {},
        Function[an,
            If[hAgentsLastDay[[an]] == +Infinity && !agentsAlive[an],
            	hAgentsLastDay[[an]] = dayNumber;
            ];    
    	] /@ agentsNums;
    	
    	AppendTo[hAgentsProduced, dailyProds];
    	AppendTo[hAgentsConsumed, dailyCons];
    	
    	AppendTo[hTotalProduced, Total@dailyProds];
    	AppendTo[hTotalConsumed, Total@dailyCons];
    	
    	AppendTo[hAgentsCurrency, mainCurrencies];
    	AppendTo[hAgentsCapitals, getAgentCapital /@ agentsNums]; 
    	
    	AppendTo[hAgentsGoods, agentGoods /@ agentsNums];
		AppendTo[hAgentsWeights, agentsWeights];
    	
    	dailyProds = (0 &/@ productsNums) &/@ agentsNums;
		dailyCons = (0 &/@ productsNums) &/@ agentsNums;
		
		consMetrics = {};
		dealMetrics = {};
		Function[an,
			AppendTo[consMetrics, Total@hAgentsConsumed[[dayNumber, an, All]] ];
			AppendTo[dealMetrics, Length@Select[Flatten[hMarketsDeals[[All, dayNumber, All]], 2], First@# == an &] ];
		] /@ agentsNums;
		
		AppendTo[hAgentsConsMetrics, consMetrics];
		AppendTo[hAgentsDealMetrics, dealMetrics];
];
	
updateMarketHistory[mn_, day_, ses_, price_] :=
	Module[{prc = price, filteredDeals},
		If[prc == 0, prc = marketLastPrice[mn] ];
		AppendTo[hMarketsPrices[[mn, day]], prc];
		
		filteredDeals = {};
		Function[an,
			If[deals[[an]] != {0,0}, 
				AppendTo[filteredDeals, {an, deals[[an]]}];
			];		
		] /@ agentsNums;
		
		AppendTo[hMarketsDeals[[mn, day]], filteredDeals];
		
	];

updateHistoryOfBooks[mn_, day_, ses_, asks_, bids_] :=
    Module[ {},	
        AppendTo[hMarketsBooks[[mn, day]], {asks, bids}];
    ];  

End[]

EndPackage[]
