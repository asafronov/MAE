(* Mathematica package *)

BeginPackage["MultiAgent`"]
(* Exported symbols added here with SymbolName::usage *) 

(* METHODS *)
initHistory			::usage = "init from Notebook";
initHistoryVars		::usage = "initing history values";
marketLastPrice		::usage = "[mn]: last traded price";
marketMeanPrice		::usage = "[mn, dn]: average price on (mn)-market during (dn)-day";
 
(* VARS *)
hMarketsBooks		::usage = "[[mn]] [[day]] [[ses]] [[ask/bid]] = order";
hMarketsPrices		::usage = "[[mn]] [[day]] [[ses]] = prc";
hMarketsDeals		::usage = "[[mn]] [[day]] [[ses]] = deals; deal = {an, {delta1, delta2}}";
hMarketsPrognoses	::usage = "[[mn]] [[an]] [[day]] = prog";

hAgentsGoodsStart	::usage = "[[an]] [[good]] = amount, generated in the beginning";
hAgentsGoods		::usage = "[[day]] [[an]] [[good]] = amount";
hAgentsCurrency		::usage = "[[day]] [[an]] = currency";
hAgentsCapitals		::usage = "[[day]] [[an]] = capital";
hAgentsLastDay		::usage = "last operating day, = +Inf if alive";
hAgentsProduced		::usage = "[[day]] [[an]] [[good]] = produced amount";
hAgentsConsumed		::usage = "[[day]] [[an]] [[good]] = consumed amount";
hTotalProduced		::usage = "[[day]] [[good]] = total pruduced amount";
hTotalConsumed		::usage = "[[day]] [[good]] = total consumed amount";

hAuctionGraphs		::usage = "[[mn]] all double auctions on (mn)-market, indexed by day/ses";

Begin["`Private`"]
(* Implementation of the package *)

initHistoryVars[] :=
    Module[ {},       
        hMarketsBooks = {};
        hMarketsPrices = {};
        hMarketsDeals = {};      
        hMarketsPrognoses = {};
        
        hAgentsLastDay = {};
        hAgentsGoods = {};
        hAgentsCurrency = {};
        hAgentsCapitals = {};
        
        hAgentsProduced = {};
        hAgentsConsumed = {};
        hTotalProduced = {};
        hTotalConsumed = {};
        
        hAuctionGraphs = {};
    ]

initHistory[] :=
    Module[ {},
    	
    	dailyProds = (0 &/@ productsNums) &/@ agentsNums;
		dailyCons  = (0 &/@ productsNums) &/@ agentsNums;

        (*AppendTo[hAgentsGoods, agentGoods /@ agentsNums];*)
        
        Function[mn,       	
        	AppendTo[hMarketsBooks, {}];  
        	AppendTo[hMarketsPrices, {}];
        	AppendTo[hMarketsDeals, {}];
        	AppendTo[hMarketsPrognoses, {}& /@ agentsNums ];  
        	
        	AppendTo[hAuctionGraphs, {}];   	
        ] /@ marketsNums;
        
        hAgentsLastDay = (+Infinity) &/@ agentsNums; 
];

marketLastPrice[g1_, g2_] :=
	Module[{},
		marketLastPrice[getMarketNumber[g1, g2]]
	];

(* returns last not null price. If there were no prices, generate it! *)    
marketLastPrice[mn_] :=
	Module[{},
		If[Length@First@hMarketsPrices[[mn]] == 0, 
			RandomReal[ OptionValue[markets[mn], FirstPrice ] ],
				If[Length@Last@hMarketsPrices[[mn]] == 0,
					Last@hMarketsPrices[[mn, -2]],
						Last@Last@hMarketsPrices[[mn]]
				]
		]
	];
	
marketMeanPrice[mn_, dn_] :=
	Module[{prices},
		prices = hMarketsPrices[[mn, dn, All]];
		Mean[prices]
	];
	
End[]

EndPackage[]