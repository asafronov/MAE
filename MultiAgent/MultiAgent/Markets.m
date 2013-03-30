(* Mathematica package *)

BeginPackage["MultiAgent`"]
(* Exported symbols added here with SymbolName::usage *) 

(* METHODS *)
initMarkets				::usage = "init variables associated with markets";
createMarket			::usage = "main function to create market, returns list of options";
addMarket				::usage = "add market to base of markets";
getMarketNumber			::usage = "finds index of market from indexes of goods it contains"; 

testCurrencyMarket		::usage = "creates one test market";
monoCurrencyMarkets		::usage = "creates markets for mono-currency system";
multiCurrencyMarkets	::usage = "creates markets for multy-currency system";

(* VARS *)
markets			::usage = "contains options of created markets";
marketsNums		::usage = "indexes of markets";
marketGoods		::usage = "[mn] = {G1,G2} - used goods in cur market";
marketSymbols	::usage = "[mn] = {gn1, gn2} ";

mainCurrencies	::usage = "main currency, which is designated with creating markets";

Options[testCurrencyMarket] :=
    {SessionsInDay -> 0, FirstPrice -> {0,0} };
    
Options[monoCurrencyMarkets] :=
    Options[testCurrencyMarket];
    
Options[multiCurrencyMarkets] :=
    Options[testCurrencyMarket] ~Join~ {NumberOfCurrencies -> 0};
    

Begin["`Private`"]

initMarkets[] :=
    Module[ {},
        markets =.;
        marketsNums = {};
        marketGoods = .;
        marketSymbols = .;
        
        mainCurrencies = {};
        
        marketsId = 0;
        marketsAsks = .;
        marketsBids = .;
        deals = .;
        
        orderNumber = 0;
        changeNumber = 0;        
    ]
   
Options[createMarket] :=
    {Title -> "unknown", SessionsInDay -> 0, FirstPrice -> 0};

createMarket[g1_,g2_, OptionsPattern[]] :=
    Module[ {mn, title = OptionValue[Title]},
        mn = ++marketsId;
        If[ title == "unknown",
            title = makeTitle[productsNames[[g1]], productsNames[[g2]] ]
        ];
        
        {MarketNumber -> mn, Title -> title, G1 -> g1, G2 -> g2,
         SessionsInDay -> OptionValue[SessionsInDay], FirstPrice -> OptionValue[FirstPrice]}
    ];
    

addMarket[m_] :=
    Module[ {g1, g2, n = OptionValue[m, MarketNumber]},
        AppendTo[marketsNums, n];
        markets[n] = Rest@m;
        
        g1 = OptionValue[m, G1];
        g2 = OptionValue[m, G2];
        marketGoods[n] = { g1, g2 };
        
        marketSymbols[n] = { StringJoin@If[ Length@# >= 3, #[[1 ;; 3]], # ]& @ Characters@productsNames[[g1]],
        						StringJoin@If[ Length@# >= 3, #[[1 ;; 3]], # ]& @ Characters@productsNames[[g2]] };
    ];

getMarketNumber[g1_, g2_] :=
	Module[ {gd1, gd2, mn, N},
		If[g1 == g2, 
			Return[$Failed]];		
		gd1 = Min[g1, g2];
		gd2 = Max[g1, g2];
		N = Last@productsNums;
		mn = Sum[N-i, {i, gd1 - 1}] + (gd2 - gd1);
		mn
	]

testCurrencyMarket[ng1_:1, ng2_:2, OptionsPattern[]] :=
    Module[ {dayses = OptionValue[SessionsInDay], price = OptionValue[FirstPrice], market},
        If[ productsN <= Max[ng1,ng2],
            Return[$Failed]
        ];
        market = createMarket[ng1,ng2, SessionsInDay -> dayses, FirstPrice -> price];
        { market }
    ];

monoCurrencyMarkets[OptionsPattern[]] :=
    Module[ {dayses = OptionValue[SessionsInDay], price = OptionValue[FirstPrice], mainProd, pairs},
 
        If[ productsN < 2,
            Return[$Failed]
        ];
        
        mainProd = 1;
        mainCurrencies = mainProd &/@ agentsNums;
        
        pairs = {mainProd, #} &/@ Delete[productsNums, mainProd];
        
        createMarket[#[[1]], #[[2]], SessionsInDay -> dayses, FirstPrice -> price] &/@ pairs
    ];

multiCurrencyMarkets[OptionsPattern[]] :=
    Module[ {dayses = OptionValue[SessionsInDay], price = OptionValue[FirstPrice], 
    	numofc = OptionValue[NumberOfCurrencies], mainProds, pairs},
        
        If[ productsN < 2,
            Return[$Failed]
        ];
        
        If[ numofc == 0,
            numofc = productsN  
        ];
        
        mainProds = RandomChoice[productsNums, agentsN];
        mainCurrencies = mainProds;
        
        pairs = {};
        For[i = 1, i <= productsN, i++,
        	For[j = i+1, j <= productsN, j++,
        		AppendTo[pairs, { productsNums[[i]], productsNums[[j]] }];
        	];
        ];

        createMarket[#[[1]], #[[2]], SessionsInDay -> dayses, FirstPrice -> price] &/@ pairs
    ];
 
End[]

EndPackage[]