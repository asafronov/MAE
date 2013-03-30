(* Mathematica package *)
	
BeginPackage["MultiAgent`"]
(*	MAEVis = Multi Agents Exchange Visualization *)

(* AGENTS *)
MAEVisAgentCurrency		::usage = "[an]: Shows how (an)-agent main curency is changing";
MAEVisAgentProduce		::usage = "[an]: Shows all production of (an)-agent";
MAEVisAgentConsume		::usage = "[an]: Shows all consumption of (an)-agent";
MAEVisAgentCapital		::usage = "[an]: Shows how (an)-agent portfolio cost is changing";
MAEVisAgentProducts		::usage = "[an]: Shows (an)-agent storage";
MAEVisAgentsDifference	::usage = "[pc]: Shows (pc)richest to (pc)poorest ratio ";
MAEVisAgentsHistogram	::usage = "[bn]: Shows histogram of agents' richness with (bn) beans";

(* GOODS *)
MAEVisProducePart		::usage = "[pn]: Shows amount of producers of (pn)-good";
MAEVisConsumePart		::usage = "[pn]: Shows amount of consumers of (pn)-good";
MAEVisDailyProduce		::usage = "[pn]: Shows daily production of (pn)-good";
MAEVisDailyConsume		::usage = "[pn]: Shows daily consumption of (pn)-good";
MAEVisAverageProduce	::usage = "[pn]: Shows average production of (pn)-good (total prod a day / prod agents number)";
MAEVisAverageConsume	::usage = "[pn]: Shows average consumption of (pn)-good (total cons a day / cons agents number)";
MAEVisTotalProduction	::usage = "[pn]: Shows cumulative production of (pn)-good";
MAEVisTotalConsumption  ::usage = "[pn]: Shows cumulative consumption of (pn)-good";
MAEVisGoodStorage		::usage = "[pn]: Shows (pn)-good storage dynamics by agents";
MAEVisTotalStorage		::usage = "[pn]: Shows total (pn)-good storage dynamics";

(* MARKETS *)
MAEVisMarketBook		::usage = "[mn, dn, sn]: Shows (mn)-market book in the (dn)-day before (sn)-session start";
MAEVisMarketBooksAll	::usage = "[mn]: Shows all (mn)-market books for all traded days and sessions";
MAEVisMarketAuction		::usage = "[mn, dn, sn]: Shows double auction picture";
MAEVisPricePrognose		::usage = "[an, mn] or [an, g1, g2]: Shows price dynamics with prognoses of [an]-agent";
MAEVisMarketVolume		::usage = "[mn]: Shows (mn)-market volume by day/session";
MAEVisMarketOrdersSize	::usage = "[mn]: Shows (mn)-market ask/bid orders amounts by day/session";
MAEVisMarketDealsSize	::usage = "[mn]: Shows (mn)-market deals amounts by day/session";
MAEVisAllMarketVolume		::usage = "[]: Shows total markets volume by day/session";
MAEVisAllMarketOrdersSize	::usage = "[]: Shows total ask/bid orders amounts by day/session";
MAEVisAllMarketDealsSize	::usage = "[]: Shows total orders amounts by day/session";

(* TABLES *)
MAEVisMarketDealsTable		::usage = "[mn]: Table of deals on (mn)-market"
MAEVisAgentActionsTable 	::usage = "[an]: Table of all actions by (an)-agent";

(* ? *)
MAEVisMarketPriceQuantity	::usage = "Visualization of market price and quantity/volume of realised goods";

Begin["`Private`"]

(* AGENTS *)

MAEVisAgentCurrency[an_] :=
	Module[ {curs},
		If[!MemberQ[agentsNums, an], 
        	Throw["NoSuchAgent"] ];
		
		curs = hAgentsCurrency[[All, an]];
		
		ListLinePlot[Tooltip[curs],
			GridLines -> Automatic,
			GridLinesStyle -> Directive[LightGray, Dashed],
			Mesh -> All,
			PlotMarkers -> {Automatic, Medium}, 
			AxesLabel -> {"Day", "main currency"}, 
        	PlotLabel -> "Agent " <> ToString[an]
		]
	]

MAEVisAgentProduce[an_] :=
	Module[ {prods},
		If[!MemberQ[agentsNums, an], 
        	Throw["NoSuchAgent"] ];
		
		prods = hAgentsProduced[[All, an, All]];
		
		BarChart[prods,
			 ChartLayout -> "Stacked", 
             ChartLegends -> Placed[productsNames, Above], 
             ChartLabels -> {Placed[Rest@dayStr, Axis], {""}},
             GridLines -> Automatic,
			 GridLinesStyle -> Directive[LightGray, Dashed],
             AxesLabel -> {"Day", "Amount"},
             PlotLabel -> "Agent " <> ToString[an]
		]
	]

MAEVisAgentConsume[an_] :=
	Module[ {cons},
		If[!MemberQ[agentsNums, an], 
        	Throw["NoSuchAgent"] ];
		
		cons = hAgentsConsumed[[All, an, All]];
		
		BarChart[cons, 
			 ChartLayout -> "Stacked", 
			 ChartLegends -> Placed[productsNames, Above],
			 ChartLabels -> {Placed[Rest@dayStr, Axis], {""}}, 
			 GridLines -> Automatic,
			 GridLinesStyle -> Directive[LightGray, Dashed],
			 AxesLabel -> {"Day", "Amount"},
			 PlotLabel->"Agent " <> ToString[an]
         ]
	]
	
MAEVisAgentCapital[an_] :=
	Module[ {amounts},
        If[!MemberQ[agentsNums, an], 
        	Throw["NoSuchAgent"] ];
        	
        amounts = hAgentsCapitals[[All, an]];
        
        BarChart[amounts,
        	ChartLabels -> Rest@dayStr,
        	GridLines -> Automatic,
			GridLinesStyle -> Directive[LightGray, Dashed],
        	AxesLabel -> {"Day", "Capital (in currency)"}, 
        	PlotLabel -> "Agent " <> ToString[an]
    	]
    ]
        
MAEVisAgentProducts[an_] :=
    Module[ {hist},
        If[!MemberQ[agentsNums, an], 
        	Throw["NoSuchAgent"] ];
        	
        hist = { hAgentsGoodsStart[[an]] } ~ Join ~ hAgentsGoods[[All, an]];
        
        BarChart[hist, 
        	ChartLayout -> "Stacked", 
			ChartLegends -> Placed[productsNames, Above], 
			ChartLabels -> {Placed[dayStr, Axis], {""}},
			GridLines -> Automatic,
			GridLinesStyle -> Directive[LightGray, Dashed], 
			AxesLabel -> {"Day", "Amount"}, 
			PlotLabel -> "Agent " <> ToString[an]
		]
    ]
    
MAEVisAgentsDifference[pc_] :=
	Module[ {res = {}, num, temparr},
		
		num = Floor[Length@agentsNums * pc * 0.01];
		num = Max[num, 1];
		
		Function[dn,
			
			temparr = hAgentsCapitals[[dn, All]];
			Sort[temparr];
			
			AppendTo[res, Total@Take[temparr, -num] / Total@Take[temparr, num] ]; 
		] /@ Range@dayN;
		
		ListLinePlot[Tooltip[res],
			GridLines -> Automatic,
			GridLinesStyle -> Directive[LightGray, Dashed],
			AxesLabel -> {"Day", "Rate"},
			Mesh -> All,
			PlotMarkers -> {Automatic, Medium},  
        	PlotLabel -> ToString[pc] <>"% rich to " <> ToString[pc] <> "% poor rate"
		]
	];
	
MAEVisAgentsHistogram[bin_] :=
	DynamicModule[{xday = dayN},
		Column@{
			Slider[Dynamic[xday], {1, dayN, 1}], Dynamic[xday],
			Dynamic[
				Histogram[hAgentsCapitals[[xday]], bin,
					GridLines -> Automatic,
					GridLinesStyle -> Directive[LightGray, Dashed],
					PlotLabel -> {"End of day", Dynamic[xday] }
				]
			]
		}
	];

    
(* GOODS *)

MAEVisProducePart[pn_] :=
	Module[ {str, part},
		If[!MemberQ[productsNums, pn], 
        	Throw["NoSuchProduct"] ];

		part = 1 / Length@agentsNums;
		str = ({part, If[agentsProds[#] == pn, 1, 0] } ) &/@ agentsNums;
		
		SectorChart[str,
			ChartLegends -> Placed[agentsNums, Above],
			ChartStyle -> "Rainbow",
			PolarAxes -> Automatic,
			PlotLabel -> "Good " <> ToString[pn] <> " : " <> productsNames[[pn]]
		]
	]

MAEVisConsumePart[pn_] :=
	Module[ {str, part},
		If[!MemberQ[productsNums, pn], 
        	Throw["NoSuchProduct"] ];
        	
		part = 1 / Length@agentsNums;
		str = ({part, If[agentsCons[#] == pn, 1, 0] } ) &/@ agentsNums;
		
		SectorChart[str,
			ChartLegends -> Placed[agentsNums, Above], 
			ChartStyle -> "Rainbow",
			PolarAxes -> Automatic,
			PlotLabel -> "Good " <> ToString[pn] <> " : " <> productsNames[[pn]]
		]
	]

MAEVisDailyProduce[pn_] :=
	Module[ {agentsN, tot},
		If[!MemberQ[productsNums, pn], 
        	Throw["NoSuchProduct"] ];

		agentsN = Length@agentsNums;
		tot = (Total /@ hAgentsProduced[[All, All, pn]] ); 
		
		BarChart[tot,
			ChartLabels -> Rest@dayStr,
			GridLines -> Automatic,
			GridLinesStyle -> Directive[LightGray, Dashed],
			AxesLabel -> {"Day", "Value"}, 
        	PlotLabel -> "Good " <> ToString[pn] <> " : " <> productsNames[[pn]]
    	]
	]

MAEVisDailyConsume[pn_] :=
	Module[ {agentsN, tot},
		If[!MemberQ[productsNums, pn], 
        	Throw["NoSuchProduct"] ];

		agentsN = Length@agentsNums;
		tot = (Total /@ hAgentsConsumed[[All, All, pn]] ); 
		
		BarChart[tot,
			ChartLabels -> Rest@dayStr, 
			GridLines -> Automatic,
			GridLinesStyle -> Directive[LightGray, Dashed],
			AxesLabel -> {"Day", "Value"}, 
        	PlotLabel -> "Good " <> ToString[pn] <> " : " <> productsNames[[pn]]
    	]
	]

MAEVisAverageProduce[pn_] :=
	Module[ {agentsN, avg, c},
		If[!MemberQ[productsNums, pn], 
        	Throw["NoSuchProduct"] ];

		agentsN = Length@agentsNums;
		avg = (c = (agentsN - Count[#, 0]); If[c == 0, 0, Total[#] / c]) &/@ hAgentsProduced[[All, All, pn]];
		
		BarChart[avg,
			ChartLabels -> Rest@dayStr,
			GridLines -> Automatic,
			GridLinesStyle -> Directive[LightGray, Dashed], 
			AxesLabel -> {"Day", "Value"}, 
        	PlotLabel -> "Good " <> ToString[pn] <> " : " <> productsNames[[pn]]
    	]
	]

MAEVisAverageConsume[pn_] :=
	Module[ {agentsN, avg, c},
		If[!MemberQ[productsNums, pn], 
        	Throw["NoSuchProduct"] ];

		agentsN = Length@agentsNums;
		avg = (c = (agentsN - Count[#, 0]); If[c == 0, 0, Total[#] / c]) &/@ hAgentsConsumed[[All, All, pn]] ;
		
		BarChart[avg,
			ChartLabels -> Rest@dayStr,
			GridLines -> Automatic,
			GridLinesStyle -> Directive[LightGray, Dashed], 
			AxesLabel -> {"Day", "Value"}, 
        	PlotLabel -> "Good " <> ToString[pn] <> " : " <> productsNames[[pn]]
    	]
	]

MAEVisTotalProduction[pn_] :=
    Module[ {amounts},
        If[!MemberQ[productsNums, pn], 
        	Throw["NoSuchProduct"] ];
        	
        amounts = Accumulate[ hTotalProduced[[All, pn]] ];
        
        BarChart[amounts,
        	ChartLabels -> Rest@dayStr,
        	GridLines -> Automatic,
			GridLinesStyle -> Directive[LightGray, Dashed], 
        	AxesLabel -> {"Day", "Produced Total"}, 
        	PlotLabel -> "Good " <> ToString[pn] <> " : " <> productsNames[[pn]]
    	]
    ]
    
MAEVisTotalConsumption[pn_] :=
    Module[ {amounts},
        If[!MemberQ[productsNums, pn], 
        	Throw["NoSuchProduct"] ];
        	
        amounts = Accumulate[ hTotalConsumed[[All, pn]] ];
        
        BarChart[amounts,
        	ChartLabels -> Rest@dayStr,
			GridLines -> Automatic,
			GridLinesStyle -> Directive[LightGray, Dashed], 
        	AxesLabel -> {"Day", "Produced Total"}, 
        	PlotLabel -> "Good " <> ToString[pn] <> " : " <> productsNames[[pn]]
    	]
    ]

MAEVisGoodStorage[pn_] :=
	Module[ {val},
		If[!MemberQ[productsNums, pn], 
        	Throw["NoSuchProduct"] ];
        	
		val = { hAgentsGoodsStart[[All, pn]] } ~ Join ~ hAgentsGoods[[All, All, pn]];
		
		BarChart[val, 
			ChartLayout -> "Stacked", 
			ChartLegends -> Placed[agentsNums, Above],
			ChartLabels -> {Placed[dayStr, Axis], {""}},
			ChartStyle -> "Rainbow", 
			GridLines -> Automatic,
			GridLinesStyle -> Directive[LightGray, Dashed],
            AxesLabel -> {"Day", "Amount"}, 
            PlotLabel -> "Good " <> ToString[pn] <> " : " <> productsNames[[pn]]
        ]
	]

MAEVisTotalStorage[pn_] :=
    Module[ {st},
        If[!MemberQ[productsNums, pn], 
        	Throw["NoSuchProduct"] ];
        	
        st = {Total @ hAgentsGoodsStart[[All, pn]] } ~ Join ~ (Total /@ hAgentsGoods [[All, All, pn]]);
        
        BarChart[st,
        	ChartLabels -> dayStr,
        	GridLines -> Automatic,
			GridLinesStyle -> Directive[LightGray, Dashed], 
        	AxesLabel -> {"Day", "Amount"},
            PlotLabel -> "Good " <> ToString[pn] <> " : " <> productsNames[[pn]]
		]
    ]

(* MARKETS *)

MAEVisMarketBooksAll[mn_] :=
	MAEVisMarketBook[mn, #[[1]], #[[2]]] &/@ Flatten[ Outer[ List, Range@dayN, Range@sesN ], 1 ]

MAEVisMarketBook[mn_, dn_, sn_] :=
    Module[ {asks, bids, ap, bp, aq, bq, x, y},
    	If[!MemberQ[marketsNums, mn], 
        	Throw["NoSuchMarket"] ];
    	
        asks = hMarketsBooks[[mn, dn, sn, 1]];
        bids = hMarketsBooks[[mn, dn, sn, 2]];
        
        aq = asks[[All, 1]];
        ap = asks[[All, 2]];
        
        bq = bids[[All, 1]];
        bp = bids[[All, 2]];
        
        {x, y} = MapThread[List, #] &/@ {{ap, -aq}, {bp, bq}};
        
        If[Length@x == 0, x = {{}, {}}];
        If[Length@y == 0, y = {{}, {}}];
        Rotate[
	        ListPlot[Tooltip[{x, y}], 
				FillingStyle -> {{Thick, Lighter@Red}, {Thick, Lighter@Green}}, 
				Filling -> Axis,
				GridLines -> Automatic,
				GridLinesStyle -> Directive[LightGray, Dashed],
				AxesLabel -> {Rotate["Price", -90 Degree], Rotate["Size", -90 Degree]}, 
				PlotMarkers -> {Automatic, Medium}, 
				PlotStyle -> {Red, Green},
				(*PlotRange -> {{0, Automatic},{Automatic, Automatic}},*) 
				PlotLegends -> Placed[{Rotate["asks", -90 Degree], Rotate["bids", -90 Degree]}, Bottom],
				PlotLabel -> ("Market " <> ToString[mn] <> ", Day " <> ToString[dn] <> ", Ses " <> ToString[sn])
			], 90 Degree
		]
    ];

MAEVisMarketAuction[mn_, dn_, sn_] :=
	Module[ {ind},
		If[!MemberQ[marketsNums, mn], 
        	Throw["NoSuchMarket"] ];
		
		ind = (dn - 1) * sesN + sn;
		If[Length@hAuctionGraphs[[mn, ind]] > 0,
			ReleaseHold@hAuctionGraphs[[mn, ind]],
				Print["No orders on " <> ToString@mn <> " market; day/ses: " <> ToString@dn <> "/" <> ToString@sn]
		] 		
	];
   
MAEVisMarketAuctionPattern[aq_, ap_, bq_, bp_] :=
	Module[{func, temp, aRanges, bRanges, s, f, maxx = 0, x, asks, bids},

		func := (
			temp = {};
		    For[i = 1, i <= Length@#, i++,
		    	s = If[i == 1, 0, #[[i - 1]]];
		    	f = #[[i]];
		    	maxx = Max[maxx, f];
		     	AppendTo[temp, {s, f} ];
		    ];
		    temp
		) &;
		
		{aRanges, bRanges} = func /@ {aq, bq};
		
		asks = MapThread[ {#1, x > #2[[1]] && x <= #2[[2]]} &, {ap, aRanges}];
		bids = MapThread[ {#1, x > #2[[1]] && x <= #2[[2]]} &, {bp, bRanges}];
		
		Plot[Tooltip[ {	Piecewise[asks], Piecewise[bids] } ], 
			{x, 0, maxx},
			Filling -> Axis,
			GridLines -> Automatic,
			GridLinesStyle -> Directive[LightGray, Dashed], 
			PlotStyle -> {{Red, Thick}, {Green, Thick}}, 
			AxesLabel -> {"Q","P"},  
			PlotLegends -> Placed[{"asks","bids"}, Right],  
			PlotRange -> {{0, Automatic}, {0, Automatic}}
		]
	];
	

MAEVisPricePrognose[an_, g1_, g2_] :=
	MAEVisPricePrognose[an, getMarketNumber[g1, g2]]

MAEVisPricePrognose[an_, mn_] :=
    Module[ {ses, dp, dm},
    	If[!MemberQ[agentsNums, an], 
        	Throw["NoSuchAgent"] ];
        If[!MemberQ[marketsNums, mn], 
        	Throw["NoSuchMarket"] ];
    	
    	ses = Length@First@First@hMarketsPrices;
        {dp, dm} = {Table[#, {ses}] &/@ hMarketsPrognoses[[mn, an]] // Flatten, hMarketsPrices[[mn]] // Flatten};
        
        ListLinePlot[Tooltip[{dp, dm}], 
             Mesh -> All,
             Filling -> Axis,
             GridLines -> Automatic,
             GridLinesStyle -> Directive[Gray, Dashed],
             PlotStyle -> {Blue, Red}, 
             (*PlotRange -> {{Automatic, Automatic}, {0, Automatic}},*)
             PlotLegends -> {"Prognose", "Market Price"}, 
             AxesLabel -> {"Sum Session", "Price"},
             PlotLabel -> ("Agent " <> ToString[an] <> ", Market " <> ToString[mn] <> ": " <> OptionValue[markets[mn], Title])
        ]
    ]
	
MAEVisMarketVolume[mn_] :=
	Module[{deals, vols = 0},
		If[!MemberQ[marketsNums, mn], 
        	Throw["NoSuchMarket"] ];
        
        deals = hMarketsDeals[[mn]];
        vols = (0.5 * Total@Abs@#)& /@ Flatten[deals[[All, All, All, 2, 2]], 1];
        BarChart[vols, 
        	GridLines -> Automatic,
			GridLinesStyle -> Directive[LightGray, Dashed], 
        	AxesLabel -> {"Day/Ses", "Volume"},
        	ChartLabels -> daySesStr, 
        	PlotLabel -> "Market " <> ToString[mn] <> " : " <> OptionValue[markets[mn], Title]
		]	
	];
	
MAEVisAllMarketVolume[] :=
	Module[{allvols = {}},
		
		Function[mn,
			AppendTo[allvols, (0.5 * Total@Abs@#)& /@ Flatten[hMarketsDeals[[mn, All, All, All, 2, 2]], 1] ];
		] /@ marketsNums;
		
		BarChart[Total@allvols, 
        	GridLines -> Automatic,
			GridLinesStyle -> Directive[LightGray, Dashed],
			ChartLabels -> daySesStr, 
        	AxesLabel -> {"Day/Ses", "Volume"}
		]
	];

MAEVisMarketOrdersSize[mn_] :=
	Module[{askSz, bidSz, res},
		If[!MemberQ[marketsNums, mn], 
        	Throw["NoSuchMarket"] ];
        	
		askSz = Length /@ Flatten[hMarketsBooks[[mn, All, All, 1]], 1];
 		bidSz = Length /@ Flatten[hMarketsBooks[[mn, All, All, 2]], 1];
 		
 		res = MapThread[{#1, #2}&, {askSz, bidSz}];
 		
 		BarChart[res,
			ChartLayout -> "Stacked",
			ChartLegends -> Placed[{"Asks", "Bids"}, Right], 
			ChartLabels -> {Placed[daySesStr, Axis], {"", ""}},  
        	GridLines -> Automatic,
			GridLinesStyle -> Directive[LightGray, Dashed], 
        	AxesLabel -> {"Day/Ses", "Amount"},
        	PlotLabel -> "Market " <> ToString[mn] <> " : " <> OptionValue[markets[mn], Title]
		]
	];
	
MAEVisAllMarketOrdersSize[] :=
	Module[{askSzs = {}, bidSzs = {}, res},
		Function[mn,
			AppendTo[askSzs, Length /@ Flatten[hMarketsBooks[[mn, All, All, 1]], 1] ];
 			AppendTo[bidSzs, Length /@ Flatten[hMarketsBooks[[mn, All, All, 2]], 1] ];
		] /@ marketsNums;
		
		res = MapThread[{#1, #2}&, {Total@askSzs, Total@bidSzs}];
		
		BarChart[res,
			ChartLayout -> "Stacked",
			ChartLegends -> Placed[{"Asks", "Bids"}, Right],
			ChartLabels -> {Placed[daySesStr, Axis], {"", ""}},  
        	GridLines -> Automatic,
			GridLinesStyle -> Directive[LightGray, Dashed], 
        	AxesLabel -> {"Day/Ses", "Amount"}
		]
	];
	
MAEVisMarketDealsSize[mn_] :=
	Module[{dealSz},
		If[!MemberQ[marketsNums, mn], 
        	Throw["NoSuchMarket"] ];
		
		dealSz = Length /@ Flatten[hMarketsDeals[[mn, All, All, All, 1]], 1];
				
		BarChart[dealSz, 
			ChartLabels -> daySesStr,  
        	GridLines -> Automatic,
			GridLinesStyle -> Directive[LightGray, Dashed], 
        	AxesLabel -> {"Day/Ses", "Amount"},
        	PlotLabel -> "Market " <> ToString[mn] <> " : " <> OptionValue[markets[mn], Title]
		]
	];

MAEVisAllMarketDealsSize[] :=
	Module[{dealSzs = {}},
		Function[mn,
 			AppendTo[dealSzs, Length /@ Flatten[hMarketsDeals[[mn, All, All, All, 1]], 1] ];
		] /@ marketsNums;
		
		BarChart[Total@dealSzs,
			ChartLabels -> daySesStr,
        	GridLines -> Automatic,
			GridLinesStyle -> Directive[LightGray, Dashed], 
        	AxesLabel -> {"Day/Ses", "Amount"}
		]
	];
	
	
MAEVisMarketDealsTable[mn_] :=
	Module[{daysNums, sesNums, res, body, head, gnm1, gnm2, totind},
		If[!MemberQ[marketsNums, mn], 
        	Throw["NoSuchMarket"] ];
        
        totind = Last@agentsNums + 1;
        
        gnm1 = StringJoin@If[ Length@# >= 3, #[[1 ;; 3]], # ]& @ Characters@productsNames[[marketGoods[mn][[1]] ]];
        gnm2 = StringJoin@If[ Length@# >= 3, #[[1 ;; 3]], # ]& @ Characters@productsNames[[marketGoods[mn][[2]] ]];
        	
		daysNums = Range@dayN;
		sesNums = Range@sesN;
		
		head = {"DAY/SES"};
		Last@(AppendTo[head, "a." <> ToString@#] & /@ agentsNums);
		AppendTo[head, "\[CapitalSigma]"];
		
		body = {};
		Module[{first, deals, str, ag, d1, d2, tot = 0},
			Function[dn,
				Function[sn,
					deals = hMarketsDeals[[mn, dn, sn]];
			    	first = ToString@dn <> "/" <> ToString@sn;
			    	str = ("---") &/@ agentsNums;
			    	Function[deal,
			    		ag = deal[[1]];
			    		d1 = deal[[2,1]];
			    		d2 = deal[[2,2]];
			    		tot += d2;
	    				str[[ag]] = If[d2 > 0, "[B]", "[S]"] 
	    								<> ToString@Round2@Abs@d2 <> gnm2 
	    								<> "/" 
	    								<> ToString@Round2@Abs@d1 <> gnm1;
			    	] /@ deals;
			    	AppendTo[str, Round@tot];
			    	
			    	AppendTo[body, {first} ~ Join ~ str];
		      	] /@ sesNums;
			] /@ daysNums;
		];
		res = {head}~Join~body;
		Labeled[
			Grid[res, 
				Frame -> All, 
				ItemStyle -> Directive[FontSize -> 10], 
				Background -> {{1 -> Pink}, {1 -> Gray}}
			],
			" \nMarket " <> ToString@mn <> ": " <> ToString@OptionValue[markets[mn], Title], 
			Top
		]
	];
	
MAEVisAgentActionsTable[an_] :=
	Module[ {res, daysNums, sesNums, head, head2, body, g1, g2, gsStr = {}},
		If[!MemberQ[agentsNums, an], 
        	Throw["NoSuchAgent"] ];
        	
        daysNums = Range@dayN;
		sesNums = Range@sesN;
		
		head = {"DAY/SES", "Produce", "Orders", "Deals", "Consume", "Storage After All"};
				
		If[hAgentsGoodsStart[[an, #]] > CEpsAmount, 
			AppendTo[gsStr, ToString@Round2@hAgentsGoodsStart[[an, #]] <> " " <> productsNames[[#]] ] 
		] &/@ productsNums;
		head2 = {"0", "---", "---", "---", "---", Column@gsStr};
		
		body = {};
		Module[{str, cval, consStr, pval, prodStr, ord, ordsStr, dls, dlsStr, gds, gdsStr},
			Function[dn,
				Function[sn,
					str = { ToString@dn <> "/" <> ToString@sn };
					
					gdsStr = {};
					If[sn == 1,
							cval = Select[hAgentsConsumed[[dn, an]], Positive];
							consStr = If[cval == {}, "---",
								ToString@Round2@First@cval <> " " <> productsNames[[ First@First@Position[hAgentsConsumed[[dn, an]], First@cval] ]] ];
							
							pval = Select[hAgentsProduced[[dn, an]], Positive];
							prodStr = If[pval == {}, "---",
								ToString@Round2@First@pval <> " " <> productsNames[[ First@First@Position[hAgentsProduced[[dn, an]], First@pval] ]] ];
							
							gds = hAgentsGoods[[dn, an]];
							(If[gds[[#]] > CEpsAmount, AppendTo[gdsStr, ToString@Round2@gds[[#]] <> " " <> productsNames[[#]] ] ]) &/@ productsNums;
							gdsStr = Column@gdsStr;
							,
									consStr = SpanFromAbove;
									prodStr = SpanFromAbove;
									gdsStr = SpanFromAbove;
					];
					
					
					AppendTo[str, prodStr];
					
					ordsStr = {};
					dlsStr = {};
					Function[mn,
						ord = Select[Flatten[hMarketsBooks[[mn, dn, sn, All]], 1], owner[#] == an&];
						If[Length@ord != 0,
							ord = ( 
								ToString@#[[3]] <> " " <> ToString@Round2@size@# <> marketSymbols[mn][[2]] <> " x " <> ToString@Round2@price@# <> marketSymbols[mn][[1]] 
							)&/@ ord;
							AppendTo[ordsStr, "m" <> ToString@mn <> ": " <> ToString@ord ]
						];
						
						dls = Select[hMarketsDeals[[mn, dn, sn]], First@# == an&];
						If[Length@dls != 0,
							g1 = (First@dls)[[2,1]];
				    		g2 = (First@dls)[[2,2]];
				    		
							AppendTo[dlsStr, "m" <> ToString@mn <> ": " 
								<> If[g2 > 0, "[B]", "[S]"] 
								<> ToString@Round2@Abs@g2 <> marketSymbols[mn][[2]] 
								<> "/" 
								<> ToString@Round2@Abs@g1 <> marketSymbols[mn][[1]]
							];
						];
						
						If[Length@dls > 1, Print["More than 1 deal!", mn, an] ];
							
					] /@ marketsNums;
										
					AppendTo[str, If[Length@ordsStr == 0, "---", Column@ordsStr] ];
					AppendTo[str, If[Length@dlsStr == 0, "---", Column@dlsStr] ];
					
					AppendTo[str, consStr];
					AppendTo[str, gdsStr];
					
					AppendTo[body, str];
				] /@ sesNums;
			] /@ daysNums;
		];
                
        res = {head} ~ Join ~ {head2} ~ Join ~ body;
        
        Labeled[
			Grid[res, 
				Frame -> All, 
				ItemStyle -> Directive[FontSize -> 10], 
				Background -> {{1 -> Pink}, {1 -> LightGray}}			
			],
			" \nAgent " <> ToString@an, 
			Top
		]
	];
  
  
MAEVisMarketPriceQuantity[mn_] :=
    Module[ {h, dm, df = {2013, 1, 1}, dataup, dates, data, max, min},
        h = hMarketsDays[[mn, 2;; ,{1, 2}]];
        If[(And @@ (Length@# == 0 & /@ {h})), 
        	Throw["NoData"] ];
        dm = {h[[All, 1]], h[[All, 2]]};
        {max, min} = #@dm[[1]] &/@ {Max,Min};
        dataup = MapThread[Join[{1, min, max}, {#1}, {#2}] &, dm];
        dates = DatePlus[df, # - 1] & /@ Range@Length@dm[[1]];
        data = MapThread[List, {dates, dataup}];
        
        TradingChart[data, 
        	Appearance -> "Line", 
        	PlotLabel -> "Market " <> ToString@mn
        ]
    ]
    
MAEVisMarketData[mn_] :=
	Module[ {data },
		
		
		TradingChart[data,
			PlotLabel -> "Market " <> ToString@mn
		]	
	]
     
End[]

EndPackage[]