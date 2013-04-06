(* Mathematica package *)

BeginPackage["MultiAgent`"]
(* Exported symbols added here with SymbolName::usage *) 

(* METHODS *)

describeOrder			::usage = "returns string, describing an order";
createOrder				::usage = "create an order from arguments";

sendAskOrderByCurSize	::usage = "create ask order by currency size and append to list";
sendBidOrderByCurSuze	::usage = "create bid order ";
sendAskOrderByPrice		::usage = "create ask order by price and append it to list";
sendBidOrderByPrice		::usage = "create bid order by price and append it to list";

tryToSellByLastPrognose	::usage = "[mn, an, qty, usedisc]: use prognosed price to sell (qty) on (mn)-market with discount, if (usedisc)";
tryToBuyByLastPrognose	::usage = "[mn, an, qty]: use prognosed price to buy (qty) on (mn)-market";
tryToBuyAllOnCash		::usage = "[mn, an, money]: sends order to buy as much as we can on alowed cash, using prognosed price";
handleOrderBookDA		::usage = "handling of asks and bids, executes and declines orders";

(* VARS *)

orderNumber		::usage = "last order number = total amount of orders";

Begin["`Private`"]

describeOrder[order_] :=
    "order: " <> ToString[order] <> "\n" <> 
    ToString[{ "Quantity: ", order[[1]], "Price: ", order[[2]],
    	"OrderType: ", order[[3]], "OwnerAgent: ", order[[4]],
    	"OrderNumber: ", order[[5]] } 
    ];
                    
createOrder[mn_, an_, type_, qty_, prc_] :=
    Module[ {},
        (*LogIt["OrderPack", {mn, an, type, q1, q2}];  (* CHECK !!!*) *)
        {qty, prc, type, an, ++orderNumber}
    ]
    
sendAskOrderByCurSize[mn_, an_, qty_, qcur_] :=
    AppendTo[marketsAsks[mn], createOrder[mn, an, Ask, qty, qcur / qty]];
sendBidOrderByCurSuze[mn_, an_, qty_, qcur_] :=
    AppendTo[marketsBids[mn], createOrder[mn, an, Bid, qty, qcur / qty]];
    
sendAskOrderByPrice[mn_, an_, qty_, prc_] :=
    AppendTo[marketsAsks[mn], createOrder[mn, an, Ask, qty, prc]];
sendBidOrderByPrice[mn_, an_, qty_, prc_] :=
    AppendTo[marketsBids[mn], createOrder[mn, an, Bid, qty, prc]];

tryToSellByLastPrognose[mn_, an_, qty_, usedisk_] :=
    Module[ {prognosedPrice, disc},
    	disc = If[!usedisk, 0, estimateDiscount[mn, an] ];
    	prognosedPrice = Last@hMarketsPrognoses[[mn, an]];  	
 
    	sendAskOrderByPrice[mn, an, qty, prognosedPrice * (1 - disc)];
    ];

tryToBuyByLastPrognose[mn_, an_, qty_] :=
    Module[ {prognosedPrice},
        prognosedPrice = Last@hMarketsPrognoses[[mn, an]];
        
        sendBidOrderByPrice[mn, an, qty, prognosedPrice]
    ];
    
tryToBuyAllOnCash[mn_, an_, money_] :=
    Module[ {prognosedPrice, qty},
        prognosedPrice = Last@hMarketsPrognoses[[mn, an]];
        If[prognosedPrice == 0, Print["DANGER! Pprc = 0"]];
        qty = money / prognosedPrice;
        
        sendBidOrderByPrice[mn, an, qty, prognosedPrice]
    ];    

size[order_] := order[[1]];

price[order_] := order[[2]];

type[order_] := order[[3]];

owner[order_] := order[[4]];
    
priceInv[order_] := price[order]^(-1);

handleOrderBookDA[mn_, asks_, bids_] :=
	Module[ {asksSrt, bidsSrt, asksEqPrc, bidsEqPrc, asksTableQ, bidsTableQ, asksTableP, bidsTableP, 
											askInd = 0, bidInd = 0, maxSize = 0, midPrice},
		If[ Length@asks == 0 || Length@bids == 0,
			AppendTo[ hAuctionGraphs[[mn]], {}];
            Return[0];
        ];
											
		asksSrt = SortBy[asks,  price[#]&];
		bidsSrt = SortBy[bids, -price[#]&];
				
		asksEqPrc = Gather[asksSrt, price[#1] == price[#2] &];
		bidsEqPrc = Gather[bidsSrt, price[#1] == price[#2] &];
		
		asksTableQ = Accumulate[ Total[size /@ #]& /@ asksEqPrc ];
		bidsTableQ = Accumulate[ Total[size /@ #]& /@ bidsEqPrc ];
				
		asksTableP = price /@ First /@ asksEqPrc;
		bidsTableP = price /@ First /@ bidsEqPrc; 
		
		AppendTo[ hAuctionGraphs[[mn]], Hold@MAEVisMarketAuctionPattern[asksTableQ, asksTableP, bidsTableQ, bidsTableP] ];	
		
		Module[ {i, j, prcdif, size}, 
			For[i = 1, i <= Length@asksTableP, i++,
				For[j = 1, j <= Length@bidsTableP, j++,
					
					prcdif = bidsTableP[[j]] - asksTableP[[i]];
					If[prcdif < -CEpsPrice,
						If[i == 1 && j == 1, Return [0] ]; (* Best bid is less best ask => no deals *)
						j--;
						Break[];
					];
					
					If[bidsTableQ[[j]] > asksTableQ[[i]],
						Break[];
					];
				];
				
				If[j == (Length@bidsTableP + 1), j--];
							
				size = Min[asksTableQ[[i]], bidsTableQ[[j]]];
				If[size > maxSize,
					maxSize = size;
					askInd = i;
					bidInd = j;
				];
			];
		];
		
		If[maxSize == 0,
			Return[0];   (* may be don't need *)
		];
		 
		midPrice = 0.5 * ( asksTableP[[askInd]] + bidsTableP[[bidInd]] );
		
		Module[ {i, j},
			For[i = 1, i < askInd, i++,
				executeOrders[mn, midPrice, asksEqPrc[[i]] ];
			];
			
			For[j = 1, j < bidInd, j++,
				executeOrders[mn, midPrice, bidsEqPrc[[j]] ];
			];			
		];
		
		executeOrdersPartly[mn, midPrice, (asksTableQ[[askInd]] - maxSize) , asksEqPrc[[askInd]] ];	
		executeOrdersPartly[mn, midPrice, (bidsTableQ[[bidInd]] - maxSize) , bidsEqPrc[[bidInd]] ];
		
		midPrice
	];

executeOrders[mn_, price_, orders__] :=
	Module[{},
		executeOrder[mn, price, #] &/@ orders;
	];
	
executeOrder[mn_, price_, order_, sz_:-1] :=
	Module[ {an, t, g1, g2, q1, q2},
				
		an = owner@order;
		t = type@order;
		
		g1 = marketGoods[mn][[1]];
		g2 = marketGoods[mn][[2]];
		
		q2 = If[sz == -1, size@order, sz];
		q1 = price * q2;
		
		Switch[t,
			Ask,
				goods[ an, g2 ] -= q2;
				goods[ an, g1 ] += q1;
				deals[[an]] += {q1, -q2};
				,
			Bid,
				goods[ an, g2 ] += q2;
				goods[ an, g1 ] -= q1;
				deals[[an]] += {-q1, q2};
				,
			_,
				Return[$Failed];
		];
		
	];

executeOrdersPartly[mn_, price_, deniedSize_, orders__] :=
	Module[ {avalSize, q2},
		avalSize = Total[size /@ orders] - deniedSize;
		
		Function[order,
			If[avalSize > 0,
				q2 = size@order;
				If[q2 <= avalSize,
					(* size inside avalSize *)
					executeOrder[mn, price, order];
					avalSize -= q2,
						(* larger than avalSize *)
						executeOrder[mn, price, order, avalSize];
						avalSize = 0;
				];
			];
				
		] /@ orders;
	];

End[]

EndPackage[]