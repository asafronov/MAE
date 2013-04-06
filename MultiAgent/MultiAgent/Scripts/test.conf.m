BeginPackage["MultiAgent`"]

(*	
PR[p_] := 
	Delete[PARAMS, Position[PARAMS, (x_) /; (First@x == First@p)]];
	PJ[p];
*)
Begin["`Private`"]

PARAMS = {};
PJ[p_] := 
	AppendTo[PARAMS, p];

(*
 VAR = VALUE; means that we redefine VAR
 VAR -> VALUE //PJ; means that we add parameter VAR->VALUE to init function
*)

(* ----------------------------- config data ----------------------------- *)

(* Type of currency system *)
MoneySystem = "Mono";

(* number of days to simulate *)
Days -> 5 // PJ;

(* number of products, total, have to be more than NumRes *)
NumberOfProducts -> 5 // PJ;

(* number of Agents *) 
AgentsAmount -> 10 // PJ;

(* start amount of main product (index = 1, Money) *)
StartMoney -> 10 // PJ;

(* index of production function, 0 = strProdRandom *)
ProdFuncInd -> 1 // PJ;

(* number of sessions in a day for each market *) 
SessionsInDay -> 2 // PJ;

(* the price, which all markets start trading from: {a,b} <=> Random from a to b *)
FirstPrice -> {1, 2} // PJ;


(* CONSTANTS *)

(* list of available products *)
ResourceList = {"Money", "Labor", "Water", "Energy", "Ferrum", "Gold", "Oil", "Wheat", "Coil", "Silver", "Platinum", "Bitcoin", "Teabags"};

(* types of production function *)
ProdFunctions = {"Linear", "Min", "CD", "CES"};

(* range of values (min, max) for lambda coefs genetations *)
CLambdaRange = {0.1, 0.9};

(* range of values (min, max) for cons Norm generations *)
CNormRange = {0.1, 0.9};

(* max discount part, which agents can apply to predicted price while selling *)
CMaxDiscount = 0.1;

(* dist to generate daily regeneration of resources; trancate with {? , 0} leads to no produce *)
CRegen = TruncatedDistribution[ {-1, 3}, NormalDistribution[0.0, 5.0] ];

(* dist to generate goods quality at start *)
CQuantGoods = TruncatedDistribution[ {0, 20}, NormalDistribution[10, 8.5] ];

(* dist to generate coefs for linear production function *)
CLinearProduce = TruncatedDistribution[ {0.1, 3}, NormalDistribution[1.2, 0.5] ];

(* dist to generate coefs for Min production function *)
CMinProduce = TruncatedDistribution[ {0.1, 6}, NormalDistribution[4, 2] ];

(* dist to generate coefs for Cobb Douglas production function *)
CCDTech = TruncatedDistribution[ {0, 5}, NormalDistribution[1.0, 0.5] ];

CCDProduce = TruncatedDistribution[ {0, 2}, NormalDistribution[0.8, 1.0] ];


(* other constants *)
CEpsAmount = 0.001;
CEpsPrice = 0.0001;
DebugTrade = False;

End[]

EndPackage[]
