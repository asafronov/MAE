(* Mathematica package *)

BeginPackage["MultiAgent`"]
(* Exported symbols added here with SymbolName::usage *)

ClearGlobal[] :=
    ( ClearAll["MultiAgent`*"]; );
    
RemoveGlobal[] :=
    Module[ {},
        Unprotect[AgentNUM, MarketNUM, Ask, Bid];
        Unprotect[TradeStrategies, Production, Learning, Consumption, CreateActions, Prognostication];
        Unprotect[Strategy, Produce, Learn, Consume, CreateAction, Forecast];
        Unprotect[MarketNumber, Title, G1, G2, SessionsInDay, FirstPrice];
        ClearGlobal[];
        Remove["MultiAgent`*"];
        Print["ok"];
    ]

(* db *)

Protect[AgentNUM, MarketNUM, Ask, Bid];
Protect[TradeStrategies, Production, Learning, Consumption, CreateActions, Prognostication];
Protect[Strategy, Produce, Learn, Consume, CreateAction, Forecast];
Protect[MarketNumber, Title, G1, G2, SessionsInDay, FirstPrice];


cRegen::usage = "";
prodFunc::usage = "";

cLinear::usage = "";
cMin::usage = "";
cCD::usage = "";
cCDTech::usage = "";

norm::usage = "";

CEpsPrice::usage = "";
CEpsAmount::usage = "";
   
Begin["`Private`"]

filesql = DirectoryName@FindFile["MultiAgent`"] <> "./../" <>"./Scripts/createpgwide.sql";

conn = $Failed; (* SQL connection*)


End[]

EndPackage[]

