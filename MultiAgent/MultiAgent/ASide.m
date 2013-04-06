(* Mathematica package *)

BeginPackage["MultiAgent`"]
(* Exported symbols added here with SymbolName::usage *) 

createProducts::usage = "creating product names, numbers, tables";
initAgentProducts::usage = "resets all agent product to null quantity "

productsNums::usage = "numbers of traded products";
productsNames::usage = "name of products";
productsN::usage = "amount of products";

prodByName::usage = "product number by its string-name";

agentGoods::usage = "gets current agent's portfolio by its number";

goods::usage = "";

Options[createProducts] :=
{
	Resources -> ResourceList,
	NumberOfProducts -> 0
}

Begin["`Private`"]
(* Implementation of the package *)

createProducts[OptionsPattern[]] :=
    Module[ {},
    	productsNames = Take[OptionValue[Resources], OptionValue[NumberOfProducts]];
        productsNums = Range@Length@productsNames;
        productsN = Length@productsNums;
        
        goods =.;
        
        ( prodByName[ productsNames[[#]] ] = # ) &/@ productsNums;      
    ]

initAgentProducts[na_] :=
    (goods[na, #] = 0) & /@ productsNums

agentGoods[na_] :=
    goods[na, #] & /@ productsNums

End[]

EndPackage[]
