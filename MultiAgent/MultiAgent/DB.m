(* Mathematica package *)

(* here are all needed to work with DB *)

BeginPackage["MultiAgent`"]
(* Exported symbols added here with SymbolName::usage *) 

LogIt::usage = "LogIt[type,message] - log message by type";
openDB::usage = "openDB[DB] - open JDBC connection to DB; ex: openDB[\"localhost:5431/superdb\"]";
freeDB::usage = "freeDB[] - execute script that drop all tables and create new";
closeDB::usage = "closeDB[] - close SQL connection. Really openDB - close last one if it's already exists";

createExampleDB::usage = "createExampleDB[] - insert into DB example values values";
loadDBProductNames::usage = "load products from DB";

Begin["`Private`"]
(* Implementation of the package *)

Needs["DatabaseLink`"]

Options[LogIt] :=
    {StorageType -> DB}
(*StorageType -> {PrintMessage, AppengToList, DB}*)

LogIt[type_, m___, OptionsPattern[]] :=
    Module[ { mn, an, t, qg1, qg2, orderSave},
    	
        orderSave[zmn_, zan_, zt_, zqg1_, zqg2_, zses_, zday_] :=
            Module[ {},
                Switch[OptionValue[StorageType],
                    PrintMessage, Print[{zmn, zan, zt, zqg1, zqg2, zses, zday}],
                    DB, 
                     If[ Head[conn] == SQLConnection,
                         SQLInsert[conn, "ORDERBOOK",
                         {"changetype", "day", "session", "marketnum", "quantityg1", "quantityg2", "agentid1"},
                         {{t === Ask, zday, zses, zmn, zqg1, zqg2, zan}}];
                     ],
                    _,True
                 ]
            ];
            
        Switch[type,
            "OrderPack",
                {mn, an, t, qg1, qg2} = m;
                orderSave[mn, an, t, qg1, qg2, sessionNumber, dayNumber],
             _,
             	Print["I can't log message: ",m," with type = ",type];
   ]
    ];

Clear[openDB, closeDB, freeDB];
Clear[loadProducts, loadDBProductNames, loadMarkets, savePack, 
  saveChange];

openDB[db_: "localhost:5432/darkpoolt1"] :=
    Module[ {user = "jok", pass = ""},
        If[ Head[conn] === SQLConnection,
            CloseSQLConnection[conn];
        ];
        conn = OpenSQLConnection[JDBC["PostgreSQL", db], "Username" -> user,
           "Password" -> pass]
    ]

freeDB[] :=
    Module[ {sqlexecfn = "/mnt/data/model/sql/createpg.sql", data},
        sqlexecfn = filesql;
        If[ Head[conn] === SQLConnection,
            data = Import[sqlexecfn, "Text"];
            data = StringReplace[ data, "/*" ~~ ShortestMatch[__] ~~ "*/" -> ""];
            data = StringSplit[StringReplace[data, {"\n\n" -> "\n", "\n" -> ""}], 
              "\n"];
            SQLExecute[ conn, #] & /@ Select[data, # != "" &]
        ]
    ];
  
closeDB[] :=
    Module[ {},
        If[ Head[conn] === SQLConnection,
            CloseSQLConnection[conn];
        ]
    ];


createExampleDB[] :=
    Module[ {exampleProducts},
        If[ Head[conn] === SQLConnection,
            exampleProducts[] :=
                Module[ {},
                    SQLInsert[conn, 
                     "PRODUCTS", {"title", "description"}, 
                         {{"Gold", ""}, {"Ferrum", ""}, {"Corn", ""},{"Paper",""}}]
                ];
            exampleProducts[]
        ]
    ];

loadDBProductNames[] :=
    Module[ {},
        If[ Head[conn] === SQLConnection,
            SQLSelect[conn, "PRODUCTS", {"title"}] // Flatten
        ]
    ];


End[];

EndPackage[]
