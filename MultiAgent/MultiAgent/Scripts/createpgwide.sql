/*==============================================================*/
/* DBMS name:      PostgreSQL 8                                 */
/* Created on:     9/7/2012 12:38:05 AM                         */
/*==============================================================*/


drop table AGENTS;

drop table AGENTTYPES;

drop table CHANGES;

drop table HAGENTSCHANGES;

drop table HAGENTSCONSUME;

drop table HAGENTSPRODUCE;

drop table HAGENTSPROGNOSEPRICE;

drop table HAGENTSSELLBUY;

drop table HMARKETSDAYS;

drop table HMARKETSSES;

drop table MARKETS;

drop table ORDERBOOK;

drop table PRODUCTS;

drop table STRATEGIES;

/*==============================================================*/
/* Table: AGENTS                                                */
/*==============================================================*/
create table AGENTS (
   ID                   SERIAL not null,
   DESCRIPTION          VARCHAR(1)           null,
   constraint PK_AGENTS primary key (ID)
);

/*==============================================================*/
/* Table: AGENTTYPES                                            */
/*==============================================================*/
create table AGENTTYPES (
   ID                   SERIAL not null,
   DESCRIPTION          VARCHAR(1)           null,
   constraint PK_AGENTTYPES primary key (ID)
);

/*==============================================================*/
/* Table: CHANGES                                               */
/*==============================================================*/
create table CHANGES (
   ID                   SERIAL not null,
   PARTIALLY            BOOL                 null,
   QUANTITYG1           FLOAT8               null,
   QUANTITYG2           FLOAT8               null,
   ORDERID              INT4                 null,
   constraint PK_CHANGES primary key (ID)
);

/*==============================================================*/
/* Table: HAGENTSCHANGES                                        */
/*==============================================================*/
create table HAGENTSCHANGES (
   ID                   SERIAL not null,
   AGENTID              INT4                 null,
   DAY                  INT4                 null,
   constraint PK_HAGENTSCHANGES primary key (ID)
);

/*==============================================================*/
/* Table: HAGENTSCONSUME                                        */
/*==============================================================*/
create table HAGENTSCONSUME (
   ID                   SERIAL not null,
   AGENTID              INT4                 null,
   DAY                  INT4                 null,
   constraint PK_HAGENTSCONSUME primary key (ID)
);

/*==============================================================*/
/* Table: HAGENTSPRODUCE                                        */
/*==============================================================*/
create table HAGENTSPRODUCE (
   ID                   SERIAL not null,
   AGENTID              INT4                 null,
   DAY                  INT4                 null,
   constraint PK_HAGENTSPRODUCE primary key (ID)
);

/*==============================================================*/
/* Table: HAGENTSPROGNOSEPRICE                                  */
/*==============================================================*/
create table HAGENTSPROGNOSEPRICE (
   ID                   SERIAL not null,
   AGENTID              INT4                 null,
   DAY                  INT4                 null,
   constraint PK_HAGENTSPROGNOSEPRICE primary key (ID)
);

/*==============================================================*/
/* Table: HAGENTSSELLBUY                                        */
/*==============================================================*/
create table HAGENTSSELLBUY (
   ID                   SERIAL not null,
   AGENTID              INT4                 null,
   DAY                  INT4                 null,
   constraint PK_HAGENTSSELLBUY primary key (ID)
);

/*==============================================================*/
/* Table: HMARKETSDAYS                                          */
/*==============================================================*/
create table HMARKETSDAYS (
   ID                   SERIAL not null,
   MARKETID             INT4                 null,
   DAY                  INT4                 null,
   AVGPRICE             FLOAT8               null,
   SUMGOOD1             FLOAT8               null,
   constraint PK_HMARKETSDAYS primary key (ID)
);

/*==============================================================*/
/* Table: HMARKETSSES                                           */
/*==============================================================*/
create table HMARKETSSES (
   ID                   SERIAL not null,
   MARKETID             INT4                 null,
   DAY                  INT4                 null,
   SES                  INT4                 null,
   AVGPRICE             FLOAT8               null,
   SUMGOOD1             FLOAT8               null,
   constraint PK_HMARKETSSES primary key (ID)
);

/*==============================================================*/
/* Table: MARKETS                                               */
/*==============================================================*/
create table MARKETS (
   ID                   SERIAL not null,
   GOOD1                INT4                 null,
   GOOD2                INT4                 null,
   constraint PK_MARKETS primary key (ID)
);

/*==============================================================*/
/* Table: ORDERBOOK                                             */
/*==============================================================*/
create table ORDERBOOK (
   ID                   SERIAL not null,
   CHANGETYPE           BOOL                 null,
   DAY                  INT4                 null,
   SESSION              INT2                 null,
   MARKETNUM            INT4                 null,
   QUANTITYG1           FLOAT8               null,
   QUANTITYG2           FLOAT8               null,
   AGENTID1             INT4                 null,
   constraint PK_ORDERBOOK primary key (ID)
);

/*==============================================================*/
/* Table: PRODUCTS                                              */
/*==============================================================*/
create table PRODUCTS (
   ID                   SERIAL not null,
   TITLE                VARCHAR(20)          null,
   DESCRIPTION          VARCHAR(1)           null,
   constraint PK_PRODUCTS primary key (ID)
);

/*==============================================================*/
/* Table: STRATEGIES                                            */
/*==============================================================*/
create table STRATEGIES (
   ID                   SERIAL not null,
   DESCRIPTION          VARCHAR(1)           null,
   FUCNTIONNAME         VARCHAR(1)           null,
   PARAMETERS           VARCHAR(1)           null,
   constraint PK_STRATEGIES primary key (ID)
);

