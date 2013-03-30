/*==============================================================*/
/* DBMS name:      MySQL 5.0                                    */
/* Created on:     9/7/2012 12:06:51 AM                         */
/*==============================================================*/


drop table if exists AGENTS;

drop table if exists AGENTTYPES;

drop table if exists CHANGES;

drop table if exists HAGENTSCHANGES;

drop table if exists HAGENTSCONSUME;

drop table if exists HAGENTSPRODUCE;

drop table if exists HAGENTSPROGNOSEPRICE;

drop table if exists HAGENTSSELLBUY;

drop table if exists HMARKETSDAYS;

drop table if exists HMARKETSSES;

drop table if exists MARKETS;

drop table if exists ORDERBOOK;

drop table if exists PRODUCTS;

drop table if exists STRATEGIES;

/*==============================================================*/
/* Table: AGENTS                                                */
/*==============================================================*/
create table AGENTS
(
   ID                   int not null auto_increment,
   DESCRIPTION          longtext,
   primary key (ID)
);

/*==============================================================*/
/* Table: AGENTTYPES                                            */
/*==============================================================*/
create table AGENTTYPES
(
   ID                   int not null auto_increment,
   DESCRIPTION          longtext,
   primary key (ID)
);

/*==============================================================*/
/* Table: CHANGES                                               */
/*==============================================================*/
create table CHANGES
(
   ID                   int not null auto_increment,
   PARTIALLY            bool,
   QUANTITYG1           float,
   QUANTITYG2           float,
   ORDERID              int,
   primary key (ID)
);

/*==============================================================*/
/* Table: HAGENTSCHANGES                                        */
/*==============================================================*/
create table HAGENTSCHANGES
(
   ID                   int not null auto_increment,
   AGENTID              int,
   DAY                  int,
   primary key (ID)
);

/*==============================================================*/
/* Table: HAGENTSCONSUME                                        */
/*==============================================================*/
create table HAGENTSCONSUME
(
   ID                   int not null auto_increment,
   AGENTID              int,
   DAY                  int,
   primary key (ID)
);

/*==============================================================*/
/* Table: HAGENTSPRODUCE                                        */
/*==============================================================*/
create table HAGENTSPRODUCE
(
   ID                   int not null auto_increment,
   AGENTID              int,
   DAY                  int,
   primary key (ID)
);

/*==============================================================*/
/* Table: HAGENTSPROGNOSEPRICE                                  */
/*==============================================================*/
create table HAGENTSPROGNOSEPRICE
(
   ID                   int not null auto_increment,
   AGENTID              int,
   DAY                  int,
   primary key (ID)
);

/*==============================================================*/
/* Table: HAGENTSSELLBUY                                        */
/*==============================================================*/
create table HAGENTSSELLBUY
(
   ID                   int not null auto_increment,
   AGENTID              int,
   DAY                  int,
   primary key (ID)
);

/*==============================================================*/
/* Table: HMARKETSDAYS                                          */
/*==============================================================*/
create table HMARKETSDAYS
(
   ID                   int not null auto_increment,
   MARKETID             int,
   DAY                  int,
   AVGPRICE             float,
   SUMGOOD1             float,
   primary key (ID)
);

/*==============================================================*/
/* Table: HMARKETSSES                                           */
/*==============================================================*/
create table HMARKETSSES
(
   ID                   int not null auto_increment,
   MARKETID             int,
   DAY                  int,
   SES                  int,
   AVGPRICE             float,
   SUMGOOD1             float,
   primary key (ID)
);

/*==============================================================*/
/* Table: MARKETS                                               */
/*==============================================================*/
create table MARKETS
(
   ID                   int not null auto_increment,
   GOOD1                int,
   GOOD2                int,
   primary key (ID)
);

/*==============================================================*/
/* Table: ORDERBOOK                                             */
/*==============================================================*/
create table ORDERBOOK
(
   ID                   int not null auto_increment,
   CHANGETYPE           bool,
   DAY                  int,
   SESSION              smallint,
   MARKETNUM            int,
   QUANTITYG1           float,
   QUANTITYG2           float,
   AGENTID1             int,
   primary key (ID)
);

/*==============================================================*/
/* Table: PRODUCTS                                              */
/*==============================================================*/
create table PRODUCTS
(
   ID                   int not null auto_increment,
   TITLE                varchar(20),
   DESCRIPTION          longtext,
   primary key (ID)
);

/*==============================================================*/
/* Table: STRATEGIES                                            */
/*==============================================================*/
create table STRATEGIES
(
   ID                   int not null auto_increment,
   DESCRIPTION          longtext,
   FUCNTIONNAME         longtext,
   PARAMETERS           longtext,
   primary key (ID)
);

