####################################################################

# Title:    Functions for dealing with PENTAHO
# Author:   Mathieu Jones
# Date:     18/07/2016
# Version:  

### Description: ###
# Define functions that are useful for extracting data from the PENTAHO system
# PENTAHO is where policy data is stored - i.e.monthly extracts from L400 
# (the administrative system that suncorp life uses.)

####################################################################

#Function to query SQL server in Pentaho
QueryPentaho<-function(query, server="prod",query_help=FALSE,table="LS",history="HISTORY"){
    require(RODBC); require(data.table)
    
    # 'server' can be either: c("prod","UAT","dev")
    # 'table' can be either: c("LS","DI")
    # if query_help is TRUE, then replace "TABLE" in query to LS or DI table name.
    # if history is TRUE then query the full history table (in prod)
    # NB: set history="RECENT" if you want most recent data from prod
    
    # pentaho team has three servers. UAT, dev and prod
    # prod has two table versions: recent (last two months) and history (rest of months)
    # We use prod. I had to to use uat when prod broke once though...
    # Tamara asked us to NEVER use dev (this is for her testing)
    # Details of servers:
    # prod: server=PBNESHDSQL4002\\SHIY4002001; uid=prd0SparkSQL_user; pwd=242bf7174c
    # uat: server=10.49.122.41\\SHIY4002001; uid=acp0SparkSQL_user; pwd=6294974c60
    # dev: server=VBNESHDSQL4002\\SHIY4002001; uid=dev0SparkSQL_user; pwd=8043764738
    
    #here are the details of the servers in a table:
    deets <- data.table(
        server_alias = c(rep("prod",2),"UAT","dev"),
        server_name = c(rep("PBNESHDSQL4002\\SHIY4002001",2),
                        "10.49.122.41\\SHIY4002001","VBNESHDSQL4002\\SHIY4002001"),
        uid = c(rep("prd0SparkSQL_user",2),"acp0SparkSQL_user","dev0SparkSQL_user"),
        pwd = c(rep("242bf7174c",2),"6294974c60","8043764738"),
        HISTORY=c("RECENT",rep("HISTORY",3)),
        LS_table_name = c("SPARKSQL.LA_POLICY_DATA","SPARKSQL.LA_POLICY_DATA_HISTORY",
            "Acp0SparkSQL_TWTEST.SPARKSQL.LA_POLICY_DATA_HISTORY",NA),
        DI_table_name = c("SPARKSQL.LA_DI_POLICY_DATA","SPARKSQL.LA_DI_POLICY_DATA_HISTORY",
            "Acp0SparkSQL_TWTEST.SPARKSQL.LA_DI_POLICY_DATA_HISTORY",NA)
    )
    
    #subset the server details to those we want 
    deets<-deets[server_alias==server & HISTORY==history]
    if(table=="LS"){setnames(deets,"LS_table_name","TABLE")}
    if(table=="DI"){setnames(deets,"DI_table_name","TABLE")}
    
    #define connection string
    conn_string<-paste("driver={SQL Server};server=",deets[,server_name],
                       ";uid=",deets[,uid],";pwd=",deets[,pwd],sep="")
    
    # set up connection to server                 
    dbhandle <- odbcDriverConnect(conn_string)
    
    #replace the word 'TABLE' with proper table name
    query<-gsub("TABLE",deets[,TABLE],query)
    if(table =="DI"){query<-gsub("EXTRACT_DATE","EXTRACT_DT",query)}
    
    # hit up the server
    output <- as.data.table(sqlQuery(dbhandle, query, na.strings=c("NA", "NULL")))
    
    if("EXTRACT_DT" %in% colnames(output)){
        setnames(output,"EXTRACT_DT","EXTRACT_DATE")
    }
    
    return(output)
    
    ### Sample Queries ###
    # Query LS table in prod:
    #     query1 <- paste(
    #         "SELECT 
    #         CAST(a.[POLICY_NUMBER] As varchar(20)) + '-' +
    #         CAST(a.[LIFE_NUMBER] As varchar(20)) + '-' +
    #         CAST(a.[COVER_NUMBER] As varchar(20)) + '-' +
    #         CAST(a.[RIDER_NUMBER] As varchar(20)) + '-' +
    #         CAST(a.[BENEFIT_CODE] As varchar(20)) as ID,
    #        *
    #         FROM  SPARKSQL.LA_POLICY_DATA a
    #         WHERE  a.EXTRACT_DATE = '30 Jun 2015'"
    #     )
    #
    # Query DI table in prod:
    #     query1 <- paste(
    #         "SELECT *
    #         FROM  SPARKSQL.LA_DI_POLICY_DATA a
    #         WHERE  a.EXTRACT_DT = '30 Jun 2015'"
    #         
    #     )
    #     
    #     
    #     ls<-QueryPentaho(query1)
    #
    # LS table in UAT: Acp0SparkSQL_TWTEST.SPARKSQL.LA_POLICY_DATA_HISTORY
    #     query<-"
    #         SELECT COUNT(*) AS COUNT, BENEFIT_CODE, EXTRACT_DATE
    #         FROM Acp0SparkSQL_TWTEST.SPARKSQL.LA_POLICY_DATA_HISTORY
    #         GROUP BY BENEFIT_CODE, EXTRACT_DATE
    #     "
    # DI table in UAT: Acp0SparkSQL_TWTEST.SPARKSQL.LA_DI_POLICY_DATA_HISTORY
    #
    # pull all table names from pentaho:
    # query<-"select table_name from INFORMATION_SCHEMA.TABLES where TABLE_TYPE = 'BASE TABLE'"
    # xl(QueryPentaho(query))
    
    #     QueryPentaho(query="select top 1 * from TABLE", 
    #                  server="UAT",query_help=TRUE,table="DI")
    
}

#Function to extract db of premium types (for max level age issue)
get_premium_type_db<-function(date="31 Dec 2015"){
    
    query1 <- paste(
        "SELECT CAST(a.[POLICY_NUMBER] As varchar(20)) + '-' +
        CAST(a.[LIFE_NUMBER] As varchar(20)) + '-' +
        CAST(a.[COVER_NUMBER] As varchar(20)) + '-' +
        CAST(a.[RIDER_NUMBER] As varchar(20)) + '-' +
        CAST(a.[BENEFIT_CODE] As varchar(20)) as ID,
        PREMIUM_TYPE
        FROM  SPARKSQL.LA_POLICY_DATA a
        WHERE  a.EXTRACT_DATE = '",date,"'",sep=""
    )
    
    library(data.table)
    PremType_db<-as.data.table(QueryPentaho(query1))
    
    PremType_db[PREMIUM_TYPE=="L",MAX_LEV_AGE_PENTAHO:=65,]
    PremType_db[PREMIUM_TYPE=="M",MAX_LEV_AGE_PENTAHO:=70,]
    PremType_db[PREMIUM_TYPE=="K",MAX_LEV_AGE_PENTAHO:=55,]
    
    #checks
    #     PremType_db[,.N,PREMIUM_TYPE]
    #     PremType_db[,.N,MAX_LEV_AGE]
    
    # QueryPentaho("select distinct a.EXTRACT_DATE from SPARKSQL.LA_POLICY_DATA a")
    
    #remove dups in PremType_db
    PremType_db<-PremType_db[!duplicated(PremType_db$ID)]
    
    return(PremType_db)
    
    
    
}

#Function to extract product link tables from pentaho:
ProductLinkTable<-function(){
    
    # Purpose is to pull all the link tables from pentaho and combine
    
    #There are 4 Product Link Tables in the SQL Server
    #2 for DI and 2 for LS:
    #SPARKSQL.LA_PRODUCT_LINK_BASIS_1 and SPARKSQL.LA_PRODUCT_LINK_BASIS_2
    #SPARKSQL.LA_DI_PRODUCT_LINK_BASIS_1 and SPARKSQL.LA_DI_PRODUCT_LINK_BASIS_2
    #1 and 2 seem to be identical
    #Andrew W thinks this is just because someone was lazy 
    #(ie they were recreating a bad existing process in PENTAHO without 
    #looking at logic)
    
    q_link_LS<-"SELECT * from SPARKSQL.LA_PRODUCT_LINK_BASIS_2"
    q_link_DI<-"SELECT * from SPARKSQL.LA_DI_PRODUCT_LINK_BASIS_2"
    
    link_LS<-as.data.table(QueryPentaho(q_link_LS))
    link_DI<-as.data.table(QueryPentaho(q_link_DI))
    
    #merge DI and LS:
    PRODUCT_LINK<-rbind(link_LS,link_DI)
    
    #Add an ID col:
    PRODUCT_LINK[,IDtable:=paste(BENEFIT_CODE,VERSION,BUSINESS_CLASS,sep="-")]
    
    #there are lots of dups!
    # length(PRODUCT_LINK$IDtable)-length(unique(PRODUCT_LINK$IDtable))
    
    #remove these dups:
    PRODUCT_LINK<-PRODUCT_LINK[!duplicated(PRODUCT_LINK,by="IDtable")]
    
    #rename product code col:
    setnames(PRODUCT_LINK,"PRODUCT_CODE","PENTAHO_PRODUCT_CODE")
    
    #return PRODUCT_LINK
    return(PRODUCT_LINK)
    
    # NB some pretty strong assumptions went into this de-dup...
    #namely: I'm using benefit code, version and business class as a unique id
    #but this isn't unique for each (pentaho) product code. 
    #upon reflection, I don't think this is too bad...
    #eg this guy:
    #         PRODUCT_LINK[PRODUCT_LINK$BENEFIT_CODE=="SD50"
    #                      &PRODUCT_LINK$VERSION=="ASC"
    #                      &PRODUCT_LINK$BUSINESS_CLASS=="O",]
    
    ### SUN_PCODE ###
    # Below code pulls the sun_pcode table from PENTAHO (don't need for the moment):
    # as.data.table(QueryPentaho("select * from SPARKSQL.LA_SUN_PCODE_DEFAULTS"))
    
}
make_map_table<-function(ref_table_dir, dir_MPFs="T:/MPFILES/1606", ending=".af15"){
    
    #first read in product link table
    PRODUCT_LINK<-ProductLinkTable()
    #get unique combinations of BENEFIT code and indicators
    map_table<-unique(PRODUCT_LINK[,.(BENEFIT_CODE,DEATH_INDICATOR,
                                      TPD_INDICATOR,CCB_INDICATOR)])
    #get levels of PACK_IND
    pack_ind_levels<-data.table(PACK_IND=1:(choose(3,1)+choose(3,2)+choose(3,3)))
    pack_ind_levels[PACK_IND==1,PACK_IND_DESC:='DEATH_ONLY']
    pack_ind_levels[PACK_IND==2,PACK_IND_DESC:='DEATH_WITH_TRAUMA_RIDER']
    pack_ind_levels[PACK_IND==3,PACK_IND_DESC:='DEATH_WITH_TPD_RIDER']
    pack_ind_levels[PACK_IND==4,PACK_IND_DESC:='DEATH_WITH_TRAUMA_AND_TPD_RIDER']
    pack_ind_levels[PACK_IND==5,PACK_IND_DESC:='TRAUMA_WITH_TPD_RIDER']
    pack_ind_levels[PACK_IND==6,PACK_IND_DESC:='TRAUMA_ONLY']
    pack_ind_levels[PACK_IND==7,PACK_IND_DESC:='TPD_ONLY']
    
    #loop through pack ind levels and add to map table
    
    map_table_db<-data.table()
    for (i in 1:nrow(pack_ind_levels)){
        map_table_i<-copy(map_table)
        map_table_i[,PACK_IND:=pack_ind_levels[i,PACK_IND]]
        map_table_i[,PACK_IND_DESC:=pack_ind_levels[i,PACK_IND_DESC]]
        map_table_db<-rbind(map_table_db,map_table_i)
        
    }
    
    #to add prophet product code, look through MPFs and extract benefit codes in each mpf
    #NB: benefit code can fall in muptple mpfs
    
    prods<-list_products_in_file(location_original=dir_MPFs,ending)
    
    ben_prod_db<-data.table()
    bad_prods<-data.table()
    for (prod in prods){
        
        setwd(dir_MPFs)
        mpf<-fread(paste(prod,ending,sep=""),showProgress = FALSE)  
        
        if(!"BENEFIT_CODE"%in%colnames(mpf) & !"Ben_Code"%in%colnames(mpf)) {
            
            #might be a problem with format of mpf (doesn't work with fread)
            #so double check with read_MPF (which is slower than fread)
            mpf<-as.data.table(read_MPF(file_dir=dir_MPFs,product=prod,ending=ending)[[1]])
            
            #if BENEFIT_CODE still isn't in mpf, then this is a bad_prod
            if(!"BENEFIT_CODE"%in%colnames(mpf) & !"Ben_Code"%in%colnames(mpf)) {
                
                # print(paste("BENEFIT_CODE not in mpf for ",prod,sep = ""))
                bad_prods<-rbind(bad_prods,data.table(PROD=prod,NROW=nrow(mpf)))
                next()
                
            }
        }
        
        ben_name<-"BENEFIT_CODE"
        if(!"BENEFIT_CODE"%in%colnames(mpf)){ben_name<-"Ben_Code"}
        
        ben_prod<-mpf[,.(PROD=prod,COUNT=.N),.(BENEFIT_CODE=get(ben_name))]
        
        ben_prod_db<-rbind(ben_prod_db,ben_prod)
        
    }
    
    
    map_table_db<-merge(map_table_db,ben_prod_db[,.(BENEFIT_CODE,PROPHET_PRODUCT_CODE=PROD)],
                         by="BENEFIT_CODE",all.x=TRUE,allow.cartesian=TRUE)
    
    setwd(ref_table_dir)
    write.csv(map_table_db,"MAP_INDS2.csv",row.names = FALSE)
    
    return(map_table_db)
    
#     x <- make_map_table(
#         ref_table_dir = "U:/Experience/Experience Investigations/2017 FY/Projects/Exposure_Data_Creation/2.Data/2.ReferenceTables",
#         dir_MPFs="T:/MPFILES/1606", ending=".af15")
#     x[PROPHET_PRODUCT_CODE == "CATDOS"]
    
}
