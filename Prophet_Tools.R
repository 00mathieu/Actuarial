####################################################################

# Title:    Read, adjust and write MPFs
# Author:   Mathieu Jones
# Date:     8/4/2016
# Version:  1

####################################################################


#Function to do things with Prophet outputs/inputs 
dir_file<-function(file_dir,product,ending=".af15"){
    paste(file_dir,"/",product,ending, sep="")
}
read_MPF<-function (file_dir,product,ending=".af15"){
    #NB: need to redo this function with fread once I get a 64 bit machine.
    
    #function to read in model point file 
    #output is a list of:
    #1. the data
    #2. the raw first column
    #3. the raw last column
    #4. the raw first five rows
    
    location<-dir_file(file_dir,product,ending)
    
    if (!file.exists(location)){return()}
    
    raw <- read.csv(location, header = FALSE)
    
    #Generally, first five rows are filler.
    #But sometimes, some genius deletes first four lines
    #to check if this, we know that column name columns starts with a '!'
    row_filler <- grep('!',raw[,1])
    #now sometimes, there are two column name rows... second starts with '&'
    if (length(grep('&',substr(raw[,1],0,1))) != 0) {
        row_filler <- row_filler + 1
    }
    
    #last column can either be just the last col of the table, or can be just a 
    #column header (like 'T4') and full of nulls. if latter, need to keep separate
    last_col <- raw[,ncol(raw)]
    is_empty<-length(which(last_col==""|is.na(last_col)))==(length(last_col)-1)
    if(is_empty) {
        raw <- raw[,-ncol(raw)] #get rid of last col
    }
    if (!is_empty) {
        last_col <- NA
    }
    
    first_rows_numbers <- 1:row_filler
    first_col <- raw[,1]
    first_rows <- raw[first_rows_numbers,]
    
    raw <- raw[,-1] #get rid of first col
    cn <- raw[row_filler,] #line 5 is the colnames
    raw <- raw[-first_rows_numbers,] #get rid of first five rows
    #why can't i do this without a loop????
    for (i in 1:ncol(raw)) {
        colnames(raw)[i] <- as.character(cn[,i])
    }
    row.names(raw) <- 1:nrow(raw)
    
    results <- list(raw,first_col,last_col,first_rows)
    
    rm(raw); gc(); 
    
    return(results)
        
    
}
read_MPF_fast<-function (file_dir,product,ending=".af15"){
    #function to read in model point file 
    require(data.table)
    
    location<-dir_file(file_dir,product,ending)
    
    if (!file.exists(location)){return()}
    
    #fread seems to be able to automatically detect where data starts!
    raw <- fread(location)
    
    if(length(which(colnames(raw)%in%"!"))!=0){raw[,`!`:=NULL]}
    
    return(raw)
    
}
save_MPF<-function(list_things,location,product,ending=".af15"){
    
    #function to save in MPF format. 
    #NB prophet is very particular about the format that it wants its data in
    #input a list of four things: 1) data, 2) the raw first column, 
    #3) the raw last column, 4) the raw first five rows
    
    require(data.table)
    
    #load data
    data<-as.data.table(list_things[[1]])
    first_col<-as.data.table(list_things[[2]])
    last_col<-as.data.table(list_things[[3]])
    first_rows<-as.data.table(list_things[[4]])
    
    #get datatable ready for adding the extra rows&columns
    data<-cbind(V1=NA,data,Vlast=NA)
    last_col_number<-paste("V",ncol(data),sep="")
    colnames(data)<-colnames(first_rows)
    
    #add first rows to datatable
    data<-rbind(first_rows[4:5,],data)
    
    #add first and last columns to datatable
    data[,V1:=first_col[4:nrow(first_col)]]
    data[,eval(last_col_number):=last_col[4:nrow(last_col)]]
    
    #check which cols are not numeric
    data1<-as.data.table(sapply(data,as.character))
    data2<-as.data.table(sapply(data1,as.numeric))
    char_cols<-sapply(data2, is.na)[3,]
    non_numeric_cols<-which(colnames(data)%in%colnames(data)[char_cols])
    T_cols<-which(substr(data1[1,],0,1) == "T")-1
    non_numeric_cols<-append(non_numeric_cols,T_cols)
    non_numeric_cols<-unique(non_numeric_cols)
    non_numeric_cols<-sort(non_numeric_cols)
    
    non_numeric_cols<-non_numeric_cols[-1] #don't put quotes around first col
    non_numeric_cols<-non_numeric_cols[-length(non_numeric_cols)] 
    
    #save results
    setwd(location)
    name_file<-paste(product,ending,sep="")
    
    # create and open the file connection
    datafile <- file(name_file, open = 'wt')
    
    writeLines(paste(first_rows[1,V1]),con=datafile)
    writeLines(paste(first_rows[2]$V1,first_rows[2]$V2,sep=","),con=datafile)
    writeLines(paste(first_rows[3]$V1,first_rows[3]$V2,sep=","),con=datafile)
    
    close(datafile)
    
    write.table(data[1,],file=name_file,sep=",", append=TRUE,
                row.names=FALSE, col.names=FALSE, quote=FALSE)
    
    write.table(data[2,-ncol(data),with=FALSE],file=name_file,sep=",", append=TRUE,
                row.names=FALSE, col.names=FALSE, quote=FALSE)
    
    write.table(data[3:nrow(data),-ncol(data),with=FALSE],file=name_file,sep=",", append=TRUE,
                row.names=FALSE, col.names=FALSE, quote=non_numeric_cols)
    
}
list_products_in_file<-function(location_original,ending){
    
    #get list of all files in folder
    files<-list.files(location_original)
    
    #case sensitive!~!
    ending1<-c(ending,toupper(ending))
    
    #only take ones ending in .af14
    #also only take files that are less than 20 characters
    files<-files[substr(files,nchar(files)-(nchar(ending)-1),99) %in% ending1 ]
    
    #removed length restribtion  in the above: & nchar(files)<20
    #get rid of .af14 in strings
    files<-substr(files,1,nchar(files)-nchar(ending))
    
    return(files)
    
}
adjust_pol_term<-function(data,max_age=65,prop_sample=1){
    
    #function to take in db and adjust the term for each policy such that the 
    #max age is max_age
    
    #NOTE: because you're covered till you end the max_age 
    #(ie you're covered for your entire 75th year)
    #therefore add one to max_age
    
    max_age<-max_age+1
    
    require(data.table)
    data<-as.data.table(data)
    
    #check if necessary columns are there, if not return unchanged
    necessary_cols<-c("POLICY_NO","LIFE_NUMBER","COVER_NUMBER","RIDER_NUMBER",
                      "BENEFIT_CODE","AGE_AT_ENTRY","POL_TERM_Y","STEPPED_IND")
    exit_cond<-length(which(necessary_cols %in% colnames(data)==TRUE)) !=
        length(necessary_cols)
    
    if(exit_cond){
        print("not all the necessary cols are included in MPF")
        return(data)
    }
    
    data[,ID:=paste(POLICY_NO,LIFE_NUMBER,COVER_NUMBER,RIDER_NUMBER,
                    BENEFIT_CODE, sep="-")]
    data[,AGE_AT_ENTRY:=as.numeric(as.character(AGE_AT_ENTRY))]
    data[,POL_TERM_Y:=as.numeric(as.character(POL_TERM_Y))]
    
    # create id columns and sample from this (if prop =1 then apply to all)
    IDs<-unique(data[,ID])
    sample_IDS<-sample(IDs,prop_sample*round(length(IDs)),replace = FALSE)
    
    #if STEPPED_IND == 0
    #if AGE_AT_ENTRY + POL_TERM_Y>max_age, then cap POL_TERM_Y
    data[,POL_TERM_Y_1:=pmax(max_age-AGE_AT_ENTRY,1)]
    data[STEPPED_IND!="1" & ID %in% sample_IDS,
         POL_TERM_Y:=pmin(POL_TERM_Y,POL_TERM_Y_1)]
    
    #remove fabricated column
    data[,POL_TERM_Y_1:=NULL]
    data[,ID:=NULL]
    
    return(data)
    
}
create_log_MaxLevelAge<-function(data,exit_cond=FALSE){
    
    #I also want to record: name of product, total rows db, no rows matched, 
    #no rows not matched, no rows changed for each age
    #and same for policy IDs
    
    #if not enough cols to merge the db to the MPF then exit early
    if(exit_cond){
        
        remember<-data.table(
            Total_Rows=data[,.N],
            RowsMatched=0,
            RowsNotMatched=data[,.N],
            RowsUpdated_To56=0,
            RowsUpdated_To66=0,
            RowsUpdated_To71=0,
            RowsNotUpdated99=0,
            Total_Pols=0,
            PolsMatched=0,
            PolsNotMatched=0,
            PolsUpdated_To56=0,
            PolsUpdated_To66=0,
            PolsUpdated_To71=0,
            PolsNotUpdated99=0
        )
        return(remember)
    }
    
    #keep record of number of rows updated
    no_updated56<-data[!is.na(MAX_LEV_AGE_PENTAHO) & 
                           MAX_LEV_AGE == 66 &
                           MAX_LEV_AGE!=(MAX_LEV_AGE_PENTAHO+1) & 
                           MAX_LEV_AGE_PENTAHO+1 == 56,.N]
    no_updated66<-data[!is.na(MAX_LEV_AGE_PENTAHO) & 
                           MAX_LEV_AGE == 66 &
                           MAX_LEV_AGE!=(MAX_LEV_AGE_PENTAHO+1) & 
                           MAX_LEV_AGE_PENTAHO+1 == 66,.N]
    no_updated71<-data[!is.na(MAX_LEV_AGE_PENTAHO) & 
                           MAX_LEV_AGE == 66 &
                           MAX_LEV_AGE!=(MAX_LEV_AGE_PENTAHO+1) & 
                           MAX_LEV_AGE_PENTAHO+1 == 71,.N]
    no_99<-data[MAX_LEV_AGE == 99,.N]
    #keep record of number of policies updated
    pols_updated56<-data[!is.na(MAX_LEV_AGE_PENTAHO) & 
                             MAX_LEV_AGE == 66 &
                             MAX_LEV_AGE!=(MAX_LEV_AGE_PENTAHO+1) & 
                             MAX_LEV_AGE_PENTAHO+1 == 56 &
                             !duplicated(ID),.N]
    pols_updated66<-data[!is.na(MAX_LEV_AGE_PENTAHO) & 
                             MAX_LEV_AGE == 66 &
                             MAX_LEV_AGE!=(MAX_LEV_AGE_PENTAHO+1) & 
                             MAX_LEV_AGE_PENTAHO+1 == 66 &
                             !duplicated(ID),.N]
    pols_updated71<-data[!is.na(MAX_LEV_AGE_PENTAHO) & 
                             MAX_LEV_AGE == 66 &
                             MAX_LEV_AGE!=(MAX_LEV_AGE_PENTAHO+1) & 
                             MAX_LEV_AGE_PENTAHO+1 == 71 &
                             !duplicated(ID),.N]
    pols_notUpdated<-data[MAX_LEV_AGE == 99 & 
                              !duplicated(ID),.N]
    
    remember<-data.table(
        Total_Rows=data[,.N],
        RowsMatched=data[!is.na(MAX_LEV_AGE_PENTAHO),.N],
        RowsNotMatched=data[is.na(MAX_LEV_AGE_PENTAHO),.N],
        RowsUpdated_To56=no_updated56,
        RowsUpdated_To66=no_updated66,
        RowsUpdated_To71=no_updated71,
        RowsNotUpdated99=no_99,
        Total_Pols=data[!duplicated(ID),.N],
        PolsMatched=data[!is.na(MAX_LEV_AGE_PENTAHO) & !duplicated(ID),.N],
        PolsNotMatched=data[is.na(MAX_LEV_AGE_PENTAHO)& !duplicated(ID),.N],
        PolsUpdated_To56=pols_updated56,
        PolsUpdated_To66=pols_updated66,
        PolsUpdated_To71=pols_updated71,
        PolsNotUpdated99=pols_notUpdated
    )
    
    return(remember)
    
}
adjust_MaxLevelAge<-function(data,PremType_db){
    
    #function to take in db and adjust the variable MAX_LEV_AGE to value
    #in prem_type_db
    
    require(data.table)
    data<-as.data.table(data)
    PremType_db<-as.data.table(PremType_db)

    #check if necessary columns are there, if not return unchanged
    necessary_cols<-c("POLICY_NO","LIFE_NUMBER","COVER_NUMBER","RIDER_NUMBER",
                      "BENEFIT_CODE","MAX_LEV_AGE","STEPPED_IND")
    exit_cond<-length(which(necessary_cols %in% colnames(data)==TRUE)) !=
        length(necessary_cols)
    
    if(exit_cond){
        print("not all the necessary cols are included in MPF")
        
        remember<-create_log_MaxLevelAge(data, exit_cond=TRUE)
            
        return(list(data,remember))
    }
    
    #add ID column
    data[,ID:=paste(POLICY_NO,LIFE_NUMBER,COVER_NUMBER,RIDER_NUMBER,
                   BENEFIT_CODE, sep="-")]
    
    #make sure MAX_LEV_AGE is numeric
    data[,MAX_LEV_AGE:=as.numeric(as.character(MAX_LEV_AGE))]
    PremType_db[,MAX_LEV_AGE_PENTAHO:=as.numeric(as.character(MAX_LEV_AGE_PENTAHO))]
    
    #merge PremType_db (from PENTAHO) to our db
    data<-merge(data,PremType_db,by="ID", all.x=TRUE)
    
    #create log of policies updated
    remember<-create_log_MaxLevelAge(data, exit_cond=FALSE)
    
    #update MAX_LEV_AGE to MAX_LEV_AGE_PENTAHO iff: 
    #!is.na(MAX_LEV_AGE_PENTAHO)
    #MAX_LEV_AGE == 66 (ie sometimes age is 99 so don't want to change )
    #need to add 1 to max age... because MPFS were set to 66...
    data[!is.na(MAX_LEV_AGE_PENTAHO) & MAX_LEV_AGE == 66,
         MAX_LEV_AGE:=(MAX_LEV_AGE_PENTAHO+1)]
    
    #remove columns that were added (ID, MAX_LEV_AGE_PENTAHO, PREMIUM_TYPE)
    data[,ID:=NULL]
    data[,MAX_LEV_AGE_PENTAHO:=NULL]
    data[,PREMIUM_TYPE:=NULL]
    
    output<-list(data,remember)
    
    return(output)
    
}
pull_demography_mpf<-function(file_dir,product,ending=".af15"){
    
    list_things<-read_MPF(file_dir=file_dir,product,ending)
    
    data<-as.data.table(list_things[[1]]) #extract just the MPF
    
    demog<-data[STEPPED_IND=="1",.(NUMBER=length(unique(POLICY_NO))),
         by=.(AGE=AGE_AT_ENTRY,SEX=ifelse(SEX=="1","MALE","FEMALE"))][order(-SEX,AGE)]
    
    demog[,AGE:=as.numeric(as.character(AGE))]
    
    return(demog)
    
}

#function to summarise exposure in each of the MPFS:
MPF_Exposure_summary<-function(dir_MPFs="T:/MPFILES/1606", ending=".af15"){
    
    #purpose of this function is to loop through MPFs in directory and 
    #to summarise the policyholder expsoure 
    
    prods<-list_products_in_file(location_original=dir_MPFs,ending)
    summary_cols<-c("SUM_ASSURED_MORT","SUM_ASSURED_MORB","POL_SUM_ASSD","SUM_ASSURED",
                    "ANN_PHI_BEN","POL_PHI_BEN","ANNUAL_PREM")
    ID_cols<-c("POLICY_NUMBER","POLICY_NO","BENEFIT_CODE","LIFE_NUMBER",
               "COVER_NUMBER","RIDER_NUMBER")
    
    prod<-prods[120]
    MPF_summary<-data.table()
    prod<-"CAWPOD"
    for (prod in prods){
        
        print(paste("Checking MPF: ",prod,sep=""))
        
        setwd(dir_MPFs)
        mpf<-fread(paste(prod,ending,sep=""),showProgress = FALSE)  
        
        #might be a problem with format of mpf (doesn't work with fread)
        #so double check with read_MPF (which is slower than fread)
        if(length(which(colnames(mpf) %in% summary_cols)) == 0){
            mpf<-as.data.table(read_MPF(file_dir=dir_MPFs,product=prod,ending=ending)[[1]])
        }
        
        cols<-summary_cols[summary_cols%in%colnames(mpf)]
        IDs<-ID_cols[ID_cols %in% colnames(mpf)]
        mpf[, (cols) := lapply(.SD, as.character), .SDcols = cols]
        mpf[, (cols) := lapply(.SD, as.numeric), .SDcols = cols]
        mpf[, (IDs) := lapply(.SD, as.character), .SDcols = IDs]
        
        #Attach Id to MPF
        if(length(IDs)==0){
            mpf[,ID_col:= 1:nrow(mpf)]
        }
        if(length(IDs)>0){
            mpf[,ID_col:= get(IDs[1])]    
            for (ID_i in IDs[-1]){
                mpf[,ID_col:= paste(ID_col,get(ID_i),sep="-")]
            }    
        }
        
        #summarise MPF
        summary_row<-mpf[,.(PROPHET_PRODUCT_CODE=prod,COUNT=length(unique(ID_col)))]
        
        for (col_i in cols){
            summary_row<-cbind(summary_row,mpf[,.(sum(get(col_i))),])
            setnames(summary_row,"V1",col_i)
            #there is a better way of doing this but i can't remember atm
        }
        
        MPF_summary<-rbind(MPF_summary,summary_row,fill=TRUE)
    }
    
    return(MPF_summary)
    
    #usage:
    # MPF_summary<-MPF_Exposure_summary(dir_MPFs="T:/MPFILES/1606", ending=".af15")
    
    
}

#Functions to pull out data useful for (binomial) simulations
add_rec_rate_month<-function(db,month=1){
    
    #given policyholder characteristics the recovery rates for duration months 1:24  
    #and years 3:10 and an ultimate (11+) are given in the data
    #the purpose of this function is to extract the relevant recovery rate 
    #given the number of months that the policyholder has already been in force
    #this is given by the DUR_M column
    
    #column to update
    col_name<-paste("REC_RATE_M",month,sep="")
    
    #get right recovery rate for policyholder.
    #if dur < 23 then REC_RATE_M(dur+1) 
    #if dur >= 24 and dur < 119 then REC_RATE_Y(floor(dur/12)) 
    #if dur >= 120 then REC_RATE_ULT
    db[(DUR_M+month)<=24,REC_RATE_FINDER:=paste("REC_RATE_M(",DUR_M+month,")",sep="")]
    db[(DUR_M+month)>=24 & (DUR_M+month)<=120,
       REC_RATE_FINDER:=paste("REC_RATE_Y(",
                              ceiling((DUR_M+month)/12),")",sep="")]
    db[(DUR_M+month)>=120,REC_RATE_FINDER:="REC_RATE_ULT"]
    
    #get right column
    db[,eval(col_name):=get(REC_RATE_FINDER), by=REC_RATE_FINDER]
    
    #get rid of working column
    db[,REC_RATE_FINDER:=NULL]
    
    return(db)
    
}
add_monthly_rec_rate<-function(db){
    
    for (month in 1:12){
        db<-add_rec_rate_month(db=db,month)
    }
    
    #checks:
    # head(db_results)
    #     db_results[is.na(REC_RATE_M1)] #check
    #     db_results[DUR_M==17 & `REC_RATE_M(18)`!=REC_RATE_M1]
    #     db_results[DUR_M==24 & `REC_RATE_Y(3)`!=REC_RATE_M1]
    #     db_results[DUR_M==120 & `REC_RATE_ULT`!=REC_RATE_M1]
    #     db_results[DUR_M==17 & `REC_RATE_M(22)`!=REC_RATE_M5]
    #     db_results[DUR_M==24 & `REC_RATE_Y(3)`!=REC_RATE_M5]
    #     db_results[DUR_M==120 & `REC_RATE_ULT`!=REC_RATE_M5]
    #     db_results[DUR_M==34 & `REC_RATE_Y(3)`!=REC_RATE_M1]
    #     db_results[DUR_M==34]
    
    return(db)
    
}
add_cond_sick_rates<-function(db){
    
    #add conditional probs that remain sick
    
    for(month in 1:12){
        
        col_name_sick<-paste("SICK_M",month,sep="")
        col_name_rec<-paste("REC_RATE_M",month,sep="")
        col_name_sick_minus1<-paste("SICK_M",month-1,sep="")
        
        if(month==1){
            db[,eval(col_name_sick):=1-get(col_name_rec)]
            next
        }
        
        
        db[,eval(col_name_sick) :=
               (1-get(col_name_rec))*get(col_name_sick_minus1)]
        
    }
    
    # Checks
    # sum(db_results[,SICK_M1-(1-REC_RATE_M1)]) #check
    # sum(db_results[,SICK_M2-(1-REC_RATE_M2)*SICK_M1])
    # sum(db_results[,SICK_M12-(1-REC_RATE_M12)*SICK_M11])
    
    return(db)
    
}
add_recovery_rates_wrapper<-function(db,clean_up=TRUE){
    
    db_results<-copy(db)
    
    #get monthly recovery rates
    db_results<-add_monthly_rec_rate(db_results)
    db_results<-add_cond_sick_rates(db_results)
    
    #define DI Termination columns 
    rec_rate_cols<-c(paste("REC_RATE_M(",1:24,")",sep=""),
                     paste("REC_RATE_Y(",1:10,")",sep=""),
                     "REC_RATE_ULT")
    
    #remove unecessary cols now
    if(clean_up){db_results[,(rec_rate_cols):=NULL]}
    
    return(db_results)
    
}
get_prophet_output_data <- function(location_original = dir_15,
                                    ending = ending_15,
                                    product_list = c("PACOO1","PACOO2"),
                                    extract_vars = c("DUR_M"),
                                    add_recovery_rates = FALSE,
                                    clean_up=TRUE){
    
    require(data.table)
    
    prods<-list_products_in_file(location_original=location_original,ending=ending)
    #TODO add error catcher here: if prod isn't in list
    prods<-prods[prods %in% product_list]
    
    # prod<-prods[1]
    extract_vars_orig<-extract_vars
    
    db_results<-data.table()
    for (j in 1:length(prods)){
        
        prod<-prods[j]
        
        print(paste("Importing ", j," out of ",length(prods),sep=""))
        
        extract_vars<-extract_vars_orig
        
        #download data file for product
        res_file<-read_MPF(file_dir=location_original, product=prod,ending=ending)
        res_file<-as.data.table(res_file[[1]])
        
        #check that all necessary columns are in product file
        exit_cond <-
            length(which(extract_vars %in% colnames(res_file) == TRUE)) != 
            length(extract_vars)
        if(exit_cond){
            missing_cols<-extract_vars[!extract_vars %in% 
                                           colnames(res_file)]
            print(paste("not all columns are here for product: ", prod, 
                        "...",missing_cols, sep=""))
            
            extract_vars<-extract_vars[extract_vars %in% colnames(res_file)]
            
        }
        
        #only take necessary cols
        data_prod<-res_file[,(extract_vars),with=FALSE]
        remove(res_file);gc();
        
        data_prod[, (extract_vars) := lapply(.SD, as.character), 
                  .SDcols = extract_vars]
        data_prod[, (extract_vars) := lapply(.SD, as.numeric), 
                  .SDcols = extract_vars]
        
        
        
        #check conversion
        #     sum(data_prod[ ,REC_RATE_ULT] - 
        #             data_prod1[ ,as.numeric(as.character(REC_RATE_ULT))])
        
        
        data_prod[,PROPHET_PRODUCT_CODE:=prod]
        
#         if(exit_cond){
#             data_prod[,missing_cols:=0,with=FALSE]
#         }
        
        db_results<-rbind(db_results,data_prod,fill=TRUE)
        remove(data_prod);gc();
        
    }
    
    #Add recovery rates for DI
    if(add_recovery_rates){db_results<-add_recovery_rates_wrapper(db_results,clean_up = clean_up)}
    
    #replace all NAs with 0
    db_results[is.na(db_results)] = 0
    
    return(db_results)
    
}
find_min_surv<-function(db,surv_vars,rv_var,max_surv=12){
    
    #function to look along row to find minimum column whose prob > rv
    
    look<-copy(db[,c(surv_vars,rv_var),with=FALSE])
    
    look[,SICK_M0:=1]
    surv_vars_alt<-c(surv_vars,"SICK_M0")
    survival<-c(1:12,0)
    
    #SICK_M# is the probability of remaining # number of months
    #I want to find the minimum # (SICK_M#) that the simulated rv is greater than
    look[,(surv_vars_alt) := look[,surv_vars_alt,with=FALSE] - look[,get(rv_var)]]
    look[look<0]<-99 #if<0 then terminated
    look[,RV_recover:=NULL]
    look[,SURVIVAL_T:=which.min(.SD),by=1:nrow(look)]
    look[,SURVIVAL_T:=survival[SURVIVAL_T]]
    
    #add one since round up
    look[SURVIVAL_T<max_surv,SURVIVAL_T:=SURVIVAL_T + as.integer(1)] 
    
    db[,SURVIVAL_T:=look[,SURVIVAL_T]]
    return(db)
    
    #testing:
    #     surv_vars<-names(db_termins)[substr(names(db_termins),0,4)=="SICK"]
    #     db_termins[,RV_recover:=runif(nrow(db_termins))]
    #     rv_var<-"RV_recover"
    #     db<-copy(db_termins)
    #     
    #     looky<-find_min_surv(db,surv_vars,rv_var,max_surv=12)
    #     head(looky)
    #     looky[SURVIVAL_T!=12 ]
    #     looky[SURVIVAL_T!=12 & SICK_M12 != SICK_M11]
    #     looky[RV_recover>SICK_M1]
    #     look[SURVIVAL_T>12]
}
get_prophet_results<-function(
    prod="CADRSD",
    location_prophet_output="T:/PROJECTS/LapseFeb16/LapseFeb16/results/RUN_125",
    ending_p_out=".rpt",
    reduce_variance=TRUE){
    
    res_file<-read_MPF(
        file_dir=location_prophet_output, 
        product=prod,
        ending=ending_p_out)
    prophet_out<-as.data.table(res_file[[1]])
    rm(res_file); gc(); 
    
    #if reduce_variance, then only output columns where variance > threshold
    if(reduce_variance){
        
        #first, need to convert columns to numeric
        extract_vars<-colnames(prophet_out)
        factor_cols<-c("TPD_PAYOUT","TPD_DEF","PROD_CODE","POL_NUMBER","MORT_TABLE",
                       "MORB_TABLE_2","MORB_TABLE","MDL_VERSION","IRC_VAR_RC",
                       "GS_PROD_NAME","CCB_RO","CCB_PAYOUT","CCB_DEF","BENEFIT_CODE",
                       "ASSET_POOL","AOC_FLAG","SPCODE")
        extract_vars<-extract_vars[!extract_vars%in%factor_cols]
        
        prophet_out[, (extract_vars) := lapply(.SD, as.character), 
                    .SDcols = extract_vars]
        prophet_out[, (extract_vars) := lapply(.SD, as.numeric), 
                    .SDcols = extract_vars]
        
        ### check if col is na: ###
        # cols<-c()
        # for(i in 1:ncol(prophet_out)) {
        #     if (any(is.na(res[,i,with = FALSE]))) {
        #         cols <- c(i,cols)
        #     }
        # }
        # colnames(prophet_out)[cols]
        ### ###
        
        vars<-data.table(prophet_out[,lapply(.SD,var),.SDcols=extract_vars])
        non_zero_var_cols<-extract_vars[which(vars>0)]
        prophet_out<-prophet_out[,non_zero_var_cols,with=FALSE]
        
        
    }
    
    
    return(prophet_out)
    
    
}
make_augmented_MPF<-function(
    location_MPF="T:/MPFILES/1601/For Macro",
    ending_MPF=".af15",
    location_prophet_output="T:/PROJECTS/LapseFeb16/LapseFeb16/results/RUN_125",
    ending_p_out=".rpt" ,
    prod="CADRSD",
    reduce_variance=TRUE,
    save_csv=TRUE
){
    
    #read original MPF data
    list_things <- read_MPF(file_dir = location_MPF,prod,ending_MPF)
    data <- as.data.table(list_things[[1]]) #extract just the MPF
    rm(list_things); gc(); 
    
    #read prophet results for product
    proph_out <- get_prophet_results(
        prod = prod,
        location_prophet_output = location_prophet_output,
        ending_p_out = ending_p_out,
        reduce_variance = TRUE
    )
    
    #if condition, then skip this product
    if (nrow(proph_out)!=nrow(data)) {
        paste("ERROR: nrow(prophet output) != nrow(MPF)")
        return()
    }
    
    #combine original MPF and prophet output
    data <- cbind(data,proph_out)
    rm(proph_out); gc();
    
    return(data)
    
} 
### example code to loop through MPF products, do something and then re-save ###
example_loop<-function(){
    
    #folder parameters
    ending<-".af15"
    location_original<-"T:/MPFILES/1601"
    location_final<-"U:/Experience/Experience Investigations/2016 FY/Projects/Yuri/1601"
    
    #get prods
    all_prods<-list_products_in_file(location_original,ending)    
    
    #loop through prods. 
    
    for (prod in all_prods){
        
        #if condition, then skip this product
        if(cond){next}
        
        #ticker
        print(paste("Doing ... ", prod, sep=""))
        
        #read MPF data
        list_things<-read_MPF(file_dir=location_original,prod,ending)
        data<-as.data.table(list_things[[1]]) #extract just the MPF
        
        #convert the number_columns from factor format to numeric
        data<-factor_to_num(data,number_columns)
        
        #run some kind of function on your data
        collapsed<-run_CA_query(data)
        
        #save altered MPF to original list
        #NB: prophet needs to be ordered by spcode
        list_things[[1]]<-collapsed[order(SPCODE,POLICY_NO)] 
        
        #save results to new set of MPFs
        save_MPF(list_things,location=location_final,product=prod,ending)
        
    } 
        
        
}

#Funciton to pull .FAC tables
clean_prophet_table<-function(tab,col_not_null){
    #this function cleans the a csv file
    #col_not_null is the name of a column that doesn't have any nulls
    #(ie it's used to eliminate empty rows)
    require(data.table)
    
    #first, find how many filler rows there are
    for (i in 1:ncol(tab)){
        
        row_filler<-grep(col_not_null,tab[,i])
        
        if(length(row_filler)!=0){break}
        
        
    }
    
    
    #clean this up:
    #delete first row
    rows_to_delete<- 1:(row_filler-1)
    tab<-tab[-rows_to_delete,]
    tab<-tab[,-1]#delete first col
    
    #get colnames... for some reason this only works in a loop...
    for (j in 1:ncol(tab)) {
        colnames(tab)[j]<-as.character(tab[1,j])
    }
    
    tab<-tab[-1,]
    row.names(tab)<-1:nrow(tab)
    tab<-tab[tab[col_not_null]!="",] #get rid of empty rows
    
    tab<-as.data.table(tab)
    
    tab<-tab[,which(unlist(lapply(tab, function(x)!all(is.na(x))))),with=F]
    
    tab
    
}

get_TABLE<-function(directory="T:/Valuation/TABLES/1603/Val_New",
                    table_name="PARAMA",
                    ending=".FAC",
                    col_not_null="PROD_CODE"){
    
    read_name<-paste(directory,"/",table_name,ending,sep="")
    tabl<-read.csv(read_name, header = FALSE)
    tabl<-clean_prophet_table(tab=tabl,col_not_null=col_not_null)
    
    return(tabl)
    
}
compare_table <- function(table1,table2) {
    table1[,names(table1):= lapply(.SD,as.character)]
    table1[,names(table1):= lapply(.SD,as.numeric)]
    table2[,names(table2):= lapply(.SD,as.character)]
    table2[,names(table2):= lapply(.SD,as.numeric)]
    # orig1[,names(orig1):=lapply(.SD,as.character)]
    # orig1[,names(orig1):=lapply(.SD,as.numeric)]
    # rows<-c()
    # i<-1
    
    diffs <- data.table()
    for (i in 1:nrow(table1)) {
        if (sum(table1[i]) != sum(table2[i])) {
            row_T1 <- data.table(TABLE = "table1",table1[i])
            row_T2 <- data.frame(TABLE = "table2",table2[i])
            
            diffs <- rbind(diffs,row_T1,row_T2,fill=TRUE)
        }
    }
    xl(diffs)
    
    return(diffs)
    
    #     compare_table(table1 = as.data.table(get_TABLE(directory=mat,
    #                                                    table_name="LAPSE_RATE",
    #                                                    ending=".FAC",
    #                                                    col_not_null="LAPSE_BASIS")),
    #                   table2=as.data.table(get_TABLE(directory=mark,
    #                                                  table_name="LAPSE_RATE",
    #                                                  ending=".FAC",
    #                                                  col_not_null="LAPSE_BASIS")))
    
}
create_MPF_DB<-function(location_MPF,ending,list_of_prods){
    
    #if list_of_prods == "ALL", then all MPFs in directory will be aggregated
    #having trouble with memory allocaion with this feature.
    
    all_prods<-list_products_in_file(
        location_original=location_MPF,
        ending=ending
        )   
    
    if (list_of_prods[1]!="ALL"){
        all_prods<-all_prods[all_prods%in%list_of_prods]
    }
    
    #loop through all products
    # prod<-all_prods[1]
    MPF_DB<-data.table()
    for (prod in all_prods){
        
        #unforunately, need a 64 bit machine to use fread with big files... ironic!
        file_size<-file.info(dir_file(location_MPF,prod,ending))$size/10^6
        
        if(file_size<5){
            #if less than 5mb, then can use fread
            #read in MPF data
            MPF_prod<-read_MPF_fast(file_dir=location_MPF,
                                    product=prod,
                                    ending=ending)
        }
        if(file_size>=5){
            #if less than 5mb, then can use fread
            #read in MPF data
            MPF_prod<-read_MPF(file_dir=location_MPF,
                               product=prod,
                               ending=ending)
            MPF_prod<-as.data.table(MPF_prod[[1]])
        }
        
        
        print(paste(prod,"___",colnames(MPF_prod),sep=""))
        
        #add product to extract
        MPF_prod[,PROPHET_PRODUCT_CODE:=prod]
        
        #append MPF to DB
        MPF_DB<-rbind(MPF_DB,MPF_prod,fill=TRUE)
        
        rm(MPF_prod); gc(); #cleanup
        
    }
    
    return(MPF_DB)
    
}

### function to pull all the param tables append together to create super table ###
pull_param_tables <- function(location_tables = "T:/Valuation/TABLES/1603/Val_New",
                              param_list = list("C","A","P","G","U")) {
    
    #purpose of function is to extract each of the PARAM tables;
    #merge these into one big table 
    #and return the result.
    
    #loop through and import each of the param tables
    
    param_all <- data.table()
    for (letter in param_list) {
        param_name <- paste("Param",letter,sep = "")
        #pull param table
        assign(param_name,
               as.data.table(
                   read_MPF(
                       file_dir = location_tables,
                       product = param_name,
                       ending = ".FAC"
                   )[[1]]
               ))
        #some cols in PARAMG are NA for some reason. filter
        not_na_cols <-
            colnames(get(param_name))[!is.na(colnames(get(param_name)))]
        #rbind all of the param tables
        param_all <- rbind(param_all,
                           get(param_name)[!is.na(PROD_CODE) &
                                               PROD_CODE != "",(not_na_cols),with = FALSE],
                           fill = TRUE)
        
    }
    
    #check no dups in param tables:
    # param_all[duplicated(param_all,by="PROD_CODE")]
    # length(param_all$PROD_CODE)-length(unique(param_all$PROD_CODE)) #none!
    
    setnames(param_all,'PROD_CODE',"PROPHET_PRODUCT_CODE")
    
    return(param_all)
    
}