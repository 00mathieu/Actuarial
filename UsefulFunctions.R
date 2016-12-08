####################################################################

# Title:    Useful R functions
# Author:   Mathieu Jones
# Date:     18/07/2016
# Version:  

### Description: ###
# Define functions that are useful 
# Or functions that I produce that don't pertain to any specific job

####################################################################

#Function to pull data from MS Access DB
QueryAccessDB<-function(access_db_dir,query="star"){
    
    require(RODBC);require(data.table)
    
    channel_string<-paste("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=",
                          access_db_dir,sep="")
    channel <- odbcDriverConnect(channel_string)
    
    retrieved_data<-as.data.table(sqlQuery(channel, query))
    
    return(retrieved_data)
    
    #example of use:
#         DI<-QueryAccessDB(
#             access_db_dir=
#                 "U:/Experience/Experience Investigations/2016 FY/Disability Income/1. Data/Data-new - dec2015/Access data base/EXPOSURE 2015- CONSOLIDATED.accdb",
#             query="select top 1000 * from [Exposure Data DI - Consolidated]")
    
    #NB:
    #if the above example doesn't work, it's probably because of the office 
    #upgrade to 2013. You need to get and admin to install the following:
    #https://www.microsoft.com/en-ca/download/details.aspx?id=13255
    
}

#Function to copy to excel
xl<-function(db){
    
    write.table(db
                , "clipboard", sep = "\t", row.names = FALSE, col.names = TRUE)
}

#other functions:
AgeLastBirthday <- function(Birthday, today_day) {
    Birthday = as.POSIXlt(Birthday)
    today_day = as.POSIXlt(today_day)
    
    ALB = today_day$year - Birthday$year
    
    if(today_day$mon < Birthday$mon |
       (today_day$mon == Birthday$mon & today_day$mday < Birthday$mday)) {
        ALB <- ALB - 1
    }
    
    return(ALB)
}
factor_to_num<-function(db,factor_cols){
    ### turns columns in factor_cols from factor format to numeric format
    
    #I do this in a loop... inefficient I know, but i couldn't get lapply to 
    #work with a list of columns referred to in a variable format
    
    for (numcol in factor_cols){
        db[,eval(numcol):=as.character(get(numcol)),]
        db[,eval(numcol):=as.numeric(get(numcol)),]
    }
    
    return(db)
    
}
fit_ln<-function(db,variable="Gross_Claims_Cost",dist="lnorm"){
    
    require(fitdistrplus)
    
    data<-as.data.frame(db[INCIDENCE=="1",.(
        Policy_Claim_Count=sum(COUNT),
        Total_Months_OnClaim=sum(COUNT*SURVIVAL_T),
        Avg_Months_OnClaim=sum(COUNT*SURVIVAL_T)/sum(COUNT),
        Net_Claims_Cost=sum(Claims_Cost_Net),
        Gross_Claims_Cost=sum(Claims_Cost_Gross)
    ),by=SIM_NUMBER])
    
    data<-data[,variable]
    
    f<-fitdist(data,dist)
    
    desc<-descdist(data,discrete=FALSE,boot=500)
    
    # plo<-plot(f)
    
    return(list(f,desc))
    
    
}
t_test_plot<-function(col1,col2,name1,name2,overwrite_title=NA){
    #idea is that I want a function to compare two datasets to see if they are the same
    #want to output ttest
    #want to also output boxplot and density graphs
    require(data.table); require(ggplot2)
    
    db1<-data.table(name=name1,data=col1)
    db2<-data.table(name=name2,data=col2)
    db<-rbind(db1,db2)
    
    
    mean1<-as.numeric(db[name==name1,.(MEAN=mean(data))])
    mean2<-as.numeric(db[name==name2,.(MEAN=mean(data))])
    
    t_result<-t.test(col1,col2)
    
    title1<-"Two Samples Are Same"
    if(t_result$p.value<0.05){title1<-"Two Samples Are Different"}
    if(!is.na(overwrite_title)){title1<=overwrite_title}
    
    density_p<-ggplot(db, environment=environment(),
                      aes(data, fill = name)) + 
        geom_density(alpha = 0.2)+
        geom_vline(xintercept=mean1, color="blue")+
        geom_vline(xintercept=mean2, color="red")+
        labs(title = title1)+
        theme_bw()+
        theme(
            plot.title = element_text(size = 20,face = "bold"),
            axis.text = element_text(size = 12),
            axis.title = element_text(size = 16,face = "bold"),
            legend.justification = c(1,1), legend.position = c(1,1),
            legend.title = element_blank()
        )
    
    print(density_p)
    
    return(density_p)
    
    #example:
    #     t_test_plot(
    #         col1=rnorm(200,12,1),
    #         col2=rnorm(150,10,5),
    #         name1="smaller",
    #         name2="larger")
    
}
