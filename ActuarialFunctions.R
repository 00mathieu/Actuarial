####################################################################

# Title:        Actuarial Functions
# Author:       Mathieu Jones
# Date:         8/12/2016
# Description:  This file has a bunch of functions that I created that have to  
#               do with 'actuarial calculations'... might be useful again

####################################################################

add_variance<-function(x,N,x_2){
    exected_x<-x/N
    expected_x2<-x_2/N
    variance<-expected_x2-exected_x^2
    return(variance)
}
add_confidence_int<-function(p=0.95,Nx,sigma2,m){
    
    #prob(lapsed amount within +/- Y) = p
    #Y=(k*N*x*m)/sd(Nx(sigma^2 + m^2))
    #k = Y / (xN/(sigma^2/m^2+1))^0.5
    
    #Y is area under N(0,1) with p probability
    Y=qnorm(1-(1-p)/2,mean=0,sd=1)
    
    #var_premium_lapsed = NX(simga2 + m^2) (i.e. poison claim freq, size~m,sigma2)
    var_premium_lapsed<-(sigma2+m^2)*Nx
    
    k <- Y * (var_premium_lapsed^0.5) / (Nx*m)
    
    return(k)
    
    
}
Add_Lapse_Rate<-function(result_summary){
    
    ### add lapse rates ###
    
    #Lapse rate (by count) = sum(#lapses)/((sum(#IF0)+sum(#IF1))/2)
    #Lapse rate (by premium) = sum(#lapses*premium + #partial_lapses*-1*change(premium))
    # / ((sum(#IF0*premium1)+sum(#IF1*premium2))/2)
    #NB: if new business lapses within first month, then won't be captured.
    
    result_summary[,ACTUAL_LAPSE_RATE_COUNT:= LAPSE_COUNT /((IF1_COUNT + IF2_COUNT)/2)]
    result_summary[,ACTUAL_LAPSE_RATE_PREMIUM := 
                       (LAPSE_ANNUAL_PREMIUM-PLAP_ANNUAL_PREMIUM) / 
                       ((IF1_ANNUAL_PREMIUM+IF2_ANNUAL_PREMIUM)/2)]
    result_summary[,ACTUAL_PARTIAL_LAPSE_RATE := 
                       (-1*PLAP_ANNUAL_PREMIUM) / 
                       ((IF1_ANNUAL_PREMIUM+IF2_ANNUAL_PREMIUM)/2)]
    result_summary[,ACTUAL_CFI_RATE_COUNT:= CFI_COUNT /((IF1_COUNT + IF2_COUNT)/2)]
    result_summary[,NO_DAYS:=as.numeric(EXTRACT_DATE2-EXTRACT_DATE1)]
    
    ### confidence intervals ###
    # result_summary[,var := add_variance(x=IF1_ANNUAL_PREMIUM,N=IF1_COUNT,x_2=IF1_ANNUAL_PREMIUM2)]
    result_summary[,ACTUAL_LAPSE_RATE_PREMIUM_CI := add_confidence_int(
        p=0.95, 
        Nx = LAPSE_COUNT,
        sigma2 = add_variance(x=IF1_ANNUAL_PREMIUM,N=IF1_COUNT,x_2=IF1_ANNUAL_PREMIUM2),
        m=IF1_ANNUAL_PREMIUM/IF1_COUNT)
        ]
    result_summary[,ACTUAL_LAPSE_RATE_COUNT_CI := add_confidence_int(
        p=0.95, 
        Nx = LAPSE_COUNT,
        sigma2 = 0,
        m=1)
        ]
    result_summary[,ACTUAL_PARTIAL_LAPSE_RATE_CI := add_confidence_int(
        p=0.95, 
        Nx = PLAP_COUNT,
        sigma2 = add_variance(x=PLAP_ANNUAL_PREMIUM, N=PLAP_COUNT,x_2=PLAP_ANNUAL_PREMIUM2),
        m=1)
        ]
    
    #annulised lapse rate
    result_summary[,ACTUAL_LAPSE_RATE_COUNT_ANN := 1- (1-ACTUAL_LAPSE_RATE_COUNT)^(365/NO_DAYS)]
    result_summary[,ACTUAL_LAPSE_RATE_PREMIUM_ANN := 1-(1-ACTUAL_LAPSE_RATE_PREMIUM)^(365/NO_DAYS)]
    result_summary[,ACTUAL_PARTIAL_LAPSE_RATE_ANN := 1-(1-ACTUAL_PARTIAL_LAPSE_RATE)^(365/NO_DAYS)]
    result_summary[,ACTUAL_CFI_RATE_COUNT_ANN := 1-(1-ACTUAL_CFI_RATE_COUNT)^(365/NO_DAYS)]
    
    #upper CI on lapse rates
    result_summary[,ACTUAL_LAPSE_RATE_COUNT_ANN_UPPER := 
                       1-(1-ACTUAL_LAPSE_RATE_COUNT*(1+ACTUAL_LAPSE_RATE_COUNT_CI))^(365/NO_DAYS)]
    result_summary[,ACTUAL_LAPSE_RATE_PREMIUM_ANN_UPPER := 
                       1-(1-ACTUAL_LAPSE_RATE_PREMIUM*(1+ACTUAL_LAPSE_RATE_PREMIUM_CI))^(365/NO_DAYS)]
    result_summary[,ACTUAL_PARTIAL_LAPSE_RATE_UPPER := 
                       1-(1-ACTUAL_PARTIAL_LAPSE_RATE *(1+ACTUAL_PARTIAL_LAPSE_RATE_CI))^(365/NO_DAYS)]
    #lower CI on lapse rates
    result_summary[,ACTUAL_LAPSE_RATE_COUNT_ANN_LOWER := 
                       1-(1-ACTUAL_LAPSE_RATE_COUNT*(1-ACTUAL_LAPSE_RATE_COUNT_CI))^(365/NO_DAYS)]
    result_summary[,ACTUAL_LAPSE_RATE_PREMIUM_ANN_LOWER := 
                       1-(1-ACTUAL_LAPSE_RATE_PREMIUM*(1-ACTUAL_LAPSE_RATE_PREMIUM_CI))^(365/NO_DAYS)]
    result_summary[,ACTUAL_PARTIAL_LAPSE_RATE_LOWER := 
                       1-(1-ACTUAL_PARTIAL_LAPSE_RATE *(1-ACTUAL_PARTIAL_LAPSE_RATE_CI))^(365/NO_DAYS)]
    
    
    return(result_summary[order(-IF1_COUNT)])
    
}