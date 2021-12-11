interpolate.age.over.time <- function(points.in, factors.in=rep(1,times=8)) {
  
  # Define function to produce working table from input data and initial factors
  iaot.base <- function(points.in,factors.in) {
    
    # Initialize working table
    wt <- matrix(0,nrow=11,ncol=5)  # working table
    rownames(wt) <- 2010:2020
    colnames(wt) <- c("int1","factors","int2","ma3","sd")
    
    # Preliminary calculations in working table
    wt[1,"int1"]    <- y1 <- points.in[1]  # Input data
    wt[6,"int1"]    <- y2 <- points.in[2]
    wt[11,"int1"]   <- y3 <- points.in[3]
    wt[2:5,"int1"]  <- approx(c(2010,2015),c(y1,y2),xout=2011:2014)$y  # Linear interpolation
    wt[7:10,"int1"] <- approx(c(2015,2020),c(y2,y3),xout=2016:2019)$y
    wt[c(2:5,7:10),"factors"]  <- factors.in
    wt[c(1,6,11), "int2"]  <- wt[c(1,6,11),"int1"]  # Define factors
    wt[c(2:5,7:10),"int2"] <- wt[c(2:5,7:10), "int1"] * wt[c(2:5,7:10), "factors"]
    
    # Calculate 3  point moving average of int2 and squared differences
    ma3 <- function(x) {
      i <- 2:(length(x)-1)
      c(x[1],(x[i-1] + x[i] + x[i+1])/3,x[length(x)])
    }
    wt[,"ma3"] <- ma3(wt[,"int2"])
    wt[,"sd"]  <- 1000*((wt[,"int2"] - wt[,"ma3"])^2)
    wt[,"sd"]  <- wt[,"sd"]
    
    # Calculate sum of squared deviations as measure of roughness
    ssd <- sum(wt[,"sd"])
    
    # Round working table entries for display
    wt[,c("int1","int2","ma3","sd")] <- round(wt[,c("int1","int2","ma3","sd")],3)
    wt[,"factors"] <- round(wt[,"factors"],6)
    
    
    # Return result, working table and sum of squared deviations (measure of roughness)
    list(working.table=wt,ssd=ssd,interpolated.values=wt[,"int2"])
    
  }
  
  # Define function to which optim() will be applied
  ssd.from.factors <- function(factors.in) {
    iaot.base(points.in,factors.in)$ssd
  }
  
  # Apply optim() to get factors that minimize roughness
  factors.out <- optim(factors.in, ssd.from.factors, method="BFGS")$par
  
  # Apply iaot.base() to get results for factors.out, return.result
  iaot.base(points.in,factors.out)
  
}  

interpolate.along.cohorts <- function(perimeter.table) {
  
  pt <- perimeter.table
  
  # UPPER RIGHT TRIANGLE: Cohorts age 5 at midyear Year2, Year3, Year4
  multiplier <- (pt["9",6]/pt["5",2])^(1/4)
  pt["6",3] <- pt["5",2]*multiplier
  pt["7",4] <- pt["6",3]*multiplier
  pt["8",5] <- pt["7",4]*multiplier
  
  multiplier <- (pt["8",6]/pt["5",3])^(1/3)
  pt["6",4] <- pt["5",3]*multiplier
  pt["7",5] <- pt["6",4]*multiplier
  
  multiplier <- (pt["7",6]/pt["5",4])^(1/2)
  pt["6",5] <- pt["5",4]*multiplier
  
  # MAIN PARALLELOGRAM: Cohorts age 5-74 at midyear 2010
  multipliers <- (pt[6:75,6]/pt[1:70,1])^(1/5)
  j <- 1  # Not logically required, but helpful to keep from getting confused
  # i is age, j is year, k is movement down cohort
  for (i in 1:70) {
    for (k in 1:4) {
      pt[i+k,j+k] <- pt[i+k-1,j+k-1]*multipliers[i]
    }
  }
  
  # LOWER LEFT TRIANGLE: Cohorts age 75-77 at midyear Year1
  multiplier <- (pt["79",5]/pt["75",1])^(1/4)
  pt["76",2] <- pt["75",1]*multiplier
  pt["77",3] <- pt["76",2]*multiplier
  pt["78",4] <- pt["77",3]*multiplier
  
  multiplier <- (pt["79",4]/pt["76",1])^(1/4)
  pt["77",2] <- pt["76",1]*multiplier
  pt["78",3] <- pt["77",2]*multiplier
  
  multiplier <- (pt["79",3]/pt["77",1])^(1/4)
  pt["78",2] <- pt["77",1]*multiplier
  
  pt  # Return result, which is now full table for 5 year time period
}

modified.midpoint.interpolation <- function(ad5.in,factors=rep(1,times=16)) {
  
  # Define function mmpi.base()
  mmpi.base <- function(ad5.in,factors) {
    # Initialize table1
    table1 <- matrix(0,nrow=16,ncol=6)
    rownames(table1) <- c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79")
    colnames(table1) <- c("mid","obs","fac","fxO","fit","df^2")
    table1[,"mid"] <- 5*1:16 - 2.5
    table1[,"obs"] <- ad5.in[1:16]
    table1[,"fac"] <- factors
    table1[,"fxO"] <- ad5.in[1:16]*factors
    
    # Create table2
    table2 <- matrix(0,nrow=80,ncol=5)
    rownames(table2) <- 0:79
    colnames(table2) <- c("mid","int1","int2","int3","sn")
    table2[,"mid"] <- 0:79+0.5
    table2[,"sn"] <- 1:80
    x <- 5*1:16 -3
    y <- table1[,"fxO"]/5
    table2[3:78,"int1"] <- approx(x,y,2:77)$y
    table2[1:2,"int1"] <- table2[3,"int1"]
    table2[79:80,"int1"] <- table2[78,"int1"]
    table2[2:79,"int2"] <- (table2[1:78,"int1"]+table2[2:79,"int1"]+table2[3:80,"int1"])/3
    table2[1,"int2"] <- table2[2,"int2"]
    table2[80,"int2"] <- table2[79,"int2"]
    table2[2:79,"int3"] <- (table2[1:78,"int2"]+table2[2:79,"int2"]+table2[3:80,"int2"])/3
    table2[1,"int3"] <- table2[2,"int3"]
    table2[80,"int3"] <- table2[79,"int3"]
    
    # Finalize table1
    table1[,"fit"] <- apply(matrix(table2[,"int3"],nrow=16,ncol=5,byrow=TRUE),1,sum)
    table1[,"df^2"] <- ((table1[,"obs"]-table1[,"fit"])^2)/1000
    ssd <- sum(table1[,"df^2"])
    
    # Return results
    list(table1=table1,ssd=ssd,table2=table2,interpolated.values=table2[,"int3"])
  }
  
  
  # Define function for optim()
  ssd.from.factors <- function(factors) {
    mmpi.base(ad5.in,factors)$ssd
  }
  
  # Apply optim() to get factors that minimize SSD
  factors.out <- optim(factors, ssd.from.factors, method="BFGS")$par
  
  # Use these factors to produce mmpi.base() output
  mmpi.base(ad5.in,factors.out)
  
}

calculate.country.sex.data.out <- function(country.sex.data.in) {
  
  source("modified.midpoint.interpolation.R")
  source("interpolate.age.over.time.R")
  source("interpolate.along.cohorts.R")
  
  # Initialization
  country   <- country.sex.data.in$country
  sex       <- country.sex.data.in$sex
  table.in  <- country.sex.data.in$table
  table.out <- matrix(0,nrow=75,ncol=11)
  rownames(table.out) <- 5:79
  colnames(table.out) <- 2010:2020
  
  # Interpolate perimeter numbers
  # First, input 5 year age groups to single years (to generalize, loop on years)
  table.out[,"2010"]  <- modified.midpoint.interpolation(table.in[,"2010"])$table2[5:79,"int3"]
  table.out[,"2015"]  <- modified.midpoint.interpolation(table.in[,"2015"])$table2[5:79,"int3"]
  table.out[,"2020"]  <- modified.midpoint.interpolation(table.in[,"2020"])$table2[5:79,"int3"]
  
  # Then numbers age 5 and 79 for ALL in-between years
  years <- as.character(c(2011:2014,2016:2019))
  table.out[1,years]  <- interpolate.age.over.time(table.out[1,c(1,6,11)])$interpolated.values[years]
  table.out[75,years] <- interpolate.age.over.time(table.out[75,c(1,6,11)])$interpolated.values[years]
  
  # Interpolate interior numbers along cohorts (to generalize, loop on time periods)
  perimeter.table <- table.out[,1:6]
  table.out[,1:6] <- interpolate.along.cohorts(perimeter.table)
  
  perimeter.table <- table.out[,6:11]
  table.out[,6:11] <- interpolate.along.cohorts(perimeter.table)
  
  # Return result
  list(country=country, sex=sex, table.in=table.in, table.out=round(table.out,3))
  
}
