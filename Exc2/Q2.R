library(lubridate)

A <- read.delim('Exc2/table.tsv')
A$DateTime <- as.POSIXct( A$megawatthours, tz = "EST", "%H:%M EST %m/%d/%Y" )

D <- A[ order(A$DateTime), ]
print(is.data.frame(D))
D$Hour <- as.integer(hour(D$DateTime))
# filter by hour, if it is between 8 and 20
D <- D[(D$Hour >= 10 & D$Hour <= 18) ,] # | (D$Hour >= 20 | D$Hour <= 3)


# get the demand for the next Regions JM, NYIS, ISNE, FPL, CPLE
C <- with(D, cbind(  Demand.2,Demand.4, Demand.5,
                    Demand.7 , Demand.9 , Hour))
C <- as.data.frame(C)
# aggregate by identical hours
C <- aggregate(C, by=list(C$Hour), FUN=mean)
# mean values
M <- list( Demand.2 = NA, Demand.4 = NA, Demand.5 = NA,
           Demand.7 = NA, Demand.9 = NA )

# standard deviations
S <- list( Demand.2 = NA, Demand.4 = NA, Demand.5 = NA,
           Demand.7 = NA, Demand.9 = NA )
# linear fit
LM <- list( Demand.2 = NA, Demand.4 = NA, Demand.5 = NA,
            Demand.7 = NA, Demand.9 = NA )

# create an empty plot
plot(1, type="n", xlab="", ylab="", xlim = c(10,18), ylim=c(-1000, 120000))

# calculate means and stdev
demands <- seq(5) # [ -c(1,4,8) ]
for ( i in demands ) {
  M[[ i ]] <- mean( C[ !is.na(C[, i]) ,i ] )
  S[[ i ]] <- sd( C[ !is.na(C[, i]), i ] )
}

MD <- list( Demand.2 = NA, Demand.4 = NA, Demand.5 = NA,
            Demand.7 = NA, Demand.9 = NA )




# scale and center the consumption series (normalize)
norm.C <- t( (t(C) - unlist(M)) / unlist(S) )
print(norm.C)

demands <- 2:6
for ( i in demands ) {
  # rearrange in a new, temporary dataframe
  DF <- data.frame ( Time = 10:18, Demand = C[ , i ] )
  # fill NA with 0
  DF[ is.na(DF) ] <- 0
  # plot
  lines( DF, col = i, type = 'b' )
  # linear fit
  LM[[ i-1 ]] <- lm( Demand ~ Time, data = DF)
  a <- coef(LM[[ i-1 ]])[1]
  b <- coef(LM[[ i-1 ]])[2]
  #abline(a, b, col = i, lw =2)
}
legend( 'bottomleft', col = demands, pch = 19,
        legend = sapply(demands, function(x) paste0('Demand.',x) ))

# mean regression line
lm.df <- sapply(LM, function(c) coef(c))
n <- dim(lm.df)[2]
tot <- rowSums(lm.df)
a <- tot[1] / n
b <- tot[2] / n

# plot the mean regression line
abline(a, b, col = 'black', lw = 4, lt = 2)
time.min <- "10:00"
time.max <- "18:00"
title(paste0("Normalized demand over " , time.min, '-', time.max) )




xml <- 5