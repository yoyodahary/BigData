library(lubridate)
library(reshape2)
library(plyr)

A <- read.delim('Exc2/table.tsv')
A$DateTime <- as.POSIXct( A$megawatthours, tz = "EST", "%H:%M EST %m/%d/%Y" )

D <- A[ order(A$DateTime), ]

# fill the columns of the regions with the region name
D$BPAT <- "BPAT"
D$CISO <- "CISO"
D$CPLE <- "CPLE"
D$ERCO <- "ERCO"
D$FPL <- "FPL"
D$ISNE <- "ISNE"
D$MISO <- "MISO"
D$NYIS <- "NYIS"
D$PACW <- "PACW"
D$PJM <- "PJM"
D$United.States.Lower.48..region. <- "United.States.Lower.48..region."

# create a data frame for each region
BPAT <- D[, c('DateTime', 'BPAT', 'Demand', 'Net.generation')]
# standarize column names
colnames(BPAT) <- c('DateTime', 'Region', 'Demand', 'Net.generation')
CISO <- D[, c('DateTime', 'CISO', 'Demand.1', 'Net.generation.1')]
# standarize column names
colnames(CISO) <- c('DateTime', 'Region', 'Demand', 'Net.generation')
CPLE <- D[, c('DateTime', 'CPLE', 'Demand.2', 'Net.generation.2')]
# standarize column names
colnames(CPLE) <- c('DateTime', 'Region', 'Demand', 'Net.generation')
ERCO <- D[, c('DateTime', 'ERCO', 'Demand.3', 'Net.generation.3')]
# standarize column names
colnames(ERCO) <- c('DateTime', 'Region', 'Demand', 'Net.generation')
FPL <- D[, c('DateTime', 'FPL', 'Demand.4', 'Net.generation.4')]
# standarize column names
colnames(FPL) <- c('DateTime', 'Region', 'Demand', 'Net.generation')
ISNE <- D[, c('DateTime', 'ISNE', 'Demand.5', 'Net.generation.5')]
# standarize column names
colnames(ISNE) <- c('DateTime', 'Region', 'Demand', 'Net.generation')
MISO <- D[, c('DateTime', 'MISO', 'Demand.6', 'Net.generation.6')]
# standarize column names
colnames(MISO) <- c('DateTime', 'Region', 'Demand', 'Net.generation')
NYIS <- D[, c('DateTime', 'NYIS', 'Demand.7', 'Net.generation.7')]
# standarize column names
colnames(NYIS) <- c('DateTime', 'Region', 'Demand', 'Net.generation')
PACW <- D[, c('DateTime', 'PACW', 'Demand.8', 'Net.generation.8')]
# standarize column names
colnames(PACW) <- c('DateTime', 'Region', 'Demand', 'Net.generation')
PJM <- D[, c('DateTime', 'PJM', 'Demand.9', 'Net.generation.9')]
# standarize column names
colnames(PJM) <- c('DateTime', 'Region', 'Demand', 'Net.generation')
United.States.Lower.48..region. <- D[, c('DateTime', 'United.States.Lower.48..region.', 'Demand.10', 'Net.generation.10')]
# standarize column names
colnames(United.States.Lower.48..region.) <- c('DateTime', 'Region', 'Demand', 'Net.generation')

# stack them one above the other
electricity_fact_table <- rbind(BPAT, CISO, CPLE, ERCO, FPL, ISNE, MISO, NYIS, PACW, PJM, United.States.Lower.48..region.)

# drop Net.generation column
electricity_fact_table <- electricity_fact_table[, c('DateTime', 'Region', 'Demand')]
#add hour column
electricity_fact_table$Hour <- hour(electricity_fact_table$DateTime)


#add hour column
tmp <- electricity_fact_table
tmp$Hour <- hour(tmp$DateTime)
#drop duplicates
tmp <- tmp[!duplicated(tmp), ]
#drop DateTime column
tmp <- tmp[, c('Hour', 'Region', 'Demand')]
# fill NA with 0
tmp[is.na(tmp)] <- 0
# make E 2d cube with hour and region, use tapply to take mean of demand for each hour and region
east_demand_cube <- tapply(tmp$Demand, list(tmp$Hour, tmp$Region), mean)

# dice the cube to hours 10 to 18 and the regions JM, NYIS, ISNE, FPL, CPLE
dice.east_demand_cube <- east_demand_cube[11:19, c('PJM', 'NYIS', 'ISNE', 'FPL', 'CPLE')]

# create an empty plot
plot(1, type="n", xlab="", ylab="", xlim = c(10,18), ylim=c(-1000, 120000))
# below the x axis put the hour name with am or pm
axis(1, at=10:18, labels=c("10am", "11am", "12pm", "1pm", "2pm", "3pm", "4pm", "5pm", "6pm"),pos=0)

LM <- list()
#for each row in east_demand plot a line
for (i in 1:ncol(dice.east_demand_cube)) {
    DF <- data.frame (  Time = 10:18,Demand = dice.east_demand_cube[ , i ] )
    # fill NA with 0
    DF[ is.na(DF) ] <- 0
    # plot
    lines( DF, col = i+1, type = 'b' )
    # linear fit
    LM[[ i ]] <- lm( Demand ~ Time, data = DF)
    a <- coef(LM[[ i ]])[1]
    b <- coef(LM[[ i ]])[2]
    abline(a, b, col = i+1, lw =2)
}


# calculate the overall mean and its regression
DF <- data.frame (  Time = 10:18,Demand = rowMeans(dice.east_demand_cube[ , 1:ncol(dice.east_demand_cube)] ) )
# fill NA with 0
DF[ is.na(DF) ] <- 0
# plot
lines( DF, col = 7, type = 'b' )
# linear fit
LM[[ 1 ]] <- lm( Demand ~ Time, data = DF)
a <- coef(LM[[ 1 ]])[1]
b <- coef(LM[[ 1 ]])[2]
abline(a, b, col = 7, lw =2)


regions <- c('PJM', 'NYIS', 'ISNE', 'FPL', 'CPLE' , 'East coast')
legend( 'bottomleft', col = 2:7, pch = 19,
        legend = sapply(regions, function(x) paste0('Demand -',x) ))

#
#
# # get the demand for the next Regions PJM, NYIS, ISNE, FPL, CPLE
# dice.east_demand <- electricity_fact_table[electricity_fact_table$Region %in% c('PJM', 'NYIS', 'ISNE', 'FPL', 'CPLE') &(electricity_fact_table$Hour >= 10 & electricity_fact_table$Hour <= 18), ]
# # drop net generation and DateTime columns
# dice.east_demand <- dice.east_demand[, c('DateTime', 'Region', 'Demand', 'Hour')]
#
#
# # drop duplicates
# dice.east_demand <- dice.east_demand[!duplicated(dice.east_demand), ]
# dice.east_demand[is.na(dice.east_demand)] <- 0
#
#
# #aggragate by identical hours, for each region
# dice.east_demand <- aggregate(dice.east_demand, by=list(dice.east_demand$Hour, dice.east_demand$Region), FUN=mean)
#
# # fill NA with 0
# dice.east_demand[is.na(dice.east_demand)] <- 0
#
# # drop hour and region columns
# dice.east_demand <- dice.east_demand[, c("Group.1", "Group.2", 'Demand')]
#
# # rename columns
# colnames(dice.east_demand) <- c('Hour', 'Region', 'Demand')
#
# # make it a 2d cube
# dice.east_demand <- dcast(dice.east_demand, Hour ~ Region, value.var = "Demand")
#
# # create an empty plot for the heatmap
# plot(1, type="n", xlab="", ylab="", xlim = c(10,18), ylim=c(-1000, 120000))
#
# #for each row in east_demand plot a line
# for (i in 2:ncol(dice.east_demand)) {
#     DF <- data.frame (  Time = 10:18,Demand = dice.east_demand[ , i ] )
#     # fill NA with 0
#     DF[ is.na(DF) ] <- 0
#     # plot
#     lines( DF, col = i, type = 'b' )
#     # linear fit
#     LM[[ i-1 ]] <- lm( Demand ~ Time, data = DF)
#     a <- coef(LM[[ i-1 ]])[1]
#     b <- coef(LM[[ i-1 ]])[2]
#     abline(a, b, col = i, lw =2)
# }
#
# # calculate the overall mean and its regression
# DF <- data.frame (  Time = 10:18,Demand = rowMeans(dice.east_demand[ , 2:ncol(dice.east_demand)] ) )
# # fill NA with 0
# DF[ is.na(DF) ] <- 0
# # plot
# lines( DF, col = 1, type = 'b' )
# # linear fit
# LM[[ 1 ]] <- lm( Demand ~ Time, data = DF)
# a <- coef(LM[[ 1 ]])[1]
# b <- coef(LM[[ 1 ]])[2]
# abline(a, b, col = 1, lw =2)


xml <- 5
# library(lubridate)
#
# A <- read.delim('Exc2/table.tsv')
# A$DateTime <- as.POSIXct( A$megawatthours, tz = "EST", "%H:%M EST %m/%d/%Y" )
#
# D <- A[ order(A$DateTime), ]
# print(is.data.frame(D))
# D$Hour <- as.integer(hour(D$DateTime))
# # filter by hour, if it is between 8 and 20
# D <- D[(D$Hour >= 10 & D$Hour <= 18) ,] # | (D$Hour >= 20 | D$Hour <= 3)
#
#
# # get the demand for the next Regions JM, NYIS, ISNE, FPL, CPLE
# C <- with(D, cbind(  Demand.2,Demand.4, Demand.5,
#                     Demand.7 , Demand.9 , Hour))
# C <- as.data.frame(C)
# # aggregate by identical hours
# C <- aggregate(C, by=list(C$Hour), FUN=mean)
# # mean values
# M <- list( Demand.2 = NA, Demand.4 = NA, Demand.5 = NA,
#            Demand.7 = NA, Demand.9 = NA )
#
# # standard deviations
# S <- list( Demand.2 = NA, Demand.4 = NA, Demand.5 = NA,
#            Demand.7 = NA, Demand.9 = NA )
# # linear fit
# LM <- list( Demand.2 = NA, Demand.4 = NA, Demand.5 = NA,
#             Demand.7 = NA, Demand.9 = NA )
#
# # create an empty plot
# plot(1, type="n", xlab="", ylab="", xlim = c(10,18), ylim=c(-1000, 120000))
#
# # calculate means and stdev
# demands <- seq(5) # [ -c(1,4,8) ]
# for ( i in demands ) {
#   M[[ i ]] <- mean( C[ !is.na(C[, i]) ,i ] )
#   S[[ i ]] <- sd( C[ !is.na(C[, i]), i ] )
# }
#
# MD <- list( Demand.2 = NA, Demand.4 = NA, Demand.5 = NA,
#             Demand.7 = NA, Demand.9 = NA )
#
#
#
#
# # scale and center the consumption series (normalize)
# norm.C <- t( (t(C) - unlist(M)) / unlist(S) )
# print(norm.C)
#
# demands <- 2:6
# for ( i in demands ) {
#   # rearrange in a new, temporary dataframe
#   DF <- data.frame ( Time = 10:18, Demand = C[ , i ] )
#   # fill NA with 0
#   DF[ is.na(DF) ] <- 0
#   # plot
#   lines( DF, col = i, type = 'b' )
#   # linear fit
#   LM[[ i-1 ]] <- lm( Demand ~ Time, data = DF)
#   a <- coef(LM[[ i-1 ]])[1]
#   b <- coef(LM[[ i-1 ]])[2]
#   #abline(a, b, col = i, lw =2)
# }
# legend( 'bottomleft', col = demands, pch = 19,
#         legend = sapply(demands, function(x) paste0('Demand.',x) ))
#
# # mean regression line
# lm.df <- sapply(LM, function(c) coef(c))
# n <- dim(lm.df)[2]
# tot <- rowSums(lm.df)
# a <- tot[1] / n
# b <- tot[2] / n
#
# # plot the mean regression line
# abline(a, b, col = 'black', lw = 4, lt = 2)
# time.min <- "10:00"
# time.max <- "18:00"
# title(paste0("Normalized demand over " , time.min, '-', time.max) )
#
#
#
#
# xml <- 5