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

# dice the cube to hours 20 to 3 and the regions PJM, NYIS, ISNE, FPL, CPLE
dice.east_demand_cube <- east_demand_cube[c(21,22,23,24,1,2,3), c('PJM', 'NYIS', 'ISNE', 'FPL', 'CPLE')]

# create an empty plot without lables at the x axis
plot(0,0, xlim = c(1,7), ylim = c(0, 110000), xlab = 'Hour', ylab = 'Demand', type = 'n')

# rename the x axis ticks to be the hours 20pm - 3am and put it under the x axis
axis(1, at = 1:7, labels = c('20pm', '21pm', '22pm', '23pm', '0am', '1am', '2am'), pos = 0)
colors <- c('red', 'blue', 'green', 'yellow', 'black')
LM <- list()
#for each row in east_demand plot a line
for (i in 1:ncol(dice.east_demand_cube)) {
  # create a data frame for each region, Time will be the rows umbers of the cube, Demand will be the values of the cube
  DF <- data.frame (  Time = 1:7,Demand = dice.east_demand_cube[ , i] )
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
DF <- data.frame (  Time = 1:7,Demand = rowMeans(dice.east_demand_cube[ , 1:ncol(dice.east_demand_cube)] ) )
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
legend( 'topleft', col = 2:7, pch = 19,
        legend = sapply(regions, function(x) paste0('Demand -',x) ),cex = 0.75)