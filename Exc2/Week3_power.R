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

# range of dates we want to analyze (Feb 1 to Feb 7) in an arrays by days
startDate <- as.POSIXct("2021-02-07 00:00:01 IST")
endDate <- as.POSIXct("2021-02-14 23:59:59 IST")

week_of_february <- electricity_fact_table[electricity_fact_table$DateTime >= startDate & electricity_fact_table$DateTime <= endDate, ]

# separate the date and time in two columns
week_of_february$Date <- as.Date(week_of_february$DateTime)
week_of_february$hour <- format(week_of_february$DateTime, "%H")

# fill NA with 0
week_of_february$Net.generation[is.na(week_of_february$Net.generation)] <- 0
#drop duplicates
week_of_february <- week_of_february[!duplicated(week_of_february), ]
# Create a 3d cube with the dimensions: Date, hour, region for the net generation
Net_generation_cube <- acast(week_of_february, Date ~ hour ~ Region, value.var = "Net.generation")
# slice the demand across the us
slice.Net_generation_cube <- Net_generation_cube[, , 'United.States.Lower.48..region.']

# convert to dataframe
slice.Net_generation_cube <- as.data.frame(slice.Net_generation_cube)

#drop the first row
slice.Net_generation_cube <- slice.Net_generation_cube[-1, ]

# fill na with 0
slice.Net_generation_cube[is.na(slice.Net_generation_cube)] <- 0

# sum over the rows (i.e sum the hours)
rollup.slice.Net_generation_cube <- rowSums(slice.Net_generation_cube)
#drop the NA
rollup.slice.Net_generation_cube <- na.omit(rollup.slice.Net_generation_cube)
#get the mean
mean <- mean(rollup.slice.Net_generation_cube)

#plot Net generation across the us
plot(rollup.slice.Net_generation_cube, type = "l", col = "blue", xlab = "Date", ylab = "Net generation across the US")

# rename the x axis to be dates
axis(1, at = 1:8, labels =c("7 feb","8 feb","9 feb","10 feb","11 feb","12 feb","13 feb","14 feb"))

# plot the mean line
abline(h = mean, col = "red")

# plot linear regression
DF <- data.frame(Time = 1:8, Net_generation =rollup.slice.Net_generation_cube)
fit <- lm(Net_generation ~ Time, data = DF)
abline(fit, col = "green")

# coefficients of the linear regression
result_week_of_februar <- coef(fit)

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
LM[[ 6 ]] <- lm( Demand ~ Time, data = DF)
a <- coef(LM[[ 6 ]])[1]
b <- coef(LM[[ 6 ]])[2]
abline(a, b, col = 7, lw =2)

result_10_18 <- sapply(LM, coef)


regions <- c('PJM', 'NYIS', 'ISNE', 'FPL', 'CPLE' , 'East coast')
legend( 'topleft', col = 2:7, pch = 19,
        legend = sapply(regions, function(x) paste0('Demand -',x) ),cex = 0.75)

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
dice.east_demand_cube_2 <- east_demand_cube[c(21,22,23,24,1,2,3), c('PJM', 'NYIS', 'ISNE', 'FPL', 'CPLE')]

# create an empty plot without lables at the x axis
plot(0,0, xlim = c(1,7), ylim = c(0, 110000), xlab = 'Hour', ylab = 'Demand', type = 'n')

# rename the x axis ticks to be the hours 20pm - 3am and put it under the x axis
axis(1, at = 1:7, labels = c('20pm', '21pm', '22pm', '23pm', '0am', '1am', '2am'), pos = 0)
colors <- c('red', 'blue', 'green', 'yellow', 'black')
LM <- list()
#for each row in east_demand plot a line
for (i in 1:ncol(dice.east_demand_cube_2)) {
  # create a data frame for each region, Time will be the rows umbers of the cube, Demand will be the values of the cube
  DF <- data.frame (  Time = 1:7,Demand = dice.east_demand_cube_2[ , i] )
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
DF <- data.frame (  Time = 1:7,Demand = rowMeans(dice.east_demand_cube_2[ , 1:ncol(dice.east_demand_cube_2)] ) )
# fill NA with 0
DF[ is.na(DF) ] <- 0
# plot
lines( DF, col = 7, type = 'b' )
# linear fit
LM[[ 6 ]] <- lm( Demand ~ Time, data = DF)
a <- coef(LM[[ 6 ]])[1]
b <- coef(LM[[ 6 ]])[2]
abline(a, b, col = 7, lw =2)

# coefficients of the linear regression
result_20_3 <- sapply(LM, coef)


regions <- c('PJM', 'NYIS', 'ISNE', 'FPL', 'CPLE' , 'East coast')
legend( 'topleft', col = 2:7, pch = 19,
        legend = sapply(regions, function(x) paste0('Demand -',x) ),cex = 0.75)

# matrix U for the linear regression with 7 columns and 2 rows for the week_of_februar regression,
#The upper row is all 1s

U_week_of_februar <- matrix(1, nrow = 2, ncol = 7)
# the second row is the numbers 1 to 7
U_week_of_februar[2,] <- 1:7
U_T_week_of_februar <- t(U_week_of_februar)

# same for the demand between 10am and 18pm
U_10_18 <- matrix(1, nrow = 2, ncol = 9)
U_10_18[2,] <- 10:18
U_T_10_18 <- t(U_10_18)

# same for the demand between 20pm and 3am
U_20_3 <- matrix(1, nrow = 2, ncol = 8)
U_20_3[2,] <- c(20:23, 0:3)
U_T_20_3 <- t(U_20_3)

save(file = '../Week3_power.rdata', electricity_fact_table, east_demand_cube, Net_generation_cube,
     rollup.slice.Net_generation_cube, dice.east_demand_cube, dice.east_demand_cube_2, U_week_of_februar,
     U_T_week_of_februar, result_week_of_februar, U_10_18, U_T_10_18, U_20_3, U_T_20_3, result_10_18, result_20_3)