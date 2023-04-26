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

# get the range of dates we want to analyze (Feb 1 to Feb 7) in an arrays by days
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

# plot the mean line
abline(h = mean, col = "red")

# plot linear regression
DF <- data.frame(Time = 1:8, Net_generation =rollup.slice.Net_generation_cube)
fit <- lm(Net_generation ~ Time, data = DF)
abline(fit, col = "green")
