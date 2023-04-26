A <- read.delim('Exc2/table.tsv')
A$DateTime <- as.POSIXct( A$megawatthours, tz = "EST", "%H:%M EST %m/%d/%Y" )


B <- A[ order(A$DateTime), ]

# get the range of dates we want to analyze (Feb 1 to Feb 7) in an arrays by days
startDate <- as.POSIXct("2021-02-08 00:00:01 IST")
endDate <- as.POSIXct("2021-02-14 23:59:59 IST")
D <- A[A$DateTime > startDate & A$DateTime < endDate,]
D <- D[order(D$DateTime),]
# filter only the datetime and the net.generation.10
D <- D[, c('DateTime', 'Net.generation.10')]
# Group the data by day
D <- aggregate(D$Net.generation.10, by = list(as.Date(D$DateTime)), FUN = sum)
# Calculate the mean and standard deviation of the net.generation.10

# Calculate the mean and standard deviation of the net.generation.10
mean <- mean(D$x)

#plot D
plot(D$x, type = "l", xlab = "Day", ylab = "Net.generation.10",
     main = "Us power generation for the week of Feb 8 to Feb 14")

# plot the mean line
abline(h = mean, col = "red")


