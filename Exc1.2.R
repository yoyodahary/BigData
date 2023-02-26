# load the mvtWeek1.csv

week1 <- read.csv("mvtWeek1.csv", header = TRUE, sep = ",")

# extract the number of unique locations

uniqueLocations <- length(unique(week1$LocationDescription))

# extract the number of unique districts

uniqueDistricts <- length(unique(week1$District))

print(uniqueLocations)
print(uniqueDistricts)
