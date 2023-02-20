acronym <- function(input) {
    # input is a character vector
    # output is a character vector
    # your code here
    output <- toupper(substring(input, 1, 1))
    z <- nchar(input)
    for (i in 2:z) {
      x<-substring(input, i, i)
      if (x == " " || x == "-" || x=="_") {
        y <- substring(input, i+1, i+1)
        if (y != " " && y != "-" && y!="_") {
          output <- paste(output, toupper(y), sep="")
        }
      }
    }
    return(output)
}

input <- 'Portable Network Graphics'
print(acronym(input)=="PNG")