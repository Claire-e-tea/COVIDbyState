## Load relevant libraries
library(ggplot2)
library(dplyr)
library (readr)
require(maps)
require(viridis)
require(readr)

## Read .csv files for data used
dataurl <- "https://github.com/nytimes/covid-19-data/blob/master/us-states.csv"
statedata <- read.csv("us-states.csv")
states <- read.csv("states.csv")


## Define a function that creates a dataframe for a given state, including the change in cases as a column
P <- function(state){
      s <- statedata[statedata$state == state, ]
      o <- s[order(s$date), ]
      diffs <- NULL
      
      for (i in 2:nrow(o)){
            val1 <- o[i, "cases"]
            val2 <- o[i-1, "cases"]
            diff <- val1 - val2
            diffs <- c(diffs, diff)
            
      }
      o$diff <- c(NA, diffs)
      return(o)
}

## Revert Z to an empty object (for testing purposes)
Z <- NULL


## Create a list Z where each value is a dataframe for a state
for (i in states$State){
      Z[[i]] <- P(i)
}
