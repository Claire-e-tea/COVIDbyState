## Load relevant libraries
library(ggplot2)
library(dplyr)
library (readr)
require(maps)
require(viridis)
require(readr)

## Read .csv files for data used
dataurl <- "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv"
statedatao <- read.csv(url(dataurl))
statedata <- statedatao[statedatao$date >= (Sys.Date() - 14), ]
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
   o$state <- tolower(o$state)
   return(o)
}

## Revert Z to an empty object
Z <- NULL


## Create a list Z where each value is a dataframe for a state
for (i in states$State){
   Z[[i]] <- P(i)
}

## Reset W to Null
W <- data.frame(
   region = tolower(states$State),
   totaldiff = 1:51
)

## Function that creates a dataframe with just the total change in cases
Q <- function(state){
   df <- Z[[state]]
   f <- sum(df$diff, na.rm = TRUE)
   W[W$region == tolower(state), 2] <<- f
   f
}

## Execute Q across all states
for (i in states$State){
   Q(i)
}

states_map <- map_data("state")
diff_map <- left_join(states_map, W, by = "region")
ggplot(diff_map, aes(long, lat, group = group))+
   theme_void() +
   theme(legend.title = element_text(face="bold"))+
   geom_polygon(aes(fill = totaldiff), color = "white")+
   scale_fill_gradient2(low = "green",
                        mid = "grey86",
                        midpoint = 0,
                        high = "red",
                        name = "Total change in cases (in tens of thousands)",
                        labels = paste(c(-1, 0, 1, 2, 3, 4)),
                        breaks = c(-1, 0, 1, 2, 3, 4) * 10^5)
