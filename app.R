library(shiny)
library(rhandsontable)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(readr)
library(maps)

# Define UI for application
ui <- fluidPage(
    verticalLayout(
        title = "COVID-19 Cases by State",

        conditionalPanel(condition = "input.maptype == 'Continuous'",
                         plotOutput("cplot",
                                    width = 999,
                                    height = 557)),
        conditionalPanel(condition = "input.maptype == 'Binary'",
                         plotOutput("bplot",
                                    width = 999,
                                    height = 557)),
        
        radioButtons("maptype",
                     "Map Type",
                     choices = c("Continuous", "Binary"),
                     inline = TRUE
        )
    )
)


# Define server logic r
server <- function(input, output) {
    ## Read .csv files for data used
    dataurl <- "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv"
    statedatao <- read.csv(url(dataurl))
    statedata <- statedatao[statedatao$date >= (Sys.Date() - 14), ]
    states <- read.csv("states.csv")
    
    
    ## Define a function that creates a dataframe for a given state
    P <- function(state){
        s <- statedata[statedata$state == state, ]
        o <- s[order(s$date), ]
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
        totaldiff = 1:51,
        isdecreasing = 1:51
    )
    
    ## Function that creates a dataframe with just the total change in cases
    Q <- function(state){
        df <- Z[[state]]
        f <- df[nrow(df), "cases"] - df[1, "cases"]
        W[W$region == tolower(state), 2] <<- f
    }
    
    ## Execute Q across all states
    for (i in states$State){
        Q(i)
    }
    
    ## Add binary isdecreasing variable
    for (i in 1:51) {
        val <- W[i, "totaldiff"]
        if (val >= 0){
            W[[i, "isdecreasing"]] <- 0
        } else {
            W[[i, "isdecreasing"]] <- 1
        }
    }
    
    
    
    ## Prepare map data
    states_map <- map_data("state")
    diff_map <- left_join(states_map, W, by = "region")
    
    ## Output continous map
    output$cplot <- renderPlot({
        ggplot(diff_map, aes(long, lat, group = group))+
            theme_void() +
            ggtitle("COVID-19 Cases: 14 Day Trajectory") +
            theme(legend.title = element_text(face="bold"),
                  plot.title = element_text(face = "bold")) +
            geom_polygon(aes(fill = totaldiff), color = "white") +
            scale_fill_gradient2(low = "green",
                                 mid = "grey86",
                                 midpoint = 0,
                                 high = "red",
                                 name = "Total change in cases",
                                 labels = -10:10 * 10^4,
                                 breaks = -10:10 * 10^4)
        
    })
    
    output$bplot <- renderPlot({ggplot(diff_map, aes(long, lat, group = group))+
            theme_void() +
            ggtitle("COVID-19 Cases: Are Cases Decreasing? (14 Day Period)") +
            theme(plot.title = element_text(face = "bold"),
                  legend.title = element_text(face="bold")) +
            geom_polygon(aes(fill = factor(isdecreasing)), color = "white")+ 
            scale_fill_manual(name = "Decreasing?",
                              breaks = c(0, 1),
                              labels = c("No", "Yes"),
                              values = c("Red", "Green")
            )
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
