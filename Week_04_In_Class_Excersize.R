pti <- c("shiny","tidyverse","ggplot2movies")
pti <- pti[!(pti %in% installed.packages())]
if(length(pti)>0){
    install.packages(pti)
}

##########
### Shiny starter code
##########
library(shiny)
library(tidyverse)
library(ggplot2movies)

# Set randomness seed
set.seed(61)
# Prepare data
shiny_movie_set <- 
    movies %>% 
    filter(year >= 2000) %>%
    select(title,year,length,rating,votes,Action:Short) %>% 
    gather(genre,value,Action:Short) %>% 
    filter(value == 1) %>% 
    select(-value)

# Get genre list
genres <- 
    shiny_movie_set %>% 
    distinct(genre) %>% 
    unlist(.)

names(genres) <- NULL

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Movie Length and IMDB Scores"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("years",
                        "Years",
                        min = min(shiny_movie_set$year),
                        max = max(shiny_movie_set$year),
                        value = c(2002,2003),
                        step = 1,
                        ticks = FALSE,
                        sep = ""),
            selectInput("genre", h3("Genre"), 
                        choices = c("All",genres),
                        selected = genres,
                        multiple = TRUE
                        ),
            sliderInput("votes",
                        "At least X Votes",
                        min = min(shiny_movie_set$votes),
                        max = max(shiny_movie_set$votes),
                        value = 30,
                        step = 2000,
                        ticks = FALSE)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("moviePlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$moviePlot <- renderPlot({
        print(input$genre)
        
        mydf = shiny_movie_set %>% filter(year >= input$years[1], year <= input$years[2], (genre %in% input$genre | input$genre %in% "All"), votes >= input$votes)
        
        ggplot(mydf, aes(x=length,y=rating,color=genre)) + geom_point()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
