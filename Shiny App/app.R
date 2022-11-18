source("helpers.R")
library(tidyverse)
library(ggplot2)
library(lubridate)
scores <- read.csv('compas-scores-raw.csv') %>%
  mutate(Ethnic_Code_Text = if_else(Ethnic_Code_Text == "African-Am","African-American", Ethnic_Code_Text)) %>%
  mutate(Ethnic_Code_Text = if_else(Ethnic_Code_Text == "Oriental","Asian", Ethnic_Code_Text))

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("COMPAS Scores"),


    # Show a plot of the generated distribution
    mainPanel(
       plotOutput("barPlot")
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$barPlot <- renderPlot({
      race_scores_all <- scores %>%
        mutate(`Score Type` = DisplayText) %>%
        group_by(Ethnic_Code_Text, `Score Type`) %>%
        summarize(avg_score = mean(DecileScore))
      
      ggplot(race_scores_all, aes(fill=`Score Type`, y=avg_score, x=reorder(Ethnic_Code_Text, -avg_score))) + 
        geom_bar(position="dodge", stat="identity") +
        labs(title = "COMPAS Risk Score Averages By Race", x = "Race", y = "Average Score (Maximum of 10)") +
        scale_x_discrete(guide = guide_axis(n.dodge=2)) +
        scale_fill_manual(values = c("#2e282a", "#EF3E36", "#17BEBB")) +
        theme_light()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
