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
      h1("What is COMPAS?"),
      p("What if an algorithm decided who goes to prison and for how long? This may seem like a dystopian future, but we are already doing this in 2022. COMPAS (Correctional Offender Management Profiling for Alternative Sanctions) is a proprietary software that is used to predict the likelihood of arrested individuals' recidivism (to re-offend), committing violent acts, and failing to appear in court. This software is currently used in decisions of bail calculations, trials, sentencings, and paroles. The COMPAS software does not decide the actual outcomes of arrested individuals, but it does serve as additional “evidence” used by judges. The algorithm itself is proprietary, and therefore unknown to the public, but ProPublica has released a dataset of COMPAS decisions for individuals in Broward county, Florida. From this dataset, we are able to hypothesize about the accuracy, biases, and effects of the COMPAS algorithm. What factors are used to predict the COMPAS scores? How impactful are these factors? Could the predictions reproduce biases inherent in the criminal justice system? These are the questions we hope to answer in this article."),
      # plotOutput("barPlot")
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
