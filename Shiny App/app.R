source("helpers.R")
library(tidyverse)
library(ggplot2)
library(lubridate)
library(shiny)

# Dataset of all Scores variables
scores <- read.csv('compas-scores-raw.csv') %>%
  mutate(Ethnic_Code_Text = if_else(Ethnic_Code_Text == "African-Am","African-American", Ethnic_Code_Text)) %>%
  mutate(Ethnic_Code_Text = if_else(Ethnic_Code_Text == "Oriental","Asian", Ethnic_Code_Text))

# Dataset with decile factor scores
clean <- scores %>% 
  filter(DecileScore > 0, DecileScore <= 10) %>%
  mutate(Risk = case_when(DecileScore < 6 ~ "Unlikely",
                           (DecileScore >= 6 & DecileScore < 8) ~ "Probable",
                           DecileScore >= 8 ~ "Likely")) %>%
  mutate(DecileScore = as.character(DecileScore))
clean$DecileScore = factor(clean$DecileScore, levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10"))

# clean datasets based on type of score
clean_recid <- clean %>% filter(DisplayText == "Risk of Recidivism")
clean_violence <- clean %>% filter(DisplayText == "Risk of Violence")
clean_fail_appr <- clean %>% filter(DisplayText == "Risk of Failure to Appear")

# Dataset grouping by race and sex
sex_race_score_recidivism <- scores %>%
  group_by(Sex_Code_Text, Ethnic_Code_Text, DisplayText) %>%
  summarize(avg_score = mean(DecileScore))

# Dataset of Score distribution
percent_scores <- scores %>%
  group_by(ScoreText) %>%
  summarize(total = n(), DisplayText) %>%
  group_by(DisplayText, ScoreText) %>%
  summarize(total_percent = n()/total) %>%
  filter(ScoreText != "N/A") 

# Dataset grouping marital status
marital_status_recidivism <- scores %>% 
  group_by(MaritalStatus) %>% 
  summarize(avg_score = mean(DecileScore))

set.seed(229)

# decision tree datasets
ind <- sample(2, nrow(recidivism), replace = T, prob = c(0.8, 0.2))
recid_train <- recidivism[ind == 1,] %>% mutate(Race = Ethnic_Code_Text, Sex = Sex_Code_Text) %>% select(Sex, Race, AssessmentReason, Language, LegalStatus, ScoreText) %>% filter(ScoreText != "N/A")
recid_test <- recidivism[ind == 2,] %>% mutate(Race = Ethnic_Code_Text, Sex = Sex_Code_Text) %>% select(Sex, Race, AssessmentReason, Language, LegalStatus, ScoreText)%>% filter(ScoreText != "N/A")
recid_train <- recid_train[ind == 1,]
recid_test <- recid_test[ind == 2,]


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("COMPAS Scores"),


    # Show a plot of the generated distribution
    mainPanel(
      h1("What is COMPAS?"),
      p("What if an algorithm decided who goes to prison and for how long? This may seem like a dystopian future, but we are already doing this in 2022. COMPAS (Correctional Offender Management Profiling for Alternative Sanctions) is a proprietary software that is used to predict the likelihood of arrested individuals' recidivism (to re-offend), committing violent acts, and failing to appear in court. This software is currently used in decisions of bail calculations, trials, sentencings, and paroles. The COMPAS software does not decide the actual outcomes of arrested individuals, but it does serve as additional “evidence” used by judges. The algorithm itself is proprietary, and therefore unknown to the public, but ProPublica has released a dataset of COMPAS decisions for individuals in Broward county, Florida. From this dataset, we are able to hypothesize about the accuracy, biases, and effects of the COMPAS algorithm. What factors are used to predict the COMPAS scores? How impactful are these factors? Could the predictions reproduce biases inherent in the criminal justice system? These are the questions we hope to answer in this article."),
      p("	The data in question was collected by ProPublica and contains data used to calculate COMPAS scores as well as the COMPAS scores themselves, from over 60,000 inmates in Broward County, Florida. The data also includes the outcome (whether or not the inmates went back to prison) from a two-year period after the scores were calculated. A lot of demographic information is included in the dataset, including race, binary sex, first and last name, the date they were screened, age, number of criminal counts of different types, case number, dates in and out of jail, charge degree, charge description, and much more. Also notable in the dataset is what the COMPAS score was actually calculated for. This can be one of three categories: risk of recidivism, risk of violence, and risk of failure to appear in court. Given that demographic information such as race is taken into account when calculating the scores, ProPublica found the scores to be quite prejudiced and found that African American inmates were seen as more of a risk than they actually were, while white inmates were seen as less of a risk than they actually were, on average. This is also noticeable with other marginalized races when looking at the data and especially when visualized."),
      # plotOutput("barPlot")
      
      h2("COMPAS Algorithm and Scoring"),
      p("The COMPAS algorithm determines risk factors based on scores derived from questionnaires and biographical data. The software reports a raw score and a decile score. The raw score is not directly interpretable, but low numbers indicate low risk and high numbers indicate high-risk scores. The decile scores are integers between 1 and 10. A decile score of 1 signifies that the individual has a risk score greater than 0% and less than 10% of the average score. The scores are roughly correlated to different measures of qualitative risk. A score from 1-4 represents low risk, a score from 5-7 represents a medium risk, and a score from 8-10 represents a high risk. The risk for recidivism types have a slightly different language, with 1-5 being unlikely, 6-7 being probable, and 8-10 being likely."),
      p("The following plot shows the distribution of Decile and Raw Scores from the COMPAS dataset."),
      plotOutput("AllScores"),
      p("The overall distribution of scores tends to skew towards low risk."),
      plotOutput("ScoreDistribution"),
      p("The algorithm takes the answers from the survey given and assigns them a weight (which is proprietary and unknown). Together, a sort of linear regression model is evaluated to decide on the final raw score. The categories from the survey include questions on criminal involvement, relationships and lifestyle, personality and attitudes, family, and social exclusion. Some of these questions are related to criminal history. Others are questions seemingly unrelated to personal history, including questions on moral beliefs (e.g. Do you agree with the following: “The law doesn’t help average people.”). Others are strongly correlated with race, such that even if race isn’t an included category technically, it’s still relevant to the scoring system due to racism inherent in the prison pipeline (e.g. “Were any of the adults who raised you ever arrested, that you know of?”)."),
      p("The plots below show the distribution of Decile Scores for each type of risk factor in the dataset."),
      plotOutput("ScoreByType"),
      p("The following plot shows the distribution of Decile scores based on race and sex."),
      plotOutput("ScoreByRaceSex"),
      p("The following plot shows the distribution of Decile scores based on marital status."),
      plotOutput("ScoreByMaritalStatus"),
      plotOutput("DecisionTree")
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$AllScores <- renderPlot({
    # bar plot of decile scores
    bar <- ggplot(clean, aes(x = DecileScore, fill = Risk)) + 
      geom_bar() + 
      scale_fill_manual(values=c("#ED2938", "#FFE733", "#024E1B")) + 
      labs(title = "Decile Score Distribution", x = "Decile Scores")
    # density plot of raw scores
    dense <- ggplot(clean, aes(x = RawScore)) + 
      geom_density() + 
      labs(title = "Raw Score Distribution", x = "Raw Scores")
    # pannel of charts
    ggarrange(bar, dense,  ncol = 1, nrow = 2)
  })
  
  output$ScoreByType <- renderPlot({
    # bar plot of recidivism scores distribution
    bar_recid <- ggplot(clean_recid, aes(x = DecileScore, fill = Risk)) + 
      geom_bar() + 
      scale_fill_manual(values=c("#ED2938", "#FFE733", "#024E1B")) +
      labs(title = "Recidivism Scores", x = "Decile Score Distribution")
    # bar plot of violence scores distribution
    bar_violence <- ggplot(clean_violence, aes(x = DecileScore, fill = Risk)) + 
      geom_bar() + 
      scale_fill_manual(values=c("#ED2938", "#FFE733", "#024E1B")) + 
      labs(title = "Violence Scores", x = "Decile Score Distribution") + 
      theme(legend.position = "none")
    # bar plot of failure to appear scores distribution
    bar_fail_appr <- ggplot(clean_fail_appr, aes(x = DecileScore, fill = Risk)) + 
      geom_bar() + 
      scale_fill_manual(values=c("#ED2938", "#FFE733", "#024E1B")) + 
      labs(title = "Failure to Appear Scores", x = "Decile Score Distribution") + 
      theme(legend.position = "none")
    # pannel of charts
    ggarrange(bar_recid, ggarrange(bar_violence, bar_fail_appr, ncol = 2), nrow = 2, common.legend = TRUE, legend = "bottom")
  })
  
  output$ScoreByRaceSex <- renderPlot({
  ggplot(sex_race_score_recidivism, aes(x = reorder(Ethnic_Code_Text, -avg_score), y = avg_score, fill = Sex_Code_Text)) +
    geom_bar(stat = 'identity', position = 'dodge') +
    labs(title = "Risk of Recidivism Score", x = "Race", y = "Average Score", fill = "Sex") +
    theme_minimal()
  })

  output$ScoreDistribution <- renderPlot({
    ggplot(percent_scores, aes(x = reorder(ScoreText, -total_percent), y = total_percent, fill = ScoreText)) + 
      geom_col() +
      facet_wrap(~DisplayText) + 
      labs(x = "Risk Score Given", y = "Percent of Population", title = "Distribution of Scores Given By Risk Type") + 
      guides(fill = guide_legend(title = "Score Given")) })

  output$ScoreByMaritalStatus <- renderPlot({
    ggplot(marital_status_recidivism, aes(x = reorder(MaritalStatus, -avg_score), y = avg_score, fill = MaritalStatus)) +
      geom_bar(stat = 'identity') +
      labs(title = "Risk of Recidivism Score", x = "Marital Status", y = "Average Score", fill = "Sex") +
      theme_minimal()
  })
  
  output$DecisionTree <- renderPlot ({
    tree <- rpart(ScoreText ~., data = recid_train, control =rpart.control(minsplit =1,minbucket=1, cp=0.00005))
    rpart.plot(tree, box.palette = list("Reds", "Greens", "Oranges"), fallen.leaves = FALSE, tweak = 1.38, Margin = 0)
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
