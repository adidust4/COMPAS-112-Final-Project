library(tidyverse)
library(ggplot2)
library(lubridate)
library(shiny)
library(shinythemes)
library(ggpubr)
library(scales)
library(ggpubr)
library(rpart)
library(rpart.plot)
library(rsconnect)

# Dataset of all Scores variables
scores <- read.csv('https://raw.githubusercontent.com/adidust4/COMPAS-112-Final-Project/main/compas-scores-raw.csv') %>%
  mutate(Ethnic_Code_Text = if_else(Ethnic_Code_Text == "African-Am","African-American", Ethnic_Code_Text)) %>%
  mutate(Ethnic_Code_Text = if_else(Ethnic_Code_Text == "Oriental","Asian", Ethnic_Code_Text))

# Dataset with decile factor scores
clean <- scores %>% 
  filter(DecileScore > 0, DecileScore <= 10) %>%
  mutate(Risk = ScoreText) %>%
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

# Dataset grouping by race
race_scores_all <- scores %>%
  mutate(`Score Type` = DisplayText) %>%
  group_by(Ethnic_Code_Text, `Score Type`) %>%
  summarize(avg_score = mean(DecileScore))

# Dataset grouping marital status
marital_status_recidivism <- scores %>% 
  group_by(MaritalStatus) %>% 
  summarize(avg_score = mean(DecileScore))

set.seed(229)

# decision tree datasets
recidivism <- scores %>%
  filter(DisplayText == "Risk of Recidivism") 
ind <- sample(2, nrow(recidivism), replace = T, prob = c(0.8, 0.2))
recid_train <- recidivism[ind == 1,] %>% mutate(Race = Ethnic_Code_Text, Sex = Sex_Code_Text) %>% select(Sex, Race, AssessmentReason, Language, LegalStatus, ScoreText) %>% filter(ScoreText != "N/A")
recid_test <- recidivism[ind == 2,] %>% mutate(Race = Ethnic_Code_Text, Sex = Sex_Code_Text) %>% select(Sex, Race, AssessmentReason, Language, LegalStatus, ScoreText)%>% filter(ScoreText != "N/A")
recid_train <- recid_train[ind == 1,]
recid_test <- recid_test[ind == 2,]

# app pallete
red <- "#fd7f6f"
orange <- "#ffb55a"
pink <- "#fdcce5"
yellow <- "#ffee65"
green <- "#b2e061"
light_blue <- "#8bd3c7"
purple <- "#bd7ebe"
black <- "#000000"

risk_palette <- c(red, green, yellow)
sex_palette <- c(red, light_blue)
palette7 <- c(red, orange, pink, yellow, green, light_blue, purple)

#Define custom theme function
our_theme <- function(){ 
  
  theme_minimal() %+replace%    #replace elements we want to change
    
    theme(
      
      #grid elements
      panel.grid.minor = element_blank(),    
      axis.ticks = element_blank(),          
      
      #text elements
      plot.title = element_text(            #set font family
        size = 20,                #set font size
        face = 'bold',            #bold typeface
        hjust = 0,                #left align
        vjust = 2),               #raise slightly
      
      plot.subtitle = element_text(           #font family
        size = 14),               #font size
      
      plot.caption = element_text(       #font family
        size = 9,                 #font size
        hjust = 1),               #right align
      
      axis.title = element_text(   #font family
        size = 10),               #font size
      
      axis.text = element_text(       #axis famuly
        size = 9),                #font size
      
      axis.text.x = element_text(            #margin for axis text
        margin=margin(5, b = 10))
    )
}

# Define UI for application that draws a histogram
ui <- fluidPage(

    theme = shinytheme("readable"),

    # Application title
    fluidRow(
      column(6, offset = 3, align="center",
        titlePanel("How likely you are to go to prison: Prediction Biases")
      )
    ),
    
    fluidRow( 
      column(6, offset = 3,
      
      # introduction
      h2("What is COMPAS?"),
      p(style="text-align: justify;","What if an algorithm decided who goes to prison and for how long? This may seem like a dystopian future, but we are already doing this in 2022. COMPAS (Correctional Offender Management Profiling for Alternative Sanctions) is a proprietary software that is used to predict the likelihood of arrested individuals' recidivism (to re-offend), committing violent acts, and failing to appear in court. This software is currently used in decisions of bail calculations, trials, sentencings, and paroles. The COMPAS software does not decide the actual outcomes of arrested individuals, but it does serve as additional “evidence” used by judges. The algorithm itself is proprietary, and therefore unknown to the public, but ProPublica has released a dataset of COMPAS decisions for individuals in Broward county, Florida. From this dataset, we are able to hypothesize about the accuracy, biases, and effects of the COMPAS algorithm. What factors are used to predict the COMPAS scores? How impactful are these factors? Could the predictions reproduce biases inherent in the criminal justice system? These are the questions we hope to answer in this article."),
      p("	The data in question was collected by ProPublica and contains data used to calculate COMPAS scores as well as the COMPAS scores themselves, from over 60,000 inmates in Broward County, Florida. The data also includes the outcome (whether or not the inmates went back to prison) from a two-year period after the scores were calculated. A lot of demographic information is included in the dataset, including race, binary sex, first and last name, the date they were screened, age, number of criminal counts of different types, case number, dates in and out of jail, charge degree, charge description, and much more. Also notable in the dataset is what the COMPAS score was actually calculated for. This can be one of three categories: risk of recidivism, risk of violence, and risk of failure to appear in court. Given that demographic information such as race is taken into account when calculating the scores, ProPublica found the scores to be quite prejudiced and found that African American inmates were seen as more of a risk than they actually were, while white inmates were seen as less of a risk than they actually were, on average. This is also noticeable with other marginalized races when looking at the data and especially when visualized."),
      p("The demographics from the dataset do notably differ from the overall demographics for Broward County. Broward County, according to the 2020 Census, is about 30.6% African American, whereas 44.5% of the people in the dataset are African American. Additionally, the county is 50.9% female, but only about 22.9% of the people in the dataset are women."),
      
      # context
      h2("COMPAS Algorithm and Scoring"),
      p("The COMPAS algorithm determines risk factors based on scores derived from questionnaires and biographical data. The software reports a raw score and a decile score. The raw score is not directly interpretable, but low numbers indicate low risk and high numbers indicate high-risk scores. The decile scores are integers between 1 and 10. A decile score of 1 signifies that the individual has a risk score greater than 0% and less than 10% of the average score. The scores are roughly correlated to different measures of qualitative risk. A score from 1-4 represents low risk, a score from 5-7 represents a medium risk, and a score from 8-10 represents a high risk. The risk for recidivism types have a slightly different language, with 1-5 being unlikely, 6-7 being probable, and 8-10 being likely."),
      
      # data distribution
      h4("The following plots show the distribution of Decile and Raw Scores from the COMPAS dataset."),
      h5("The overall distribution of scores tends to skew towards low risk."),
      plotOutput("AllScores"),
      p("The algorithm takes the answers from the survey given and assigns them a weight (which is proprietary and unknown). Together, a sort of linear regression model is evaluated to decide on the final raw score. The categories from the survey include questions on criminal involvement, relationships and lifestyle, personality and attitudes, family, and social exclusion. Some of these questions are related to criminal history. Others are questions seemingly unrelated to personal history, including questions on moral beliefs (e.g. Do you agree with the following: “The law doesn’t help average people.”). Others are strongly correlated with race, such that even if race isn’t an included category technically, it’s still relevant to the scoring system due to racism inherent in the prison pipeline (e.g. “Were any of the adults who raised you ever arrested, that you know of?”)."),
      h4("The plots below show the distribution of Decile Scores for each type of risk factor in the dataset."),
      plotOutput("ScoreByType"),
      
      # biases
      h2("Possible Biases"),
      h4("The following plot shows the distribution of Decile scores based on race."),
      plotOutput("Race"),
      p("Based on the average COMPAS scores for people of each categorized race, African-Americans are disproportionately more likely to recidivate and are disproportionately at risk of committing acts of violence compared to other race demographics. However, there is no reason for this to be the case. Race is a factor in the COMPAS scores, but yet it should obviously not be taken into account when considering how much of a risk a person may be."),
      h4("The following plot shows the distribution of Decile scores based on race and sex."),
      plotOutput("ScoreByRaceSex"),
      p("These disproportionate scores become even more clear when sex is factored in as well. Native American women and African American men come out on top as the demographics with the highest average COMPAS scores."),
      h4("The following plot shows the distribution of Decile scores based on marital status."),
      plotOutput("ScoreByMaritalStatus"),
      p("Disparities can also be seen between different marital statuses. Single people are more likely to have significantly higher COMPAS scores on average when compared to married people."),
      
      # decision tree
      h1("Predicting Recidivism Risk"),
      p("Although this decision tree is rather overfit with high complexity, it does give us a good idea of which variables contribute most to the algorithm recidivism risk ratings. Branches nearer to the top have a higher contribution to the decisions made by the tree. For this tree, we included all demographic variables included in the dataset. These include binary sex, race, the reason for assesment, language spoken, and the individual’s legal status (post-trial, pre-trial, etc). As you can see, scores are more likely to be low risk than high or medium risk. The first big predictor of recidivism risk was whether or not the individual was African-American or Native American. If the individual was not identified as either of these races, the algorithm was most likely to predict low risk. The next biggest predictor for recidivism risk was sex. The COMPAS documentation explicitly mentions sex as part of their model, so there is no surprise that it ended up so high on the tree. The next most important predictor was legal status and finally if the individual was African-America or Native American. The race of individuals is not directly included in the algorithm, so their presence as a main predictor of recidivism is very alarming and telling of correlations between questions asked and individuals’ race."),
      plotOutput("DecisionTree", height = 500),
      
      # interactive viz
      h1("Take a Closer Look"),
      h4("Use the drop-downs to take a closer look at factors influencing risk scores."),
      selectInput("riskType", "Type of Risk", c("Risk of Recidivism", "Risk of Violence", "Risk of Failure to Appear")),
      selectInput("sex", "Sex", c("Female", "Male")),
      selectInput("marital", "Marital Status", c("Single", "Significant Other", "Widowed", "Separated", "Unknown", "Divorced", "Married")),
      plotOutput("interactive"),
      
      # conclusion
      h1("Conclusion"),
      p("In conclusion, the COMPAS algorithm shows clear reinforcement of existing race and sex-based prejudice in the legal system. While there are differing sample sizes for different demographics, there are a disproportionate number of African Americans and men in general within the dataset. This also means that a disproportionate number of people in the Broward County inmate system are African American and/or male to begin with. The COMPAS algorithm scores show that Native Americans (of whom there are very few within the dataset) are at the highest risk of recidivism, followed by African Americans. White and hispanic inmates, however, have a much lower risk of recidivism, with an over 1 point lower average COMPAS score. There is also a clear disparity in sex, with inmates identified in the dataset as Arabic having the biggest difference between men and women. Arabic men have a COMPAS score that is roughly double, on average, that of Arabic women. Similar trends can be seen when looking at marital status, with single people having the highest COMPAS scores on average, and married people having the lowest. The difference between these is also well over 1 point. While slight disparities in the scores could be attributed to a smaller sample size for some demographics over others, none of that would explain some of the extreme differences in COMPAS scores. There is clear evidence in this dataset of unfair treatment, expectations of marginalized populations, and a general racist prejudice within the Broward County legal system that is either contributing to, or being contributed to by these COMPAS scores."),
  )))

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$AllScores <- renderPlot({
    # bar plot of decile scores
    bar <- ggplot(clean, aes(x = DecileScore, fill = ScoreText)) + 
      geom_bar() + 
      scale_fill_manual(values = risk_palette) + 
      labs(title = "Decile Score Distribution", x = "Decile Scores")+
      our_theme()
    # density plot of raw scores
    dense <- ggplot(clean, aes(x = RawScore)) + 
      geom_density() + 
      labs(title = "Raw Score Distribution", x = "Raw Scores")+
      our_theme()
    # pannel of charts
    ggarrange(bar, dense,  ncol = 1, nrow = 2)
  })
  
  output$Race <- renderPlot({
    # bar plot comparing average COMPAS scores of each type for different race demographics
    ggplot(race_scores_all, aes(fill=`Score Type`, y=avg_score, x=reorder(Ethnic_Code_Text, -avg_score))) + 
      geom_bar(position="dodge", stat="identity") +
      labs(title = "COMPAS Risk Score Averages By Race", x = "Race", y = "Average Score (Maximum of 10)") +
      scale_x_discrete(guide = guide_axis(n.dodge=2)) +
      scale_fill_manual(values = c(purple, light_blue, black)) +
      theme_light()
  })
  
  output$ScoreByType <- renderPlot({
    # bar plot of recidivism scores distribution
    bar_recid <- ggplot(clean_recid, aes(x = DecileScore, fill = ScoreText)) + 
      geom_bar() + 
      scale_fill_manual(values=risk_palette) +
      labs(title = "Recidivism Scores", x = "Decile Score Distribution")+
      our_theme()
    # bar plot of violence scores distribution
    bar_violence <- ggplot(clean_violence, aes(x = DecileScore, fill = ScoreText)) + 
      geom_bar() + 
      scale_fill_manual(values=risk_palette) + 
      labs(title = "Violence Scores", x = "Decile Score Distribution") + 
      guides(Risk = "none") + 
      our_theme()
    # bar plot of failure to appear scores distribution
    bar_fail_appr <- ggplot(clean_fail_appr, aes(x = DecileScore, fill = ScoreText)) + 
      geom_bar() + 
      scale_fill_manual(values=risk_palette) + 
      labs(title = "Failure to Appear Scores", x = "Decile Score Distribution") + 
      guides(Risk = "none") + 
      our_theme()
    # pannel of charts
    ggarrange(bar_recid, ggarrange(bar_violence, bar_fail_appr, ncol = 2), nrow = 2, common.legend = TRUE, legend = "bottom")
  })
  
  output$ScoreByRaceSex <- renderPlot({
  # bar plot comparing average COMPAS scores for different race and sex demographics
  ggplot(sex_race_score_recidivism, aes(x = reorder(Ethnic_Code_Text, -avg_score), y = avg_score, fill = Sex_Code_Text)) +
    geom_bar(stat = 'identity', position = 'dodge') +
    labs(x = "Race", y = "Average Score", fill = "Sex") +
    scale_fill_manual(values = sex_palette) +
    scale_x_discrete(guide = guide_axis(n.dodge=2)) +
    our_theme()
  })


  output$ScoreByMaritalStatus <- renderPlot({
    # bar plot comparing average COMPAS scores for different marital statuses
    ggplot(marital_status_recidivism, aes(x = reorder(MaritalStatus, -avg_score), y = avg_score, fill = MaritalStatus)) +
      geom_bar(stat = 'identity') +
      scale_fill_manual(values = palette7) +
      labs(x = "Marital Status", y = "Average Score") +
      guides(MaritalStatus = "none") + 
      our_theme()
  })
  
  output$DecisionTree <- renderPlot ({
    # decision tree
    tree <- rpart(ScoreText ~., data = recid_train, control =rpart.control(minsplit =1,minbucket=1, cp=0.00005))
    rpart.plot(tree, box.palette = list(red, green, yellow), fallen.leaves = FALSE, tweak = 1.38, Margin = 0)
  })
  
  interactiveDF <- reactive({
    scores %>%
      filter(DisplayText == input$riskType, Sex_Code_Text == input$sex, MaritalStatus == input$marital, ScoreText != "N/A")
  })
  
  output$interactive <- renderPlot({
    # interactive viz
    ggplot(interactiveDF(), aes(x = Ethnic_Code_Text, fill = ScoreText))+
      geom_bar(position="fill", stat = "count") +
      labs(title = "Taking a Closer Look", x = "Race", y = "Proportion of Race by Score", fill = "Risk Prediction") +
      scale_fill_manual(values = c("Low" = green, "Medium" = yellow, "High" = red)) + 
      geom_label(aes(label=..count..), stat='count', position='fill', size=2,) + 
      our_theme()
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
