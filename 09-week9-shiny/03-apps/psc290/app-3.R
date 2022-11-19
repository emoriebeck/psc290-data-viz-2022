#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plyr)
library(tidyverse)
library(shinybrowser)
library(rio)
library(imager)
library(magick)

url <- "https://github.com/emoriebeck/behavior-prediction/blob/main/01-codebooks/codebook_R1.xlsx?raw=true"
codebook <- import(file = url,which = 2) %>% as_tibble()
outcomes <- codebook %>% filter(category == "outcome") %>% select(trait, long_name)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Persons, Situations, and Time: Idiographic Behavior Prediction"),
    
  tabsetPanel(
    
  ############# Sequence Plots of Feature Categories #############################
    tabPanel(
      "Feature Proportions"
      , sidebarLayout(
          sidebarPanel(
            selectInput(
                  "outcome1"
                  , label = "Choose Outcome"
                  , selected = "Procrastinating"
                  , choices = c(
                    "Procrastinating", "Lonely", "Tired", "Sick",
                    "Studying", "Argument", "Interacted"
                    )
                  )
                )
              , mainPanel(
                htmlOutput("seq_plot_text")
                , shinybrowser::detect()
                , htmlOutput("seq_plot")
                )
              )
          )
          ############# Frequency Plots of Feature Categories #############################
          , tabPanel("Feature Frequency",
                     sidebarLayout(
                       sidebarPanel(
                         selectInput(
                           "outcome2"
                           , label = "Choose Outcome"
                           , selected = "Procrastinating"
                           , choices = c("Procrastinating",
                                         "Lonely",
                                         "Tired",
                                         "Sick",
                                         "Studying",
                                         "Argument",
                                         "Interacted")
                         )
                       ),
                       mainPanel(
                         htmlOutput("var_freq_text")
                         , htmlOutput("var_freq")
                         )
                       )
                     )
        )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  ############# Sequence Plots of Feature Categories #############################
  output$seq_plot <- renderText({
    o <- mapvalues(input$outcome1, outcomes$long_name, outcomes$trait, warn_missing = F)
    file <- sprintf("https://github.com/emoriebeck/behavior-prediction/raw/main/05-results/05-figures/05-seq-plots/%s.png"
                    , o)
    dims <- image_info(image_read(file))
    screen_wdth <- shinybrowser::get_width()
    img_wdth <- screen_wdth*.5
    img_ht <- (img_wdth*dims$height)/dims$width
    return(c('<center><img src="',file,'" width="', img_wdth, '" height="', img_ht,'"></center>', sep = ""))
  })
  
  output$seq_plot_text <- renderText({
    paste("<style=color: blue; text-align: center; background-color: white; padding: 20px>"
          , "These supplementary figures are sequence plots of the proportion of features from psychological, situational, and time feature categories for each participants' best models for each outcome and model. These are analogous to Figure 3 in the manuscript."
          , "</style>"
          , collapse = "")
  })
  
  ############# Frequency Plots of Feature Categories #############################
  output$var_freq <- renderText({
    o <- mapvalues(input$outcome2, outcomes$long_name, outcomes$trait, warn_missing = F)
    file <- sprintf("https://github.com/emoriebeck/behavior-prediction/raw/main/05-results/05-figures/06-var-freq/%s.png"
                    , o)
    dims <- image_info(image_read(file))
    screen_wdth <- shinybrowser::get_width()
    img_wdth <- screen_wdth*.5
    img_ht <- (img_wdth*dims$height)/dims$width
    return(c('<center><img src="',file,'" width="', img_wdth, '" height="', img_ht,'"></center>', sep = ""))
  })
  
  output$var_freq_text <- renderText({
    paste("<style=color: blue; text-align: center; background-color: white; padding: 20px>"
          , "These supplementary figures display the frequency of features across people from psychological, situational, and time feature categories for each participants' best models for each outcome and model. These are analogous to Figure 4 in the manuscript."
          , "</style>"
          , collapse = "")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
