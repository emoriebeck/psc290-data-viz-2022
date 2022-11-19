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
library(patchwork)

load("ipcs_data.RData")

ipcs_long <- ipcs_data %>%
  select(SID, Full_Date, afraid, angry, content, excited, purposeful) %>%
  group_by(SID) %>%
  mutate(beep = 1:n()) %>%
  ungroup() %>%
  pivot_longer(
    cols = c(-SID, -Full_Date, -beep)
    , values_to = "value"
    , names_to = "item"
    , values_drop_na = T
  )

states <- tribble(
  ~old,   ~new
  , "Afraid", "afraid"
  , "Angry", "angry"
  , "Content", "content"
  , "Excited", "excited"
  , "Purposeful", "purposeful"
)

my_theme <- function(){
  theme_classic() + 
    theme(
      legend.position = "bottom"
      , legend.title = element_text(face = "bold", size = rel(1))
      , legend.text = element_text(face = "italic", size = rel(1))
      , axis.text = element_text(face = "bold", size = rel(1.1), color = "black")
      , axis.title = element_text(face = "bold", size = rel(1.2))
      , plot.title = element_text(face = "bold", size = rel(1.2), hjust = .5)
      , plot.subtitle = element_text(face = "italic", size = rel(1.2), hjust = .5)
      , strip.text = element_text(face = "bold", size = rel(1.1), color = "white")
      , strip.background = element_rect(fill = "black")
    )
}

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Personality States Over Time"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectizeInput("SID1",
                        "Participant ID"
                        , choices = "02"
                        , selected = "02")
            , checkboxGroupInput("state1"
                        , "Choose a psychological state"
                        , choices = c("Afraid", "Angry", "Content", "Excited", "Purposeful")
                        , selected = "Afraid")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("timeSeriesPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

  ## create a a reactive value to update the selectizeInput 
  observe({
    subs <- unique((ipcs_data %>% 
      group_by(SID) %>% 
      filter(n() > 40) %>%
      ungroup())$SID)
    set.seed(4)
    subs <- sample(subs, 50)
    updateSelectizeInput(session, 'SID1', choices = c("", subs))
  })  
  
    output$timeSeriesPlot <- renderPlot({
      item_cols <- RColorBrewer::brewer.pal(length(input$state1), "Set2")[1:length(input$state1)]
      items <- mapvalues(input$state1, states$old, states$new, warn_missing = F)
      ipcs_long %>%
        filter(SID == input$SID1 & item %in% items) %>%
        ggplot(aes(x = beep, y = value)) + 
          geom_line(aes(color = item), size = 1.2) + 
          geom_point(shape = "square") + 
          scale_color_manual(values = item_cols, labels = input$state1) + 
          labs(x = "Experience Sampling Beep"
               , y = "Momentary Rating (1-5)"
               , color = "Psychological State(s)") + 
          my_theme()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
