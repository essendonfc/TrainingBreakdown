####################################################################################################
# Breakdown of training drills for 2019 season into Major and Sub classifications
# RL - 6 DEC 18
####################################################################################################


library(dplyr)
library(readxl)
library(lubridate)
library(ggplot2)
library(condformat)
library(googlesheets)
library(shiny)

####################################################################################################
# Import data using googlesheets
####################################################################################################

# Use key to register the workbook
training <- gs_key("1Je6bsSYtx1bQo6vOZQ7bt-AeX_zeSYgig6gMO3MLu3c")
training_data <- gs_read_csv(training, ws = "Sheet1") 

# Create list of major classifications
classification <- training_data %>%
  select(Major) %>%
  distinct()

# Create dataframe of the most recent date each drill was performed
recent_drill <- training_data %>%
  mutate(Date = as.Date(Date, format = "%d/%m/%Y")) %>%
  group_by(Drill) %>%
  slice(which.max(Date)) %>%
  rename(Last_Date = Date) %>%
  select(Last_Date, Drill, Major, Sub)

# User interface for Shiny
ui <- fluidPage(
  titlePanel("2019 Training Drill Breakdown"),
    # Create dropdown menu to select the major classification for subsequent breakdown 
    fluidRow(column(width = 3, selectInput('classification', label = 'Select Drill Classification',
                                                choices = classification,
                                                selected = c("Transition"))),
                  # Create radio buttons to select time period for break down
                  column(width = 9, radioButtons("period", label = "Select time period",
                                                 choices = c("2019", "Pre-Season", "In-Season", "Last Month"),
                                                 selected = c("2019"),
                                                 inline = TRUE))),
         fluidRow(column(width = 6, plotOutput("overall_plot")),
                  column(width = 6, plotOutput("sub_plot"))),
         fluidRow(column(width = 12, condformatOutput("recent")))
)

server = function(input, output) {
  
  # Reactive code that provides filters for time period
  userinputs <- reactive({
    if (input$period == "2019") {
      filters = filter(training_data)
    } else if (input$period == "Pre-Season") {
      filters = filter(training_data, Date >= "1/11/2018" & Date <= "7/3/2018")
    } else if (input$period == "In-Season") {
      filters = filter(training_data, Date >= "8/3/2019" & Date <= "10/10/2019")
    } else {
      filters = filter(training_data, Date >= Sys.time() - 86400 * 30)
    }
  })

  # Create the plot that shows the major classification breakdown by total duration
  output$overall_plot <-  renderPlot({

    overall <- userinputs() %>%
      group_by(Major) %>%
      summarise(Duration = sum(Duration))
    
    overall_plot <- ggplot(overall, aes(reorder(Major, Duration, sum), Duration, label = Duration)) +
      geom_col(fill = "grey80") +
      geom_text(hjust = 1.5) +
      coord_flip() +
      theme_minimal() +
      theme(panel.grid = element_blank(),
            axis.text.x = element_blank()) +
      labs(title = "2019 Training Drill Breakdown",
          subtitle = "Major classification (Minutes)",
          x = "Major classification",
          y = "Total Duration (Minutes)")
    
    return(overall_plot)
  })

  # Create the plot that shows the sub classification breakdown by total duration
  output$sub_plot <-  renderPlot({
  
    sub <- userinputs() %>%
      filter(Major == input$classification) %>%
      group_by(Sub) %>%
      summarise(Duration = sum(Duration))

    sub_plot <- ggplot(sub, aes(reorder(Sub, Duration, sum), Duration, label = Duration)) +
      geom_col(fill = "grey80") +
      geom_text(hjust = 1.5) +
      coord_flip() +
      theme_minimal() +
      theme(panel.grid = element_blank(),
            axis.text.x = element_blank()) +
      labs(title = paste("2019", input$classification, "Drills Breakdown"),
          subtitle = "Sub classification (Minutes)",
          x = "Sub classification",
          y = "Total Duration (Minutes)")
    
    return(sub_plot)
    
  })
  
  # Create the table showing the most recent date each drill was performed
  output$recent <-  renderCondformat({
  
    recent <- condformat(recent_drill) %>%
      theme_htmlTable(rnames = FALSE) %>%
      theme_htmlWidget(number_of_entries = 20) %>%
      theme_caption(caption = "Drill most recently performed")
    
    return(recent)
  })
    
}

shinyApp(ui, server)
