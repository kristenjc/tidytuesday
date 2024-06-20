
#R Shiny App
#Date of Creation: 2024-06-02

library(tidyverse)
library(tidytuesdayR)
library(tidymodels) 
library(ggplot2)
library(dplyr)
library(shiny)

data <- tidytuesdayR::tt_load('2024-01-09')

cdn_births <- data$canada_births_1991_2022
nhl_births <- data$nhl_player_births
nhl_rosters <- data$nhl_rosters
nhl_teams <- data$nhl_teams

#
nhl_hw <- nhl_rosters |>
  select(season, position_type, position_code, height_in_centimeters, weight_in_kilograms) |>
  group_by(season, position_type, position_code) |>
  summarise(avg_height = mean(height_in_centimeters, na.rm=TRUE),
            avg_weight = mean(weight_in_kilograms, na.rm=TRUE)) |>
  ungroup()

# Define UI that draws scatter plot
ui <- fluidPage(
  selectInput("position_type", 
              label = "Position", 
              choices = c("goalies", "forwards", "defensemen")),
  plotOutput("plot")
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  #convert hockey season to numeric value
  nhl_hw_pos <- nhl_hw |> 
    mutate(season = as.numeric(substr(season,0,4)))
  
  
  output$plot <- renderPlot({
    nhl_hw_pos <- nhl_hw_pos |>
      filter(position_type == input$position_type)
    
    #Plot
    ggplot(nhl_hw_pos, 
           aes(x = avg_weight, 
               y = avg_height)) +
      geom_point(aes(color = nhl_hw_pos$season)) +
      scale_color_continuous(name = "Season") +
      ggtitle(paste("The Evolution of the Modern Hockey",
                    tools::toTitleCase(input$position_type),
                    ": Examining the Changing Physique Over a Century")) +
      xlab("Weight (kg)") + 
      ylab("Height (cm)") +
      theme_minimal() +
      theme(
        panel.grid.major = element_blank()
      )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
