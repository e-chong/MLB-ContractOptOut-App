

library(shiny)
library(tidyverse)
source("iterate_year.R")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("MLB Contract Opt-out Simulator"),

    # Sidebar inputs
    sidebarLayout(
        sidebarPanel(
            helpText("Simulate the likelihood of a player opting out of his contract.",
                     br(),
                     br(),
                     "A Shiny and R implementation of this",
                     a("piece",     
                       href="https://blogs.fangraphs.com/whats-an-opt-out-worth/",
                       target = "_blank"),
                     "and", 
                     a("Python script",     
                       href="https://github.com/bclemens6/variable-contract-opt-outs/blob/master/WAR%20Per%20Dollar%20With%20Options.py",
                       target = "_blank"),
                       "by Ben Clemens at FanGraphs."),
            textInput("player", 
                      "Player Name:", 
                      value = "Gerrit Cole"),
            sliderInput("starting_age",
                         "Age during year 0:",
                         min = 18,
                         max = 50,
                         value = 28,
                         step = 1),
            numericInput("starting_WAR",
                         "WAR during year 0:",
                         min = -5,
                         max = 20,
                         value = 6.3,
                         step = 0.1),
            sliderInput("cost_per_WAR",
                         "$mm per WAR during year 0:",
                         min = 0,
                         max = 20,
                         value = 8,
                         step = 0.5),
            numericInput("years_until_optOut",
                        "Years until opt-out year:",
                        min = 1,
                        max = 20,
                        value = 5,
                        step = 1),
            numericInput("pre_opt_out_annualDollars",
                         "Pre-opt-out year annual salary ($mm):",
                         min = 0.5,
                         max = 50,
                         value = 36,
                         step = 0.1),
            numericInput("no_opt_out_years",
                        "Years after opt-out year:",
                        min = 1,
                        max = 20,
                        value = 4,
                        step = 1),
            numericInput("no_opt_out_annualDollars",
                         "Post-opt-out year annual salary ($mm):",
                         min = 0.5,
                         max = 50,
                         value = 36,
                         step = 0.1),
            numericInput("simulations",
                         "Number of simulations (1 to 1mm):",
                         min = 1,
                         max = 1000000,
                         value = 1000,
                         step = 1),
            # actionButton("go", "Run Simulation!"),
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("simulationPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$simulationPlot <- renderPlot({
        
        simulation_list <- replicate(input$simulations, iterate_year(WAR0 = input$starting_WAR,
                                                               ExpiryYears = input$years_until_optOut,
                                                               Age0 = input$starting_age,
                                                               WinCost = input$cost_per_WAR,
                                                               OptionValue = input$no_opt_out_annualDollars,
                                                               TenorYears = input$no_opt_out_years,
                                                               FrontYearCost = input$pre_opt_out_annualDollars), 
                                     simplify = FALSE)
        
        simulation_df <- bind_rows(simulation_list, 
                                   .id = "iteration")
        
        simulation_summary <- simulation_df %>% 
            group_by(Opted_Out) %>% 
            summarize(count = n_distinct(iteration)) %>% 
            mutate(percentage = count * 100 / sum(count)) %>% 
            arrange(desc(Opted_Out))
        
        ggplot(simulation_df, aes(x = YearCount, 
                                  y = iterationWAR)) +
            geom_line(aes(group = iteration),
                      color = "lightgray") + 
            geom_line(stat = 'summary', fun.y = mean,
                      color = "red",
                      size = 1.5) +
            geom_vline(xintercept = input$years_until_optOut,
                       color = "black",
                       linetype = "dashed") +
            annotate(geom = "text", 
                     label = "\nOpt-out Year", 
                     x = input$years_until_optOut, 
                     y = -Inf, 
                     hjust = 1.25, vjust = 0.35, 
                     angle = -90,
                     color = "black") +
            facet_wrap(~ Opted_Out,
                       ncol = 2) +
            scale_x_continuous(breaks = 0:(input$years_until_optOut + input$no_opt_out_years)) +
            scale_y_continuous(breaks = seq(from = round(min(simulation_df$iterationWAR), -1), 
                                            to = round(max(simulation_df$iterationWAR), -1), 
                                            by = 10)) +
            theme_minimal() +
            labs(title = paste("Outcomes of ", input$simulations, " Simulations of ", input$player, "'s Contract",
                               sep = ""),
                 subtitle = paste(input$player, " opts out in ", simulation_summary$percentage[1], "% of simulations", sep = ""),
                 x = "Contract Year",
                 y = "Cumulative WAR")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
