library(tidyverse)

# Inputs

starting_WAR <- 6.3
years_until_optOut <- 5 # years until opt out
starting_age <- 28
no_opt_out_annualDollars <- 36 # dollars per year after no opt out
no_opt_out_years <- 4 # additional years if no opt out
pre_opt_out_annualDollars <- 36 # dollars per year pre opt out
cost_per_WAR <- 8
player <- "Gerrit Cole"

set.seed(123)

# function
iterate_year <- function(WAR0,
                         ExpiryYears,
                         Age0,
                         WinCost,
                         OptionValue,
                         TenorYears,
                         FrontYearCost) {
  
  iteration_df <- data.frame(YearCount = 0,
                             AgeCount = Age0,
                             yearWAR = WAR0,
                             WinCost = WinCost,
                             yearSalary = 0)
  
  YearCount <- 0

  
  while (YearCount < ExpiryYears) { # add a row for every year until the opt-out year
    
    SkillChange <- rnorm(1) * 1.4 # random variation in skill
    newWAR <- tail(iteration_df$yearWAR, 1) + SkillChange # previous year's WAR plus change in skill
    if (tail(iteration_df$AgeCount, 1) > 29) { # 0.5 WAR per year aging penalty
      newWAR <- newWAR - 0.5
    }
    
    CostChange <- rnorm(1) * 0.8 + 0.25 # random variation in cost per win
    newWinCost <- tail(iteration_df$WinCost, 1) + CostChange # last year's cost per win plus change
    
    iteration_df[nrow(iteration_df) + 1, ] <- list(max(iteration_df$YearCount) + 1, # add one year
                                                   max(iteration_df$AgeCount) + 1, # add one year to age
                                                   newWAR, # the WAR provided that year
                                                   newWinCost, # the cost per WAR that year
                                                   FrontYearCost)
    YearCount <- YearCount + 1
    
  } 
  
  # iteration_df <- iteration_df %>% 
  #   dplyr::mutate(iterationWAR = cumsum(yearWAR) - WAR0, # cumulative WAR provided
  #                 totalSalary = cumsum(yearSalary))
  
  AgeFinal <- max(iteration_df$AgeCount) + TenorYears - 1 # what would age be at contract end assuming no opt out?
  DeclineYears <- (max(AgeFinal, 29) - max(max(iteration_df$AgeCount), 29)) # how many years of decline left during contract?
  AgePenalty <- DeclineYears * (DeclineYears + 1) / 4 # half a WAR per year aging penalty -- triangular numbers
  
  expiryYearWAR <- tail(iteration_df$yearWAR, 1)
  WinCost <- tail(iteration_df$WinCost, 1)
  
  if (expiryYearWAR * WinCost * TenorYears - AgePenalty * WinCost < OptionValue * TenorYears) {
    while (YearCount < ExpiryYears + TenorYears) {
    
    newWAR <- tail(iteration_df$yearWAR, 1) - 0.5
    CostChange <- rnorm(1) * 0.8 + 0.25 # random variation in cost per win
    newWinCost <- tail(iteration_df$WinCost, 1) + CostChange # last year's cost per win plus change
    
    iteration_df[nrow(iteration_df) + 1, ] <- list(max(iteration_df$YearCount) + 1, # add one year
                                                   max(iteration_df$AgeCount) + 1, # add one year to age
                                                   newWAR, # the WAR provided that year
                                                   newWinCost, # the cost per WAR that year
                                                   OptionValue)
    YearCount <- YearCount + 1
    }
    
    iteration_df <- iteration_df %>% 
      mutate(iterationWAR = cumsum(yearWAR) - WAR0, # cumulative WAR provided
             totalSalary = cumsum(yearSalary),
             Opted_Out = "No")
    
    } else {
      
      iteration_df <- iteration_df %>% 
        mutate(iterationWAR = cumsum(yearWAR) - WAR0, # cumulative WAR provided
               totalSalary = cumsum(yearSalary),
               Opted_Out = "Yes")
    
    }
    iteration_df
}

simulations <- 100
simulation_list <- replicate(simulations, iterate_year(WAR0 = starting_WAR,
                                      ExpiryYears = years_until_optOut,
                                      Age0 = starting_age,
                                      WinCost = cost_per_WAR,
                                      OptionValue = no_opt_out_annualDollars,
                                      TenorYears = no_opt_out_years,
                                      FrontYearCost = pre_opt_out_annualDollars), 
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
  geom_vline(xintercept = years_until_optOut,
             color = "black",
             linetype = "dashed") +
  annotate(geom = "text", 
           label = "\nOpt-out Year", 
           x = years_until_optOut, 
           y = -Inf, 
           hjust = 1.25, vjust = 0.35, 
           angle = -90,
           color = "black") +
  facet_wrap(~ Opted_Out,
             ncol = 2) +
  scale_x_continuous(breaks = 0:(years_until_optOut + no_opt_out_years)) +
  scale_y_continuous(breaks = seq(from = round(min(simulation_df$iterationWAR), -1), 
                                  to = round(max(simulation_df$iterationWAR), -1), 
                                  by = 10)) +
  theme_minimal() +
  labs(title = paste("Outcomes of ", simulations, " Simulations of ", player, "'s Contract",
                     sep = ""),
       subtitle = paste(player, " opts out in ", simulation_summary$percentage[1], "% of simulations", sep = ""),
       x = "Contract Year",
       y = "Cumulative WAR")



