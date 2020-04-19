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