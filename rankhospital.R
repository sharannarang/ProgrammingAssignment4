source('~/Desktop/R projects/R Prog/Assignment 4/rankheader.R')
rankhospital <-function(state, outcome, num="best") { 

    outcome_data <- read_file()
    
    check_inputs(state, outcome, levels(outcome_data$state))
        
    ## Save the individual state outcomes only
    state_outcome <- outcome_data[outcome_data$state == state, ]
    
    ## Sort data..
    ordered_hospitals <- sort_data(state_outcome, outcome, num)

    hospital <- pick_hospital(ordered_hospitals, num)
    
    ## Return the hospital
    hospital
}
