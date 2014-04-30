source('~/Desktop/R projects/R Prog/Assignment 4/rankheader.R')

rankall<- function(outcome, num = "best") {
    ## Read outcome data
    outcome_data <- read_file()
    
    ## Check that state and outcome are valid
    check_outcome_input(outcome)
    
    states <- sort(levels(outcome_data$state))
    ## For each state, find the hospital of the given rank
    df <- data.frame()
    for (state in states) {
        state_outcome <- outcome_data[outcome_data$state == state, ]
        ordered_hospitals <- sort_data(state_outcome, outcome, num)
        hospital <- pick_hospital(ordered_hospitals, num)
        df1 <- data.frame(hospital,state)
        df <- rbind(df, df1)
    }
    
    ## Return a data frame with the hospital names and the ## (abbreviated) state name
    df
}
