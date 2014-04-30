
require(plyr)

read_file <- function() {
    ## Read outcome data
    outcome <- read.csv("rprog-data-ProgAssignment3-data/outcome-of-care-measures.csv", 
                        colClasses="character")
    
    ## Clean the column names
    names(outcome) <- gsub("\\.", "", names(outcome))
    names(outcome) <- tolower(names(outcome))
    outcome$state <- as.factor(outcome$state)
    
    ## Turn columns to numeric
    outcome$hospital30daydeathmortalityratesfromheartfailure <- as.numeric(outcome$hospital30daydeathmortalityratesfromheartfailure)
    outcome$hospital30daydeathmortalityratesfromheartattack <- as.numeric(outcome$hospital30daydeathmortalityratesfromheartattack)
    outcome$hospital30daydeathmortalityratesfrompneumonia <- as.numeric(outcome$hospital30daydeathmortalityratesfrompneumonia)
    
    outcome
}
check_state_input <- function(state, states) {
    ## Check that state and outcome are valid    
    if (!(state %in% states)) {
        stop("invalid state")
    }    
}

check_outcome_input <- function(outcome) {
    if (!(outcome %in% c("heart attack", "heart failure", "pneumonia"))) {
        stop("invalid outcome")
    }    
}

check_inputs <- function(state, outcome, states) {
    
    check_state_input(state,states)
    check_outcome_input(outcome)
}

sort_data <- function(state_data, outcome, num) {
    ## prepare the search string
    search_string <- paste("hospital30daydeathmortalityratesfrom",outcome)
    search_string <- gsub(" ", "", search_string)
    #print(search_string)
    
    ## sort the hospitals in correct order
    if (num == 'worst') {
        ordered_hospitals <- state_data[order(-state_data[,search_string], xtfrm(state_data[,"hospitalname"])),]
    }
    else {
        ordered_hospitals <- state_data[order(state_data[,search_string], xtfrm(state_data[,"hospitalname"])),]
    }
    
    ## Alternate way to sort the list. 
    #if (outcome == 'heart attack') {
    #    if (num == 'worst') {
    #        ordered_hospitals <- arrange(state_outcome, desc(hospital30daydeathmortalityratesfromheartattack), hospitalname)            
    #    }
    #    else {
    #        ordered_hospitals <- arrange(state_outcome, hospital30daydeathmortalityratesfromheartattack, hospitalname)            
    #    }
    #}
    #else if (outcome == 'heart failure'){
    #    if (num == 'worst') {
    #        ordered_hospitals <- arrange(state_outcome, desc(hospital30daydeathmortalityratesfromheartfailure), hospitalname)            
    #    }
    #    else {
    #        ordered_hospitals <- arrange(state_outcome, hospital30daydeathmortalityratesfromheartfailure, hospitalname)            
    #    }        
    #}
    #else {
    #    if (num == 'worst') {
    #        ordered_hospitals <- arrange(state_outcome, desc(hospital30daydeathmortalityratesfrompneumonia), hospitalname)            
    #    }
    #    else {
    #        ordered_hospitals <- arrange(state_outcome, hospital30daydeathmortalityratesfrompneumonia, hospitalname)            
    #    }    
    #}
    ordered_hospitals
}

pick_hospital <- function(ordered_hospitals, num) {
    
    ## Pick the required hostpital based on num arguement
    if (num == "best") {
        hospital <- ordered_hospitals[1,2] 
    }
    else if (num == "worst") {
        hospital <- ordered_hospitals[1,2]
    }
    else {
        if (num > nrow(ordered_hospitals))
            hospital <- NA
        else 
            hospital <- ordered_hospitals[num,2]     
    }
}