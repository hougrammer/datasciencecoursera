best <- function(state, outcome) {
    if (outcome == 'heart attack') colnum = 11
    else if (outcome == 'heart failure') colnum = 17
    else if (outcome == 'pneumonia') colnum = 23
    else stop('invalid outcome')
    
    
    #if (is.null(df)) df = read.csv('outcome-of-care-measures.csv', colClasses = 'character')
    if (!(state %in% df$State)) stop('invalid state')
    
    df[,colnum] <<- as.numeric(df[,colnum])
    df <<- df[order(df[colnum], df[2]), ]
    
    df[match(state, df$State), 2]
}
