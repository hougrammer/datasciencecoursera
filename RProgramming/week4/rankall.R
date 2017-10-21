rankall <- function(outcome, num = "best") {
    if (outcome == 'heart attack') colnum = 11
    else if (outcome == 'heart failure') colnum = 17
    else if (outcome == 'pneumonia') colnum = 23
    else stop('invalid outcome')
    
    df[,colnum] <<- as.numeric(df[,colnum])
    df <<- df[order(df[colnum], df[2]), ]
    
    allstates = sort(unique(df$State))
    ret = data.frame(hospital = character(length(allstates)), state = character(length(allstates)), row.names = allstates, stringsAsFactors = FALSE)
    for (s in allstates) {
        hospitals = df[!is.na(df[colnum]) & df$State == s, 2]
        num =
            if (num == 'best') 1
            else if (num == 'worst') length(hospitals)
            else num
        ret[s,] = c(hospitals[num], s)
        #ret[s,] = c('a','b')
    }
    ret
}
