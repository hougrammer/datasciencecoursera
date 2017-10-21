pollutantmean <- function(directory = 'specdata', pollutant, id = 1:332) {
    s = 0
    n = 0
    for(i in id) {
        j = if (i < 10) paste('00', i, sep = '')
            else if (i < 100) paste('0', i, sep = '')
            else i
        
        d = read.csv(paste(directory, '/', j, '.csv', sep = ''))
        for (p in d[[pollutant]]) {
            if (!is.na(p)) {
                s = s + p
                n = n + 1
            }
        }
    }
    s / n
}

complete <- function(directory = 'specdata', id = 1:332) {
    ret = data.frame(id=integer(), nobs=integer())
    for(i in id) {
        j = if (i < 10) paste('00', i, sep = '')
        else if (i < 100) paste('0', i, sep = '')
        else i
        
        d = read.csv(paste(directory, '/', j, '.csv', sep = ''))
        
        ret[nrow(ret)+1,] = c(i, length(which(!is.na(d$sulfate) & !is.na(d$nitrate))))
    }
    ret
}

corr <- function(directory = 'specdata', threshold = 0) {
    ret = rep(NA, 332)
    for(i in 1:332) {
        j = if (i < 10) paste('00', i, sep = '')
        else if (i < 100) paste('0', i, sep = '')
        else i
        
        d = read.csv(paste(directory, '/', j, '.csv', sep = ''))
        
        good = !is.na(d$sulfate) & !is.na(d$nitrate)
        if (length(which(good)) > threshold) {
            good_d = d[good,]
            ret[i] = cor(good_d$sulfate, good_d$nitrate)
        }
            
    }
    ret[!is.na(ret)]
}