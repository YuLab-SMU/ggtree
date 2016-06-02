## not used.
## we don't guess mrsd from tip labels
## user should provide mrsd parameter in ggtree if they want to use time-scaled tree
scaleX_by_time <- function(df, as.Date=FALSE) {
    time <- with(df, gsub(".*[_/]{1}(\\d+\\.*\\d+)$", "\\1", label[isTip])) %>% as.numeric
    latest <- which.max(time)

    scaleX_by_time_from_mrsd(df, decimal2Date(time[latest]), as.Date)
}


scaleX_by_time_from_mrsd <- function(df, mrsd, as.Date) {
    mrsd %<>% as.Date
    date <- Date2decimal(mrsd)

    df$x <- df$x + date - max(df$x)
    df$branch <- (df[df$parent, "x"] + df[, "x"])/2

    if (as.Date) {
        df$x <- decimal2Date(df$x)
        df$branch <- decimal2Date(df$branch)
    }
    
    return(df)
}



##' convert Date to decimal format, eg "2014-05-05" to "2014.34"
##'
##' 
##' @title Date2decimal
##' @param x Date
##' @return numeric
##' @export
##' @author Guangchuang Yu
Date2decimal <- function(x) {
    if (is(x, "numeric")) {
        return(x)
    }
    
    if (is(x, "character")) {
        x <- as.Date(x)
    }
    year <- format(x, "%Y")
    y <- x - as.Date(paste0(year, "-01-01"))
    as.numeric(year) + as.numeric(y)/365
}

##' convert decimal format to Date, eg "2014.34" to "2014-05-05"
##'
##' 
##' @title decimal2Date
##' @param x numerical number, eg 2014.34
##' @return Date
##' @export
##' @author Guangchuang Yu
decimal2Date <- function(x) {
    date <- as.Date(paste0(floor(x), "-01-01"))
    date + as.numeric(sub("^\\d+", "0", x)) * 365
}


