##' convert Date to decimal format, eg "2014-05-05" to "2014.34"
##'
##' 
##' @title Date2decimal
##' @param x Date
##' @return numeric
##' @export
##' @author Guangchuang Yu
Date2decimal <- function(x) {
    x %<>% as.Date
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


