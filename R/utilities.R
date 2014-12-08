roundDigit <- function(d) {
    i <- 0
    while(d < 1) {
        d <- d * 10
        i <- i + 1
    }
    round(d)/10^i
}




## . function was from plyr package
. <- function (..., .env = parent.frame()) {
    structure(as.list(match.call()[-1]), env = .env, class = "quoted")
}


