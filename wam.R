sum.con <- function(x, c, i)
{
    l <- x[[i]]
    m <- 0.8 ** abs(l - c[[i - 1]])
    return ((l + m*c[[i - 1]]) / (1 + m))
}


all.con <- function(x, c, i)
{
    l <- x[[i]]
    m <- 0.8 ** abs(l - x[1:(i - 1)])
    return ((l + sum(m * x[1:(i - 1)])) / (1 + sum(m)))
}

main <- function()
{
    x <- runif(1000)
    c <- 0
    ylim <- c(0, 1)
    xlab <- "Number Summaries"
    ylab <- "Weighted-Average Context"
    for (i in 2:length(x)) {
        c[[i]] <- sum.con(x, c, i)
    }
    plot(c, ylim=ylim, main="Summarised context summarization", xlab=xlab, ylab=ylab)

    for (i in 2:length(x)) {
        c[[i]] <- all.con(x, c, i)
    }
    plot(c, ylim=ylim, main="All context summarization", xlab=xlab, ylab=ylab)

}


main()
