## utilities for win probability simulation/heuristics
## Author: Dan Cervone, 3/5/2017

library(ggplot2)
library(data.table)
library(grid)
library(gridExtra)

## simulate brownian-motion "win probability" for N steps
simWPBM <- function(N) {
    z <- cumsum(rnorm(N))
    wp <- 1 - pnorm(0, z, sqrt(N - 1:N))
    return(wp)
}

## calibration plot of predicted win prob against observed win frequency
calibrationPlot <- function(prob, outcome, bins=100) {
    prob.bins <- cut(prob, unique(quantile(prob, seq(0, 1, l=bins+1))))
    prob.means <- tapply(prob, prob.bins, mean)
    outc.means <- tapply(outcome, prob.bins, mean)
    outc.sd <- tapply(prob, prob.bins, function(p) sqrt(mean(p * (1- p)) / length(p)))
    df <- data.frame(prob.means, outc.means, outc.sd)

    ggplot(df, aes(x = prob.means, y = outc.means)) +
        geom_abline(slope=1, color="red", lwd=2) +
        geom_point(size = 2) +
        geom_errorbar(aes(ymin = outc.means - 2 * outc.sd, ymax = outc.means + 2 * outc.sd), color="gray50") +
        labs(x="predicted win prob", y="observed win %") +
        lims(x=c(0, 1), y=c(0, 1)) +
        theme(plot.title = element_text(size=16, hjust = 0.5)) +
        theme(axis.text=element_text(size=14), axis.title=element_text(size=16))
}

## count number of upcrossings between a and 1 - a for wp
upcrossCount <- function(wp, a) {
    below.a <- which(wp < a)
    above.a <- which(wp > 1 - a)
    if(length(below.a) != 0 & length(above.a) != 0) {
        upcrossed.points <- above.a[which(above.a > min(below.a))]
        if(length(upcrossed.points) > 0) {
            first.upcrossed <- min(upcrossed.points)
            return(1 + upcrossCount(wp[-(1:first.upcrossed)], a))
        }
    }
    return(0)
}
