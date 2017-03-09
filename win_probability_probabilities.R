## Code for win probability results
## Author: Dan Cervone, 3/5/2017

source("win_prob_util.R")
##############################
## COMPILE DATA
##############################

## load NFL data
load(file="win_prob_data.Rdata")
## data tables pbp and games combiled using nflscrapR package

## merge pbp and game info
pbp <- merge(pbp, games[, list(homescore, awayscore, GameID)], by="GameID")
pbp <- pbp[!is.na(Home.WP.pre)]

## randomly switch home and away teams to get rid of home field advantage
pbp[, c("team1", "team2", "wp1.pre", "wp2.pre", "wp1.post", "wp2.post", "win1") := {
    switch.teams <- HomeTeam[1] < AwayTeam[1]
    if(switch.teams) {
        list(HomeTeam, AwayTeam, Home.WP.pre, Away.WP.pre, Home.WP.post, Away.WP.post, homescore > awayscore)
    } else {
        list(AwayTeam, HomeTeam, Away.WP.pre, Home.WP.pre, Away.WP.post, Home.WP.post, awayscore > homescore)
    }}, by="GameID"]

## simulate 10000 BM WPs of length 10000 (takes a min or two)
set.seed(0)
wp <- t(sapply(1:10000, function(i) simWPBM(10000)))

##############################
## 1. CHECK CALIBRATION
##############################

## for simulated win probability
png("~/git/xy/xywww/public/images/blog/simulated_wp_calibration.png", width=1200, height=400)
grid.arrange(
    calibrationPlot(wp[, 1000], wp[, 10000]) + ggtitle("10% through game"),
    calibrationPlot(wp[, 5000], wp[, 10000]) + ggtitle("50% through game"),
    calibrationPlot(wp[, 9000], wp[, 10000]) + ggtitle("90% through game"),
    nrow=1, ncol=3, top=textGrob("Simulated win probabilities match win frequencies", gp=gpar(fontsize=24,font=8)))
dev.off()

## for NFL win probability
png("~/git/xy/xywww/public/images/blog/nfl_wp_calibration.png", width=1200, height=400)
grid.arrange(
    with(pbp[qtr == "1"], calibrationPlot(wp1.pre, win1) + ggtitle("first quarter")),
    with(pbp[TimeSecs <= 1850 & TimeSecs > 1750], calibrationPlot(wp1.pre, win1) + ggtitle("halftime")),
    with(pbp[qtr == "4"], calibrationPlot(wp1.pre, win1) + ggtitle("fourth quarter")),
    nrow=1, ncol=3, top=textGrob("NFL probabilities match win frequencies", gp=gpar(fontsize=24,font=8)))
dev.off()

##############################
## 2. CHECK AUTOCORRELATION
##############################

wp.diff <- t(diff(t(wp)))
wp.acf <- acf(c(wp.diff), plot=F)

nfl.wp.diff <- with(pbp, wp1.post - wp1.pre)
nfl.wp.diff[which(nfl.wp.diff == 0)] <- NA ## remove non-plays
nfl.wp.acf <- acf(na.omit(nfl.wp.diff), plot=F)

png("~/git/xy/xywww/public/images/blog/wp_acf.png", width=1000, height=400)
par(mfrow=c(1, 2))
plot(wp.acf, ylim=c(-1, 1), type="b", pch=20, xlim=c(0, 6), main="Autocorrelation for simulated win probability changes", xlab="Change steps", ylab="Autocorrelation")
# text(2, -0.0025, "NOTE THE ZOOMED IN SCALE", col="red")
plot(nfl.wp.acf, ylim=c(-1, 1), type="b", pch=20, xlim=c(0, 6), main="Autocorrelation for NFL win probability changes", xlab="Change steps", ylab="Autocorrelation")
dev.off()

##############################
## 3. MIN WP OF WINING TEAM
##############################

## simulated minimum wp by winner
min.wp.winner <- apply(wp, 1, function(w) ifelse(tail(w, 1) == 1, min(w), min(1-w)))
c.choices <- seq(0, 0.5, l=100)
prob.c <- sapply(c.choices, function(p) mean(min.wp.winner < p))

nfl.min.wp.winner <- pbp[, list(min.wp=ifelse(win1[1] == 1, min(wp1.pre), min(1 - wp1.pre))), by="GameID"]$min.wp
nfl.prob.c <- sapply(c.choices, function(p) mean(nfl.min.wp.winner < p))

png("~/git/xy/xywww/public/images/blog/min_wp_winner.png", width=1000, height=400)
par(mfrow=c(1, 2))
plot(c.choices, c.choices / (1-c.choices), type="l", lwd=4, col="red", xlab="c", ylab="", main=expression(frac(c, 1-c) ~ " comeback rule for simulated win prob"))
text(0.4, 0.9, expression(frac(c, 1-c)), cex=1.5, col="red")
lines(c.choices, prob.c, lwd=4)
text(0.32, 0.2, "min simulated wp for winning team", cex=1)
plot(c.choices, c.choices / (1-c.choices), type="l", lwd=4, col="red", xlab="c", ylab="", main=expression(frac(c, 1-c) ~ " comeback rule for NFL win prob"))
text(0.38, 0.85, expression(frac(c, 1-c)), cex=1.5, col="red")
lines(c.choices, nfl.prob.c, lwd=4)
text(0.34, 0.2, "min  wp for winning NFL team", cex=1)
dev.off()

##############################
## 3. UPCROSSINGS
##############################

## upcrossings between c, (1-c)
## this takes a little while to calculate
c.choices <- seq(.01, .45, .01)
wp.upcrosses <- sapply(c.choices, function(x) mean(apply(wp, 1, upcrossCount, a=x))) * 2

nfl.wp.upcrosses <- sapply(c.choices, function(x) mean(pbp[, list(upcrosses=upcrossCount(wp1.pre, x)), by="GameID"]$upcrosses)) * 2

png("~/git/xy/xywww/public/images/blog/wp_upcrossings.png", width=1000, height=400)
par(mfrow=c(1, 2))
plot(c.choices, c.choices / (1 - 2 * c.choices), type="l", lwd=4, col="red", xlab="c", ylab="", main=expression(frac(c, 1 - 2*c) ~ " switching rule for simulated win prob"))
text(0.38, 3, expression(frac(c, 1 - 2*c)), cex=1.5, col="red")
lines(c.choices, wp.upcrosses, lwd=4)
text(0.32, 0.15, "simulated # win prob switches", cex=1)
plot(c.choices, c.choices / (1 - 2 * c.choices), type="l", lwd=4, col="red", xlab="c", ylab="", main=expression(frac(c, 1 - 2*c) ~ " switching rule for NFL win prob"))
text(0.38, 3, expression(frac(c, 1 - 2*c)), cex=1.5, col="red")
lines(c.choices, nfl.wp.upcrosses, lwd=4)
text(0.34, 0.16, "NFL # win prob switches", cex=1)
dev.off()
