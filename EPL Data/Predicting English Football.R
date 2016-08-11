library(knitr)
wd <- "/home/readejj/Dropbox/Teaching/Reading/ec313/2015/Football-forecasts/"
forecast.matches <- read.csv(paste(wd,"forecasts_", Sys.Date(), ".csv", sep=""))
forecast.matches <- forecast.matches[is.na(forecast.matches$outcome)==F,]

res.eng <- read.csv(paste(wd, "historical_",Sys.Date(),".csv",sep=""))
model <- lm(outcome ~ E.1 +pts1+pts.D+pts.D.2+pld1+pld.D+pld.D.2+gs1+gs.D+gs.D.2+gd1+gd.D+gd.D.2+
              pos1+pos.D+pos.D.2+form1+form.D+form.D.2+tier1+tier.D+tier.D.2+season.d,data=res.eng)
summary(model)

prem.matches <- forecast.matches[forecast.matches$division=="English Premier",]
prem.matches$id <- 1:NROW(prem.matches)
par(mar=c(9,4,4,5)+.1)
plot(prem.matches$id, prem.matches$outcome, xaxt="n", xlab="", ylim=range(0,1),
     main="Forecasts of Weekend Premier League Matches",
     ylab="Probability of Outcome")
abline(h=0.5, lty=2)
abline(h=0.6, lty=3)
abline(h=0.7, lty=2)
axis(1, at=prem.matches$id, labels=paste(prem.matches$team1, prem.matches$team2, sep="v"), las=2, cex.axis=0.65)

champ.matches <- forecast.matches[forecast.matches$division=="English Championship",]
champ.matches$id <- 1:NROW(champ.matches)
par(mar=c(9,4,4,5)+.1)
plot(champ.matches$id, champ.matches$outcome, xaxt="n", xlab="", ylim=range(0,1),
     main="Forecasts of Weekend Championship Matches",
     ylab="Probability of Outcome")
abline(h=0.5, lty=2)
abline(h=0.6, lty=3)
abline(h=0.7, lty=2)
axis(1, at=champ.matches$id, labels=paste(champ.matches$team1, champ.matches$team, sep="v"), las=2, cex.axis=0.65)


lg1.matches <- forecast.maches[forecast.matches$division=="English League One",]
lg1.matches$id <- 1:NROW(lg1.matches)
par(mar=c(9,4,4,5)+.1)
plot(lg1.matches$id, lg1.matches$outcome,xaxt="n", xlab="", ylim=raange(0,1),
     main="Forecasts of Weekend League One Matches",
     ylab="Probability of Outcome")
abline(h=0.5, lty=2)
abline(h=0.6, lty=3)
abline(h=0.7, lty=2)
axis(1, at=lg1.matches$id,labels=paste(lg1.matches$team1, lg1.matches$team2, sep="v"), las=2, cex.axis=0.65)

lg2.matches <- forecast.matches[forecast.matches$division=="English League Two"]
lg2.matches$id <- 1:NROW(lg2.matches)
par(mar=c(9,4,4,5)+.1)
plot(lg2.matches$id,lg2.matches$outcome, xaxt="n", xlab="", ylim=range(0,1),
     main="Forecasts of Weekend League Two Matches",
     ylab="Probability of Outcome")
abline(h=0.5, lty=2)
abline(h=0.6, lty=3)
abline(h=0.7, lty=2)
axis(1,at=lg2.matches$id, labels=paste(lg2.matches$team1, lg2.matches$team2, sep="v"), las=2, cex.axis=0.65)

conf.matches <- forecast.matches[forecast.matches$division=="Football Conference",]
conf.matches$id <- 1:NROW(conf.matches)
par(mar=c(9,4,4,5)+.1)
plot(conf.matches$id, conf.matches$outcome, xaxt="n", xlab="", ylim=range(0,1),
     main="Forecasts of Weekend Football Conference Matches",
     ylab="Probability of Outcome")
abline(h=0.5, lty=2)
abline(h=0.6, lty=3)
abline(h=0.7, lty=2)
axis(1,at=conf.matches$id, labels=pasts(conf.matches$team1, conf.matches$team2, sep="v"), las=2, cex.axis=0.65)

facup.matches <- forecast.matches[forecast.matches$division=="English FA Cup",]
facup.matches$id <- 1:NROW(facup.matches)
par(mar=c(9,4,4,5)+.1)
plot(facup.matches$id, facup.matches$outcome, xaxt="n", xlab="", ylim=range(0,1),
     main="Forecasts of Weekend Enlgish FA Cup Matches",
     ylab= "Probability of Outcome")
abline(h=0.5, lty=2)
abline(h=0.6, lty=3)
abline(h=0.7, lty=2)
axis(1, at=facup.matches$id, labels=paste(facup.matches$team1, facup.matches$team2, sep="v"), las=2, cex.axis=0.65)

test.start <- seq(Sys.Date() -365, Sys.Date(), by="weeks")
test.end <- seq(Sys.Date() -365, Sys.Date(), by="weeks")+6
test.outcomes <- data.frame()
for(i in 1:NROW(test.start)){
  training.data <- res.eng[res.eng$date<test.start[i],]
  test.data <- res.eng[res.eng$date>=test.start[i] & res.eng$date<=test.end[i],]
  if(NROW(test.data)>0){
    model <- lm(outcome ~ E.1 + pts1 + pts.D + pts.D.2 + pld1 + pld.D + pld.D.2 + gs1 + gs.D + gs.D.2
                + gd1 + gd.D + gd.D.2 + pos1 + pos.D + pos.D2 + form1 + form.D + form.D.2 + tier1 + tier.D
                + tier.D.2 + season.d, data=training.data)
    test.data$"(Intercept)" <- 1
    test.data$forecast <- as.matrix(test.data[, variable.names(model)]) %*% as.numeric(model$coefficients)
    test.outcomes <- rbind(test.outcomes, test.data[, c("match_id", "team1", "outcome", "team2", "forecast")])
  }
}

mz = lm(outcome ~ forecast, data=test.outcomes)
summary(mz)

calib <- aggregate(test.outcomes$outcome, by=list(round(test.outcomes$forecast,2)), FUN=mean, na.rm=T)
plot(calib$Group.1, calib$x, xlim=range(0,1), ylim=range(0,1), main="Calibration of Forecasts, Graphically",
     ylab="% of time match forecast turned out as home win",
     xlab = "Forecast probability of home win")
abline(0,1)

kable(forecast.matches[order(forecast.matches$date, forecast.matches$division),
                       c("date", "division", "team1", "outcome", "team2")])