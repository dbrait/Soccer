#clear
rm(list=ls())

libraries
library(ggplot2)

#what are urls

years = c(rep("0001",4), rep("0102",4), rep("0203",4), rep("0405",4),
          rep("0506",5), rep("0607",5), rep("0708",5), rep("0809",5),
          rep("0910",5), rep("1011",5), rep("1112",5), rep("1213",5),
          rep("1314",5), rep("1415",5))
divis = c(rep(c("E0", "E1", "E2", "E3"),4), rep(c("E0","E1","E2","E3","EC"),10))

urls = paste(years, divis, sep="/")
urls = paste("http://www.football-data.co.uk/mmz4281", urls, sep="/")

odds = c("B365H", "B365D", "B365A",
         "BSH", "BSD", "BSA",
         "BWH", "BWD", "BWA",
         "GBH", "GBD", "GBA",
         "IWH", "IWD", "IWA",
         "LBH", "LBD", "LBA",
         "PSH", "PSD", "PSA",
         "SOH", "SOD", "SOA",
         "SBH", "SBD", "SBA",
         "SJH", "SJD", "SJA",
         "SYH", "SYD", "SYA",
         "VCH", "VCD", "VCA",
         "WHH", "WHD", "WHA")

home = odds[seq(1, length(odds),3)]
draw = odds[seq(2, length(odds), 3)]
away = odds[seq(3, length(odds),3)]

#load all data in a loop
full.data = NULL
for(i in 1:length(urls)){
  temp = read.csv(urls[i])
  #calc average odds
  temp$homeodds = apply(temp[,names(temp) %in% home], 1, function(x) mean(x,na.rm=T))
  temp$drawodds = apply(temp[,names(temp) %in% draw], 1, function(x) mean(x,na.rm=T))
  temp$awayodds = apply(temp[,names(temp) %in% away], 1, function(x) mean(x,na.rm=T))
  temp = temp[, c("Div", "Date", "FTHG", "FTAG", "FTR", "homeodds", "drawodds", "awayodds")]
  full.data = rbind(full.data, temp)
}

full.data$homewin = ifelse(full.data$FTR=="H",1,0)
full.data$draw = ifelse(full.data$FTR=="D",1,0)
full.data$awaywin = ifelse(full.data$FTR=="A",1,0)

#convert to probs with overrind
full.data$homeprob = (1/full.data$homeodds)/(1/full.data$homeodds+1/full.data$drawodds+1/full.data$awayodds)
full.data$drawprob = (1/full.data$drawodds)/(1/full.data$homeodds+1/full.data$drawodds+1/full.data$awayodds)
full.data$awayprob = (1/full.data$awayodds)/(1/full.data$homeodds+1/full.data$drawodds+1/full.data$awayodds)

#bookie residual
full.data$bookieres = 1-full.data$homeprob
full.data$bookiere[full.data$FTR=="D"] = 1- full.data$drawprob[full.data$FTR=="D"]
full.data$bookiere[full.data$FTR=="A"] = 1-full.data$awayprob[full.data$FTR=="A"]

#plot over time
full.data$time = ifelse(nchar(as.character(full.data$Date))==8,
                        as.Date(full.data$Date, format="%d/%m/%y"),
                        as.Date(full.data$Date, format="%d/%m/%Y"))
full.data$date = as.Date(full.data$time, origin = "1970-01-01")

full.data$Division = "Premier League"
full.data$Division[full.data$Div=="E1"] = "Championship"
full.data$Division[full.data$Div=="E2"] = "League 1"
full.data$Division[full.data$Div=="E3"] = "League 2"
full.data$Division[full.data$Div=="EC"] = "Conference"

full.data$Division = factor(full.data$Division, levels=c("Premier League", "Championship", "League 1", "League 2", "Conference"))

ggplot(full.data, aes(date, bookieres, colour=Division)) +
  stat_smooth(size = 1.25, alpha = 0.2) +
  labs(x = "Year", y = "Uncertainty") +
  theme_bw() +
  theme(legend.position="bottom") +
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=20),
        legend.title=element_text(size=20),
        legend.text=element_text(size=20))
