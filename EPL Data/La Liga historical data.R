library(devtools)

install_github("jalapic/engsoccerdata", username="jalapic")

library(engsoccerdata)
library(dplyr)
library(ggplot2)

df <- spainliga
head(df)

table(df$round)

table(df$group)

temp <- rbind(
  df %>% select(Season, team=home, opp=visitor, GF=hgoal, GA=vgoal),
  df %>% select(Season, team=visitor, opp=home, GF=vgoal, GA=hgoal)
  ) #rbind two copries of original df, simply reversing home/away team for each match

temp$GF <- as.numeric(temp$GF) #make sure is numeric
temp$GA <- as.numeric(temp$GA) #make sure is numeric
temp <- temp %>% mutate(GD = GF-GA)
head(temp)

temp1 <-
  temp %>% group_by(team) %>%
  summarize(GP = n(),
            goalsF= sum(GF),
            goalsA = sum(GA),
            goaldif = sum(GD),
            W = sum(GD>0),
            D = sum(GD==0),
            L = sum(GD<0)
  )

temp1

#just setting my theme
mytheme <- theme(
  plot.title = element_text(hjust=0, vjust=1, size=rel(1.7)),
  panel.background = element_blank(),
  panel.grid.major.y = element_line(color="gray65"),
  panel.grid.major.x = element_line(color="gray65"),
  panel.grid.minor = element_blank(),
  plot.background = element_blank(),
  text = element_text(color="gray20", size=10),
  axis.text = element_text(size=rel(1.0)),
  axis.text.x = element_text(color="gray20", size=rel(1.5)),
  axis.text.y = element_text(color="gray20", size=rel(1.5)),
  axis.title.x = element_text(size=rel(1.5), vjust=0),
  axis.title.y = element_text(size=rel(1.5), vjust=1),
  axis.ticks.y = element_blank(),
  axis.ticks.x = element_blank(),
  legend.position = "none"
)

ggplot(temp1, aes(GP, goaldif)) + geom_point(size=4, color"firebrick1") + mytheme +
  xlab("Games played in La Liga") +
  ylab("Cumulative goal difference") +
  ggtitle("All time goal difference in La Liga")

df <- df %>% filter(Season == 2013)
df <- df %>%
  select(home, visitor, FT, hgoal, vgoal) %>%
  mutate(GD = hgoal - vgoal,
         result = ifelse(GD>0, "H", ifelse(GD<0, "A", "D"))
         )

head(df)

temp <-
  rbind(
    df %>% select(team=home, opp=visitor, GF=hgoal, GA=vgoal),
    df %>% select(team=visitor, opp=home, GF=vgoal, GA=hgoal)
    ) #rbind two copies of original df, simply reversing home/away team for each match

temp1 <-
  temp %>%
  mutate(GD = GF-GA) %>%
  group_by(team) %>%
  summarize(GP = n(),
            gf = sum(GF),
            ga = sum(GA),
            gd = sum(GD),
            W = sum(GD>0),
            D = sum(GD==0),
            L = sum(GD<0)
            ) %>%
  mutate(Pts = (W*3) + D) %>%
  arrange(desc(Pts))

temp1 <- temp1 %>% mutate(pos = rank(desc(Pts)))
temp1

temp2 <- temp1 %>% select(team, pos)

#manually edit tied teams,
temp2[2,2] <- 2
temp2[3,2] <- 3
temp2[7,2] <- 6
temp2[6,2] <- 7
temp2[9,2] <- 8
temp2[8,2] <- 9
temp2[14,2] <- 13
temp2[13,2] <- 14
temp2[16,2] <- 16
temp2[17,2] <- 17

temp2 <- temp2 %>% arrange(pos)
temp2

df$home <- factor(df$home, levels=rev(temp2$team))
levels(df$home)

df$visitor <- factor(df$visitor, levels=temp2$team)
levels(df$visitor)

ggplot(df, aes(home, visitor, fill=factor(result))) +
  geom_tile(colour="gray20", size=1.5, family="bold", stat="identity", height=1, width=1) +
  geom_text(data=df, aes(home, visitor, label=FT), color="black", size=rel(2)) +
  coord_flip() +
  scale_x_discrete(expand=c(0,0)) +
  scale_y_discrete(expand=c(0,0)) +
  xlab("") +
  ylab("") +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(fill=NA, color="gray20", size=0.5, linetype="solid"),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_text(color="white", size=rel(1)),
    panel.background = element_rect(fill="gray20"),
    plot.background = element_rect(fill="gray20"),
    legend.position = "none",
    axis.text.x = element_text(angle=90, vjust=0.5, hjust=0)
)