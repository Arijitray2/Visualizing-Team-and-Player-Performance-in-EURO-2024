library(devtools)
devtools::install_github("statsbomb/SDMTools")
devtools::install_github("statsbomb/StatsBombR", force = T)
library(StatsBombR)
install.packages('remotes')
install.packages("StatsBombR", lib = "C:\\Program Files\\R\\R-VERSION.NUMBER.NUMBER\\library")

install.packages("SDMTools",,"http://rforge.net/",type="source")
remotes::install_version("SDMTools", "1.1-221")

d <- StatsBombR::FreeCompetitions()
devtools::install_github("statsbomb/SDMTools")
devtools::install_github("statsbomb/StatsBombR")
devtools::install_github("FCrSTATS/SBpitch")


#install.packages("ggrepel")
library(devtools)
library(StatsBombR)
library(tidyverse)
library(ggrepel)
library(cowplot)
#Extracting Data
Comp <- FreeCompetitions()
View(Comp)

#Accessing a Football Competition

AFCON <- FreeCompetitions() %>%
  filter(competition_id==55 & season_name=="2024") 
Matches <- FreeMatches(AFCON) 
StatsBombData <- free_allevents(MatchesDF = Matches, Parallel = T) 
StatsBombData <- allclean(StatsBombData) 
#View(StatsBombData)
names(StatsBombData)
#Extracting Shots and Goals Count Over the Competition
shots_goals <-StatsBombData %>%
  group_by(team.name) %>%
  summarise(shots = sum(type.name=="Shot", na.rm = TRUE),
            goals = sum(shot.outcome.name=="Goal", na.rm = TRUE))
#View(shots_goals)

#Scatterplot Between G and shot

ggplot(shots_goals,aes(x=shots,y=goals))+
  geom_point(shape=16,col="blue")+
  labs(x="Total Shots",y="Goals Scored")+
  theme()+
  geom_hline(yintercept=median(shots_goals$goals),linetype="dashed",col="red")+
  geom_vline(xintercept=median(shots_goals$shots),linetype="dashed",col="red")+
  geom_text_repel(data = subset(shots_goals, shots > mean(shots_goals$shots)),
                  aes(label = team.name),size = 3, col = "black")+
  geom_text_repel(data = subset(shots_goals, shots < 30),aes(label = team.name)
                  ,size = 3,col = "black")

#which teams scored the most goals

s_g<-shots_goals %>% mutate(team.name=reorder(team.name,goals))

ggplot(s_g,aes(y=team.name,x=goals))+
  geom_bar(stat="identity",fill="steelblue",col="black")+
  geom_text(aes(label=team.name), size = 2)+
  labs(y="\nTeams",x="\nGoals Scored")+
  theme_SB()+
  theme(axis.text.y = element_blank()) 


#Which team took their chances most*

s_g <- shots_goals %>% 
  mutate(Shots_Converted = shots/goals)%>%
  mutate(team.name=reorder(team.name,Shots_Converted))

ggplot(s_g,aes(x=Shots_Converted,y=team.name))+
  geom_bar(stat="identity",fill="yellow",col="black")+
  geom_text(aes(label=team.name), size = 2)+
  labs(x="\nShots Conversion Ratio",y="\nTeams")+
  theme_light()+
  theme(axis.text.y = element_blank())

#Plotting Passes(ex:-Cote D Ivore)

library(SBpitch)
create_Pitch() #Basic operation to create a pitch in R
create_Pitch(grass_colour = "#538032", line_colour =  "#ffffff", background_colour = "#538032", 
             goal_colour = "#000000")



Teampassmap <- function(team_id, team_name) {
  
  passes <- StatsBombData %>%
    filter(type.name == "Pass" & is.na(pass.outcome.name) & team.id == team_id) %>% 
    filter(pass.end_location.x >= 102 & pass.end_location.y <= 62 & pass.end_location.y >= 18) 
  
  if (nrow(passes) == 0) {
    stop("Please Check the team_id.")
  }
  
  create_Pitch(grass_colour = "#538032", line_colour = "#ffffff", background_colour = "#538032", goal_colour = "#000000") +
    geom_segment(data = passes, aes(x = location.x, y = location.y,xend = pass.end_location.x, yend = pass.end_location.y),
                 lineend = "round", size = 0.5, colour = "red", arrow = arrow(length = unit(0.07, "inches"), ends = "last", type = "open")) + 
    labs(title = paste(team_name, ", Completed Box Passes"), subtitle = "EURO 2024") + 
    scale_y_reverse() + 
    coord_fixed(ratio = 105/100)
}

Teampassmap(772,"Spain")

#player pass Map(ex:-Mo Salah)

playerpassmap<- function(player_id,player_name){
  
  pass<-StatsBombData %>% filter(type.name=="Pass" & is.na(pass.outcome.name) & player.id==player_id)
  
  if(nrow(pass)==0){
    stop("Please Check the Player Id again.!!!")
  }  
  
  create_Pitch(grass_colour = "#538032", line_colour = "#ffffff", background_colour = "#538032", goal_colour = "#000000") +
    geom_segment(data = pass, aes(x = location.x, y = location.y,xend = pass.end_location.x, yend = pass.end_location.y),
                 lineend = "round", size = 0.5, colour = "red", arrow = arrow(length = unit(0.07, "inches"), ends = "last", type = "open")) + 
    labs(title = paste(player_name, ", Completed  Passes"), subtitle = "EURO 2024") + 
    scale_y_reverse() + 
    coord_fixed(ratio = 105/100)
} 

playerpassmap( 316046,"Lamine Yamal Nasraoui Ebana")

#which player has received the most passes


rpass<- StatsBombData %>% filter(type.name=="Pass" & is.na(pass.outcome.name)) %>%
  group_by(pass.recipient.name) %>% summarise(pass_count = n()) %>%
  arrange(desc(pass_count))

head(rpass,10)

#Which Goalkeeper Saved the Most shots

gk <- StatsBombData %>%
  filter(type.name == "Shot" & shot.outcome.name == "Saved" & !is.na(player.name.GK) )

gk_saved <- gk %>%
  group_by(player.name.GK) %>%
  summarise(total_saves = n()) 

gk_Shot_faced <- StatsBombData %>%
  filter(type.name == "Shot" & !is.na(player.name.GK)) %>%
  group_by(player.name.GK) %>%
  summarise(total_shots_faced = n()) 

gk_Saves <- left_join(gk_saved, gk_Shot_faced, by = "player.name.GK")

View(gk_Saves)


#Gk_Shotssaved<- left_join(gk_Saves,player_minutes,by="player.id")

#calculating XGa

xGA <- StatsBombData %>%
  filter(type.name=="Shot") %>% 
  select(shot.key_pass_id, xGA = shot.statsbomb_xg) 

shot_assists <- left_join(StatsBombData, xGA, by = c("id" = "shot.key_pass_id")) %>% select(team.name, player.name, player.id, type.name, pass.shot_assist, 
                                                                                            pass.goal_assist, xGA ) %>% 
  filter(pass.shot_assist==TRUE | pass.goal_assist==TRUE) 
View(shot_assists)

player_xGA <-shot_assists %>%
  group_by(player.name, player.id, team.name) %>%
  summarise(xGA = sum(xGA, na.rm = TRUE)) 

player_xG<- StatsBombData%>%
  filter(type.name=="Shot") %>%
  filter(shot.type.name!="Penalty" | is.na(shot.type.name)) %>%
  group_by(player.name, player.id, team.name) %>%
  summarise(xG = sum(shot.statsbomb_xg, na.rm = TRUE)) %>%
  left_join(player_xGA) %>%
  mutate(xG_xGA = sum(xG+xGA, na.rm =TRUE) ) 

player_minutes <- get.minutesplayed(StatsBombData)
player_minutes <- player_minutes %>%
  group_by(player.id) %>%
  summarise(minutes = sum(MinutesPlayed)) 
player_xG_xGA <- left_join(player_xG, player_minutes) %>%
  mutate(nineties = minutes/90,xG_90 = round(xG/nineties, 2),
         xGA_90 = round(xGA/nineties,2),xG_xGA90 = round(xG_xGA/nineties,2) ) 

chart <- player_xG_xGA %>% ungroup() %>%filter(minutes>=300) %>%
  top_n(n = 15, w = xG_xGA90) 

chart<-chart %>%
  select(1, 9:10)%>%
  pivot_longer(-player.name, names_to = "variable", values_to = "value") %>%
  filter(variable=="xG_90" | variable=="xGA_90") 
View(chart)
ggplot(chart, aes(x =reorder(player.name, value), y = value, fill=fct_rev(variable))) + #1
  geom_bar(stat="identity", colour="white")+
  labs(title = "Expected Goal Contribution", subtitle = "EURO 2024",
       x="", y="Per 90", caption ="Minimum 300 minutes\nNPxG = Non_penalty xG\nxG assisted = Value of shots assisted")+
  theme(axis.text.y = element_text(size=14, color="#333333"),
        axis.title = element_text(size=14, color="#333333"),
        axis.text.x = element_text(size=14, color="#333333"),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = "white", colour = "white"),
        plot.background = element_rect(fill = "white", colour ="white"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        plot.title=element_text(size=24, color="#333333", face="bold"),
        plot.subtitle=element_text(size=18, color="#333333", face="bold"),
        plot.caption=element_text(color="#333333", size =10),
        text=element_text(family="Source Sans Pro"),
        legend.title=element_blank(),
        legend.text = element_text(size=14, color="#333333"),
        legend.position = "bottom") + #2
  scale_fill_manual(values=c("#3371AC", "#DC2228"), labels = c( "xG Assisted","NPxG")) +
  scale_y_continuous(expand = c(0, 0), limits= c(0,max(chart$value) + 0.3)) + 
  coord_flip()+ 
  guides(fill = guide_legend(reverse = TRUE)) 

#Which players took the most shots

shots_player<-StatsBombData %>%
  group_by(player.name) %>%
  summarise(shots = sum(type.name=="Shot", na.rm = TRUE),
            goals = sum(shot.outcome.name=="Goal", na.rm = TRUE),
            xG=sum(shot.statsbomb_xg,na.rm="TRUE"))

#View(shots_player)



#Histogram of  XG of a player (ex:-Victor Oshimen) 

player_xG<- StatsBombData%>%
  filter(type.name=="Shot") %>%
  filter(shot.type.name!="Penalty" | is.na(shot.type.name)) %>%
  group_by(player.name, player.id, team.name) %>%
  summarise(xG = shot.statsbomb_xg)

View(player_xG)


Shotquality <- function(player_id, player_name) {
  
  player_xG_filtered <- player_xG %>% filter(player.id == player_id)
  
  if (nrow(player_xG_filtered) == 0) {
    stop("No data found for the specified player ID.")
  }
  
  ggplot(player_xG_filtered, aes(x = xG)) +
    geom_histogram(binwidth = 0.05, fill = "blue", color = "black", alpha = 0.4) +
    labs(x = "XG", y = "No Of Shots", title = paste("Shot Quality of", player_name),subtitle = "EURO 2024") +
    theme_dark()
}


Shotquality(316046,"Lamine Yamal Nasraoui Ebana")


# Defensive Heat  map of all teams Vs Competition Average 
library(grid)

heatmap<- StatsBombData %>% mutate(location.x = ifelse(location.x>120, 120, location.x),
                                   location.y = ifelse(location.y>80, 80, location.y),
                                   location.x = ifelse(location.x<0, 0, location.x),
                                   location.y = ifelse(location.y<0, 0, location.y)) 
heatmap$xbin <- cut(heatmap$location.x, breaks = seq(0, 120, 20),include.lowest=TRUE )
heatmap$ybin <- cut(heatmap$location.y, breaks = seq(0, 80, by = 20),include.lowest=TRUE) 

heatmap<- heatmap%>%
  filter(type.name=="Pressure" | duel.type.name=="Tackle" | 
           type.name=="Foul Committed" | type.name=="Interception" | 
           type.name=="Block" ) %>%
  group_by(team.name) %>%
  mutate(total_DA = n()) %>%
  group_by(team.name, xbin, ybin) %>%
  summarise(total_DA = max(total_DA),
            bin_DA = n(),
            bin_pct = bin_DA/total_DA,
            location.x = median(location.x),
            location.y = median(location.y)) %>%
  group_by(xbin, ybin) %>%
  mutate(league_ave = mean(bin_pct)) %>%
  group_by(team.name, xbin, ybin) %>%
  mutate(diff_vs_ave = bin_pct - league_ave)


defensiveactivitycolors<-c("#192780", "#2a5d9f", "#40a7d0", "#87cdcf", "#e7f8e6", "#FF5733", "#FF8C66", "#FFB199", "#FFD6CC", "#FFE6E6", "#E60000", "#CC0000",
                           "#990000", "#660000", "#330000", "#FF1A1A", "#FF4C4C", "#FF7E7E", "#FFB0B0", "#FFD2D2", "#FF6347", "#FF8247", "#FFA07A", "#FF7F50",
                           "#CD5C5C", "#DC143C", "#FF4500", "#FF6347", "#FF0000", "#8B0000", "#CD5555", "#A52A2A", "#8B4513", "#800000", "#B22222", "#DC143C",
                           "#FF6347", "#FF4500", "#FF0000", "#8B0000", "#800000", "#B22222", "#FF6347", "#FF4500")


ggplot(data= heatmap, aes(x = location.x, y = location.y, fill = diff_vs_ave, group =diff_vs_ave)) +
  geom_bin2d(binwidth = c(30, 30), position = "identity", alpha = 0.9) + 
  annotate("rect",xmin = 0, xmax = 120, ymin = 0, ymax = 80, fill = NA, colour = "black", size = 0.6) +
  annotate("rect",xmin = 0, xmax = 60, ymin = 0, ymax = 80, fill = NA, colour = "black", size = 0.6) +
  annotate("rect",xmin = 18, xmax = 0, ymin = 18, ymax = 62, fill = NA, colour = "white", size = 0.6) +
  annotate("rect",xmin = 102, xmax = 120, ymin = 18, ymax = 62, fill = NA, colour = "white", size = 0.6) +
  annotate("rect",xmin = 0, xmax = 6, ymin = 30, ymax = 50, fill = NA, colour = "white", size = 0.6) +
  annotate("rect",xmin = 120, xmax = 114, ymin = 30, ymax = 50, fill = NA, colour = "white", size = 0.6) +
  annotate("rect",xmin = 120, xmax = 120.5, ymin =36, ymax = 44, fill = NA, colour = "black", size = 0.6) +
  annotate("rect",xmin = 0, xmax = -0.5, ymin =36, ymax = 44, fill = NA, colour = "black", size = 0.6) +
  annotate("segment", x = 60, xend = 60, y = -0.5, yend = 80.5, colour = "white", size = 0.6)+
  annotate("segment", x = 0, xend = 0, y = 0, yend = 80, colour = "black", size = 0.6)+
  annotate("segment", x = 120, xend = 120, y = 0, yend = 80, colour = "black", size = 0.6)+
  theme(rect = element_blank(),
        line = element_blank()) +
  annotate("point", x = 12 , y = 40, colour = "white", size = 1.05) +
  annotate("point", x = 108 , y = 40, colour = "white", size = 1.05) +
  annotate("path", colour = "white", size = 0.6,
           x=60+10*cos(seq(0,2*pi,length.out=2000)),
           y=40+10*sin(seq(0,2*pi,length.out=2000)))+
  annotate("point", x = 60 , y = 40, colour = "white", size = 1.05) +
  annotate("path", x=12+10*cos(seq(-0.3*pi,0.3*pi,length.out=30)), size = 0.6,
           y=40+10*sin(seq(-0.3*pi,0.3*pi,length.out=30)), col="white") +
  annotate("path", x=108-10*cos(seq(-0.3*pi,0.3*pi,length.out=30)), size = 0.6,
           y=40-10*sin(seq(-0.3*pi,0.3*pi,length.out=30)), col="white") +
  
  theme(axis.text.x=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.caption=element_text(size=13,family="Source Sans Pro", hjust=0.5, vjust=0.5),
        plot.subtitle = element_text(size = 18, family="Source Sans Pro", hjust = 0.5),
        axis.text.y=element_blank(),
        legend.title = element_blank(),
        legend.text=element_text(size=22,family="Source Sans Pro"),
        legend.key.size = unit(1.5, "cm"),
        plot.title = element_text(margin = margin(r = 10, b = 10), face="bold",size = 32.5, 
                                  family="Source Sans Pro", colour = "black", hjust = 0.5),
        legend.direction = "vertical",
        axis.ticks=element_blank(),
        plot.background = element_rect(fill = "white"),
        strip.text.x = element_text(size=8.49,family="Source Sans Pro")) +
  scale_y_reverse() + 
  
  scale_fill_gradientn(colours = defensiveactivitycolors, trans = "reverse", labels = 
                         scales::percent_format(accuracy = 1), limits = c(0.03, -0.03)) + 
  labs(title = "Where Do Teams Defend vs League Average?", subtitle = "EURO, 2024") + 
  coord_fixed(ratio = 95/100) +
  annotation_custom(grob = linesGrob(arrow=arrow(type="open", ends="last",
                                                 length=unit(2.55,"mm")), gp=gpar(col="black", fill=NA, lwd=2.2)),
                    xmin=25, xmax = 95, ymin = -83, ymax = -83) + #9
  facet_wrap(~team.name)+ 
  
  guides(fill = guide_legend(reverse = TRUE))



#Save Map of a Goalkeeper From Open play

Savemap<- function(goalkeeper_name) {
  shots_ontar <- StatsBombData %>%
    filter(player.name.GK == goalkeeper_name, type.name == "Shot") %>%
    select(shot.end_location.x, shot.end_location.y, shot.end_location.z,
           shot.statsbomb_xg, shot.outcome.name) %>%
    filter(shot.end_location.y >= 36 & shot.end_location.y <= 44 &
             shot.end_location.z >= 0 & shot.end_location.z <= 2.67) %>%
    select(shot.end_location.x, shot.end_location.y, shot.end_location.z,
           shot.statsbomb_xg, shot.outcome.name) %>%
    mutate(Outcome = case_when(shot.outcome.name == "Goal" ~ "Goal",
                               shot.outcome.name != "Goal" ~ "Saved"))
  
  ggplot(shots_ontar, aes(shot.end_location.y, shot.end_location.z,
                          size = shot.statsbomb_xg, 
                          colour = Outcome)) +
    geom_point() +
    labs(size = 'xG', x = NULL, y = NULL,
         title = paste('Save map of', goalkeeper_name),
         subtitle = 'EURO 2024') +
    scale_colour_manual(values = c('red', 'white')) +
    scale_x_continuous(limits = c(36, 44)) +
    scale_y_continuous(limits = c(0, 2.8)) +
    geom_segment(x = 36, y = 0, xend = 36, yend = 2.67,
                 lwd = 2, colour = 'black') +
    geom_segment(x = 44, y = 0, xend = 44, yend = 2.67,
                 lwd = 2, colour = 'black') +
    geom_segment(x = 35.98, y = 2.67, xend = 44.02, yend = 2.67,
                 lwd = 2, colour = 'black') +
    theme_void() + 
    theme(axis.text.x = element_blank(),
          panel.background = element_rect(fill = 'darkgrey'),
          legend.background = element_rect(fill = 'lightblue'),
          plot.title = element_text(hjust = 0.5, face = 'bold', size = 15),
          plot.subtitle = element_text(hjust = 0.5, face = 'bold'))
}


Savemap("Giorgi Mamardashvili")


#ShotMap Of a team (ex:- Côte d'Ivoire)


team_shotmap<-function(team_name,url){
  
  team<- StatsBombData %>% filter(team.name==team_name, type.name=="Shot") %>%
    select(period,minute,type.name,pass.length,pass.angle,player.name,
           location.x,location.y,shot.statsbomb_xg,shot.technique.name,shot.body_part.name,
           shot.type.name,shot.outcome.name,shot.end_location.x,shot.end_location.y,shot.end_location.z) %>%
    mutate(
      goal=case_when(
        shot.outcome.name == "Goal"~ "True",
        shot.outcome.name != "Goal"~ "False"
      )
    ) %>% filter(period != 5)
  
  total_xg <- sum(team$shot.statsbomb_xg)
  total_goals <- sum(team$goal == "True")
  
  p1<- create_Pitch(grass_colour = "gray15",background_colour = "gray15",line_colour = "white")+
    geom_point(team,mapping=aes(location.x,location.y,fill=goal,size=shot.statsbomb_xg),color="gray60",pch=21)+
    scale_size_continuous(limits = c(0,1),breaks=c(0.2,0.4,0.6,0.8,1),labels=c("0.2","0.4","0.6","0.8","1"))+
    scale_fill_manual(breaks=c("True","False"),values=c("red","white"),labels=c("Goal","No Goal"))+
    scale_x_continuous(limits = c(0,120))+
    scale_y_continuous(limits=c(0.80))+
    theme(
      plot.background = element_rect(color="gray15",fill="gray15"),
      plot.title=element_text(color='white',hjust=.5,size=8,family="Comic sans Ms",face="bold",vjust=-2),
      plot.subtitle=element_text(color='lightgrey',hjust=.5,size=7,family="Comic sans Ms",face="bold",vjust=-2),
      plot.caption = element_text(color='white',hjust=.5,size=10,family="Comic sans Ms",face="bold",vjust=-2),
      legend.position = c(.5,.2),
      legend.key = element_rect(fill="transparent",colour = "transparent"),
      legend.background  = element_rect(fill="transparent",colour = "transparent"),
      legend.title = element_text(hjust=.4,vjust=.5,size=10,family="Comic sans Ms",face="bold",colour="white"),
      legend.text = element_text(hjust=.4,vjust=.5,size=8,family="Comic sans Ms",face="bold",colour="white"),
      legend.direction = "horizontal",
      legend.box = "vertical",
      legend.box.just = "center",
      legend.margin = margin(t=.1,b=-.3,r=.1,l=.1,unit="cm")
    ) +
    
    labs(title=paste(team_name ,"Shot Map"),
         subtitle = "Euro 2024 ",
         fill="Outcome",
         size="xG")+
    coord_flip(xlim=c(70,120),ylim=c(0,80))+
    guides(fill=guide_legend(order=1))+
    annotate(geom="text", x = 120, y = 77, label = paste("Total xG:", round(total_xg,2)), color = "green3", size = 3) +
    annotate(geom="text", x = 116, y = 77, label = paste("Goals Scored:", total_goals), color = "green3", size = 3)
  
  p1+draw_image(url,x=110,y=1,width=7.5,height=7.5)
  
  
}

team_shotmap("Spain","C:\\Users\\user\\OneDrive\\Desktop\\Spain_flag_300.png")


#key passes plot

library(gridExtra)
passes_to_shot <- StatsBombData %>%
  filter(!is.na(pass.assisted_shot_id) & pass.assisted_shot_id != "") 

top_key_passers <- passes_to_shot %>%
  group_by(player.name,team.name) %>%
  summarise(total_key_passes = n()) %>%
  arrange(desc(total_key_passes))



keypassmap<- function(player_id,player_name){
  
  passes_to_shot <- StatsBombData %>%
    filter(player.id==player_id,!is.na(pass.assisted_shot_id) & pass.assisted_shot_id != "") 
  
  if(nrow(passes_to_shot)==0){
    stop("Please Check the Player Id again.!!!")
  }  
  
  create_Pitch(grass_colour = "#FFFF96", line_colour = "black", background_colour = "#FFFF96", goal_colour = "#000000") +
    geom_segment(data = passes_to_shot, aes(x = location.x, y = location.y,xend = pass.end_location.x, yend = pass.end_location.y),
                 lineend = "round", size = 0.5, colour = "Steelblue", arrow = arrow(length = unit(0.07, "inches"), ends = "last", type = "open")) + 
    labs(title = paste(player_name, "KeyPass"),subtitle ="EURO 2024" ) + 
    scale_y_reverse() + 
    coord_fixed(ratio = 105/100)
} 


keypassmap(316046,"Lamine Yamal Nasraoui Ebana")

#Heatmap Of a player(ex:- Tshibala)

Heatmap <- function(player_name) {
  
  heatmap <- StatsBombData %>%
    mutate(location.x = ifelse(location.x > 120, 120, location.x),
           location.y = ifelse(location.y > 80, 80, location.y),
           location.x = ifelse(location.x < 0, 0, location.x),
           location.y = ifelse(location.y < 0, 0, location.y)) 
  
  heatmap <- heatmap %>%
    filter(type.name == "Pressure" | duel.type.name == "Tackle" | 
             type.name == "Foul Committed" | type.name == "Interception" | 
             type.name == "Block" | type.name == "Shot"|
             type.name == "pass") %>%
    group_by(player.name) %>%
    filter(player.name == player_name)
  
  if (nrow(heatmap) == 0) {
    return("Please Check the Player Name Once again!!!!")
  }
  
  ggplot(heatmap,aes(x = location.x, y = location.y)) +
    geom_density_2d_filled(alpha = 0.9) +
    annotate("rect", xmin = 0, xmax = 120, ymin = 0, ymax = 80, fill = NA, colour = "black", size = 0.6) +
    annotate("rect", xmin = 0, xmax = 60, ymin = 0, ymax = 80, fill = NA, colour = "black", size = 0.6) +
    annotate("rect", xmin = 18, xmax = 0, ymin = 18, ymax = 62, fill = NA, colour = "white", size = 0.6) +
    annotate("rect", xmin = 102, xmax = 120, ymin = 18, ymax = 62, fill = NA, colour = "white", size = 0.6) +
    annotate("rect", xmin = 0, xmax = 6, ymin = 30, ymax = 50, fill = NA, colour = "white", size = 0.6) +
    annotate("rect", xmin = 120, xmax = 114, ymin = 30, ymax = 50, fill = NA, colour = "white", size = 0.6) +
    annotate("rect", xmin = 120, xmax = 120.5, ymin = 36, ymax = 44, fill = NA, colour = "black", size = 0.6) +
    annotate("rect", xmin = 0, xmax = -0.5, ymin = 36, ymax = 44, fill = NA, colour = "black", size = 0.6) +
    annotate("segment", x = 60, xend = 60, y = -0.5, yend = 80.5, colour = "white", size = 0.6) +
    annotate("segment", x = 0, xend = 0, y = 0, yend = 80, colour = "black", size = 0.6) +
    annotate("segment", x = 120, xend = 120, y = 0, yend = 80, colour = "black", size = 0.6) +
    annotate("point", x = 12, y = 40, colour = "white", size = 1.05) +
    annotate("point", x = 108, y = 40, colour = "white", size = 1.05) +
    annotate("path", colour = "white", size = 0.6,
             x = 60 + 10 * cos(seq(0, 2 * pi, length.out = 2000)),
             y = 40 + 10 * sin(seq(0, 2 * pi, length.out = 2000))) +
    annotate("point", x = 60, y = 40, colour = "white", size = 1.05) +
    annotate("path", x = 12 + 10 * cos(seq(-0.3 * pi, 0.3 * pi, length.out = 30)), size = 0.6,
             y = 40 + 10 * sin(seq(-0.3 * pi, 0.3 * pi, length.out = 30)), col = "white") +
    annotate("path", x = 108 - 10 * cos(seq(-0.3 * pi, 0.3 * pi, length.out = 30)), size = 0.6,
             y = 40 - 10 * sin(seq(-0.3 * pi, 0.3 * pi, length.out = 30)), col = "white") +
    theme(axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          plot.caption = element_text(size = 13, hjust = 0.5, vjust = 0.5),
          plot.subtitle = element_text(size = 18, hjust = 0.5),
          axis.text.y = element_blank(),
          legend.title = element_blank(),
          legend.text = element_text(size = 22),
          legend.key.size = unit(1.5, "cm"),
          plot.title = element_text(margin = margin(r = 10, b = 10), face = "bold", size = 32.5, 
                                    colour = "black", hjust = 0.5),
          legend.direction = "vertical",
          axis.ticks = element_blank(),
          plot.background = element_rect(fill = "white"),
          strip.text.x = element_text(size = 8.49)) +
    scale_y_reverse() + 
    labs(title = paste("Heatmap Of"), player_name)
         ,subtitle = "EURO, 2024") + 
    coord_fixed(ratio = 4/5) +
    annotation_custom(grob = linesGrob(arrow = arrow(type = "open", ends = "last",
                                                     length = unit(2.55, "mm")), gp = gpar(col = "black", fill = NA, lwd = 2.2)),
                      xmin = 25, xmax = 95, ymin = -83, ymax = -83)
}


Heatmap("Lamine Yamal Nasraoui Ebana")

#Classification of shots

shots <- StatsBombData %>%
  filter(type.name=="Shot" & (shot.type.name!="Penalty" | is.na(shot.type.name)) & player.name=="Lamine Yamal Nasraoui Ebana")



right_foot_goals <- subset(shots, shot.body_part.name == "Right Foot")


num_goals_right_foot <- sum(right_foot_goals$shot.outcome.name == "Goal")
total_xg_right_foot <- sum(right_foot_goals$shot.statsbomb_xg, na.rm = TRUE)
num_right_foot_shots <- nrow(right_foot_goals)


left_foot_goals <- subset(shots, shot.body_part.name == "Left Foot")


num_goals_left_foot <- sum(left_foot_goals$shot.outcome.name == "Goal")
total_xg_left_foot <- sum(left_foot_goals$shot.statsbomb_xg, na.rm = TRUE)
num_left_foot_shots <- nrow(left_foot_goals)


head_goals <- subset(shots, shot.body_part.name == "Head")

num_goals_head <- sum(head_goals$shot.outcome.name == "Goal")
total_xg_head <- sum(head_goals$shot.statsbomb_xg, na.rm = TRUE)
num_head_shots <- nrow(head_goals)


shot_summary <- matrix(NA, nrow = 3, ncol = 3,
                       dimnames = list(c("No. of Shots", "Goals", "Total xG"),
                                       c("Right Foot", "Left Foot", "Head")))

# Fill
shot_summary[, "Right Foot"] <- c(num_right_foot_shots, num_goals_right_foot, total_xg_right_foot)
shot_summary[, "Left Foot"] <- c(num_left_foot_shots, num_goals_left_foot, total_xg_left_foot)
shot_summary[, "Head"] <- c(num_head_shots, num_goals_head, total_xg_head)


View(shot_summary)




















team<- StatsBombData %>% filter(player.name=="Cristiano Ronaldo dos Santos Aveiro", type.name=="Shot") %>%
  select(period,minute,type.name,pass.length,pass.angle,player.name,
         location.x,location.y,shot.statsbomb_xg,shot.technique.name,shot.body_part.name,
         shot.type.name,shot.outcome.name,shot.end_location.x,shot.end_location.y,shot.end_location.z) %>%
  mutate(
    goal=case_when(
      shot.outcome.name == "Goal"~ "True",
      shot.outcome.name != "Goal"~ "False"
    )
  ) %>% filter(period != 5)

total_xg <- sum(team$shot.statsbomb_xg)
total_goals <- sum(team$goal == "True")

p1<-create_Pitch(grass_colour = "gray15",background_colour = "gray15",line_colour = "white")+
  geom_point(team,mapping=aes(location.x,location.y,fill=goal,size=shot.statsbomb_xg),color="gray60",pch=21)+
  scale_size_continuous(limits = c(0,1),breaks=c(0.2,0.4,0.6,0.8,1),labels=c("0.2","0.4","0.6","0.8","1"))+
  scale_fill_manual(breaks=c("True","False"),values=c("red","white"),labels=c("Goal","No Goal"))+
  scale_x_continuous(limits = c(0,120))+
  scale_y_continuous(limits=c(0.80))+
  theme(
    plot.background = element_rect(color="gray15",fill="gray15"),
    plot.title=element_text(color='white',hjust=.5,size=8,family="Comic sans Ms",face="bold",vjust=-2),
    plot.subtitle=element_text(color='lightgrey',hjust=.5,size=7,family="Comic sans Ms",face="bold",vjust=-2),
    plot.caption = element_text(color='white',hjust=.5,size=10,family="Comic sans Ms",face="bold",vjust=-2),
    legend.position = c(.5,.2),
    legend.key = element_rect(fill="transparent",colour = "transparent"),
    legend.background  = element_rect(fill="transparent",colour = "transparent"),
    legend.title = element_text(hjust=.4,vjust=.5,size=10,family="Comic sans Ms",face="bold",colour="white"),
    legend.text = element_text(hjust=.4,vjust=.5,size=8,family="Comic sans Ms",face="bold",colour="white"),
    legend.direction = "horizontal",
    legend.box = "vertical",
    legend.box.just = "center",
    legend.margin = margin(t=.1,b=-.3,r=.1,l=.1,unit="cm")
  ) +
  
  labs(title=paste("Cristiano Ronaldo Shot Map"),
       subtitle = "EURO 2024 ",
       fill="Outcome",
       size="xG")+
  coord_flip(xlim=c(70,120),ylim=c(0,80))+
  guides(fill=guide_legend(order=1))+
  annotate(geom="text", x = 120, y = 77, label = paste("Total xG:", round(total_xg,2)), color = "green3", size = 3) +
  annotate(geom="text", x = 116, y = 77, label = paste("Goals Scored:", total_goals), color = "green3", size = 3)

p1+draw_image("https://banner2.cleanpng.com/lnd/20240518/kjf/awbu3ssvh.webp",x=110,y=1,width=13.5,height=13.5)
