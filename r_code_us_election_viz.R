#https://socviz.co/workgeoms.html

library(tidyverse)
library(socviz)
library(ggrepel)
library(viridis)



rel_by_region <- gss_sm %>%
  drop_na(religion) %>%
  group_by(bigregion,religion) %>%
  summarize(freq=n()) %>%
  mutate("pct"=round(100*freq/sum(freq),1))

ggplot(rel_by_region,aes(bigregion,pct,fill=religion))+
  geom_bar(stat="identity",position="dodge")

ggplot(rel_by_region,aes(religion,pct,fill=religion))+
  geom_bar(stat="identity")+
  facet_wrap(.~bigregion,nrow=2)

ggplot(rel_by_region,aes(religion,pct,fill=religion))+
  geom_bar(stat="identity")+
  geom_text(aes(label=religion),vjust=-1)+
  ylim(0,70)+
  #coord_flip()+
  labs(title="Religion by Region in the US")+
  facet_wrap(.~bigregion,nrow=2)+
  theme(legend.position = "none",
        plot.title=element_text(hjust=.5,size=18))

ggplot(elections_historic,aes(popular_pct,ec_pct,label=winner_label))+
  geom_point()+
  geom_label_repel(aes(label=winner_label,col=win_party))

p_title <- "Presidential Elections: Popular & Electoral College Margins"
p_subtitle <- "1824-2016"
p_caption <- "Data for 2016 are provisional"
x_label <- "Winner's share of Popular Vote"
y_label <- "Winner's share of Electoral College Votes"

p <- ggplot(elections_historic,aes(popular_pct,ec_pct,label=winner_label))
p + geom_hline(yintercept=0.5,size=1.4,color="gray80") +
  geom_vline(xintercept=0.5,size=1.4,color="gray80")+
  geom_point()+
  geom_text_repel(aes(col=two_term))+
  scale_x_continuous(labels=scales::percent)+
  scale_y_continuous(labels=scales::percent)+
  labs(x=x_label,
       y=y_label,
       title=p_title,
       subtitle=p_subtitle,
       caption=p_caption)+
  facet_grid(win_party~.)+
  theme_bw()

elections_historic$year2 <- cut(elections_historic$year,breaks=c(1800,1850,1900,1950,1999,2050))
levels(elections_historic$year2) <- c("1800-1850","1851-1900","1900-1950","1951-1999","2000-2020")

ggplot(elections_historic,aes(popular_pct,ec_pct,label=winner_label))+
  geom_point(aes(col=win_party))+
  geom_label_repel()+
  facet_grid(year2~.)

ggplot(elections_historic,aes(popular_pct,ec_pct,label=winner_label))+
  #geom_point(aes(col=win_party))+
  geom_point()+
  geom_vline(xintercept=.5)+
  geom_hline(yintercept=.5)+
  geom_label_repel(aes(fill=win_party))+
  #scale_color_manual(values=c("yellow","blue","black","red"))+
  facet_grid(.~year2)

#https://www.thinkingondata.com/something-about-viridis-library/

ggplot(elections_historic,aes(popular_pct,year,label=winner_label))+
  #geom_point(aes(col=win_party))+
  geom_point(size=2)+
  geom_vline(xintercept=.5)+
  #geom_hline(yintercept=.5)+
  geom_label_repel(aes(fill=ec_pct))+
  scale_color_viridis()+
  #scale_color_manual(values=c("yellow","blue","black","red"))+
  facet_grid(.~year2,scales="free_y")

ggplot(elections_historic,aes(popular_pct,year,label=winner_label))+
  #geom_point(aes(col=win_party))+
  geom_point(size=2)+
  geom_vline(xintercept=.5,linetype=2)+
  #geom_hline(yintercept=1929)+
  geom_label_repel(aes(fill=ec_pct))+
  scale_fill_viridis(option="cividis")+
  theme_bw()+
  facet_wrap(.~year2,scales="free_y",nrow=1)

data.crash=data.frame(popular_pct=c(.3,.3),
                      year=c(1929,1943),
                      year2=c("1900-1950","1900-1950"),
                      winner_label=c(NA,NA),
                      event=c("Stock Market Crash 1929",
                              "World War II"))

##################################
############Visual###############
#################################

data.crash=data.frame(popular_pct=c(.3,.3),
                      year=c(1929,1941.5,1914.5,1863,1871,1935,2008,1972.5),
                      year2=c("1900-1950","1900-1950","1900-1950","1851-1900","1851-1900","1900-1950","2000-2020","1951-1999"),
                      winner_label=c(NA,NA,NA,NA,NA,NA,NA,NA),
                      event=c("Stock Market Crash",
                              "World\nWar II",
                              "World\nWar I",
                              "Civil War",
                              "Reconstruction",
                              "Great\nDepression",
                              "Recession",
                              "Watergate"))

ggplot(elections_historic,aes(popular_pct,year,label=winner_label))+
  geom_point(size=2)+
  geom_vline(xintercept=.5,linetype=1)+
  #geom_point(data=data.crash,aes(popular_pct,year),col="red",size=2)+
  geom_rect(xmin=-Inf,xmax=Inf,ymin=1941,ymax=1945,fill="gray5",alpha=.05)+
  geom_rect(xmin=-Inf,xmax=Inf,ymin=1914,ymax=1918,fill="gray5",alpha=.05)+
  geom_rect(xmin=-Inf,xmax=Inf,ymin=1861,ymax=1865,fill="gray5",alpha=.05)+
  geom_rect(xmin=-Inf,xmax=Inf,ymin=1865,ymax=1877,fill="gray45",alpha=.05)+
  geom_rect(xmin=-Inf,xmax=Inf,ymin=1929,ymax=1941,fill="gray45",alpha=.05)+
  geom_rect(xmin=-Inf,xmax=Inf,ymin=2008,ymax=2009,fill="gray5",alpha=.05)+
  geom_rect(xmin=-Inf,xmax=Inf,ymin=1972,ymax=1974,fill="gray5",alpha=.05)+
  geom_label(data=data.crash,aes(popular_pct,year,label=event),
             hjust=-.05,vjust=.05,col="black",size=3)+
  geom_label_repel(aes(fill=ec_pct))+
  scale_fill_viridis_c()+
  scale_x_continuous(labels=scales::percent)+ #setting the scale by %
  theme_bw()+ #setting the theme
  labs(title="Presidential Elections", #text for title
       subtitle = "Popular & Electoral Margins (1800-2020)", #text for subtitle
       caption = "Made by Kristina Borovkova\n Date: February 23, 2020", #text for caption
       x="Popular Vote %", #title text for x
       y="Election Year", #title text for y
       fill="% Electoral Votes")+ #text for legend
  theme(plot.title = element_text(hjust=.5,size=16), #title
        plot.subtitle = element_text(hjust=.5,size=12),#subtitle
        axis.text.y=element_text(face="bold"),#y-axis set-up
        axis.text.x = element_text(angle=45, hjust=1, face="bold",size=8),#x-axis set-up
        strip.background=element_rect(fill="dark blue"), # label box fill
        strip.text.x=element_text(color="white",face="bold",size=10),#text type on the top boxes
        legend.position = "top", #position of the legend
        legend.background=element_rect(colour = 'black', 
                                       fill = 'light grey', size = 0.5, linetype='dashed'), # setting up the legend
        plot.background = element_rect(fill="ivory2"))+ #background fill
  facet_wrap(.~year2, scales="free_y",nrow=1) 


##################
