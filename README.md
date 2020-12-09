# Visualization of United States Presidential Historic Elections

#### Inspired by Dr. Follis
#### Adapted from Dr. Follis
* Original DataSource: R-package socviz[Link](https://cran.r-project.org/web/packages/socviz/socviz.pdf) 

## Description

The visualizing of United States presidential historic elections
- by popular vote and election year
- with highlights of important historic periods 
- Popular & Electoral Margins (1800-2020)

## Prerequisites

```{r}
library(tidyverse)
library(socviz)
library(ggrepel)
library(viridis)
```
### Data Visualisation* - [R-code](https://github.com/borovkk/VisualizationOfTheUSHistoricElections/blob/master/r_code_us_election_viz.R)

![Visualization](https://raw.githubusercontent.com/borovkk/VisualizationOfTheUSHistoricElections/master/Picture1.png)

##### Preparing data grouping 

```{r}
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
```

##### Preparing visualization

```{r}
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
  
```
  
## Author:

* **Kristina Borovkova** - [*Full Project*](https://github.com/borovkk/VisualizationOfTheUSHistoricElections/blob/master/US_Elections_Viz_by_Borovkova.pdf)

