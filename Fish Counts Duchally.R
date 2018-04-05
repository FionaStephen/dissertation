rm(list=ls())
setwd("M:/4th year/Dissertation")

library(dplyr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(readr)
library(gridExtra)
library(ggpubr)
library(colourpicker)

library(agridat)

Fish_Duchally <-read.csv ("DUCHALLY FINAL 3.csv", header = TRUE, skip = 0, sep = ",")

Fish_Duchally$Year <- as.numeric(Fish_Duchally$Year)


#general linear model

#GLM with non fish days 

Duchally_model_1 <- glm(Fish~Freshet+Year, family = poisson, data = Fish_Duchally)
summary(Duchally_model_1)
plot(Duchally_model_1)

Duchally_model_2 <- glm(Fish~Freshet, family = poisson, data = Fish_Duchally)
summary(Duchally_model_2)
plot(Duchally_model_2)


Clunie_AIC<- AIC(Duchally_model_1, Duchally_model_2)
summary(Duchally_AIC)



#plots?

plot(Fish_Duchally$Fish~Fish_Duchally$Freshet)
plot(Fish_Duchally$Fish~Fish_Duchally$Year)
plot(Fish_Duchally$Fish~Fish_Duchally$Temperature)



#box plot

theme.clean <- function(){
  theme_bw()+
    theme(axis.text.x = element_text(size = 12, angle = 45, vjust = 1, hjust = 1),
          axis.text.y = element_text(size = 12),
          axis.title.x = element_text(size = 14, face = "plain"),             
          axis.title.y = element_text(size = 14, face = "plain"),             
          panel.grid.major.x = element_blank(),                                          
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_blank(),  
          plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), units = , "cm"),
          plot.title = element_text(size = 20, vjust = 1, hjust = 0.5),
          legend.text = element_text(size = 12, face = "italic"),          
          legend.title = element_blank(),                              
          legend.position = c(0.9, 0.9))
}

Fish_Duchally$Freshet <- as.factor(Fish_Duchally$Freshet)

(fish.box.duchally <- ggplot(Fish_Duchally, aes(Freshet, Fish)) +
    geom_boxplot(fill = "#CD3333", alpha = 0.8, colour = "#8B2323") +
    theme.clean() +  
    theme(axis.text.x = element_text(size = 12, angle = 0)) +
    labs(x = "Freshet", y = "Fish Count"))





#without non fish days 

Fish_Duchally2 <- na.omit(Fish_Duchally)

Duchally_model_2.1 <- glm(Fish~Freshet+Year, family = poisson, data = Fish_Duchally2)
summary(Duchally_model_2.1)
plot(Duchally_model_2.1)


Duchally_model_2.2 <- glm(Fish~Freshet, family = poisson, data = Fish_Duchally2)
summary(Duchally_model_2.2)

Duchally_model_2.3<- glm(Fish~Freshet+Year+Temperature, family = poisson, data = Fish_Duchally2)
summary(Duchally_model_2.3)


Duchally_model_2.4<- glm(Fish~Freshet+Year+Temperature+Conductivity, family = poisson, data = Fish_Duchally2)
summary(Duchally_model_2.4)



Duchally_AIC2<- AIC(Duchally_model_2.1, Duchally_model_2.2, Duchally_model_2.3, Duchally_model_2.4)
summary(Clunie_AIC2)

#box plot

(fish.box.duchally2 <- ggplot(Fish_Duchally2, aes(Freshet, Fish)) +
    geom_boxplot(fill = "#CD3333", alpha = 0.8, colour = "#8B2323") +
    theme.clean() +  
    theme(axis.text.x = element_text(size = 12, angle = 0)) +
    labs(x = "Freshet", y = "Fish Count"))




#year / fish 


fish_years2 <- Fish_Duchally %>% group_by(Year) %>% mutate(total_fish = sum(Fish, na.rm = TRUE)) %>% distinct(Year, total_fish)
View(fish_years2)


(year_barplot.duchally <- ggplot(fish_years2, aes(x=Year, y=total_fish)) +
    geom_bar(position=position_dodge(), stat="identity", colour="black", fill="#00868B") +
    theme_bw() +
    ylab("Total Fish Count\n") +                             
    xlab("\nYear")  +
    scale_x_continuous(breaks = c(2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016),
                       labels = c("2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016")) +
    theme(axis.text.x=element_text(size=12),  # x axis labels angled, so that text doesn't overlap
          axis.text.y=element_text(size=12),
          axis.title.x=element_text(size=14, face="plain"),             
          axis.title.y=element_text(size=14, face="plain"),             
          panel.grid.major.x=element_blank(),                                          
          panel.grid.minor.x=element_blank(),
          panel.grid.minor.y=element_blank(),
          panel.grid.major.y=element_blank(),  
          plot.margin = unit(c(1,1,1,1), units = , "cm")))




