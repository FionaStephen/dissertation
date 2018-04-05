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


#formatting for converting to csv
clunie <- read.csv("CLUNIE_NEW.csv") 

str(clunie)

#Make a new dataframe with the number of fish moving upstream on each day
cluniesum <- data.frame(tapply(clunie$Direction, clunie$Date, length)) 

#Change the column name to something more manageable!
colnames(cluniesum) <- c("fish")

#You can add new columns for mean temperature and conductivity per day
cluniesum$conduct <- tapply(clunie$Conductivity, clunie$Date, mean)
cluniesum$temp <- tapply(clunie$Temp, clunie$Date, mean)

#And then read the file out
write.csv(cluniesum, file="Cluniefish.csv")

Clunie_Flow<- read.csv ("Clunie_Flow.csv", header = TRUE, skip = 0, sep = ",")
clunieflow<- select(Clunie_Flow, Date, Flow)
clunieflow <- clunieflow %>% group_by(Date) %>%
  summarise(mean=mean(Flow))

write.csv(clunieflow, file = "clunieflow1.csv")
                       




duchally<-read.csv("DUCHALLY_NEW.csv")

str(duchally)

#Make a new dataframe with the number of fish moving upstream on each day
duchallysum <- data.frame(tapply(duchally$Direction, duchally$Date, length)) 

#Change the column name to something more manageable!
colnames(duchallysum) <- c("fish")

#You can add new columns for mean temperature and conductivity per day
duchallysum$conduct <- tapply(duchally$Conductivity, duchally$Date, mean)
duchallysum$temp <- tapply(duchally$Temp, duchally$Date, mean)

#And then read the file out
write.csv(duchallysum, file="duchallysum1.csv")


Duchally_Flow<- read.csv ("Duchally_Flow.csv", header = TRUE, skip = 0, sep = ",")
duchallyflow<- select(Duchally_Flow, Date, Flow)
duchallyflow <- duchallyflow %>% group_by(Date) %>%
  summarise(mean=mean(Flow))

write.csv(duchallyflow, file = "duchallyflow1.csv")





#FORMATTING DATA 

Clunie<- read.csv ("CLUNIE.csv", header = TRUE, skip = 0, sep = ",")


#changing date format into numerical month day
Clunie$Date <- as.Date(paste(Clunie$Date, format = "%m-%d"), format = "%d-%b")
Clunie$Date <- format(Clunie$Date, format = "%m-%d")


#sorting into count per day
summary_Clunie<- Clunie %>% group_by(Year, Date) %>%
summarise(Count=length(Date),
          Temperature=mean(Temp),
          Conductivity=mean(Conductivity))


Duchally<- read.csv ("DUCHALLY.csv", header = TRUE, skip = 0, sep = ",")

#changing date format inot numerical month day
Duchally$Date <- as.Date(paste(Duchally$Date, format = "%m-%d"), format = "%d-%b")
Duchally$Date <- format(Duchally$Date, format = "%m-%d")

#sorting into count per day 
summary_Duchally<- Duchally %>% group_by(Year, Date) %>%
summarise(Count=length(Date),
          Temperature=mean(Temp),
          Conductivity=mean(Conductivity))


Clunie_Flow<- read.csv ("Clunie_Flow.csv", header = TRUE, skip = 0, sep = ",")

#changing date format into numerical month day
Clunie_Flow$Date <- as.Date(paste(Clunie_Flow$Date, format = "%m-%d"), format = "%d-%b")
Clunie_Flow$Date <- format(Clunie_Flow$Date, format = "%m-%d")

#mean flow per day
summary_Clunie_Flow<- Clunie_Flow %>% group_by(Year, Date) %>%
  summarise(Flow=mean(Flow))

Duchally_Flow<-read.csv ("Duchally_Flow.csv", header = TRUE, skip = 0, sep = ",")

#changing date format into numerical month day
Duchally_Flow$Date <- as.Date(paste(Duchally_Flow$Date, format = "%m-%d"), format = "%d-%b")
Duchally_Flow$Date <- format(Duchally_Flow$Date, format = "%m-%d")

#mean flow per day
summary_Duchally_Flow<- Duchally_Flow %>% group_by(Year, Date) %>%
  summarise(Flow=mean(Flow))

Fish_Clunie2$Date <- as.Date(paste(Fish_Clunie2$Date, format = "%m-%d"), format = "%d-%b")









#General Linear Model Analysis ----

Fish_Clunie <-read.csv ("Clunie Final 3.csv", header = TRUE, skip = 0, sep = ",")

Fish_Clunie$Year <- as.numeric(Fish_Clunie$Year)

# Making a histogram to asses data distribution
(Fish_Clunie.hist <- ggplot(Fish_Clunie, aes(Fish)) + geom_histogram() + theme.clean())

hist(Fish_Clunie2$log(Fish))





#GLM with non fish days 

Clunie_model_1 <- glm(Fish~Freshet+Year, family = poisson, data = Fish_Clunie)
summary(Clunie_model_1)
plot(Clunie_model_1)

Clunie_model_2 <- glm(Fish~Freshet, family = poisson, data = Fish_Clunie)
summary(Clunie_model_2)
plot(Clunie_model_2)


Clunie_AIC<- AIC(Clunie_model_1, Clunie_model_2)
summary(Clunie_AIC)


#plots?

plot(Fish_Clunie$Fish~Fish_Clunie$Freshet)
plot(Fish_Clunie$Fish~Fish_Clunie$Year)
plot(Fish_Clunie$Fish~Fish_Clunie$Temperature)



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
Fish_Clunie$Freshet <- as.factor(Fish_Clunie$Freshet)

(fish.box <- ggplot(Fish_Clunie, aes(Freshet, Fish)) +
    geom_boxplot(fill = "#CD3333", alpha = 0.8, colour = "#8B2323") +
    theme.clean() +  
    theme(axis.text.x = element_text(size = 12, angle = 0)) +
    labs(x = "Freshet", y = "Fish Count"))









#without non fish days 

Fish_Clunie2 <- na.omit(Fish_Clunie)

Clunie_model_12 <- glm(Fish~Freshet+Year, family = poisson, data = Fish_Clunie2)
summary(Clunie_model_1)
plot(Clunie_model_1)


Clunie_model_22 <- glm(Fish~Freshet, family = poisson, data = Fish_Clunie2)
summary(Clunie_model_2)

Clunie_model_32<- glm(Fish~Freshet+Year+Temperature, family = poisson, data = Fish_Clunie2)
summary(Clunie_model_32)


Clunie_model_33<- glm(Fish~Freshet+Year+Temperature+Conductivity, family = poisson, data = Fish_Clunie2)
summary(Clunie_model_33)



Clunie_AIC2<- AIC(Clunie_model_12, Clunie_model_22, Clunie_model_32, Clunie_model_33)
summary(Clunie_AIC2)

#box plot

(fish.box2 <- ggplot(Fish_Clunie2, aes(Freshet, Fish)) +
    geom_boxplot(fill = "#CD3333", alpha = 0.8, colour = "#8B2323") +
    theme.clean() +  
    theme(axis.text.x = element_text(size = 12, angle = 0)) +
    labs(x = "Freshet", y = "Fish Count"))

#plots

plot(Fish_Clunie2$Fish~Fish_Clunie2$Freshet)
plot(Fish_Clunie2$Fish~Fish_Clunie2$Year)
plot(Fish_Clunie2$Fish~Fish_Clunie2$Temperature)
plot(Fish_Clunie2$Change~Fish_Clunie2$Freshet)
plot(Fish_Clunie2$Change~Fish_Clunie2$ChangeF)







#year / fish 


fish_years <- Fish_Clunie2 %>% group_by(Year) %>% mutate(total_fish = sum(Fish)) %>% distinct(Year, total_fish)
View(fish_years)


(year_barplot <- ggplot(fish_years, aes(x=Year, y=total_fish)) +
      geom_bar(position=position_dodge(), stat="identity", colour="black", fill="#00868B") +
      theme_bw() +
      ylab("Total Fish Count") +                             
      xlab("Year")  +
      theme(axis.text.x=element_text(size=12, angle=45, vjust=1, hjust=1),  # x axis labels angled, so that text doesn't overlap
            axis.text.y=element_text(size=12),
            axis.title.x=element_text(size=14, face="plain"),             
            axis.title.y=element_text(size=14, face="plain"),             
            panel.grid.major.x=element_blank(),                                          
            panel.grid.minor.x=element_blank(),
            panel.grid.minor.y=element_blank(),
            panel.grid.major.y=element_blank(),  
            plot.margin = unit(c(1,1,1,1), units = , "cm")))



fish_2009 <- filter(Fish_Clunie2, Year == "2009")
View(fish_2009)

str(fish_years$total_fish)
summary(year_barplot)



#fish / temp / month

#2009
fish_months_09 <- Fish_Clunie %>% filter(Year == "2009") %>% group_by(Month) %>% mutate(total_fish = sum(Fish), avg_temp = mean(Temperature, na.rm = TRUE)) %>% distinct(total_fish, avg_temp)
View(fish_months_09)





#you need this to get your data in for the graph below
fish_months_2009 <- read_csv("fish_months_09.csv")
fish_months_09$Month <- as.Date(fish_months_09$Month)

str(fish_months_2009)


#I AM THE ONE THE ONE THE ONE DONT NEED A GUN TO GET RESPECT
fish_months_09 %>% ggplot() + 
  geom_bar(mapping = aes(x = Month, y = total_fish), 
           stat = "identity", colour ="white", fill = "#468893") + 
  geom_point(mapping = aes(x = Month, y = avg_temp *100/20), 
             size = 3, shape = 21, colour="tomato", fill = "tomato") + 
  geom_line(mapping = aes(x = Month, y = avg_temp *100/20), 
            size = 1, colour="tomato") + 
  #scale_x_date(name = "\n2009", breaks = seq.Date(as.Date("2009-04-01", "%Y-%m-%d"), 
  #                                                as.Date("2009-10-01", "%Y-%m-%d"), by = "1 month"), 
  #             labels=c("April","May","June","July","August","September","October")) +
  scale_y_continuous(name = expression("Total Fish Count"),
    sec.axis = sec_axis(~ . * 20 / 100 , name = "Average Temperature ("~degree~"C)")) + 
  theme_bw() + 
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.border = element_blank()
  )

fish_months_09$Month <- factor(fish_months_09$Month, 
                             levels = c("April", "May",
                                        "June", "July", "August", "September", "October"),
                             labels=c("April", "May",
                                      "June", "July", "August", "September", "October"))

fish_months_09 %>% ggplot() + 
  geom_bar(mapping = aes(x = Month, y = total_fish), 
           stat = "identity", colour ="white", fill = "#468893") + 
  geom_point(mapping = aes(x = Month, y = avg_temp *100/20), 
             size = 3, shape = 21, colour="tomato", fill = "tomato") + 
  geom_line(mapping = aes(x = Month, y = avg_temp *100/20, group = 1), 
            size = 1, colour="tomato") + 
  #scale_x_date(name = "\n2009", breaks = seq.Date(as.Date("2009-04-01", "%Y-%m-%d"), 
  #                                                as.Date("2009-10-01", "%Y-%m-%d"), by = "1 month"), 
  #             labels=c("April","May","June","July","August","September","October")) +
  scale_y_continuous(name = expression("Total Fish Count"),
                     sec.axis = sec_axis(~ . * 20 / 100 , name = "Average Temperature ("~degree~"C)")) + 
  theme_bw() + 
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank()
  )


#2010
fish_months_10 <- Fish_Clunie2 %>% filter(Year == "2010") %>% group_by(Month) %>% mutate(total_fish = sum(Fish), avg_temp = mean(Temperature)) %>% distinct(total_fish, avg_temp)
View(fish_months_10)



(month_barplot <- ggplot(fish_months_09, aes(x=Month, y=total_fish, y=avg_temp)) +
    geom_bar(position=position_dodge(), stat="identity", colour="black", fill="#00868B") +
    geom_line()
  theme_bw() +
    ylab("Total Fish Count") +
    ylab("Mean Monthly Temperature") +
    xlab("2009")  +
    theme(axis.text.x=element_text(size=12, angle=45, vjust=1, hjust=1),  # x axis labels angled, so that text doesn't overlap
          axis.text.y=element_text(size=12),
          axis.title.x=element_text(size=14, face="plain"),             
          axis.title.y=element_text(size=14, face="plain"),             
          panel.grid.major.x=element_blank(),                                          
          panel.grid.minor.x=element_blank(),
          panel.grid.minor.y=element_blank(),
          panel.grid.major.y=element_blank(),  
          plot.margin = unit(c(1,1,1,1), units = , "cm")))


