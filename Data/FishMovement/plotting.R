
library(tidyverse)
library(lubridate)
library(scales)


#Histogram for Chinook movement between mark/recapture
df <- read.csv("D:/Analysis/FishMovement/Movement2013.csv")

ggplot(df,
       aes(x = Distance))+
  geom_histogram(aes(y = ..density..),
                 position = 'dodge',
                 binwidth = 40) +
  geom_density(alpha = 0.2,
               color = "darkred")+
  theme_bw() +
  facet_wrap(~ Species)+
  labs(x="Distance Traveled",y="Density")+
  scale_x_continuous(limits=c(0,600),
                     breaks = c(seq(0,600,
                                    by=40)),
                     labels=c(seq(00,560,
                                  by=40),
                              "600+"))+
  theme(axis.text.x=element_text(angle=45, hjust = 1))

  

