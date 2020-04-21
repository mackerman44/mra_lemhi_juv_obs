
library(sf)
library(tidyverse)

setwd("C:/Users/lottb/Desktop/Test")

abundance <- read.csv("Lemhi_abundances_2019.csv",
                      na.strings=" ",
                      header=TRUE)
sampled <- st_read("Lemhi2019.shp") %>%
  left_join(abundance, by = c("ChannelTY", "Stream"))

st_write(sampled, "Lemhi2019_density.gpkg")
