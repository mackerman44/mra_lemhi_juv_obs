
library(FSA) #citation: Ogle, D.H. 2017. FSA: Fisheries Stock Analysis. R package version 0.8.12.
library(dplyr) #needs citation
library(tidyr) # I like this better than reshape2
library(forcats) # for dealing with factors

setwd("D:/Analysis/Abundance/2019")  #sets my working directory

# read in the fish capture csv and filter it a little
bio <- read.csv("RovingFish2019_Final.csv",
                na.strings=" ",
                header=TRUE) %>%
  filter(Species %in% c('Chinook',
                        'Steelhead',
                        'Brook Trout',
                        'Bull Trout'),
         ChannelTY != '') %>%
  tbl_df() %>%
  mutate(Species = fct_drop(Species)) %>%
  filter(!(FishStatus == 'Dead'& dceType == 'Mark'))

#--------------------------------------------
# which fish moved geomorphic reaches?
#--------------------------------------------
allFish = bio %>%
  filter(!is.na(PitTagID),
         !is.na(Tag2Recapt)) %>%
  tbl_df() %>%
  select(Species, StreamName, PitTagID, ChannelTY) %>%
  distinct() %>%
  mutate(seen = 1) %>%
  spread(ChannelTY, seen, fill = 0) %>%
  mutate(totRchs = select(., -(Species:PitTagID)) %>%
           rowSums())

movingFish = allFish %>%
  group_by(Species, StreamName) %>%
  filter(totRchs > 1) %>%
  ungroup()

# how many and what percent moved, by species and stream name?
allFish %>%
  group_by(Species, StreamName) %>%
  summarise(n_tot = n_distinct(PitTagID)) %>%
  left_join(movingFish %>%
              group_by(Species, StreamName) %>%
              summarise(n_move = n_distinct(PitTagID))) %>%
  mutate(n_move = ifelse(is.na(n_move), 0, n_move)) %>%
  mutate(perc_move = n_move / n_tot) %>%
  filter(n_move > 0)
# looks like hardly any moved, so we'll ignore movement, or toss out those fish

#--------------------------------------------
# summarise capture histories by species, stream name and geomorphic reach
#--------------------------------------------
capHist = bio %>%
  filter(dceType == 'Mark') %>%
  group_by(Species, StreamName, ChannelTY) %>%
  summarise(M = n()) %>%
  left_join(bio %>%
              filter(dceType == 'Recapture') %>%
              group_by(Species, StreamName, ChannelTY) %>%
              summarise(C = n())) %>%
  left_join(bio %>%
              # drop fish recaptured in a different reach than they were marked in
              filter(!PitTagID %in% movingFish$PitTagID) %>%
              filter(dceType == 'Recapture',
                     Tag2Recapt == 'Non-Efficiency Recapture') %>%
              group_by(Species, StreamName, ChannelTY) %>%
              summarise(R = n())) %>%
  ungroup() %>%
  mutate_at(vars(M:R),
            funs(ifelse(is.na(.), 0, .)))

# fit mark-recapture models
Nmods = capHist %>%
  # filter out places with no recaptures (no way to make estimate)
  filter(R > 0) %>%
  with(.,
       mrClosed(M,
                C,
                R,
                method = 'Chapman'))

# combine summarised data with model output
abund_df = capHist %>%
  # filter out places with no recaptures (no way to make estimate)
  filter(R > 0) %>%
  bind_cols(summary(Nmods,
                    incl.SE = T,
                    incl.all = F) %>%
              tbl_df()) %>%
  bind_cols(confint(Nmods,
                    incl.all = F) %>%
              tbl_df()) %>%
  mutate(probCap = M / N,
         probCap_SE = M * SE / (N^2)) %>%
  # add the reaches back in with no recaptures
  bind_rows(capHist %>%
              filter(R == 0)) %>%
  arrange(Species, StreamName, ChannelTY)


# save as .csv file
write.csv(abund_df,
          'AbundanceEst2019.csv',
          row.names = F)

