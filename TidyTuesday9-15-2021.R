library(tidyverse)
library(ggplot2)
library(here)
library(lubridate)
library(parallel)
library(patchwork)
library(extrafont)
library(skimr)
library(beepr)
library(ggbump)
library(tidytext)
library(stringr)
library(ggrepel)
library(extrafont)
library(rayshader)

here()
usethis::edit_r_environ()
set.seed(1)

options(scipen=999)
source("TidyTuesdayTheme.R")
rawData<- tidytuesdayR::tt_load(2021, week = 38)


modelData<-left_join(rawData$billboard,rawData$audio_features, by = "song_id")

skim(modelData)

modelData%>%
  group_by(song_id)%>%
  mutate(billboardPopularity = sum(week_position,na.rm = T)/n(),
         week = min(mdy(week_id)))%>%
  ungroup()%>%
  mutate(billboardPopularity = 100*(1- ecdf(billboardPopularity)(billboardPopularity)))%>%
  ggplot(aes(y = spotify_track_popularity/billboardPopularity+.0001, x = week))+
  geom_point(alpha=.1)+
  geom_smooth()+
  scale_y_log10()

modelData%>%
  group_by(song_id)%>%
  mutate(billboardPopularity = sum(week_position,na.rm = T)/n(),
         week = min(mdy(week_id)))%>%
  ungroup()%>%
  mutate(billboardPopularity = 100*(1- ecdf(billboardPopularity)(billboardPopularity)))%>%
  ggplot(aes(y = billboardPopularity, x = week, color = fct_lump(spotify_genre,9)))+
  geom_point(alpha=.1)+
  geom_smooth()

library(tidytext)
library(stringr)

topGenres<-unlist((modelData%>%
  unnest_tokens(word,spotify_genre, token ="words") %>%
  filter(!word %in% stop_words$word,
         !word %in% str_remove_all(stop_words$word, "'"),
         str_detect(word, "[a-z]"))%>%
  count(word, sort = TRUE))[1:20,1])
  

modelData[,topGenres]<-lapply(topGenres,function(genre){grepl(genre,modelData$spotify_genre,ignore.case =T)})%>%do.call(bind_cols,.)
  
modelData<-modelData%>%
  mutate(genre = ifelse(rock,"Rock",
                        ifelse(pop,"Pop",
                               ifelse(country,"Country",
                                      ifelse(rock,"Rock",
                                             ifelse(rap,"Rap",
                                                    ifelse(hip,"Rap",
                                                           ifelse(soul,"Soul",
                                                                  ifelse(hop,"Rap",
                                                                         ifelse(dance,"Dance",
                                                                                ifelse(classic,"Classical",
                                                                                       ifelse(contemporary,"Contemporary","Other")
                                                                                )
                                                                         )
                                                                  )
                                                           )
                                                    )
                                             )
                                      )
                               )
                        )
  ))

library(gganimate)

myanim<-modelData%>%
  dplyr::select(week_id,  week_position, tempo, genre)%>%
  distinct()%>%
  mutate(week = mdy(week_id))%>%
  filter(week>=mdy("1-1-1980"),week<=mdy("1-1-2001"))%>%
  ggplot(aes(y = week_position, x = tempo, color = as.factor(genre)))+
  geom_point(size = 3,shape=8)+
  # geom_path(size = 2)+
  scale_color_tidyTues()+
  scale_x_continuous(limits = c(70,180))+
  scale_y_reverse()+
  coord_polar(start =130)+
  theme_void()+
  theme(
    plot.background = element_rect(fill="#6c757d", colour=NA),
        legend.position = "none")+
  transition_time(week)+
  ease_aes()+
  enter_fade() + 
  exit_shrink()+
  shadow_trail(distance = .1, size = 2,alpha =.25,max_frames =10)
  
  
animate(myanim,fps = 26,nframes = 52*20)
anim_save(filename = "FireworkPlotofBillboardPositions_TT-9-15-2021_small.gif")
