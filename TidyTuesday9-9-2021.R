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
rawData<- tidytuesdayR::tt_load(2021, week = 37)

rawData$races
set.seed(1)
rawData$pit_stops%>%left_join(rawData$races%>%dplyr::select(raceId,  year, round, name))%>%
  mutate(cluster = as.factor(kmeans(milliseconds,centers = 3, iter.max = 1000,nstart=200 )$cluster))%>%
  ggplot(aes(milliseconds))+
  geom_density(aes(fill = cluster, color = cluster),position = "stack", alpha = .5)+
  scale_color_tidyTues()+
  scale_fill_tidyTues()+
  scale_x_log10()+
  # geom_smooth(formula = y~ s(x,k=8))+
  # facet_wrap(~ name, ncol = 4) +
  labs(
    title = "The Types of Pit-Stops",
       subtitle = "Densities from Clustering on Pit-stop Times in Formula1",
       y = "Density",
       x = "Pit-stop Time",
       caption = "What is going on in Brazil?"
       )

set.seed(1)
rawData$pit_stops%>%left_join(rawData$races%>%dplyr::select(raceId,  year, round, name))%>%
  filter(kmeans(milliseconds,centers = 3, iter.max = 1000,nstart=200 )$cluster ==2)%>%
  mutate(name = fct_lump(name, 15))%>%
  ggplot(aes(y=milliseconds,x = lap, color = name))+
  geom_point(alpha = .5)+
  geom_smooth(formula = y~ s(x,k=8))+
  facet_wrap(~ name, ncol = 4) +
  labs(title = "Pit Party!",
       subtitle = "Effect of Lap on Pit-stop Times in Formula1 by Race",
       x = "Lap Number",
       y = "Pit-stop Time",
       caption = "What is going on at the start in Brazil and China?")+
  theme(legend.position = "NULL")+
  scale_color_tidyTues()

rawData$pit_stops%>%left_join(rawData$races%>%dplyr::select(raceId,  year, round, name))%>%
  mutate(name = fct_lump(name, 12))%>%
  filter(milliseconds<quantile(milliseconds,.95))%>%
  ggplot(aes(y=milliseconds,x=year))+
  geom_point(aes(color = name))+
  geom_smooth(formula = y~ s(x,k=8))
set.seed(1)
pitModel<-rawData$pit_stops%>%
  filter(kmeans(milliseconds,centers = 2, iter.max = 1000,nstart=200 )$cluster ==2)%>%
  left_join(rawData$races%>%dplyr::select(raceId,  year, round, name),by = c("raceId"))%>%
  left_join(rawData$results%>%dplyr::select(raceId, driverId, constructorId ), by = c("raceId", "driverId"))%>%
  mutate(name = as.factor(name),
         constructorId = as.factor(constructorId),
         driverId = as.factor(driverId),
         stop = as.factor(stop),
         year = year - min(year))%>%
  mgcv::gam(milliseconds ~ s(year,k=3) + s(lap, k=8) + s(constructorId,bs="re")+ s(name,bs="re"), data = .)

summary(pitModel)
p<-plot(pitModel)
mgcv::gam.vcomp(pitModel)

lapEffect<-tibble(lap = p[[2]]$x, effect = p[[2]]$fit[,1], se =p[[2]]$se )

lapEffect%>%
  ggplot(aes(x = lap, y = effect))+
  geom_hline(yintercept = 0, linetype = 2 )+
  geom_point(alpha=.5,color = TidyTues_Sequential_Palette[10])+
  geom_line(color =TidyTues_Sequential_Palette[10])+
  geom_ribbon(aes(ymin = effect - 1.96*se, ymax = effect + 1.96*se), fill = TidyTues_Sequential_Palette[9], alpha = .2)+
  scale_x_continuous(limits = c(0,60))+
  scale_y_continuous(limits = c(-1500,3500))+
  labs(title = "Starting Slow is the Pits!",
       subtitle = "Effect of Lap on Pit-stop Times in Formula1 Measured with a Mixed Effects Model",
       x = "Lap Number",
       y = "Effect on Pit-stop Time",
       caption = "Interesting effect that at the start of races there is a linear reduction in pit-stop times.")

