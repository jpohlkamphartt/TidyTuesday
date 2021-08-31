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
rawData<- tidytuesdayR::tt_load(2021, week = 36)

modelData<-rawData$bird_baths

skim(modelData)

head(modelData)

hist(modelData$bird_count[modelData$bird_count>1],breaks = 50)

modelData%>%
  unnest_tokens(namePart, bird_type, token ="words")%>%
  group_by(namePart)%>%
  summarise(count = n())%>%
  arrange(desc(count))


library(sf)
OzShp<-st_read("1270055001_aus_2016_aust_shape/AUS_2016_AUST.shp")

OzShpMin<-st_combine(st_cast(OzShp$geometry,"POLYGON")[as.numeric(st_area(st_cast(OzShp$geometry,"POLYGON")))>10000000])
OzShpMin<-st_simplify(OzShpMin,dTolerance = 10000)
shpz<-st_read("IBRA7_regions_states/IBRA7_regions_states.shp")

shpzPrint<-shpz%>%
  filter(toupper(REG_NAME_6)%in%toupper(unique(modelData$bioregions)))%>%st_simplify(dTolerance = 10000)
  

p1<-ggplot() + 
  geom_sf(data =OzShpMin)+
  geom_sf(data =shpzPrint ) +
  coord_sf()+
  scale_x_continuous(limits = c(130,160))+
  scale_y_continuous(limits = c(-20,-45))
ggsave("test1.png", p1)

plotData<-modelData%>%
  mutate(honeyEater = grepl("honeyeater",tolower(bird_type)))%>%
  group_by(bioregions)%>%
  summarise(EaterProp = sum(bird_count[honeyEater])/sum(bird_count))%>%
  ungroup()%>%
  left_join(shpzPrint%>%dplyr::select(REG_NAME_7,geometry)%>%rename(bioregions =REG_NAME_7))%>%
  filter(!st_is_empty(geometry))

p2<-ggplot() + 
  geom_sf(data =OzShpMin,fill = "#2d2d2d60")+
  geom_sf(data = plotData,
          aes(geometry = geometry,fill = EaterProp)
          ) + 
  coord_sf()+
  scale_x_continuous(limits = c(135,155))+
  scale_y_continuous(limits = c(-20,-45))+
  scale_fill_tidyTues("Sequential",discrete = F,reverse = T)+
  labs(
    fill = "% of Honeyeaters",
    title = "Birdbath Regionality of Australian Honeyeaters",
    caption = "We can see that there is a lower concentration near urban coastal communities."
  )

plot_gg(p2,scale=200,width = 8, height = 8)
render_snapshot(filename = "3D_OzHoney")

render_movie("movie_OzHoney.mp4",frames = 720, fps=30,zoom=0.6,fov = 30)
