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

here()
usethis::edit_r_environ()
set.seed(1)

options(scipen=999)
source("TidyTuesdayTheme.R")
rawData<- tidytuesdayR::tt_load(2021, week = 33)

modelData<-full_join(full_join(rawData$ipd,rawData$chain_investment, by = c("group_num" , "year","category","meta_cat")),rawData$investment, by = c("group_num" , "year","category","meta_cat"))

metaRename<-function(name){
  lapply(name,function(nm){switch(nm,
        "GDP" = "GDP",
        "Infrastructure" = "Infrastructure",
        "Total basic infrastructure" = "Infrastructure",
        "Basic"                            = "Infrastructure"                           ,
        "Water supply" = "Water supply",
        "Sewer and waste" = "Sewer and waste",
        "Conservation and development" = "Conservation",
        "Power"                            = "Power"                           ,
        "Electric power" = "Power",
        "Natural gas /petroleum power" = "Power",
        "Transportation" = "Transportation",
        "Highways and streets"             = "Roads"            ,
         "Air /water /other transportation" =  "Transportation",
         "Social" =  "Social",
         "Health care" =  "Health",
         "Education"                        =  "Education"                       ,
         "Public safety" =  "Public safety",
         "Digital" =  "Digital",
         "Total infrastructure" =  "Infrastructure",
         "Health"         =  "Health"
  )})%>%unlist()
}
  

modelData%>%
  mutate(category = fct_lump(category,10))%>%
  group_by(category)%>%
  summarise(gross_inv_ipd = mean(gross_inv_ipd,na.rm = T))%>%
  ungroup()%>%
  mutate(category = fct_reorder(category,gross_inv_ipd))%>%
  ggplot(aes(x = category, y =gross_inv_ipd,fill = category))+
  geom_bar(stat = "identity")+
  scale_fill_tidyTues(discrete = T)+
  theme(axis.text.x = element_text(angle = 35,vjust = .5,size = 9 ),legend.position = "none")

library(gganimate)

myanim<-modelData%>%  
  ggplot(aes(x = year, y = log(gross_inv_ipd,10),color = fct_lump(meta_cat,8)))+
  geom_point(size =4)+
  scale_color_tidyTues(palette = "Sequential",discrete = T)+
  labs(
    y = "Log10(IPD)",
    x = "Year",
    title = "Hypno-Blob! Vote for the weird viz",
    subtitle = 'Log of IPD of Year {round(frame_time,0)}',
    caption = "Notice that at the start of the school year gaming goes down, but comes back in the spring.
    Addiction is a bitch."
  )+
  transition_time(year) +
  ease_aes()+
  enter_fade() + 
  exit_shrink()+
  shadow_trail( size = 2,alpha =.5)

animate(myanim, height = 800, width =1600)

model_cum<-modelData%>%
  mutate(meta_cat = metaRename(meta_cat))%>%
  complete(year = seq(min(year),max(year)),
           fill=list(gross_inv_ipd=0))%>%
  group_by(year, meta_cat)%>%
  summarise(gross_inv_ipd = cumsum(gross_inv_ipd))%>%
  summarise(gross_inv_ipd = max(gross_inv_ipd))%>%
  ungroup()%>%
  group_by(year)%>%
  mutate(Rank = order(order(gross_inv_ipd, decreasing = T)))%>%
  ungroup()

myanim<-model_cum%>%
  # mutate(year2 = round(year/2,0))%>%
  ggplot(aes(x = year, y = Rank,color =meta_cat))+
  geom_point(size =5)+
  geom_path(size = 2)+
  scale_color_tidyTues(palette = "Qualitative",discrete = T)+
  scale_y_reverse()+
  labs(
    y = "IPD Rank",
    x = "Year",
    title = "Rally For The Funds",
    subtitle = 'Log of IPD by Year (Current Year: {round(frame_along,0)})',
    caption = "Notice the rise in Digital in the 80s & significant drop off after the dot-com bubble.",
    color = "Category"
  )+
  transition_reveal(year) +
  ease_aes()+
  enter_fade() + 
  exit_fade()

animate(myanim,nframes= 300,fps=25, height = 800, width =1600, end_pause = 50)
anim_save(filename = "Category IPD Rank_TT-8-10-2021.gif")
