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

here()
usethis::edit_r_environ()
set.seed(1)

options(scipen=999)
source("TidyTuesdayTheme.R")
rawData<- tidytuesdayR::tt_load('2021-08-24')
lemurs <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-08-24/lemur_data.csv',
                   col_types = cols(
                     taxon = col_character(),
                     dlc_id = col_character(),
                     hybrid = col_character(),
                     sex = col_character(),
                     name = col_character(),
                     current_resident = col_character(),
                     stud_book = col_character(),
                     dob = col_date(format = ""),
                     birth_month = col_double(),
                     estimated_dob = col_character(),
                     birth_type = col_character(),
                     birth_institution = col_character(),
                     litter_size = col_double(),
                     expected_gestation = col_double(),
                     estimated_concep = col_date(format = ""),
                     concep_month = col_double(),
                     dam_id = col_character(),
                     dam_name = col_character(),
                     dam_taxon = col_character(),
                     dam_dob = col_date(format = ""),
                     dam_age_at_concep_y = col_double(),
                     sire_id = col_character(),
                     sire_name = col_character(),
                     sire_taxon = col_character(),
                     sire_dob = col_date(format = ""),
                     sire_age_at_concep_y = col_double(),
                     dod = col_date(format = ""),
                     age_at_death_y = col_double(),
                     age_of_living_y = col_double(),
                     age_last_verified_y = col_double(),
                     age_max_live_or_dead_y = col_double(),
                     n_known_offspring = col_double(),
                     dob_estimated = col_character(),
                     weight_g = col_double(),
                     weight_date = col_date(format = ""),
                     month_of_weight = col_double(),
                     age_at_wt_d = col_double(),
                     age_at_wt_wk = col_double(),
                     age_at_wt_mo = col_double(),
                     age_at_wt_mo_no_dec = col_double(),
                     age_at_wt_y = col_double(),
                     change_since_prev_wt_g = col_double(),
                     days_since_prev_wt = col_double(),
                     avg_daily_wt_change_g = col_double(),
                     days_before_death = col_double(),
                     r_min_dam_age_at_concep_y = col_double(),
                     age_category = col_character(),
                     preg_status = col_character(),
                     expected_gestation_d = col_double(),
                     concep_date_if_preg = col_date(format = ""),
                     infant_dob_if_preg = col_date(format = ""),
                     days_before_inf_birth_if_preg = col_double(),
                     pct_preg_remain_if_preg = col_double(),
                     infant_lit_sz_if_preg = col_double()
                   )
)

lemurTaxonomy <- read_csv('LemurTaxonomy.csv')
lemurTaxonomy%>%
  unnest_tokens(namePart, `Latin name`, token ="words")%>%
  group_by(namePart)%>%
  summarise(count = n())%>%
  arrange(desc(count))

lemurTaxonomy%>%
  unnest_tokens(namePart, `Common name`, token ="words")%>%
  group_by(namePart)%>%
  summarise(count = n())%>%
  arrange(desc(count))

modelData<-lemurs%>%
  left_join(lemurTaxonomy, by = c("taxon"="Taxon"))%>%
  mutate(
    eulemur = as.numeric(grepl("eulemur",`Latin name`)),
    varecia = as.numeric(grepl("varecia",`Latin name`))
  )

skim(modelData)

modelData%>%
  mutate(age_roundhalf = round(2*age_at_wt_y,0)/2)%>%
  group_by(age_roundhalf,taxon)%>%
  summarise(wt = mean(weight_g,na.rm=T),
            wt_sd = sd(weight_g,na.rm=T),
            n = n()
  )%>%
  ggplot(aes(x = (age_roundhalf), y = (wt), color = taxon))+
  geom_point(aes(size = n))+
  geom_line()+
  # geom_smooth(method = "glm", formula = y~log(x+.0002))+
  theme(legend.position = "none")+
  scale_color_tidyTues(discrete = T)+
  labs(
    x = "Age",
    y = "Average Weight",
    color = "Taxonomy",
    title = "Lemur Growth Curves",
    subtitle = "Each Colour Represents a Lemur Taxonomy",
    caption = "Notice the logarithmic growth paths"
  )

modelData%>%
  group_by(taxon)%>%
  summarise(minSireAge = quantile(sire_age_at_concep_y,.05,na.rm=T),
            minDamAge = quantile(dam_age_at_concep_y,.05,na.rm=T)
  )%>%
  ggplot(aes(x = minSireAge, y = minDamAge, color = taxon))+
  geom_point()+
  # geom_line()+
  # geom_smooth(method = "glm", formula = y~log(x+.0002))+
  theme(legend.position = "none")+
  scale_color_tidyTues(discrete = T)+
  labs(
    x = "Age",
    y = "Average Weight",
    color = "Taxonomy",
    title = "Lemur Growth Curves",
    subtitle = "Each Colour Represents a Lemur Taxonomy",
    caption = "Notice the logarithmic growth paths"
  )

modelData%>%
  mutate(age_roundhalf = round(2*age_at_wt_y,0)/2)%>%
  # group_by(age_roundhalf,taxon)%>%
  # summarise(wt = mean(weight_g,na.rm=T),
  #           wt_sd = sd(weight_g,na.rm=T),
  #           n = n()
  # )%>%
  ggplot(aes(x = (age_roundhalf), y = (weight_g), color = taxon))+
  # geom_point(aes(size = n))+
  # geom_line()+
  geom_smooth(method = "glm", formula = y~log(x+.5/365),se = F)+
  theme(legend.position = "none")+
  scale_color_tidyTues(discrete = T)+
  labs(
    x = "Age",
    y = "Average Weight",
    color = "Taxonomy",
    title = "Fitted Lemur Growth Curves",
    subtitle = "Curves from Logarithmic Regression, Each Colour Represents a Lemur Taxonomy"
  )

taxonSum<-modelData%>%
  group_by(taxon)%>%
  do(fitWt = tidy(glm(weight_g ~ log(age_at_wt_y+.5/365), data = .))) %>% 
  unnest(fitWt)%>%
  pivot_wider(id_cols = taxon, names_from = term, values_from = estimate)%>%
  rename(avgWt = `(Intercept)`,
         growthSpeed = `log(age_at_wt_y + 0.5/365)`
  )%>%
    left_join(
      modelData%>%
        group_by(taxon)%>%
        summarise(maxAge= quantile(age_max_live_or_dead_y,.95,na.rm=T),
                  meanOffspring = mean(n_known_offspring,na.rm=T),
                  meanLitter = mean(litter_size,na.rm=T)
                  )
        )

set.seed(1)

kclusts <- 
  tibble(k = 1:9) %>%
  mutate(
    kclust = map(k, ~kmeans(taxonSum%>%dplyr::select(-taxon)%>%mutate(growthSpeed=(growthSpeed))%>%scale(), .x)),
    tidied = map(kclust, tidy),
    glanced = map(kclust, glance),
    augmented = map(kclust, augment, taxonSum%>%mutate(growthSpeed=(growthSpeed)))
  )

clusters <- 
  kclusts %>%
  unnest(cols = c(tidied))

assignments <- 
  kclusts %>% 
  unnest(cols = c(augmented))

clusterings <- 
  kclusts %>%
  unnest(cols = c(glanced))

ggplot(assignments, aes(x = avgWt  , size = maxAge, y = growthSpeed)) +
  geom_point(aes(color = .cluster), alpha = 0.8) + 
  facet_wrap(~ k)

ggplot(clusterings, aes(k, tot.withinss)) +
  geom_line() +
  geom_point()

assignments%>%filter(k == 4)%>%left_join(lemurTaxonomy, by = c("taxon"="Taxon"))%>%
  mutate(
    Eulemur = as.numeric(grepl("eulemur",tolower(`Latin name`))),
    Ruffed = as.numeric(grepl("varecia",tolower(`Latin name`))),
    Loris = as.numeric(grepl("loris",tolower(`Common name`))),
    type = ifelse(Eulemur,"Eulemur",ifelse(Loris,"Loris",ifelse(Ruffed, "Ruffed","Other")))
  )%>%
ggplot(aes(x = (avgWt)  , y = growthSpeed,shape = type, label = trimws(str_remove(`Common name`,"lemur")))) +
  geom_point(aes(color = .cluster,  size = maxAge), alpha = 0.8)+
  # geom_label_repel(max.iter = 1000000, max.time = 5,box.padding=.1,label.size =.3,max.overlaps =30,alpha = .7)+
  scale_color_tidyTues(discrete = T)+
  # scale_x_continuous(limits = c(0,3000))+
  scale_y_continuous(limits = c(0,750))+
  scale_shape_manual(values=c(16,17,15,18))+
  labs(
    x = "Average Adult Weight",
    y = "Weight Gain Factor",
    shape = "Sub-Class",
    color = "Cluster",
    size = "Life Span",
    title = "Learning Lemurs",
    subtitle = "K-Means Clustering of Lemurs by Growth, Aging and Gestation Variables",
    caption = "Coquerel's Sifaka is the large fast growing Other subtype. Zoboomafoo was one of them!"
  )
  
  
unique(damData$dlc_id)

damData<-modelData%>%
  group_by(dlc_id)%>%
  mutate(wasYoung = "IJ"%in%age_category,n = n(),minAge = min(age_at_wt_d))%>%
  ungroup()%>%
  filter(wasYoung, n>5,minAge<100)


P<-ggplot()
k<-7
for(index in unique(damData$dlc_id)){

  
 
 indvDF<-damData%>%
   filter(dlc_id ==index)%>%
   arrange(age_at_wt_d)
 # while(nrow(indvDF)<k){
 #   
 #   index<-sample(unique(damData$dlc_id),1)
 #   indvDF<-damData%>%
 #     filter(dlc_id ==index)%>%
 #     arrange(age_at_wt_d)
 #   
 # }
 
 side<-(-1+2*which(lemurTaxonomy$Taxon==head(indvDF$taxon,1))%%2)
 print(side)
 indvDF_adult_wt<-indvDF%>%filter(age_category =="adult")%>%summarise(wt = mean(weight_g))%>%unlist()
 indvDF_adult_wt_sd<-indvDF%>%filter(age_category =="adult")%>%summarise(wt = sd(weight_g))%>%unlist()
 indvDF$weight_g<-side*indvDF$weight_g#+rnorm(1,0,indvDF_adult_wt_sd/.5)
 indvDF$weight_g<-rollmean(indvDF$weight_g,k=3,align='right',fill=NA)
 P<-P+ geom_line(
   # stat="smooth",
                 data = indvDF, 
                  aes(
                    y = weight_g, 
                    x = age_at_wt_d,
                    # size = n_known_offspring
                   ),
                  color = sample(c(TidyTues_Sequential_Palette,TidyTues_Divergent_Palette,TidyTues_Qualitative_Palette)[lemurTaxonomy$Taxon==head(indvDF$taxon,1)],1),
                  orientation = "x",
                 # se = F,method = "gam",formula =  y ~ s(x, bs = "bs",k=k),
                 alpha = .5)
 # offspringDF<-modelData%>%
 #   filter(dam_id ==index)
 
 
 # print(P)
  
  
}
P<-P+ theme_void()+
  theme(plot.background = element_rect(fill="#56637A", colour=NA),legend.position = "none")+
  coord_polar()
P
