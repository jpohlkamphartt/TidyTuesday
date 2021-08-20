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
library(stacks)
library(tidymodels)
library(recipes)
library(themis)
library(textrecipes)
library(vip)
library(ggrepel)

here()
usethis::edit_r_environ()
set.seed(1)

options(scipen=999)
source("TidyTuesdayTheme.R")
rawData<- tidytuesdayR::tt_load(2021, week = 34)

modelData<-rawData$computer
skim(modelData)

modelData$char<-toupper(trimws(str_remove_all(str_remove_all(str_remove_all(str_remove_all(modelData$char,"\\([[:print:]]+\\)"),"[[:punct:]]"),"Young"),"Voice")))

charSwitch<-
  function(w){
    lapply(w,function(v){
      switch(v,
             JEANLUC =  "PICARD",
             PICARD =  "PICARD",
             "NEW COMPUTER" = "COMPUTER",
             COMPUTERS ="COMPUTER",
             "COM PANEL" ="COMPUTER",
             COMPUTER = "COMPUTER",
             RIKERS ="RIKER",
             RIKER = "RIKER",
             "MRS TROI" ="LWAXANA",
             LWAXANA = "LWAXANA",
             v
      )
    })%>%unlist()
  }

table(modelData$char)

modelData<-modelData%>%
  filter(!error,char_type=="Person")%>%
  transmute(
    char=fct_lump(charSwitch(char),10),
    line, #pull ngrams
    direction = coalesce(direction,""), #pull ngrams
    type = toupper(type),
    pri_type = toupper(type),
    domain= coalesce(toupper(domain),"UNKNOWN"),
    sub_domain = toupper(str_remove_all(sub_domain, "[[:punct:]]")),
    nv_resp,
    interaction, #pull ngrams
    char_type
  )


# text mining
library(tidytext)
library(stringr)

swc <- paste(paste0("\\b", stop_words$word , "\\b"), collapse = "|")
lineText <- modelData %>% 
  mutate(line = str_remove_all(line, "\\({1}[[:print:]]+\\){1}")) %>%
  distinct_at(vars(char,   line, direction,interaction),.keep_all = T)%>%
  unnest_tokens(word, line, token ="ngrams", n = 2) %>%
  filter(!word %in% stop_words$word,
         !word %in% str_remove_all(stop_words$word, "'"),
         !str_detect(word,swc),
         str_detect(word, "[a-z]"))
lineText %>% 
  # filter(!type %in% toupper(c("Password","Wake Word")))%>%
  group_by(char) %>%
  count(word, sort = TRUE) %>% 
  left_join(lineText %>% 
              group_by(word) %>%
              summarise(total = n())) %>%
  mutate(freq = n/total)%>%
  ungroup()%>%
  filter(total >4,
         !word %in% c("computer freeze","grey hot","tea earl","luc picard"))%>%
  mutate(word = fct_reorder(word, total))%>%
  ggplot(aes(y = n, x = word, fill = char))+
  geom_bar(stat = "Identity", position = "stack")+ 
  scale_fill_tidyTues(discrete = T)+
  theme(axis.text.x = element_text(angle = 35,vjust = .5,size = 10 ))+
  labs(fill = "Character",
       x = "Phrase",
       y = "Total Occurrences",
       title = "Phrases To Trek By",
       subtitle = "Common Computer Prompt Bigrams From Star Trek TNG Sorted By Occurrence And Character",
       caption = "A lot of people used the hollowdeck, Geordi and Picard always getting up to simulated trouble")

ggsave("ST-TNG Phrase Occurrences by Char_TT-8-17-2021.jpeg",device = "jpeg", height = 12, width = 15,dpi = 320)

lineText %>% 
  # filter(!type %in% toupper(c("Password","Wake Word")))%>%
  group_by(char) %>%
  count(word, sort = TRUE) %>% 
  left_join(lineText %>% 
              group_by(word) %>%
              summarise(total = n())) %>%
  mutate(freq = n/total)%>%
  ungroup()%>%
  filter(total >4,
         !word %in% c("computer freeze","grey hot","tea earl","luc picard"))%>%
  mutate(word = fct_reorder(word, total))%>%
  group_by(word)%>%
  summarise(
    char = head(char[freq == max(freq)],1),
    freq = head(max(freq),1),
    total = head(max(total),1)
  )%>%
  ggplot(aes(x = total, y = freq, label = word, color = char))+
  geom_point( size = 5, alpha = .25) +
  geom_label_repel(max.iter = 1000000, max.time = 2,box.padding=.25, fill ="gray20",label.size =.3,max.overlaps =20)+
  scale_color_tidyTues(discrete = T)+
  scale_x_continuous(limits = c(0,25))+
  theme(
        panel.background  = element_blank(),
        plot.background = element_rect(fill="gray20", colour=NA),
        legend.background = element_rect(fill="transparent", colour=NA),
        legend.key = element_rect(fill="transparent", colour=NA),
        legend.text = element_text(colour="white"),
        legend.title = element_text(colour="white"),
        axis.text = element_text(colour="white"),
        axis.line = element_line(colour="white"),
        axis.ticks = element_line(colour="white"),
        axis.title = element_text(colour="white"),
        plot.title = element_text(colour="white"),
        plot.subtitle  = element_text(colour="white"),
        plot.caption  = element_text(colour="white")
      )+
  labs(color = "Most Frequent\nCharacter",
       x = "Total Occurrences",
       y = "Uniqueness (Highest Character Frequency by Word)",
       title = "That's So Captain!",
       subtitle = "Common Computer Prompt Bigrams From Star Trek TNG Sorted By Occurrence And Uniqueness",
       caption = "Picard loved his team and Data really wanted the computer to stop..")

ggsave("ST-TNG Phrase Occurrences by Unique_TT-8-17-2021.jpeg",device = "jpeg", height = 12, width = 15,dpi = 320)

### modeling the speaker
modelSplit<-initial_split(modelData)
modelTrain<-training(modelSplit)
modelTest<-testing(modelSplit)
modelFoldz<-vfold_cv(modelTrain,v=5,repeats=1,strata = char)

# binary, multiclass, ordinal
Rec_Cat<-
  recipe(char~. ,data=modelTrain)%>%
  step_mutate(nv_resp = as.numeric(nv_resp))%>%
  step_rm(char_type,interaction,line,direction)%>%
  step_novel(all_nominal_predictors()) %>%
  step_other(all_nominal_predictors(), threshold = 0.01) %>%
  step_unknown(all_nominal_predictors()) %>%
  step_impute_bag(sub_domain, impute_with = imp_vars(domain))%>%
  step_zv(all_predictors())%>%
  step_dummy(all_nominal_predictors(),one_hot=T)%>%
  step_smote(char)

Rec_glm<-
  recipe(char~. ,data=modelTrain)%>%
  step_rm(char_type)%>%
  step_mutate(nv_resp = as.numeric(nv_resp))%>%
  step_tokenize(interaction,line,direction)%>%
  step_stem(interaction,line,direction)%>%
  step_stopwords(interaction,line,direction)%>%
  step_tokenfilter(interaction,line,direction, max_tokens = 20) %>%
  step_tfidf(interaction,line,direction) %>%
  step_novel(all_nominal_predictors()) %>%
  step_other(all_nominal_predictors(), threshold = 0.01) %>%
  step_unknown(all_nominal_predictors()) %>%
  step_impute_bag(sub_domain, impute_with = imp_vars(domain))%>%
  step_zv(all_predictors())%>%
  step_dummy(all_nominal_predictors(),one_hot=T)%>%
  step_smote(char)

ctrl_grid <- control_stack_grid()

glmSpec<-multinom_reg(mixture = tune(),penalty = tune())%>% 
  set_engine("glmnet")%>%
  set_mode("classification")


glmGrid<-grid_max_entropy(
  mixture(),
  penalty(),
  size = 10,
  variogram_range = .75
)


glmWflow<-workflow()%>%
  add_recipe(Rec_glm)%>%
  add_model(glmSpec)

glmRes<-tune_grid(
  glmWflow,
  resamples = modelFoldz,
  grid = glmGrid,
  metrics = metric_set(mn_log_loss,roc_auc,accuracy),
  control = ctrl_grid
)

glmWflow%>%finalize_workflow(parameters = select_best(glmRes, "mn_log_loss"))%>%
  fit(modelTrain)%>%pull_workflow_fit()%>%vip()

catSpec<-boost_tree( trees = tune(), min_n = tune(), tree_depth = tune(),
                     sample_size = tune(),learn_rate = tune(),mtry = tune())%>%
  set_engine("xgboost", objective = "multi:softprob") %>% 
  set_mode("classification")

catGrid<-grid_max_entropy(
  trees(range = c(100L, 1200L)),
  tree_depth(range = c(1L, 10L)),
  min_n(range = c(2L, 25L)),
  sample_size = sample_prop(range = c(2/10, 1)),
  learn_rate(range = c(-5, -1)),
  finalize(mtry(range = c(2L, unknown())), modelTrain),
  size = 20,
  variogram_range = .75
)

catWflow<-workflow()%>%
  add_recipe(Rec_Cat)%>%
  add_model(catSpec)

catRes<-tune_grid(
  catWflow,
  resamples = modelFoldz,
  grid = catGrid,
  metrics = metric_set(mn_log_loss,roc_auc),
  control = ctrl_grid
)

catWflow%>%finalize_workflow(parameters = select_best(catRes, "mn_log_loss"))%>%
  fit(modelTrain)%>%pull_workflow_fit()%>%vip()


### stack for others
model_st <- 
  stacks() %>%
  add_candidates(glmRes) %>%
  add_candidates(catRes) %>%
  # determine how to combine their predictions
  blend_predictions(
    metric =metric_set(mn_log_loss),# roc_auc rmse rsq mae 
    penalty = 10^(-6:-1),mixture = (0:5)/5) %>%
  # fit the candidates with nonzero stacking coefficients
  fit_members()
model_st

# model insights
autoplot(model_st)
autoplot(model_st, type = "members")
autoplot(model_st, type = "weights")

predz<-predict(model_st,
               new_data = modelData,
               type = "prob"
)

# roc
modelData%>%
  mutate(probComputer = predz$.pred_Computer,
         char = fct_lump(char,20))%>%
  group_by(char)%>%
  summarise(probComputer = mean(probComputer),
            n = n())%>%
  arrange(desc(probComputer))

modelData%>%
  mutate(predz = predz$.pred)%>%
  ggplot(aes(x= TotalViews-predz, y = predz))+
  geom_point(alpha = .5)+
  geom_smooth()

modelData%>%
  mutate(predz = predz$.pred)%>%
  ggplot(aes(sample = predz)) + stat_qq() + stat_qq_line()




