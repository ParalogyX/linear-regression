#Q7


dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR = HR/G,
         R = R/G) %>%
  select(lgID, HR, BB, R) 

dat %>% 
  group_by(lgID) %>% 
  do(tidy(lm(R ~ HR, data = .), conf.int = T)) %>% 
  filter(term == "HR") 

dat %>% 
  group_by(lgID) %>% 
  do(glance(lm(R ~ HR, data = .)))

dat %>% 
  do(tidy(lm(R ~ HR, data = .), conf.int = T)) %>% 
  filter(term == "HR")

dat %>% 
  group_by(lgID) %>% 
  do(mod = lm(R ~ HR, data = .))

#Q8

ibrary(tidyverse)
library(HistData)
data("GaltonFamilies")
#set.seed(1) # if you are using R 3.5 or earlier
set.seed(1, sample.kind = "Rounding") # if you are using R 3.6 or later
galton <- GaltonFamilies %>%
  group_by(family, gender) %>%
  sample_n(1) %>%
  ungroup() %>% 
  gather(parent, parentHeight, father:mother) %>%
  mutate(child = ifelse(gender == "female", "daughter", "son")) %>%
  unite(pair, c("parent", "child"))

galton

#A8
galton %>% group_by(pair) %>% summarise(n = n())

#A9
galton %>% group_by(pair) %>%
  summarize(cor = cor(childHeight, parentHeight)) %>% filter(cor == max(cor))
galton %>% group_by(pair) %>%
  summarize(cor = cor(childHeight, parentHeight)) %>% filter(cor == min(cor))


#Q10a
library(broom)

galton %>%  
  group_by(pair) %>%
  do(tidy(lm(childHeight ~ parentHeight, data = .), conf.int = TRUE)) %>%
  filter(term == "parentHeight") 

galton %>% group_by(pair) %>% filter (pair == "mother_son") %>% 
  summarize(slope = cor(parentHeight, childHeight) * sd(childHeight) / sd(parentHeight))

#HarvardX solution (much better):
galton %>%
  group_by(pair) %>%
  do(tidy(lm(childHeight ~ parentHeight, data = .), conf.int = TRUE)) %>%
  filter(term == "parentHeight", pair == "mother_son") %>%
  pull(estimate)
  


#Q10b
galton %>%
  group_by(pair) %>%
  do(tidy(lm(childHeight ~ parentHeight, data = .), conf.int = TRUE)) %>%
  filter(term == "parentHeight" & p.value < .05)
