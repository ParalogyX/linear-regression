library(tibble)
library(tidyverse)
library(broom)
library(dslabs)
data("research_funding_rates")
research_funding_rates


#Q1
two_by_two_table <- tibble(gender = c('men', 'women'), awarded = c(sum(research_funding_rates$awards_men), sum(research_funding_rates$awards_women)), 
       not_awarded = c(sum(research_funding_rates$applications_men) - sum(research_funding_rates$awards_men), sum(research_funding_rates$applications_women) - sum(research_funding_rates$awards_women)))

#HarvardX solution:
two_by_two <- research_funding_rates %>% 
  select(-discipline) %>% 
  summarize_all(funs(sum)) %>%
  summarize(yes_men = awards_men, 
            no_men = applications_men - awards_men, 
            yes_women = awards_women, 
            no_women = applications_women - awards_women) %>%
  gather %>%
  separate(key, c("awarded", "gender")) %>%
  spread(gender, value)
two_by_two

#Q2
two_by_two$men[2]/sum(two_by_two$men) * 100
two_by_two$women[2]/sum(two_by_two$women) * 100
    

#Harvards solution:
two_by_two %>% 
  mutate(men = round(men/sum(men)*100, 1), women = round(women/sum(women)*100, 1)) %>%
  filter(awarded == "yes") %>%
  pull(men)
two_by_two %>% 
  mutate(men = round(men/sum(men)*100, 1), women = round(women/sum(women)*100, 1)) %>%
  filter(awarded == "yes") %>%
  pull(women)

#Q3
chisq_test <- two_by_two %>% select(-awarded) %>% chisq.test()
chisq_test
tidy(chisq_test)

#or
two_by_two %>% select(-awarded) %>% chisq.test() %>% tidy() %>% pull(p.value)


#Q4
dat <- research_funding_rates %>% 
  mutate(discipline = reorder(discipline, success_rates_total)) %>%
  rename(success_total = success_rates_total,
         success_men = success_rates_men,
         success_women = success_rates_women) %>%
  gather(key, value, -discipline) %>%
  separate(key, c("type", "gender")) %>%
  spread(type, value) %>%
  filter(gender != "total")
dat


dat %>% ggplot(aes(x = discipline, y = success, color = gender, size = applications)) + 
  geom_point()


#Harvards solution

dat %>% 
  ggplot(aes(discipline, success, size = applications, color = gender)) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_point()



