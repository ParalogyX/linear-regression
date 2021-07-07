library(tidyverse)
library(Lahman)
library(broom)

#Q9a
# regression with BB and HR
fit <- Teams %>% 
  filter(yearID == 1971) %>% 
  mutate(BB = BB / G, 
         HR = HR / G,
         R = R / G) %>%  
  lm(R ~ BB + HR, data = .)
coefs <- tidy(fit, conf.int = TRUE)
coefs


#HarvardX solution:
library(Lahman)
library(broom)
Teams %>%
  filter(yearID == 1971) %>%
  lm(R ~ BB + HR, data = .) %>%
  tidy() %>%
  filter(term == "BB") %>%
  pull(estimate)

Teams %>%
  filter(yearID == 1971) %>%
  lm(R ~ BB + HR, data = .) %>%
  tidy() %>%
  filter(term == "HR") %>%
  pull(estimate)


#Q10
# regression with BB and HR
fit <- Teams %>% 
  filter(yearID %in% 1961:2018) %>% 
  group_by(yearID) %>%
  mutate(BB = BB / G, 
         HR = HR / G,
         R = R / G) %>%  
  do(coefs = tidy(lm(R ~ BB + HR, data = .), conf.int = TRUE)) %>%
  mutate(BBonRuns = coefs$estimate[2])

fit %>% ggplot(aes(x = yearID, y = BBonRuns)) + 
  geom_point() + 
  geom_smooth(method=lm , color="red", se=T)

res <- fit


#HarvardX answer:
res <- Teams %>%
  filter(yearID %in% 1961:2018) %>%
  group_by(yearID) %>%
  do(tidy(lm(R ~ BB + HR, data = .))) %>%
  ungroup() 
res %>%
  filter(term == "BB") %>%
  ggplot(aes(yearID, estimate)) +
  geom_point() +
  geom_smooth(method = "lm")


#Q11
#fit <- res %>%
  #lm(BBonRuns ~ yearID, data = .)


fit <- res %>%
  filter(term == "BB") %>%
  lm(estimate ~ yearID, data = .)

tidy(fit, conf.int = TRUE)


#HarvardX solution:
res %>%
  filter(term == "BB") %>%
  lm(estimate ~ yearID, data = .) %>%
  tidy() %>%
  filter(term == "yearID") %>%
  pull(estimate)

res %>%
  filter(term == "BB") %>%
  lm(estimate ~ yearID, data = .) %>%
  tidy() %>%
  filter(term == "yearID") %>%
  pull(p.value)
