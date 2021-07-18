library(tidyverse)
library(broom)
library(Lahman)
Teams_small <- Teams %>% 
  filter(yearID %in% 1961:2001) %>% 
  mutate(avg_attendance = attendance/G)

#Q1a
Teams_small %>% mutate(RPG = R/G) %>% 
  lm(avg_attendance ~ RPG, data = .) %>%
  .$coef %>% .[2]


Teams_small %>% mutate(HRPG = HR/G) %>% 
  lm(avg_attendance ~ HRPG, data = .) %>%
  .$coef %>% .[2]

#Q1b
fit <- Teams_small %>% 
  lm(avg_attendance ~ W, data = .)

fit$coef[2]


newdata <- tibble(W = 0)
predict(fit, newdata)
#OR
fit$coef[1]

#Q1c
Teams_small %>% 
  lm(avg_attendance ~ yearID, data = .) %>%
  .$coef %>% .[2]


#Q2
# Teams_small %>%  mutate(RPG = R/G) %>%
#   lm(W ~ RPG, data = .) %>%
#   .$coef %>% .[2]
# 
# Teams_small %>% mutate(HRPG = HR/G) %>%
#   lm(W ~ HRPG, data = .) %>%
#   .$coef %>% .[2]

new_teams_small <- Teams_small %>%  mutate(RPG = R/G, HRPG = HR/G)

cor(new_teams_small$W, new_teams_small$RPG)
cor(new_teams_small$W, new_teams_small$HRPG)

#Q3
strat_teams_small <- Teams_small %>%
  mutate(Wstrat = round(W/10)) %>%
  filter(Wstrat %in% (5:10)) %>%
  group_by(Wstrat) %>%
  filter(n() >= 20)

strat_teams_small %>% filter(Wstrat == 8) %>% summarise(n = n())
#or
sum(strat_teams_small$Wstrat == 8)


strat_teams_small %>% mutate(RPG = R/G) %>%
  summarize(slope = cor(avg_attendance, RPG)*sd(RPG)/sd(avg_attendance))
  
dat <- strat_teams_small %>% mutate(RPG = R/G) %>%
  summarize(slope = cor(avg_attendance, RPG)*sd(avg_attendance)/sd(RPG))

dat
which.max(dat$slope)

strat_teams_small %>% mutate(HRPG = HR/G) %>%
  summarize(slope = cor(avg_attendance, HRPG)*sd(avg_attendance)/sd(HRPG))
  
  # 
  # ggplot(aes(avg_attendance, RPG)) +  
  # geom_point(alpha = 0.5) +
  # geom_smooth(method = "lm") +
  # facet_wrap( ~ Wstrat)
  # 

#Harvards solution almost the same:
strat_teams_small %>%  
  group_by(Wstrat) %>%
  summarize(slope = cor(R/G, avg_attendance)*sd(avg_attendance)/sd(R/G))
strat_teams_small %>%  
  group_by(Wstrat) %>%
  summarize(slope = cor(HR/G, avg_attendance)*sd(avg_attendance)/sd(HR/G))


#Q4
fit <- Teams_small %>% mutate(RPG = R/G, HRPG = HR/G) %>% 
  lm(avg_attendance ~ RPG + HRPG + W + yearID, data = .)
coefs <- tidy(fit, conf.int = TRUE)
coefs 

#Harvards solution:
tidy(fit) %>%
  filter(term == "W") %>%
  pull(estimate)
#etc


#Q5
test_team1 <- tibble(RPG = 5, HRPG = 1.2, W = 80, yearID = 2002)
test_team2 <- tibble(RPG = 5, HRPG = 1.2, W = 80, yearID = 1960)

predict(fit, test_team1)
#or
predict(fit, data.frame(RPG = 5, HRPG = 1.2, W = 80, yearID = 2002))

predict(fit, test_team2)

#Q6
dat <- Teams %>%
  filter(yearID == 2002) %>%
  mutate(avg_attendance = attendance/G, RPG = R/G, HRPG = HR/G) %>%
  mutate(hat_avg_attendance = predict(fit, newdata = .))
  

r = cor(dat$hat_avg_attendance, dat$avg_attendance)
r



#Harvards solution:
newdata <- Teams %>%
  filter(yearID == 2002) %>%
  mutate(avg_attendance = attendance/G,
         RPG = R/G,
         HRPG = HR/G)
preds <- predict(fit, newdata)
cor(preds, newdata$avg_attendance)




























































