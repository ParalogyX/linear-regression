library(tidyverse)
library(HistData)
data("GaltonFamilies")

#Q1

rss <- function(beta0, beta1){
  resid <- galton_heights$son - (beta0+beta1*galton_heights$father)
  return(sum(resid^2))
}

# plot RSS as a function of beta1 when beta0=25
beta1 = seq(0, 1, len=nrow(galton_heights))
results <- data.frame(beta1 = beta1,
                      rss = sapply(beta1, rss, beta0 = 25))
results %>% ggplot(aes(beta1, rss)) + geom_line() + 
  geom_line(aes(beta1, rss))

#A1
# plot RSS as a function of beta1 when beta0=36
beta1 = seq(0, 1, len=nrow(galton_heights))
results <- data.frame(beta1 = beta1,
                      rss = sapply(beta1, rss, beta0 = 36))
results %>% ggplot(aes(beta1, rss)) + geom_line() + 
  geom_line(aes(beta1, rss))

#Q3
library(Lahman)
teams_filtered <- Teams %>% filter(yearID %in% 1961:2001) %>% 
  mutate(HR_per_game = HR / G, R_per_game = R / G, BB_per_game = BB / G)
lm(R_per_game ~ HR_per_game + BB_per_game, data = teams_filtered)

#HarvardX solution:
library(Lahman)
library(broom)
Teams_small <- Teams %>% filter(yearID %in% 1961:2001)
Teams_small %>% 
  mutate(R_per_game = R/G, BB_per_game = BB/G, HR_per_game = HR/G) %>% 
  do(tidy(lm(R_per_game ~ BB_per_game + HR_per_game, data = .)))

#Q7
#set.seed(1989) #if you are using R 3.5 or earlier
set.seed(1989, sample.kind="Rounding") #if you are using R 3.6 or later
library(HistData)
data("GaltonFamilies")
options(digits = 3)    # report 3 significant digits

female_heights <- GaltonFamilies %>%     
  filter(gender == "female") %>%     
  group_by(family) %>%     
  sample_n(1) %>%     
  ungroup() %>%     
  select(mother, childHeight) %>%     
  rename(daughter = childHeight)

mother_daughter_model <- lm(mother ~ daughter, data = female_heights)
#Q8
predict(mother_daughter_model, se.fit = TRUE)
#or
#predict(mother_daughter_model)[1]
female_heights$mother[1]





#Q9
library(Lahman)

bat_02 <- Batting %>% filter(yearID == 2002) %>%
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100) %>%
  select(playerID, singles, bb)

bat_99_01 <- Batting %>% filter(yearID %in% 1999:2001) %>%
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100) %>%
  select(playerID, singles, bb)

mean(bat_99_01$singles)
mean(bat_99_01$bb)

means <- bat_99_01 %>% group_by(playerID) %>% 
  summarize(avg_singles = mean(singles), 
            avg_bb = mean(bb))

#Count  row
means %>% 
  filter(avg_singles >.2) %>%
  nrow()

means %>%
  filter(avg_bb >.2) %>%
  nrow()

#Q10
bat_99_02 <- inner_join(means, bat_02)

cor(bat_99_02$singles, bat_99_02$avg_singles)
cor(bat_99_02$bb, bat_99_02$avg_bb)
  
#Q11
bat_99_02 %>% ggplot(aes(avg_singles, singles)) + geom_point()
bat_99_02 %>% ggplot(aes(avg_bb, bb)) + geom_point()

#Q12
lm(singles ~ avg_singles, data = bat_99_02)

lm(bb ~ avg_bb, data = bat_99_02)
