#set.seed(1989) #if you are using R 3.5 or earlier
set.seed(1989, sample.kind="Rounding") #if you are using R 3.6 or later
library(HistData)
data("GaltonFamilies")

female_heights <- GaltonFamilies%>%     
  filter(gender == "female") %>%     
  group_by(family) %>%     
  sample_n(1) %>%     
  ungroup() %>%     
  select(mother, childHeight) %>%     
  rename(daughter = childHeight)

#Q8:
mothers_mean <- mean(female_heights$mother)
mothers_sd <- sd (female_heights$mother)
daughters_mean <- mean(female_heights$daughter)
daughters_sd <- sd (female_heights$daughter)
r <- cor(female_heights$mother, female_heights$daughter)

#A8:
mothers_mean
mothers_sd
daughters_mean
daughters_sd
r

#Q9:
m <- r * daughters_sd / mothers_sd
b <- daughters_mean - m*mothers_mean

#A9:
m
b
1 * m

#A10
r*r

#Q11
expected_daughter_height <- daughters_mean + r *(60 - mothers_mean) * daughters_sd / mothers_sd
#A11
expected_daughter_height

#HarvardX solution:
# m = r * s_y/s_x
# b = mu_y - (r * s_y/s_x)*mu_x
# x = 60
# m*x+b
