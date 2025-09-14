library(tidyverse)
library(mosaic)
library(scales)
theme_set(theme_minimal()) 
options(scipen=77)

#1 Part A

tate %>%
  group_by(artist) %>%
  summarise(artist_count = n()) %>%
  arrange(desc(artist_count))

#Part B

fem_tate = tate %>%
  filter(gender == "Female") %>%
  group_by(year) %>%
  summarise(fem_artist_count = n()) %>%
  arrange(desc(fem_artist_count))

ggplot(fem_tate) +
  geom_line(aes(x=year, y=fem_artist_count)) +
  labs(title = "Female Created Artworks Skyrocket in late 20th Century",
       x = "Year", 
       y = "Female Created Artwork Count") 
 
#Part C
tate = tate %>%
  mutate(area = height * width)

ggplot(tate) +
  geom_histogram(aes(x=area)) +
  labs(title = "Tate's Collection Features Many Small Pieces",
       x = "Area of Piece (in square meters)",
       y = "Number of Pieces")

favstats(~area, data = tate)
#Median = 0.21
#IQR = 0.43

#Part D
tate %>%
  group_by(medium) %>%
  summarise(medium_count = n()) %>%
  arrange(desc(medium_count))

tate %>%
  group_by(medium) %>%
  summarise(area_mean = mean(area)) %>%
  arrange(desc(area_mean))

tate %>%
  group_by(medium) %>%
  summarise(sd_area = sd(area)) %>%
  arrange(desc(sd_area)) 

tate %>%
  group_by(medium) %>%
  summarise(max_area = max(area)) %>%
  arrange(desc(max_area))
  
#2 
ggplot(gss) +
  geom_point(aes(x=Height, y=Income)) +
  geom_smooth(aes(x=Height, y=Income), method = 'lm') +
  labs(title = "Low Correlation Between Height and Income",
       x = "Height (inches)",
       y = "Income (annual income in U.S. dollars)") +
  scale_y_continuous(labels = dollar)

model_heightincome = lm(Income ~ Height, data=gss) 
coef(model_heightincome)

rsquared(model_heightincome)
boot_rsquared = do(10000)*lm(Income ~ Height, data = resample(gss))
confint(boot_rsquared)

ggplot(gss) +
  geom_histogram(aes(x=Income)) +
  labs(title = "Annual Income is Skewed to the Right",
       x = "Income (annual income in U.S. dollars",
       y = "Frequency") +
  scale_x_continuous(labels = dollar)

#3 Part A
georgia = georgia %>%
  mutate(undercounted = round(100*(ballots - votes)/ballots, 2),
         winner = ifelse(bush > gore, "Bush", "Gore"))

mean(undercounted ~ equipment, data = georgia)
diffmean(undercounted ~ equipment, data = georgia)

boot_equipment = do(10000)*diffmean(undercounted ~ equipment, data = resample(georgia))
head(boot_equipment)

confint(boot_equipment)

ggplot(georgia) +
  geom_histogram(aes(x=undercounted)) +
  facet_wrap(~equipment, nrow=2) +
  labs(title = "Similar Distributions of Percentage of Undercounted Votes Regardless of Equipment",
       x = "Percentage (%) of Ballots (0-100) in a County that were Undercounted",
       y = "Number of Counties") 
  #scale_x_continuous(labels = percent(scale = 1))

#Part B
georgia1 = georgia %>%
  group_by(winner, equipment) %>%
  summarize(prop = n()/nrow(georgia))

prop(equipment ~ winner, data = georgia)
diffprop(equipment ~ winner, data = georgia)
prop(~equipment, data = georgia)

boot_winner = do(10000)*diffprop(equipment ~ winner, data = resample(georgia))
confint(boot_winner)

ggplot(georgia) +
  geom_bar(aes(x=winner, fill = equipment),
           position = "dodge") +
  labs(title = "Gore-Winning Counties had Smaller Percentage of Optical Scan Equipment Use",
       x = "Candidate Who Won County",
       y = "Number of Counties Won") +
  scale_fill_discrete(name = "Equipment Type")

#4 
sim_cogtest = do(100000)*nflip(n=99, prob = 0.8)
head(sim_cogtest)
confint(sim_cogtest)

#p-value
sum(sim_cogtest >= 88) / 100000

ggplot(sim_cogtest) +
  geom_histogram(aes(x=nflip), binwidth = 1) +
  labs(title = "Bootstrap Distribution of S2 Cognition Baseline Rate of 80%",
       x = "Simulated S2 Cognition Test Score (99 Available Points)",
       y = "Frequency of Score")
