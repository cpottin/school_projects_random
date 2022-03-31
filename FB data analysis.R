install.packages('ggplot2')
library('ggplot2')
pf <- read.csv('pseudo_facebook.tsv', sep = '\t')
getwd()

names(pf)

qplot(x= dob_day, data = pf) +
  scale_x_discrete(breaks=1:31)

qplot(x = friend_count, data = pf, xlim = c(0, 1000)) +
  scale_x_continuous(limits = c(0, 1000))

qplot(x = friend_count, data = pf, binwidth = 25) +
  scale_x_continuous(limits = c(0, 1000), breaks = seq(0, 1000, 50))

ggplot(aes(x = tenure / 365), data = pf) +
  geom_histogram(color = 'black', fill = '#F79420') +
  scale_x_continuous(breaks = seq(1, 7, 1), limits = c(0, 7)) +
  xlab('Number of years using Facebook') +
  ylab('Number of users in sample')

ggplot(aes(x = age), data = pf) +
  geom_histogram(binwidth = 1, color = 'black', fill = '#5760AB') +
  scale_x_continuous(breaks = seq(0, 65, 5), limits = c(13, 65))

names(pf)
qplot(x= friend_count, data = pf)

install.packages('gridExtra')
library(gridExtra)

p1 <- qplot(x = friend_count, data = pf)
p2 <- qplot(x = log10(friend_count +1), data = pf)
p3 <- qplot(x = sqrt(friend_count), data = pf)

grid.arrange(p1, p2, p3, ncol = 1)

p1 <- ggplot(aes(x = friend_count), data = pf) + geom_histogram()
p2 <- p1 + scale_x_log10()
p3 <- p1 + scale_x_sqrt()

grid.arrange(p1, p2, p3, ncol = 1)

ggplot(aes(x = friend_count, y = ..count../sum(..count..)),
       data = subset(pf, !is.na(gender))) +
  geom_freqpoly(aes(color = gender), binwidth=10) +
  scale_x_continuous(limits = c(500, 1000), breaks = seq(500, 1000, 100)) +
  xlab('Friend Count') +
  ylab('Proportion of users with that friend count')

ggplot(aes(x = www_likes), data = subset(pf, !is.na(gender))) +
  geom_freqpoly(aes(color = gender)) +
  scale_x_log10()

#total likes and over the individual genders
by(pf$www_likes, pf$gender, sum)

qplot(x = gender, y = friend_count, 
      data = subset(pf, !is.na(gender)),
      geom = 'boxplot', ylim = c(0, 1000))

qplot(x = gender, y = friend_count, 
      data = subset(pf, !is.na(gender)),
      geom = 'boxplot') +
  coord_cartesian(ylim = c(0, 1000))

by(pf$friend_count, pf$gender, summary)

qplot(x = gender, y = friendships_initiated, 
      data = subset(pf, !is.na(gender)),
      geom = 'boxplot') +
  coord_cartesian(ylim = c(0, 250))

by(pf$friendships_initiated, pf$gender, summary)

pf$mobile_check_in <- NA
pf$mobile_check_in <- ifelse(pf$mobile_likes > 0, 1, 0)
pf$mobile_check_in <- factor(pf$mobile_check_in)
summary(pf$mobile_check_in)

##number of check ins / vector length 
sum(pf$mobile_check_in == 1)/length(pf$mobile_check_in)

install.packages('ggplot2')
library('ggplot2')

ggplot(aes(x = age, y = friend_count), data = pf) +
  geom_point(alpha = 1/20) + 
  xlim(13,90) +
  coord_trans(y = 'sqrt')



ggplot(aes(x = friendships_initiated, y = age), data = pf) +
  geom_point(aplha = 1/20) +
  xlim(0, 1000)
  ylim(0, 30)
  
ggplot(aes(x = age, y = friendships_initiated), data = pf) +
  geom_point(alpha = 1/10, position = 'jitter')

install.packages('dplyr')
library(dplyr)

age_groups <- group_by(pf, age)
pf.fc_by_age <- summarise(age_groups,
          friend_count_mean = mean(friend_count),
          friend_count_median = median(friend_count),
          n = n())
head(pf.fc_by_age)

ggplot(aes(age, friend_count_mean), data = pf.fc_by_age) +
  geom_point()

ggplot(aes(age, friend_count_mean), data = pf.fc_by_age) +
  geom_line()

ggplot(aes(x = age, y = friend_count), data = pf) +
  xlim(13, 90) +
  geom_point(alpha = 0.05,
             position = position_jitter(h = 0),
             color = 'orange') +
  coord_trans(y = 'sqrt')
  geom_line(stat = 'summary', fun.y = mean)
  

cor.test(pf$age, pf$friend_count, method = 'pearson')

with(pf, cor.test(age, friend_count, method = 'pearson'))

ggplot(aes(x = www_likes_received, y = likes_received), data = pf) +
  geom_point()

ggplot(aes(x = www_likes_received, y = likes_received), data = pf) +
  geom_point() +
  xlim(0, quantile(pf$www_likes_received, 0.95)) +
  ylim(0, quantile(pf$likes_received, 0.95))
  geom_smooth(method = 'lm', color = 'red')
  
with(pf, cor.test(www_likes_received, likes_received, methond = 'pearson'))

install.packages('alr3')
library(alr3)

install.packages(alr3)
library(alr3)

data(Mitchell)


suppressMessages(library(dplyr))
library(dplyr)
pf.fc_by_age_gender <- pf %>%
  filter(!is.na(gender)) %>%
  group_by(age, gender) %>%
  summarise(mean_friend_count = mean(friend_count),
            median_friend_count =median(friend_count),
            n = n ())%>%
  ungroup() %>%
  arrange(age)

head(pf.fc_by_age_gender)

ggplot(aes(x = age, y = median_friend_count),
       data = pf.fc_by_age_gender) +
  geom_line(aes(color = gender))

install.packages("tidyr")
library(tidyr)

spread(subset(pf.fc_by_age_gender, 
              select = c('gender', 'age', 'median_friend_count')), 
       gender, median_friend_count)

pf.fc_by_age_gender.wide <-
  subset(pf.fc_by_age_gender[c('age', 'gender', 'median_friend_count')],
         !is.na(gender)) %>%
  spread(gender, median_friend_count) %>%
  mutate(ratio = male / female)

head(pf.fc_by_age_gender.wide)

ggplot(aes(x = age, y = female/male),
       data = pf.fc_by_age_gender.wide) +
  geom_line()+
  geom_hline(yintercept = 1, alpha = 0.3, linetype = 2)


install.packages('GGally')
library(GGally)

theme_set(theme_minimal(20))
set.seed(1836)

pf_subset <- pf[, c(2:15)]
names(pf_subset)
ggpairs(pf_subset[sample.int(nrow(pf_subset), 1000), ])


