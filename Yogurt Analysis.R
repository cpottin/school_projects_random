getwd ()

yo <- read.csv('yogurt.csv')
str(yo)

yo$id <- factor(yo$id)

install.packages('ggplot2')
library('ggplot2')
qplot(data = yo, x = price, fill = I('#F79420'))
length(unique(yo$price))
table(yo$price)
summary(yo)

yo <- transform(yo, all.purchases = strawberry + blueberry + pina.colada + plain + mixed.berry)
summary(yo$all.purchases)
qplot(data = yo, x = all.purchases, binwidth = 1, fill = I('#F79420'))

ggplot(aes(x = time, y = price), data = yo) + 
  geom_jitter(alpha = 1/4, shape = 21, fill = I('#F79420'))
##set.seed sets the starting number for a random set
set.seed(4230)
sample.ids <- sample(levels(yo$id), 16)

ggplot(aes(x = time, y = price), 
  data = subset(yo, id %in% sample.ids)) +
  facet_wrap(~ id) +
  geom_line() +
  geom_point(aes(size = all.purchases), pch =1)
