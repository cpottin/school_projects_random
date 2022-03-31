getwd()
list.files()


rw <- read.csv('wineQualityReds.csv')
names(rw)

install.packages('ggplot2')
library('ggplot2')

names(rw)
qplot(data = rw, x = quality, fill = I('#F79420'), binwidth = 1)
summary(rw)

length(unique(rw$quality))
table(rw$quality)

qplot(x = pH, data = rw)
qplot(x = quality, data = rw)
qplot(x = density, data = rw, binwidth = 0.0005)
qplot(x = alcohol, data = rw, binwidth = 0.5)
qplot(x = sulphates, data = rw, binwidth = 0.05)
  
ggplot(aes(x = density, y = sulphates), data = rw) +
  geom_jitter(alpha = 1/5)
  ##xlim(13, 90)

with(rw, cor.test(quality, density, method = 'pearson'))
##-0.1749192

with(rw, cor.test(quality, alcohol, method = 'pearson'))
##0.4761663

with(rw, cor.test(quality, fixed.acidity, method = 'pearson'))
##0.1240516

with(rw, cor.test(quality, sulphates, method = 'pearson'))
##0.2513971

with(rw, cor.test(quality, pH, method = 'pearson'))

with(rw, cor.test(quality, residual.sugar, method = 'pearson'))
##0.01373164

with(rw, cor.test(quality, volatile.acidity, method = 'pearson'))
##-0.3905578

##yo <- transform(yo, all.purchases = strawberry + blueberry + pina.colada + plain + mixed.berry)
##summary(yo$all.purchases)
##qplot(data = yo, x = all.purchases, binwidth = 1, fill = I('#F79420'))

##set.seed(1000)
##mayneed to change wine_is to a Factor
##rw.sample.ids <- sample(levels(rw$wine_id), 16)

##ggplot(aes(x = time, y = price), 
       ##data = subset(yo, id %in% sample.ids)) +
  ##facet_wrap(~ id) +
  ##geom_line() +
  ##geom_point(aes(size = all.purchases), pch =1)

install.packages('GGally')
install.packages('scales')
install.packages('memisc')
install.packages('lattice')
install.packages('MASS')
install.packages('car')
install.packages('reshape')
install.packages('plyr')

library(ggplot2)
library(GGally)
library(scales)
library(memisc)

ggpairs(rw,
        lower = list(continuous = wrap("points", shape = I('.'))),
        upper = list(combo = wrap("box", outlier.shape = I('.'))))

qplot(x = quality, y = volatile.acidity, 
      data = rw),
  geom = 'boxplot', ylim = c(0, 1000)

rw$quality.f <- factor(rw$quality, levels = c('3', '4', '5', '6', '7', '8', '9'))

ggplot(aes(x = quality.f), data = rw) +
  geom_histogram(stat = 'count', fill = I('#b30000')) +
  xlab('quality') +
  ggtitle('Distribution of Wine Qualities') + 
  theme(text = element_text(size = 10))

ggplot(aes(x = alcohol, y = total.sulfur.dioxide/volatile.acidity), data = rw) +
  geom_point(aes(color = 'red')) +
  ylab('sulphates/volatile acidity') +
  xlab('alcohol, % by volume') +
  scale_y_log10() +
  scale_x_log10() +
  scale_color_brewer(type = 'seq', palette = 'YlOrBr') +
  ggtitle('Alcohol vs. Sulphates/Volatile acidity') +
  theme(text = element_text(size = 10))

ggplot(aes(x = alcohol, y = chlorides/volatile.acidity), data = rw) +
  geom_point(aes(color = 'quality')) +
  ylab('chlorides/volatile acidity') +
  xlab('alcohol, % by volume') +
  scale_y_log10() +
  scale_x_log10() +
  scale_color_brewer(type = 'seq', palette = 'Reds') +
  ggtitle('Alcohol vs. Chlorides/Volatile acidity in All Wines') +
  theme(text = element_text(size = 10))

ggplot(aes(x = volatile.acidity, y = pH, colour = quality.f), data = rw) + 
  geom_point(alpha = 0.5, size = 1,  position = 'jitter') +
  scale_color_brewer(type = 'div',
                     guide = guide_legend(title = 'Quality', reverse = T,
                                          override.aes = list(alpha = 1, size = 2))) +  
  scale_x_continuous(limits = c(0.2, 1),
                     breaks = c(0.2, 0.4, 0.6, 0.8, 1)) + 
  scale_y_continuous(limits = c(2.5, 4.5),
                     breaks = c(2.5, 3, 3.5, 4, 4.5, 5)) +
  ggtitle('pH')

ggplot(aes(x = volatile.acidity, y = pH, colour = quality.f), data = rw) + 
  geom_point(alpha = 0.5, size = 1,  position = 'jitter') +
  scale_color_brewer(type = 'div',
                     guide = guide_legend(title = 'Quality', reverse = T,
                                          override.aes = list(alpha = 1, size = 2))) +  
  scale_x_continuous(limits = c(0.2, 0.6),
                     breaks = c(0.2, 0.3, 0.4, 0.5, 0.6)) + 
  scale_y_continuous(limits = c(2.5, 3.75),
                     breaks = c(2.75, 3, 3.25, 3.5, 3.75, 4.0)) +
  ggtitle('pH')