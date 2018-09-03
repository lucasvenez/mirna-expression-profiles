require(ggplot2)
require(scales)

setwd("~/git/mirna-expression-profiles")

data <- read.table('input/p_values.csv', sep=',', header=T)

data$line = 0.5

p <- 
  ggplot(data) + 
  geom_bar(aes(x=class, y=p.adjust(ztest_two.sided, 'BH'), fill=group), 
           stat="identity", position=position_dodge()) + 
  #geom_hline(yintercept=1.3) +
  #scale_y_continuous(labels=percent) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        text = element_text(size=10)) +
  xlab('Node Degree') + ylab(expression(paste('p-value'))) +
  scale_fill_discrete(name='Group')

p
