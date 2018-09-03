require(ggplot2)
require(scales)

setwd("~/git/mirna-expression-profiles")

data <- read.table('output/pgm_solid_mirna_table.csv', sep=',', header=T)
data$group <- factor(data$group, levels=c('PGM', 'miRNA', 'SOLID'))

pgm_sld <- read.table('output/pgm_sld_test.csv', sep=',', header=T)

pgm_mir <- read.table('output/pgm_mir_test.csv', sep=',', header=T) 

sld_mir <- read.table('output/sld_mir_test.csv', sep=',', header=T) 

limits <- list()

limits_1 <- data.frame(edge_type=unique(data['edge_type']))
limits_2 <- limits_1

k = 1
#values = list(c(0.75, 1.00, -2), c(1.00, 1.25, -2), c(0.75, 1.25, -5.))
values = list(c(0.7, 1.00, -2), c(1.00, 1.3, -2), c(0.7, 1.3, -5.))

for (item in values) {
  tmp_1 = limits_1; tmp_1$x = item[1]; tmp_1$degree = item[3]
  tmp_2 = limits_2; tmp_2$x = item[2]; tmp_2$degree = item[3]
  limits[[k]] <- rbind(tmp_1, tmp_2)
  k <- k + 1
}

method <- 'BH'
pgm_sld$ztest = p.adjust(pgm_sld$ztest, method=method)
pgm_mir$ztest_two.sided = p.adjust(pgm_mir$ztest_two.sided, method=method)
sld_mir$ztest_two.sided = p.adjust(sld_mir$ztest_two.sided, method=method)

data_summary <- function(x) {
  m <- mean(x)
  ymin <- m-sd(x)
  ymax <- m+sd(x)
  return(c(y=m,ymin=ymin,ymax=ymax))
}

p <- 
  ggplot(data) + 
  #geom_boxplot(aes(x=1, y=degree, fill=group), outlier.size = .5) +
  geom_violin(aes(x=1, y=degree, fill=group)) +
  theme_bw() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(), 
        text = element_text(size=10),
        legend.position='bottom',
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  xlab(NULL) + ylab('Node Degree') +
  scale_fill_discrete(name='Group', labels=c('PGM', 'miRBase', 'SOLiD')) +
  ylim(-8, NA)

for (limit in limits)
  p <- p + 
           geom_line(data = limit, aes(x, degree), lty = 2, size=.3) +
           geom_point(data = limit, aes(x, degree), shape = 73, size=2)

p <- p + geom_label(aes(x=1, y=-6, label=round(ztest, 3), group=edge_type), data=pgm_sld, size=2.7, lineheight=.001, label.padding = unit(0.15, "lines"), alpha=.75) +
         geom_label(aes(x=.85, y=-2, label=round(ztest_two.sided, 3), group=edge_type), data=pgm_mir, size=2.7, label.padding = unit(0.15, "lines"), alpha=.75) +
         geom_label(aes(x=1.15, y=-2, label=round(ztest_two.sided, 3), group=edge_type), data=sld_mir, size=2.7, label.padding = unit(0.15, "lines"), alpha=.75)
p <- p + facet_wrap(~ edge_type, scales = 'free', ncol = 3)

p <- p + stat_summary(aes(x=1, y=degree, fill=group), fun.data=data_summary, position=position_dodge(.9), size=.3)

p
