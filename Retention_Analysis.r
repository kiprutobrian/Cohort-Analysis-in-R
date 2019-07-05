library(RColorBrewer)
library(dplyr)
library(ggplot2)
library(reshape2)
cohort_users <- data.frame(cohort=c(‘january’, ‘feburary’, ‘march’, ‘april’, ‘may’, ‘june’, ‘july’, ‘august’, ‘september’, ‘october’, ‘november’, ‘december’),
 month1=c(133000,0,0,0,0,0,0,0,0,0,0,0),
 month2=c(3400,10300,0,0,0,0,0,0,0,0,0,0),
 month3=c(1100,3000,10500,0,0,0,0,0,0,0,0,0),
 month4=c(2300,4300,6200,7000,0,0,0,0,0,0,0,0),
 month5=c(4000,1100,1400,2400,9100,0,0,0,0,0,0,0),
 month6=c(300,500,3200,2200,2900,14000,0,0,0,0,0,0),
 month7=c(600,900,1100,1300,1400,1800,12000,0,0,0,0,0),
 month8=c(900,1200,1000,1200,1100,1300,1800,13600,0,0,0,0),
 month9=c(700,700,750,3400,2100,1330,1000,1400,12000,0,0,0),
 month10=c(820,780,800,1100,1350,1200,900,1400,1800,15200,0,0),
 month11=c(1000,750,900,1000,1000,1180,800,1100,1150,2000,12300,0),
 month12=c(650,700,870,800,600,1300,500,1150,1250,1300,1800,25000))
View(cohort_users)
cohort_users1 <- cohort_users 
totcols <- ncol(cohort_users1)
for (i in 1:nrow(cohort_users1)) { 
 df <- cohort_users1[i,] 
 df <- df[ , !df[]==0] 
 partcols <- ncol(df)
if (partcols < totcols) df[, c((partcols+1):totcols)] <- 0
 cohort_users1[i,] <- df 
}
#retention
a <- cohort_users1[,c(2:13)]
b <- cohort_users1[,2]
retention <- apply(a, 2, function(a) a/b )
retention <- data.frame(cohort=(cohort_users1$cohort), retention)
View(retention)
retention <- retention[,-2]
cohort_plot <- melt(retention, id.vars = ‘cohort’)
colnames(cohort_plot) <- c(‘cohort’, ‘month’, ‘retention’)
cohort_plot <- filter(cohort_plot, retention != 0)
c <- ggplot(cohort_plot, aes(x=month, y=retention, group=cohort, colour=cohort))
c <-c + geom_line(size=2, alpha=0.5) +
 geom_point(size=3, alpha=1) +
 geom_smooth(aes(group=1), method = ‘loess’, size=3, colour=”turquoise”, se=FALSE) +
 labs(title=”Cohorts Retention ratio”)
c + scale_colour_brewer(palette=”Set3") + theme(panel.background = element_blank())
#Third Month Retention Analysis
cohort_plot1 <- filter(cohort_plot, month==’month3') 
c <- ggplot(cohort_plot1, aes(x=cohort, y=retention, colour=cohort))
c <- c + geom_point(size=3) +
 geom_line(aes(group=1), size=2, alpha=0.9) +
 geom_smooth(aes(group=1), size=2, colour=’turquoise’, method = ‘lm’, se=FALSE) +
 labs(title=”How does the users retain in their third month?”)
c + scale_colour_brewer(palette=”Pastel2") + theme(panel.background = element_blank())
#cycle plot
cohort_cycle_plot <- cohort_plot
cohort_cycle_plot <- mutate(cohort_cycle_plot, month_cohort = paste(month, cohort))
cp <- ggplot(cohort_cycle_plot, aes(x=month_cohort, y=retention, group=month, colour=month))
d1 <- filter(cohort_cycle_plot, cohort==’august’)
d2 <- filter(cohort_cycle_plot, cohort==’september’)
cp <- cp + geom_point(size=3) +
 geom_line(aes(group=month), size=2, alpha=1/2) +
 labs(title=”Cycle plot of Cohorts Retention”) +
 geom_line(data=d1, aes(group=1), colour=’turquoise’, size=2, alpha=0.6) +
 geom_line(data=d2, aes(group=1), colour=’turquoise’, size=2, alpha=0.6) +
 theme(axis.text.x = element_text(angle = 90, hjust = 1))
cp + scale_colour_brewer(palette=”Spectral”) + theme(panel.background = element_blank())