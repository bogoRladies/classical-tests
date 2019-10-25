setwd("D:\\[46] Workshop\\Classical tests")
#t test case study
# We assume the daily energy intake in kJ for 11 gorillas to be:  
  
daily.intake<-c(5260,5470,5640,6180,6390,6515,
                6805,7515,7515,8230,8770)
summary(daily.intake) #summary statistics

#Is the intake value deviates from a recommended value of 7725 kJ?  
  
t.test(daily.intake,mu=7725)

#Two-sample t-test case study
#We want to calculate whether there are differences between the energy expend by obese gorilla vs lean gorilla.

library(ISwR)
attach(energy)
View(energy)
head(energy,4)
t.test(expend~stature,var.equal=T) #expend (energy spent) is described by stature (height)
#we assume the variance is the same in two groups
detach(energy)
   
#Paired t-test
#Difference between the energy intake pre and post-release into the wild (after captured by hunters) in a group of gorillas.    

attach(intake)
View(intake)
head(intake,3)
t.test(pre,post, paired=T)
#the paired=T is important because it indicates that the sample
#comes from the same experiment
detach(intake)

#Analysis of variance (ANOVA)
# One-way ANOVA case study
#Example data of "red cell folate" (Altman 1991, p. 208). 
#But assume the data is about the number of Pangolin scales confiscated in year 2013, 2014, and 2015. 

attach(red.cell.folate)
pangolin<-red.cell.folate
levels(pangolin$ventilation) <- list(year2013="N2O+O2,24h", year2014="N2O+O2,op", year2015="O2,24h")
pangolin$scales<-pangolin$folate
pangolin$year<-pangolin$ventilation
library(tidyverse)
pangolin1<-pangolin %>% select(3:4)

head(pangolin1,3)
View(pangolin1)
anova(lm(pangolin1$scales~pangolin1$year))

summary(lm(pangolin1$scales~pangolin1$year))

pairwise.t.test(pangolin1$scales, pangolin1$year, p.adj="bonferroni")

#We can relax the assumption of equal variances for all groups by using the `oneway.test`

oneway.test(pangolin1$scales~pangolin1$year)

xbar <- tapply(pangolin1$scales, pangolin1$year, mean)
s <- tapply(pangolin1$scales, pangolin1$year, sd)
n <- tapply(pangolin1$scales, pangolin1$year, length)
sem <- s/sqrt(n)
stripchart(pangolin1$scales~pangolin1$year, method="jitter", jitter=0.05, pch=16, vert=T)
arrows(1:3,xbar+sem,1:3,xbar-sem,angle=90,code=3,length=.1)
lines(1:3,xbar,pch=4,type="b",cex=2)

# Bartless test for checking variance

bartlett.test(pangolin1$scales~pangolin1$year)
 
# Two-way ANOVA case study
# A fake dataset: mean daily catch of Silky shark (*Carcharhinus falciformis*) in January 2017. 
# We want to see the difference of catch by area (Meulaboh, Sigli and Biereun) (*factor 1*) 
# depending on the whether the shark is female/male (*factor 2*).     
# Two-way ANOVA case study

calci<-read.csv("calci_fake_data.csv")
names(calci)
View(calci)
library(multcompView)
library(lsmeans)

calci.aov<-aov(catch.per.day~sex+area,data=calci)
summary(calci.aov)

calci2.aov<-aov(catch.per.day~sex+area+sex:area,data=calci)
calci2.aov<-aov(catch.per.day~sex*area,data=calci)
summary(calci2.aov)

shapiro.test(calci$catch.per.day)

model.tables(calci.aov, type="means", se=TRUE)
model.tables(calci2.aov, type="means", se=TRUE) 

#We compute Tukey HSD (Honest Significant Differences) to perform multiple pairwise-comparison between the means of groups. 

TukeyHSD(calci.aov, which = "area")


# Two-way ANOVA case study

TukeyHSD(calci2.aov, which = "area")

# Checking assumptions of ANOVA

plot(calci2.aov, 1)

# Checking assumptions of ANOVA
# Use Levene’s test to check the homogeneity of variances. 

library(car)
leveneTest(catch.per.day~sex*area,data=calci)

# Checking assumptions of ANOVA

plot(calci2.aov, 2)

# Extract the residuals
aov_residuals <- residuals(object = calci2.aov)
# Run Shapiro-Wilk test
shapiro.test(x = aov_residuals )

calci.unbal <- aov(catch.per.day~sex*area,data=calci)
Anova(calci.unbal, type = "III")

library(ggpubr)
# Box plot with multiple groups
# +++++++++++++++++++++
# Plot tooth length ("catch.per.day" by groups ("sex")
# Color box plot by a second group: "area"
ggboxplot(calci, x = "area", y = "catch.per.day", color = "sex",
          palette = c("#00AFBB", "#E7B800"))

# ANCOVA case study

confs<-read.csv("bb_fake.csv")
head(confs,3)

plot(x   = confs$evidence.weight, 
     y   = confs$prison.time, 
     col = confs$species, 
     pch = 16,
     xlab = "Prison time",
     ylab = "Evidence weight ")

legend('bottomright', 
       legend = levels(confs$species), 
       col = 1:2, 
       cex = 1,    
       pch = 16)

model.1 = lm (prison.time ~ evidence.weight + species + evidence.weight:species,
              data = confs)
Anova(model.1, type="II")

model.2 = lm (prison.time ~ evidence.weight + species,
              data = confs)
Anova(model.2, type="II")

summary(model.2)

I.nought = -7.21091
I1 = I.nought + 0
I2 = I.nought + -10.06529
B  = 3.60275

plot(x   = confs$evidence.weight, 
     y   = confs$prison.time, 
     col = confs$species, 
     pch = 16,
     xlab = "Evidence weight",
     ylab = "Prison time")

legend('bottomright', 
       legend = levels(confs$species), 
       col = 1:2, 
       cex = 1,    
       pch = 16)
abline(I1, B,
       lty=1, lwd=2, col = 1)

abline(I2, B,
       lty=1, lwd=2, col = 2)

# Non-parametric test

# Wilcoxon test : One-sample Wilcoxon 

#daily energy intake in kJ for 11 gorillas to be:
t.test(daily.intake,mu=7725) #parametric
# Wilcoxon test : One-sample Wilcoxon

wilcox.test(daily.intake,mu=7725) #non-parametric

# Two-sample Wilcoxon signed-rank test
# Similar to two-sample t-test   

attach(energy)
#Differentiate the energy expend difference for obese gorilla vs lean gorilla.
t.test(expend~stature)#parametric
wilcox.test(expend~stature)#non parametric

# Paired Wilcoxon test

#Difference between the energy intake pre and post-release 
#into the wild (after captured by hunters) in a group of gorillas.
attach(intake)
t.test(pre,post, paired=T) #parametric

# Paired Wilcoxon test

wilcox.test(pre,post, paired=T)#non parametric
detach(intake)

# Kruskal Wallis test
#'One way ANOVA' for non-parametric data.

oneway.test(pangolin1$scales~pangolin1$year) #parametric
kruskal.test(pangolin1$scales~pangolin1$year) #non-parametric

# Chi-square test of independence

songbird<-read.csv("fake_songbird.csv",row.names = 1)
View(songbird)
library("gplots")
# 1. convert the data as a table
dt <- as.table(as.matrix(songbird))
# 2. Graph
balloonplot(t(dt), main ="Provinces", xlab ="", ylab="",
            label = FALSE, show.margins = FALSE)

mosaicplot(dt, shade = TRUE, las=2,
           main = "Provinces")

# Chi-square test of independence case study

chisq<-chisq.test(songbird)
chisq

chisq$observed #observed counts

chisq$expected #expected counts

chisq$residuals

library(corrplot)
corrplot(chisq$residuals, is.cor=FALSE)


# Chi square goodness of fit test in R

age<-c(81,50,27)
res<-chisq.test(age, p = c(1/3, 1/3,1/3)) #expected
res$expected

age <- c(81, 50, 27)
res <- chisq.test(age, p = c(1/2, 1/3, 1/6))
res

# Practice!

* What test to use on this dataset? 
  + "fake_deforestation.csv"
  + % soil organic carbon content (numeric) & deforestation rate (3 levels) 
  + check the assumption, then decide which test! 

# Practice!

* What test to use on this dataset? 
  + "fake.pangolin.csv"
  + Years of experience of the criminal, origin (province) of the criminal, weight of pangolin scale confiscated
  + check the assumption, then decide which test!
  + This is originally dataset `ToothGrowth` in R.

# Practice!

* What test to use on this dataset? 
  + "fake.manta.catch.csv"
  + Manta ray's total body length (TL in cm) is measured a year before MPA, and a year after MPA. Is there any difference?
  + check the assumption, then decide which test!
  + Original dataset, check reference list.

