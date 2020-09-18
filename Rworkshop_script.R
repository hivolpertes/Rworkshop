# R workshop given in SAM
# Sept 18, 2020

# Adapted from materials from Moin Syed, University of Minnesota
# https://osf.io/9gq4a/

# First Law of Learning R: 
# Most people do not really know what they are doing

# Second Law of Learning R: 
# Using R involves spending more time on Google looking for solutions than actually coding

### Google is your friend
### Use ? or ?? in console to look up packages
### Use documentation for packages
### Use Rstudio cheat sheets

# Third Law of Learning R:
# There is no "right" way to do anything in R. There are many different strategies and solutions to 
# achieve the same outcome

# Fourth Law of Learning R:
# When you're getting errors, always look first for typos
# Tip: R is cApS SenSItiVe
# https://twitter.com/algaebarnacle/status/1305858377537531909?s=20


# 1. Orientation to Rstudio -------------------------------------------------------------------


# 2. Read in data for practice example --------------------------------------------------------

# Example data set includes survey data from 316 people, mostly 18 y/o who identify as ethnic/racial 
# minorities
# Variables included:
# 1) Participant ID
# 2) Whether that participant was born in the US (yes/no)
# 3) Whether the participant was the first in their family to attend college (yes/no)
# 4) 12-item Multigruop Ethnic Identity Measure (MEIM; Roberts et al., 1999)
# 5) 5-item Satisfaction with Life Scale (SWL; Diener et al., 1999)

# get working directory
getwd()
# set working directory
setwd("/Users/hivolpertes/Documents/Administrative/UTEP/2020 Fall SAM/Rworkshop")
dat = read.table("EID_Data_MCAE2016.csv", sep=",", header=T)

# Note: I HIGHLY encourage you to use Projects so that you don't have to reset the working directory
# every time you open up Rstudio. Where-ever you save the Rproject file, it will use that as the 
# working directory


# 3. Load packages ----------------------------------------------------------------------------

library(tidyverse)
library(lme4)

# 3. View the data ----------------------------------------------------------------------------

View(dat)

head(dat)
?head
head(dat, 10)
tail(dat)

# indexing
dat[1,]
dat[,1]
dat[1,1]
dat[,4:15] # can select a range
dat[dat$meim01 < 4,] # can use logicals within indexing
filter(dat, meim01 < 4) # equivalent
# most common error in indexing is including or not including the comma
dat[dat$ID == "MCAE16264"] # needs comma after second quote mark

# variable types
glimpse(dat)
str(dat)

# use $ to select variables
dat$ID # in this case, equivalent to dat[1,]
dat$firstgen

# 4. Data wrangling ---------------------------------------------------------------------------

# Data transformation with dplyr cheat sheet is very helpful

# change type
dat$ID = factor(dat$ID)
glimpse(dat)

# create new variable
dat$newVariable = rep(c(3,4)) 
head(dat)

dat = mutate(dat, newerVariable = newVariable + 2)
head(dat)

dat = mutate(dat, newestVariable = newVariable + newerVariable)
head(dat)

# check for missing data (denoted by "NA")
View(dat)
dat[dat$ID == "MCAE16002",]
is.na(dat$meim01[dat$ID == "MCAE16002"])
sum(is.na(dat[dat$ID == "MCAE16002",]))

# start over, create sum scores for MEIM and SWL
dat = read.table("EID_Data_MCAE2016.csv", sep=",", header=T)

dat = mutate(dat, meim_mean = (meim01 + meim02 + meim03 + meim04 + meim05 + meim06 + meim07 + 
                                meim08 + meim09 + meim10 + meim11 + meim12)/12)

dat = mutate(dat, swl_mean = (swl01 + swl02 + swl03 + swl04 + swl05)/5)

# write data to new file
write.table(dat, "EID_Data_MCAE2016_sumscores.txt", sep="\t", row.names=F)


# 5. Data descriptives ------------------------------------------------------------------------

# histograms
hist(dat$meim_mean)
hist(dat$swl_mean)

# max, min, mean
max(dat$meim_mean) # need to include na.rm = T as argument
min(dat$meim_mean)
mean(dat$meim_mean)

max(dat$swl_mean, na.rm=T)
min(dat$swl_mean, na.rm=T)
mean(dat$swl_mean, na.rm=T)

# means as table
dat$uborn = factor(dat$usborn)
means = select(dat, usborn, meim_mean, swl_mean) %>% # uses pipe command
  group_by(usborn) %>% 
  summarise_all(mean, na.rm=T) 
# equivalent without piping is:
means = as.data.frame(summarise_all(group_by(select(dat, usborn, meim_mean, swl_mean), usborn), 
                                    mean, na.rm=T))

# prettier histograms
ggplot(dat, aes(meim_mean)) +
  geom_histogram(color = "black", fill = "white") +
  theme_bw() +
  ggtitle("Histogram of ethnic identity") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_vline(aes(xintercept=mean(meim_mean, na.rm=T)),
             color = "blue", linetype = "dashed", size = 2)

# can group data
ggplot(dat, aes(meim_mean, fill = usborn)) +
  geom_histogram(color = "black", position = "dodge") +
  theme_bw() +
  ggtitle("Histogram of ethnic identity by nativity") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_vline(data = means, (aes(xintercept = meim_mean, linetype = usborn)),
             color = "black", size = 2)

# Other ideas: correlation tables

# 6. Plotting relationships -------------------------------------------------------------------

# relationship between ethnic identity and satisfaction with life
ggplot(dat, aes(x=meim_mean, y = swl_mean)) +
  geom_point() +
  geom_smooth() #default is loess

ggplot(dat, aes(x=meim_mean, y = swl_mean)) +
  geom_point() +
  geom_smooth(method = "lm") # for linear relationship

# group by nativity
ggplot(dat, aes(x=meim_mean, y = swl_mean, color = usborn)) +
  geom_point() +
  geom_smooth(method = "lm")

# group by first gen status
ggplot(dat, aes(x=meim_mean, y = swl_mean, color = firstgen)) +
  geom_point() +
  geom_smooth(method = "lm")


# 7. Test relationships -----------------------------------------------------------------------

# Correlation between ethnic identity and satisfaction with life
cor(dat$meim_mean, dat$swl_mean, use="complete.obs") # gives only correlation coefficient
cor.test(dat$meim_mean, dat$swl_mean, na.action = na.omit) # gives statistical test

# regression with standardized variables replicates correlation
lm(scale(swl_mean) ~ scale(meim_mean), data = dat) %>% 
  summary()

# look at interaction with usborn (usborn automatically treated as a factor)
lm(scale(swl_mean) ~ scale(meim_mean)*usborn, data = dat) %>% 
  summary()

# look at interaction with firstgen
lm(scale(swl_mean) ~ scale(meim_mean)*firstgen, data = dat) %>% 
  summary()


# 8. Plotting means ---------------------------------------------------------------------------

# plot mean differences in MEIM by usborn, firstgen status
MEIM_summarydat = select(dat, usborn, firstgen, meim_mean) %>% # uses pipe command
  group_by(usborn, firstgen) %>% 
  summarise(mean = mean(meim_mean, na.rm=T), 
            se = sd(meim_mean, na.rm=TRUE)/sqrt(length(na.omit(meim_mean)))) 

ggplot(MEIM_summarydat, aes(x = usborn, y = mean, fill = firstgen)) +
  geom_bar(stat="identity", position = "dodge", color = "black") +
  scale_fill_manual(values = c("lightblue", "steelblue")) +
  theme_bw()

# add error bars
ggplot(MEIM_summarydat, aes(x = usborn, y = mean, fill = firstgen)) +
  geom_bar(stat="identity", position = "dodge", color = "black") +
  scale_fill_manual(values = c("lightblue", "steelblue")) +
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), position = position_dodge(width = .9), 
                width = .3) +
  ggtitle("Ethnic identity") +
  theme(plot.title = element_text(hjust = 0.5))

# same thing but for SWL
SWL_summarydat = select(dat, usborn, firstgen, swl_mean) %>% # uses pipe command
  group_by(usborn, firstgen) %>% 
  summarise(mean = mean(swl_mean, na.rm=T), 
            se = sd(swl_mean, na.rm=TRUE)/sqrt(length(na.omit(swl_mean)))) 

# plot
ggplot(SWL_summarydat, aes(x = usborn, y = mean, fill = firstgen)) +
  geom_bar(stat="identity", position = "dodge", color = "black") +
  scale_fill_manual(values = c("lightgreen", "forestgreen")) +
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se), position = position_dodge(width = .9), 
                width = .3) +
  ggtitle("Satisfaction with life") +
  theme(plot.title = element_text(hjust = 0.5))
  

# 9. Test difference in means -----------------------------------------------------------------

# base R
aov(meim_mean ~ usborn*firstgen, data = dat) %>% # displays Type I SS
  summary() 
  
# can use car package to get Type III SS
library(car)
Anova(lm(meim_mean ~ usborn*firstgen, data = dat), type="III")

# this is different from anova() in stats package
anova(lm(meim_mean ~ usborn*firstgen, data = dat)) # also Type I SS

# same thing but for swl
Anova(lm(swl_mean ~ usborn*firstgen, data = dat), type="III")

# Good resource for testing assumptions: https://www.datanovia.com/en/lessons/anova-in-r/
# uses anova_test() from rstatix package
# procedure for testing simple main effects in presence of significant interaction

# when doing anovas, be aware of whether your categorical variables are factor and how the function
# deals with that

################################################################################################
# R can be really frustrating at times...
# https://qph.fs.quoracdn.net/main-qimg-6b83ad0a472c63f5da778da420dcf04b
# BUT it is very very useful AND could get you a job later. Stick with it! And I'm always here to help!
################################################################################################


