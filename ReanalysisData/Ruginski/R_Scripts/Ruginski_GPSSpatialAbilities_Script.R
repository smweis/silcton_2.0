## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
knitr::opts_chunk$set(message = FALSE)


## ---- message = FALSE----------------------------------------------------
#loading libraryd packages 
library(tidyverse)
library(broom)
library(knitr)
library(kableExtra)
options(digits=2)
library(psych)
library(car)
library(lavaan)
library(tidylog)
library(Hmisc)

#read in data
clean <- read.csv("ian_diss_cleaneddata.csv")
pt <- read.csv("PerspectiveTakingDataRaw.csv")

#cleaning PT data 
#NOTE: If a participant leaves an item blank, they should be assigned an angular deviation of 90 degrees for that item (90 represents chance performance, given that the angular deviation can vary from 0 to 180) (this was done when entering data)

#If the angular deviation on any item exceeds 180, it should be subtracted from 360 to determine the smallest deviation possible. This is because the angular deviation between two angles does not exceed 180 degrees. 

pt$SOTe1c <- ifelse(pt$SOTe1>=180, 360-pt$SOTe1, pt$SOTe1)
pt$SOTe2c <- ifelse(pt$SOTe2>=180, 360-pt$SOTe2, pt$SOTe2)
pt$SOTe3c <- ifelse(pt$SOTe3>=180, 360-pt$SOTe3, pt$SOTe3)
pt$SOTe4c <- ifelse(pt$SOTe4>=180, 360-pt$SOTe4, pt$SOTe4)
pt$SOTe5c <- ifelse(pt$SOTe5>=180, 360-pt$SOTe5, pt$SOTe5)
pt$SOTe6c <- ifelse(pt$SOTe6>=180, 360-pt$SOTe6, pt$SOTe6)
pt$SOTe7c <- ifelse(pt$SOTe7>=180, 360-pt$SOTe7, pt$SOTe7)
pt$SOTe8c <- ifelse(pt$SOTe8>=180, 360-pt$SOTe8, pt$SOTe8)
pt$SOTe9c <- ifelse(pt$SOTe9>=180, 360-pt$SOTe9, pt$SOTe9)
pt$SOTe10c <- ifelse(pt$SOTe10>=180, 360-pt$SOTe10, pt$SOTe10)
pt$SOTe11c <- ifelse(pt$SOTe11>=180, 360-pt$SOTe11, pt$SOTe11)
pt$SOTe12c <- ifelse(pt$SOTe12>=180, 360-pt$S2Te10, pt$SOTe12)

pt <- select(pt, SOTe1c:SOTe12c) #subset newly coded items
pt$SOTmean <- rowMeans(select(pt, SOTe1c:SOTe12c))

#add PT data to full dataframe
clean <- cbind(clean, pt$SOTmean) 
#change names
colnames(clean)[87] <- "SOTmean"

#make aggregate distance/pointing measures
clean$Pointing <- (clean$BetweenRtPt + clean$WithinRtPt) / 2
clean$Distance <- (clean$BetweenDist + clean$WithinDist) / 2

#make GPS variable numeric
clean$GPSuse <- as.numeric(clean$Q14)

#below code writes clean datafile
#write.csv(clean, "ian_diss_cleaneddatawithpt.csv")

#add bidimensional regression cognitive map data
bidim <- read.csv("bidimregression_results.csv")
bidimrsq <- select(bidim, id, r.2) #extract rsquared column only (variance explained in cog maps)
cleanr <- merge(clean, bidimrsq, by="id")#make new dataframe w bidim data

#make inverse scores so that higher means better ability for all variables consistently
cleanr$SOTmeanI <- -cleanr$SOTmean 
cleanr$BetweenRtPtI <- -cleanr$BetweenRtPt
cleanr$WithinRtPtI <- -cleanr$WithinRtPt
cleanr$DistanceI <- -cleanr$Distance

## ----message=FALSE-------------------------------------------------------
library(moments)

#make APA style Descriptives table
statdesc <- cleanr %>%
  summarise_at(vars(SBtotal, GPSuse, MRT, SOTmean, WithinRtPt:BetweenRtPt, 
                    Distance:r.2, Q1, Q4, UTdist),
                    list(~mean(.), ~sd(.), ~skewness(.), ~kurtosis(.)), na.rm=TRUE) %>% 
    gather("Variable", "Value") %>%  
    separate(Variable, c("Variable", "Statistic"), "_") %>%
    spread("Statistic", "Value") %>% 
  select(Variable, mean, sd, skewness, kurtosis) %>% 
  arrange(factor(Variable, levels = c("SBtotal", "GPSuse", "MRT", "SOTmean", 
                                      "WithinRtPt", "BetweenRtPt", 
                                      "Distance", "r.2", "Q1", "Q4", "UTdist")))

kable(statdesc) %>% 
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))
write.table(format(statdesc, digits=2), file = "statdesc_diss.txt", sep = ",", quote = FALSE, row.names = F)

#correlations
cor.df <- cleanr %>%
  select(SBtotal, GPSuse, MRT, SOTmean, WithinRtPt:BetweenRtPt, Distance:r.2, Q1, Q4, UTdist) %>% 
  as.matrix() %>%
  rcorr(type = "spearman")
options(scipen=10)

rcors <- kable(cor.df$r) %>% 
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))
write.table(formatC(cor.df$r, digits=2), 
            file = "rcors_diss.txt", sep = ",", quote = FALSE, row.names = T)
rcors

pcors <- kable(cor.df$P) %>% 
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))
pcors
write.table(formatC(cor.df$P, digits=2, format="g"),
            file = "pcors_diss.txt", sep = ",", quote = FALSE, row.names = T)

 

## ------------------------------------------------------------------------
cleanr$GenderC <- factor(cleanr$Gender, levels=c("Female", "Male"))

sex.t <- cleanr %>% 
  select(SBtotal, GPSuse, MRT, 
         SOTmean, WithinRtPt:BetweenRtPt, 
         Distance:r.2, Q1, Q4, UTdist) %>%
  map(~tidy(t.test(. ~ cleanr$GenderC))) %>% 
  bind_rows() %>% 
  mutate(Variable = c("SBtotal", "GPSuse", "MRT", "SOTmean",
                "WithinRtPt", "BetweenRtPt",
                "Distance", "r.2", "Q1", "Q4", "UTdist"))
kable(sex.t) %>% 
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))


sex.d <- cleanr %>% 
  select(GenderC, SBtotal, GPSuse, MRT, 
         SOTmean, WithinRtPt:BetweenRtPt, 
         Distance:r.2, Q1, Q4, UTdist) %>%
  cohen.d(., "GenderC") %>% .$cohen.d

kable(sex.d) %>% 
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))


statdesc.sex <- cleanr %>% filter(GenderC != "NA") %>% 
  group_by(GenderC) %>% 
  summarise_at(vars(SBtotal, GPSuse, MRT, SOTmean, WithinRtPt:BetweenRtPt, 
                    Distance:r.2, Q1, Q4, UTdist),
                    list(~mean(.), ~sd(.)), na.rm=TRUE)

kable(statdesc.sex) %>% 
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))


## ---- echo=TRUE----------------------------------------------------------
#lets look at the variable distributions determine estimator... probably have to use MLR due to GPS-use
#get descriptives
describe(cleanr$GPSuse)
#plot GPS counts as well
theme_set(theme_classic(base_size=18))
ggplot(cleanr, aes(x=as.factor(GPSuse), fill=as.factor(GPSuse))) + geom_bar(alpha=.8) +
  labs(title="Count Plot", 
       subtitle="GPS Use",
       x="Self-reported GPS Use") + guides(fill=guide_legend(title="Likert scale")) + scale_y_continuous(breaks=seq(0, 80, 15)) + ylab("Number of participants") + scale_x_discrete(labels=c("1" = "Never", "2" = "Rarely", "3" = "Sometimes", "4" = "Often", "5" = "Always"))
#ggsave("GPSuse_distribution.png", width=8, height=6)

## ---- echo=TRUE----------------------------------------------------------
btwmodel <- lm(WithinRtPt ~ MRT + SOTmeanI, cleanr)
vif(btwmodel)

## ---- echo=TRUE, warning=FALSE-------------------------------------------
#GPS preliminary models
model.gps.env <- 'EnvLearn =~ NA*BetweenRtPtI + WithinRtPtI + DistanceI + r.2
EnvLearn ~~ 1*EnvLearn
EnvLearn~GPSuse + Q1 + Q4 + UTdist'
fit.gps.env <- sem(model.gps.env, data = cleanr, missing="FIML", estimator="MLR")
summary(fit.gps.env, standardized=TRUE)

model.gps.mrt <- '
MRT~GPSuse + Q1 + Q4 + UTdist'
fit.gps.mrt <- sem(model.gps.mrt, data = cleanr,missing="FIML", estimator='MLR')
summary(fit.gps.mrt, standardized=TRUE)

model.gps.pt <- '
SOTmean~GPSuse + Q1 + Q4 + UTdist'
fit.gps.pt <- sem(model.gps.pt, data = cleanr,missing="FIML", estimator='MLR')
summary(fit.gps.pt, standardized=TRUE)

model.gps.mob <- '
GPSuse~Q1 + Q4+UTdist'
fit.gps.mob <- sem(model.gps.mob, data = cleanr,missing="FIML", estimator='MLR')
summary(fit.gps.mob, standardized=TRUE)

## ---- echo=TRUE, warning=FALSE-------------------------------------------
model.measurement <- 'EnvLearn =~ NA*BetweenRtPt + WithinRtPt + r.2 + Distance
EnvLearn ~~ 1*EnvLearn'
fit.measurement <- sem(model.measurement, data = cleanr, missing="FIML", estimator="ML")
summary(fit.measurement, fit.measures = TRUE, standardized=TRUE)

#fits pretty well & all observed vars load

## ----model1, echo=TRUE, warning=FALSE------------------------------------
#model 1: less saturated (no path from MRT to PT)
model.lesssat <- 'EnvLearn =~ NA*BetweenRtPtI + WithinRtPtI + DistanceI + r.2
SOTmeanI ~ GPSuse + 0*MRT + SBtotal
MRT ~ GPSuse + SBtotal
GPSuse ~ SBtotal
EnvLearn ~ MRT + SOTmeanI + GPSuse + SBtotal
EnvLearn ~~ 1*EnvLearn'
fit.lesssat <- sem(model.lesssat, data = cleanr, estimator="MLR", missing="FIML")
summary(fit.lesssat, fit.measures=TRUE, standardized=TRUE)

## ----model2, echo=TRUE, warning=FALSE------------------------------------
#model 2: more saturated (path from MRT to PT)
model.moresat <- 'EnvLearn =~ NA*BetweenRtPtI + WithinRtPtI + DistanceI + r.2
SOTmeanI ~ GPSuse + MRT + SBtotal
MRT ~ a*GPSuse + SBtotal
GPSuse ~ SBtotal
EnvLearn ~ MRT + SOTmeanI + GPSuse + SBtotal
EnvLearn ~~ 1*EnvLearn'
fit.moresat <- sem(model.moresat, data = cleanr, estimator="MLR", missing="FIML")
summary(fit.moresat, fit.measures=TRUE, standardized=TRUE)

#model 2b: more saturated, but diff direction (path from transforms to GPS)
#shows that directionality does not matter
model.moresat.altdir <- 'EnvLearn =~ NA*BetweenRtPtI + WithinRtPtI + DistanceI + r.2
SOTmeanI ~ MRT + SBtotal
MRT ~ SBtotal
GPSuse ~ SBtotal + MRT + SOTmeanI
EnvLearn ~ MRT + SOTmeanI + GPSuse + SBtotal
EnvLearn ~~ 1*EnvLearn'
fit.moresat.altdir <- sem(model.moresat.altdir, data = cleanr, estimator="MLR", missing="FIML")
summary(fit.moresat.altdir, fit.measures=TRUE, standardized=TRUE)

## ----model.comparison, echo=TRUE-----------------------------------------
lavTestLRT(fit.lesssat, fit.moresat) #if p < .05, model different from data. so we want to keep more saturated model with addtl parameter estimated which in this case is path from MRT to PT

## ----model.med, echo=TRUE, warning=FALSE---------------------------------
set.seed(28) #for reproducability of bootstrapping analyses
model.boot <- 'EnvLearn =~ NA*BetweenRtPtI + WithinRtPtI + DistanceI + r.2
SOTmeanI ~ c*GPSuse + e*MRT + h*SBtotal
MRT ~ a*GPSuse + i*SBtotal
GPSuse ~ f*SBtotal
EnvLearn ~ b*MRT + d*SOTmeanI + g*GPSuse + j*SBtotal
EnvLearn ~~ 1*EnvLearn
#calculate indirect effect. a* is where our predictor predicts the mediating variable.
#b* is where our mediator predicts the final dependent variable.
ab := a*b #Indirect effect of GPS on Spatial Learning through MRT
cd := c*d #Ind of GPS on SL through Perspective Taking
de := d*e #Ind of MRT on SL through Perspective Taking
ae := a*e #Ind of GPS on PT through MRT
aed := a*e*d #Ind of GPS on SL through MRT and PT
diff1 := h - ae #Diff between SBSOD direct on PT and GPS to PT through MRT
diff2 := h - c #Diff between effects of SB and GPS on PT
diff3 := a - i #Diff between effects of SB and GPS on MRT
diff4 := j - aed #Difference between SBSOD direct on SL and Indirect from GPS to SL'
fit.boot <- sem(model.boot, data = cleanr, se="bootstrap", test="bootstrap", bootstrap=2000)

## ----model2_indeffects, echo=TRUE, warning=FALSE-------------------------
#bootstrapping  bias corrected CI's for indirect effects
parest <- parameterEstimates(fit.boot, boot.ci.type = "bca.simple", standardized = TRUE)
#pull out parameters of interest
ind_table <- parest %>%
  select(label,est,std.all,se, ci.lower:ci.upper)
ind_table

