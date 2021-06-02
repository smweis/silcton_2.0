# Loading Required Packages -----------------------------------------------
require(dplyr)
require(tidyr)
require(reshape2)
require(psych) #for getting descriptives
#library(plyr) 

# CLEANING EXPERIMENTAL DATA -----------------------------------------------------------

setwd("D:/CNS/Dissertation/Data Analysis")
#read in data
distance <- read.csv("DistanceRaw.csv")
map <- read.csv("ModelBuildingRaw.csv")
point.off <- read.csv("OffSitePointingRaw.csv")
point.on <- read.csv("OnSitePointingRaw.csv")
mrt <- read.csv("MRTraw.csv")


#276 duplicate...doublecheck. 
#delete unneccessary cols
mrt <- select(mrt, c(participant), c(date), c(MRT.SDT.style:PSAS.average))
point.off <- select(point.off, -(direction.test)) #note that these are diffs in egocentric vs allocentric disembodied pointing
point.on <- select(point.on, -(direction.test))
distance <- select(distance, -(distance.test))
map <- select(map, -(model.number))

#get means for restructuring data 
#first, split by route type
samediff <-split(point.on, point.on$same.or.different.route)
samediff.within <- samediff$same
samediff.between <- samediff$different

samediff.off <-split(point.off, point.off$same.or.different.route)
samediff.off.within <- samediff.off$same
samediff.off.between <- samediff.off$different

#calculate absolute distance error
distance$error <- abs(distance$actual.distance - distance$estimated.distance)

#split distance by route type
samediff.dist <- split(distance, distance$same.or.different.route)
samediff.dist.within <- samediff.dist$same
samediff.dist.between <- samediff.dist$different

#fixing RA's mistakes so that data isnt aggregated over two people... 
samediff.within$participant <- as.character(samediff.within$participant)
samediff.within$participant[samediff.within$participant=="UT_141" & samediff.within$date =="9/19/2017 13:54"] <- "Utah_142"
samediff.within$participant[samediff.within$participant=="Utah_176" & samediff.within$date == "12/05/2017 15:43"] <- "Utah_177"

samediff.between$participant <- as.character(samediff.between$participant)
samediff.between$participant[samediff.between$participant=="UT_141" & samediff.between$date =="9/19/2017 13:54"] <- "Utah_142"
samediff.between$participant[samediff.between$participant=="Utah_176" & samediff.between$date == "12/05/2017 15:43"] <- "Utah_177"

samediff.off.within$participant <- as.character(samediff.off.within$participant)
samediff.off.within$participant[samediff.off.within$participant=="UT_141" & samediff.off.within$date =="2017-09-19 13:54:07 -0700"] <- "Utah_142"    #2017-12-05 15:43:12 -0800	
samediff.off.within$participant[samediff.off.within$participant=="Utah_176" & samediff.off.within$date == "2017-12-05 15:43:12 -0800"] <- "Utah_177" 

samediff.off.between$participant <- as.character(samediff.off.between$participant)
samediff.off.between$participant[samediff.off.between$participant=="UT_141" & samediff.off.between$date =="2017-09-19 13:54:07 -0700"] <- "Utah_142"
samediff.off.between$participant[samediff.off.between$participant=="Utah_176" & samediff.off.between$date == "2017-12-05 15:43:12 -0800"] <- "Utah_177" 


map$participant <- as.character(map$participant)
map$participant[map$participant=="UT_141" & map$date =="2017-09-19 13:54:07 -0700"] <- "Utah_142"
map$participant[map$participant=="Utah_176" & map$date == "2017-12-05 15:43:12 -0800"] <- "Utah_177" 


samediff.dist.within$participant <- as.character(samediff.dist.within$participant)
samediff.dist.within$participant[samediff.dist.within$participant=="UT_141" & samediff.dist.within$date =="2017-09-19 13:54:07 -0700"] <- "Utah_142"
samediff.dist.within$participant[samediff.dist.within$participant=="Utah_176" & samediff.dist.within$date == "2017-12-05 15:43:12 -0800"] <- "Utah_177" 


samediff.dist.between$participant <- as.character(samediff.dist.between$participant)
samediff.dist.between$participant[samediff.dist.between$participant=="UT_141" & samediff.dist.between$date =="2017-09-19 13:54:07 -0700"] <- "Utah_142"
samediff.dist.between$participant[samediff.dist.between$participant=="Utah_176" & samediff.dist.between$date == "2017-12-05 15:43:12 -0800"] <- "Utah_177" 

mrt$participant <- as.character(mrt$participant)
mrt$date <- as.character(mrt$date)

mrt$participant[mrt$participant=="UT_141" & mrt$date =="2017-09-19 13:54:07 -0700"] <- "Utah_142"
mrt$participant[mrt$participant=="Utah_176" & mrt$date =="2017-12-05 15:43:12 -0800"] <- "Utah_177"

#aggregate the data so its not in repeated measures form since I'm not trained on multilevel SEM
pointmean.within <- aggregate(samediff.within$abs.error~samediff.within$participant, FUN=mean)
pointmean.between <- aggregate(samediff.between$abs.error~samediff.between$participant, FUN=mean)
pointmean.off.within <- aggregate(samediff.off.within$abs.error~samediff.off.within$participant, FUN=mean)
pointmean.off.between <- aggregate(samediff.off.between$abs.error~samediff.off.between$participant, FUN=mean)
mapmean <- aggregate(map$error~map$participant, FUN=mean)
distmean.within <- aggregate(samediff.dist.within$error~samediff.dist.within$participant, FUN=mean)
distmean.between <- aggregate(samediff.dist.between$error~samediff.dist.between$participant, FUN=mean)


#now, merge all the dataframes for analysis
colnames(mapmean)[1] <- "id" #rename first columns appropriately
mapmean <- rbind(mapmean, c("Utah_161", NA)) #add in missing map data for 161. 
colnames(pointmean.within)[1] <- "id" 
colnames(pointmean.between)[1] <- "id" 
colnames(pointmean.off.within)[1] <- "id" 
colnames(pointmean.off.between)[1] <- "id" 
colnames(distmean.within)[1] <- "id" 
colnames(distmean.between)[1] <- "id" 
colnames(mrt)[1] <- "id" 

expdata <- merge(mapmean, pointmean.within, by="id")
expdata <- merge(expdata, pointmean.between, by="id")
expdata <- merge(expdata, pointmean.off.within, by="id")
expdata <- merge(expdata, pointmean.off.between, by="id")
expdata <- merge(expdata, distmean.within, by="id")
expdata <- merge(expdata, distmean.between, by="id")
expdata <- merge(expdata, mrt, by="id")

# Merge Qualtrics data with Experimental Data --------------------
require(car) #for recoding
#read in data
rawdata <- read.csv("Mobility_Survey_Version_1.3_IanDiss.csv")
#note this has been changed to extract participant ID's for Ian's dissertation. also note that second row needs to be deleted in datafile exported from qualtrics before running this script. it has been deleted in this raw datafile already.
require(plyr)
rawdata$id <- as.character(rawdata$id)
rawdata$id[rawdata$V1=="R_2zprLU09Ix6N4k2"] <- "Utah_169"
#fix the wrong ID to be correct 
#rawdata$ResponseID <- as.numeric(rawdata$ResponseID) #change responseID's from long text strings to numbers
traitdata <- rawdata %>% select(id, LO1:Utah.1_41, -Map) #delete unnecessary columns of demographic data, store in a separate dataframe
#recode ethnicity
traitdata$Ethnicity <- as.numeric(traitdata$Ethnicity)
traitdata$Ethnicity[traitdata$Ethnicity == '1'] <- 'African-American'
traitdata$Ethnicity[traitdata$Ethnicity == '2'] <- 'Asian/Pacific-Islanders'
traitdata$Ethnicity[traitdata$Ethnicity == '3'] <- 'Caucasian'
traitdata$Ethnicity[traitdata$Ethnicity == '4'] <- 'Latino or Hispanic'
traitdata$Ethnicity[traitdata$Ethnicity == '5'] <- 'Native American or Aleut'
traitdata$Ethnicity[traitdata$Ethnicity == '6'] <- 'Other'

#recode gender
traitdata$Gender <- as.numeric(traitdata$Gender)
traitdata$Gender[traitdata$Gender == '1'] <- 'Male'
traitdata$Gender[traitdata$Gender == '2'] <- 'Female'
traitdata$Gender[traitdata$Gender == '3'] <- 'Androgynous'
traitdata$Gender[traitdata$Gender == '4'] <- 'Transgender'
traitdata$Gender[traitdata$Gender == '5'] <- 'Transsexual'
traitdata$Gender[traitdata$Gender == '6'] <- 'Other'

#summarize mobility data into new columns
traitdata$UStotal <- rowSums(traitdata[,79:128], na.rm=TRUE)
traitdata$UTtotal <- rowSums(traitdata[,132:172], na.rm=TRUE)

#summarizing more fine grained UT mobility measures... 
#U_rural=U2+U3+U4+U5+U10+U12+U14+U15+U18+U21+U22+U23+U24+U26+U27+U29+U31+U32 
traitdata$UTrural <- rowSums(traitdata[,c(133:136, 141, 143, 145:146, 149, 152:155, 157:158, 160, 162:163)], na.rm=TRUE)

#U_urban=U1+U6+U7+U8+U9+U13+U16+U17+U19+U20+U30+U33+U34+U35+U36+U37+U38+U39+U40+U41
traitdata$UTurban <- rowSums(traitdata[,c(132, 137:140, 144, 147:148, 150:151, 161, 164:172)], na.rm=TRUE)

#recode those w/ no UT mobility(first 42) as NA since they didn't see this in survey...
traitdata$rnum <- seq.int(nrow(traitdata))
traitdata$UTrural[traitdata$UTrural==0 & traitdata$rnum < 42 | traitdata$rnum==197 | 
                    traitdata$rnum==191 | traitdata$rnum==77] <- NA
traitdata$UTurban[traitdata$UTurban==0 & traitdata$rnum < 42 | traitdata$rnum==197 | 
                    traitdata$rnum==191 | traitdata$rnum==77] <- NA #making sure missing is actually missing. 
traitdata$UTtotal[traitdata$UTtotal==0 & traitdata$rnum < 42  | traitdata$rnum==197 | 
                    traitdata$rnum==191 | traitdata$rnum==77] <- NA #making sure missing is actually missing. 


#Total distance in miles: sum of (locations visited *distance of that place from UofU): 
#Udist=U1*84.2+U2*75+U3*184+U4*38.1+U5*426+U6*17.3+U7*19.3+U8*3.7+U9*3.7+U10*48.7+U11*294+U12*34.2+U13*29.2+U14*61.4+U15*22.4+U16*3.3+U17*2.2+U18*24.8+U19*305+U20*40.1+U21*114+U22*105+U23*221+U24*315
traitdata$Utahdist.1 <- traitdata$Utah.1_1*84.2
traitdata$Utahdist.2 <- traitdata$Utah.1_2*75
traitdata$Utahdist.3 <- traitdata$Utah.1_3*184
traitdata$Utahdist.4 <- traitdata$Utah.1_4*38.1
traitdata$Utahdist.5 <- traitdata$Utah.1_5*426
traitdata$Utahdist.6 <- traitdata$Utah.1_6*17.3
traitdata$Utahdist.7 <- traitdata$Utah.1_7*19.3
traitdata$Utahdist.8 <- traitdata$Utah.1_8*3.7
traitdata$Utahdist.9 <- traitdata$Utah.1_9*3.7
traitdata$Utahdist.10 <- traitdata$Utah.1_10*48.7
traitdata$Utahdist.11 <- traitdata$Utah.1_11*294
traitdata$Utahdist.12 <- traitdata$Utah.1_12*34.2
traitdata$Utahdist.13 <- traitdata$Utah.1_13*29.2
traitdata$Utahdist.14 <- traitdata$Utah.1_14*61.4
traitdata$Utahdist.15 <- traitdata$Utah.1_15*22.4
traitdata$Utahdist.16 <- traitdata$Utah.1_16*3.3
traitdata$Utahdist.17 <- traitdata$Utah.1_17*2.2
traitdata$Utahdist.18 <- traitdata$Utah.1_18*24.8
traitdata$Utahdist.19 <- traitdata$Utah.1_19*305
traitdata$Utahdist.20 <- traitdata$Utah.1_20*40.1
traitdata$Utahdist.21 <- traitdata$Utah.1_21*114
traitdata$Utahdist.22 <- traitdata$Utah.1_22*105
traitdata$Utahdist.23 <- traitdata$Utah.1_23*221
traitdata$Utahdist.24 <- traitdata$Utah.1_24*315
traitdata$Utahdist.25 <- traitdata$Utah.1_25*237
  #+U25*237+U26*311+U27*272+U28*217+U29*227+U30*48.2+U31*52.6+U32*71+U33*3.8+U34*3.4+U35*3.5+U36*4.1+U37*3.9+U38*5.7+U39*29+U40*3.4+U41*25.2
traitdata$Utahdist.26 <- traitdata$Utah.1_26*311
traitdata$Utahdist.27 <- traitdata$Utah.1_27*272
traitdata$Utahdist.28 <- traitdata$Utah.1_28*217
traitdata$Utahdist.29 <- traitdata$Utah.1_29*227
traitdata$Utahdist.30 <- traitdata$Utah.1_30*48.2
traitdata$Utahdist.31 <- traitdata$Utah.1_31*52.6
traitdata$Utahdist.32 <- traitdata$Utah.1_32*71
traitdata$Utahdist.33 <- traitdata$Utah.1_33*3.8
traitdata$Utahdist.34 <- traitdata$Utah.1_34*3.4
traitdata$Utahdist.35 <- traitdata$Utah.1_35*3.5
traitdata$Utahdist.36 <- traitdata$Utah.1_36*4.1
traitdata$Utahdist.37 <- traitdata$Utah.1_37*3.9
traitdata$Utahdist.38 <- traitdata$Utah.1_38*5.7
traitdata$Utahdist.39 <- traitdata$Utah.1_39*29
traitdata$Utahdist.40 <- traitdata$Utah.1_40*3.4
traitdata$Utahdist.41 <- traitdata$Utah.1_41*25.2
traitdata$UTdist <- rowSums(traitdata[,178:218], na.rm=TRUE)
traitdata$UTdist[traitdata$UTdist==0] <- NA #make 0 values actually missing. use these to find ppl who didnt answer (wouldn't make sense to have 0 distance unless missing, since all p's lived in Utah)

#fix US mobility data so that 0 places is missing data
traitdata$UStotal <- car::recode(traitdata$UStotal, c("0=NA"))

#reverse code SBSOD data using car package, then summarize into single variable
#these actually have to be the reverse of the suggested recodings by the scale creators since we made 7 strongly agree.
traitdata$SB2r <- car::recode(traitdata$SB2, c("1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1"))
traitdata$SB6r <- car::recode(traitdata$SB6, c("1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1"))
traitdata$SB8r <- car::recode(traitdata$SB8, c("1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1"))
traitdata$SB10r <- car::recode(traitdata$SB10, c("1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1"))
traitdata$SB11r <- car::recode(traitdata$SB11, c("1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1"))
traitdata$SB12r <- car::recode(traitdata$SB12, c("1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1"))
traitdata$SB13r <- car::recode(traitdata$SB13, c("1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1"))
traitdata$SBtotal <- rowSums(select(traitdata,("SB2r"),("SB6r"),("SB8r"),("SB10r"),("SB11r"), ("SB12r"), ("SB13r"),("SB1"),("SB3"),("SB4"),("SB5"),("SB7"),("SB9"),("SB14")), na.rm=TRUE)
traitdata$SBtotal <- traitdata$SBtotal/15

#summarize the lawton strategy questions
traitdata$Orient <- rowSums(select(traitdata,c(LO1:LO11)), na.rm=TRUE)
traitdata$Orient <- traitdata$Orient/11
traitdata$Route <- rowSums(select(traitdata,c(LR1:LR6)), na.rm=TRUE)
traitdata$Route <- traitdata$Route/6

#summarize the lawton spatial anxiety questions
traitdata$SA <- rowSums(select(traitdata,c(SA1:SA8)), na.rm=TRUE)
traitdata$SA <- traitdata$SA/8

#big five personality questions - recode and summarize
#Gosling, S. D., Rentfrow, P. J., & Swann, W. B., Jr. (2003). A Very Brief Measure of the Big Five Personality Domains. Journal of Research in Personality, 37, 504-528.
#Extraversion: 1, 6R; Agreeableness: 2R, 7; Conscientiousness; 3, 8R; Emotional Stability: 4R, 9; Openness to Experiences: 5, 10R.

traitdata$BF2r <- car::recode(traitdata$BF2, c("1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1"))
traitdata$BF4r <- car::recode(traitdata$BF4, c("1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1"))
traitdata$BF6r <- car::recode(traitdata$BF6, c("1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1"))
traitdata$BF8r <- car::recode(traitdata$BF8, c("1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1"))
traitdata$BF10r <- car::recode(traitdata$BF10, c("1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1"))

traitdata$BFExtraverted <- (rowSums(select(traitdata,('BF1'),('BF6r')), na.rm=TRUE))/2
traitdata$BFAgreeableness <- (rowSums(select(traitdata,('BF2r'),('BF7')), na.rm=TRUE))/2
traitdata$BFConscient <- (rowSums(select(traitdata,('BF3'),('BF8r')), na.rm=TRUE))/2
traitdata$BFEmoStabil <- (rowSums(select(traitdata,('BF4r'),('BF9')), na.rm=TRUE))/2
traitdata$BFOpennness <- (rowSums(select(traitdata,('BF5'),('BF10r')), na.rm=TRUE))/2

#next: SOI Sociosexual orientation index
#Items 1-3 should be coded as 0 = 1, 1 = 2, 2 = 3, 3 = 4, 4 = 5, 5-6 = 6, 7-9 = 7, 10-19 = 8, 20
#or more = 9; they can then be aggregated (i.e., summed or averaged) to form the Behavior
#facet ( = .85). After reverse-coding item 6, items 4-6 can be aggregated to form the Attitude
#facet ( = .87). Aggregating items 7-9 results in the Desire facet ( = .86). Finally, all nine
#items can be aggregated to a total score of global sociosexual orientation ( = .83). 
#http://www.larspenke.eu/pdfs/SOI-R%20Manual.pdf

#recode those who indicated prefer not to answer to missing data
traitdata$SOI1 <- car::recode(traitdata$SOI1, c("10=NA"))
traitdata$SOI2 <- car::recode(traitdata$SOI2, c("10=NA"))
traitdata$SOI3 <- car::recode(traitdata$SOI3, c("10=NA"))
traitdata$SOI4 <- car::recode(traitdata$SOI4, c("10=NA"))
traitdata$SOI5 <- car::recode(traitdata$SOI5, c("10=NA"))
traitdata$SOI6 <- car::recode(traitdata$SOI6, c("10=NA"))
traitdata$SOI7 <- car::recode(traitdata$SOI7, c("10=NA"))
traitdata$SOI8 <- car::recode(traitdata$SOI8, c("10=NA"))
traitdata$SOI9 <- car::recode(traitdata$SOI9, c("10=NA"))

#reverse code item 6
traitdata$SOI6r <- car::recode(traitdata$SOI6, c("1=9; 2=8; 3=7; 4=6; 5=5; 6=4; 7=3; 8=2; 9=1"))

#aggregate into SOI scores
traitdata$SOIbehavior <- (rowSums(select(traitdata,c(SOI1:SOI3)), na.rm=TRUE))/3
traitdata$SOIattitude <- (rowSums(select(traitdata,c(SOI4:SOI5),('SOI6r')), na.rm=TRUE))/3
traitdata$SOIdesire <- (rowSums(select(traitdata,c(SOI7:SOI9)), na.rm=TRUE))/3
traitdata$SOItotal <- (rowSums(select(traitdata,c(SOI1:SOI5),c(SOI7:SOI9), ('SOI6r')), na.rm=TRUE))/9

#ATOT questions
#This comprises 10 items relating to two factors: (i) pleasure in exploring places (items: 1, 2, 5, 6, 9);
#and (ii) no pleasure in exploring places (3, 4, 7, 8, 10).
#Judgments are expressed using a Likert scale from 1 (not at all) to 6 (very much). The sum is
#calculated for each factor.
#The factor analysis confirmed two-factors: the first pleasure in exploring places accounted for
#the 40% of variance; the second the no pleasure in exploring places accounted for the 15% of
#variance (loads from.40 to 82); the internal consistency is .78 for pleasure in exploring places
#and .83 for no pleasure in exploring places (De Beni et al., 2014).
traitdata$ATOTpleasure <- (rowSums(select(traitdata,c(ATOT1:ATOT2),c(ATOT5:ATOT6),('ATOT9')), na.rm=TRUE))/5
traitdata$ATOTnopleasure <- (rowSums(select(traitdata,c(ATOT3:ATOT4), c(ATOT7:ATOT8), ('ATOT10')), na.rm=TRUE))/5

#remove original trait variables from dataset now that they are summarized #could we learn something from within-person response variability, though? or does this just tell us about each test? ignore for now...
#taking out unnecessary columns
traitdata.sub <- select(traitdata, -c(LO1:ATOT10), -c(US.1_1:US.1_50), -(Utah.1_1:Utah.1_41), -c(rnum), -c(Utahdist.1:Utahdist.41), -c(SB2r:SB13r), -c(BF2r:BF10r), -c(SOI6r))


require(dplyr)
rawdata.sub <- select(rawdata, id, c(Q1:Q3), c(Q4:Q73), -c(text3), -c(text4), -c(text5))
final <- merge(traitdata.sub, rawdata.sub, by='id')

final <- final[-2,] #take out person who is all missing
require(plyr)
final$id <- revalue(final$id, c("UT_140"="Utah_140", "utah140"="Utah_141",
                                                "UT_143"="Utah_143", "UT142" = "Utah_142"))
expdata.sub <- expdata
#before merging files, fix the mismatched IDs..
expdata.sub$id <- revalue(expdata.sub$id, c("u1075688"="Utah_302", "UT_168"="Utah_168",
                                                "UT_143"="Utah_143", "Utah140" = "Utah_140",
                                            "UT_141" = "Utah_141", "Utah_2256" = "Utah_256",
                                            "utah_155"="Utah_155", "UTah_137" = "Utah_137")) #fix these ID's

expdata.sub <- rbind(expdata.sub, c("Utah_182", rep(NA, 10))) #add missing data for p182.
colnames(expdata.sub) <- c("id", "Map", "WithinRtPt", "BetweenRtPt", "WithinRtPtAllo", "BetweenRtPtAllo", "WithinDist", "BetweenDist", "date", "MRT", "PSAS")

finalclean <- merge(final, expdata.sub, by='id')
finalclean <- finalclean[-184,] #take out the artifact participant from SILCTON (same person, has missing for some vars)

write.csv(finalclean, "ian_diss_cleaneddata.csv")

#Remainder of cleaning and analysis in Diss_Analysis.R

#extra map data things for bidimreg...
require(plyr) #first make sure to run script above! to fix a few other names.
map$participant <- revalue(map$participant, c("u1075688"="Utah_302", "UT_168"="Utah_168",
                                            "UT_143"="Utah_143", "Utah140" = "Utah_140",
                                            "UT_141" = "Utah_141", "Utah_2256" = "Utah_256",
                                            "utah_155"="Utah_155", "UTah_137" = "Utah_137")) 

#save out raw map file for use with bidimreg macro.
write.csv(map, "mapdataforbidim.csv")