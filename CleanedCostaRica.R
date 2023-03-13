##############################################
# BEGIN WITH DATA CLEANING
# REMOVE BAD FILES, COMBINE ALL TOGETHER, ADD SITE TYPES, MAKE TIME CODES UNQIUE. 
#############################################

library(ggplot2)
library(dplyr)
library(tidyverse)
library(ggbiplot)
library(rlist)
library(gridExtra)
library(caTools)
library(randomForest)
library(broom)
library(ggpubr)
library(caret)
library(pacman)

# FIND THE FILES IN THE NAS

setwd("Z://Johan/costa_rica")
getwd()

#IMPORT THE DATA, NAME IT CORRECTLY

csvname <- list.files(pattern="*.csv")
myfiles <- lapply(csvname, read.csv)
names(myfiles) <- csvname

#REMOVE LOW QUALITY RECORDINGS

toremove <- c("ZH1_17052022_PSA0077-2013.csv", "RFX6_19052022_PSA0431-2013.csv", "RFX17_03062022_PSA0026-2020.csv", "RFX16_31052022_PSA0202-2016.csv", "RFX37_30052022_PSA0098-2013.csv")
myfiles <- myfiles[names(myfiles) %in% toremove == FALSE]

#ADD SITE TYPE COLUMN 
sitenames <- as.data.frame(names(myfiles))

sitenames$types <- rep(0, 120)
sitenames[grepl("Past", sitenames[,1], ignore.case = TRUE), 2] <- "Pasture"
sitenames[grepl("Ref", sitenames[,1], ignore.case = TRUE), 2] <- "Reference Forest"
nametype <- read.csv("~/Master Thesis/site and type.csv")
conssites <- nametype[nametype[, 2] == "C",1]
reforestsites <- nametype[nametype[, 2] == "R",1]
for (i in 1:length(conssites)) {
  sitenames[grepl(conssites[i], sitenames[,1], ignore.case = TRUE), 2] <- "Conservation"
}
for (i in 1:length(reforestsites)) {
  sitenames[grepl(reforestsites[i], sitenames[,1], ignore.case = TRUE), 2] <- "Reforestation"
}

for (i in 1:length(myfiles)) {
  myfiles[[i]]['site type'] <- sitenames[i, 2]
}

#COMBINE INTO SINGLE DATA FRAME

alldata <- list.rbind(myfiles)

# MAKE TIME CODES UNIQUE

for (i in 1:nrow(alldata)) {
  if (grepl("1800", alldata[i,13]) == TRUE) {
    alldata[i, 12] <- alldata[i, 12] + 2000
  } else if (grepl("0625", alldata[i, 13]) == TRUE) {
    alldata[i, 12] <- alldata[i, 12] + 1000
  }
  
}
alldata$TOM <- 0

for (i in 1:nrow(alldata)) {
  if (alldata[i, 12] < 2000){
    alldata[i, 16] <- "Day"
  } else {
    alldata[i, 16] <- "Night"
  }
}

# SAVE THE CLEANED DATAFRAME

write.csv(alldata, "C://Users/giaco/Documents/Master Thesis/CleanedCostaRicaData.csv", row.names = FALSE)

###
#Make the start times corrolate to the minutes of the day like you should have done originally you idiot
badtimes <- alldata$start > 999
rfdata <- subset(alldata, badtimes)

for (i in 1:nrow(rfdata)) {
  if (rfdata[i, 12] > 2359) {
    rfdata[i, 12] <- rfdata[i, 12] - 2359
  } else if (rfdata[i, 12] > 1000 & rfdata[i, 12] < 2000) {
    rfdata[i, 12] <- rfdata[i, 12] - 615
  } else if (rfdata[i, 12] > 1999 & rfdata[i, 12] < 2360) {
    rfdata[i, 12] <- rfdata[i, 12] - 919
  }
  
}

# SAVE THE CLEANED DATAFRAME

write.csv(rfdata, "C://Users/giaco/Documents/Master Thesis/CleanedDataCostaRica.csv", row.names = FALSE)


#################################################################################################################
#START FROM HERE IF YOU ALREADY HAVE CLEANED DATA TABLE

alldata <- read.csv("C://Users/giaco/Documents/Master Thesis/CleanedDataCostaRica.csv")
alldata$site.type <- factor(alldata$site.type, levels = c("Reference Forest", "Conservation", "Reforestation", "Pasture"))
#BOX PLOTS FOR WITHIN SITE DIURNAL CYCLE VARIATION AND BETWEEN SITE VARIATION COMBINED 


bp1 <- ggplot(alldata, aes(x = site.type, y = AcouOccupancy_left, color = TOM)) +  
  geom_boxplot()      
       
bp2 <- ggplot(alldata, aes(x = site.type, y = Bioac_left, color = TOM)) +  
  geom_boxplot() 

bp3 <- ggplot(alldata, aes(x = site.type, y = Ht_left, color = TOM)) +  
  geom_boxplot() 

bp4 <- ggplot(alldata, aes(x = site.type, y = Hf_left, color = TOM)) +  
  geom_boxplot() 

bp5 <- ggplot(alldata, aes(x = site.type, y = H_left, color = TOM)) +  
  geom_boxplot() 

bp6 <- ggplot(alldata, aes(x = site.type, y = ACI_left, color = TOM)) +  
  geom_boxplot() 

bp7 <- ggplot(alldata, aes(x = site.type, y = AEI_villa_left, color = TOM)) +  
  geom_boxplot() 

bp8 <- ggplot(alldata, aes(x = site.type, y = M_left, color = TOM)) +  
  geom_boxplot() 

bp9 <- ggplot(alldata, aes(x = site.type, y = NDSI_left, color = TOM)) +  
  geom_boxplot() 

bp10<- ggplot(alldata, aes(x = site.type, y = ADI_left, color = TOM)) +  
  geom_boxplot() 

bp11 <- ggplot(alldata, aes(x = site.type, y = npic_left, color = TOM)) +  
  geom_boxplot() 

grid.arrange(bp1, bp2, bp3, bp4, bp5, bp6, bp7, bp8, bp9, bp10, bp11, ncol=4)
   
       
# PLOTTING THE INDICES AS A FUNCTION OF TIME



pg1<- alldata %>% group_by(start,site.type) %>% 
  summarise_at(vars(AcouOccupancy_left), list(Avg = mean)) %>%
  ggplot(aes(x= start, y=Avg, color = site.type)) +
  geom_point() +
  geom_vline(xintercept = 420) +
  geom_vline(xintercept = 1080) +
  xlab("Minute of the Day") +
  ylab("Acoustic Occupancy Index") +
  labs(color = "Site Type") +
  theme(text=element_text(family="serif", size = 14))  +
  theme(legend.position="none")
  

pg2<-alldata %>% group_by(start,site.type) %>% 
  summarise_at(vars(Bioac_left), list(Avg = mean)) %>%
  ggplot(aes(x= start, y=Avg, color = site.type)) +
  geom_point() +
  geom_vline(xintercept = 420) +
  geom_vline(xintercept = 1080) +
  xlab("Minute of the Day") +
  ylab("Bioacoustic Index")+
  labs(color = "Site Type") +
  theme(text=element_text(family="serif", size = 14))+
  theme(legend.position="none")

pg3<-alldata %>% group_by(start,site.type) %>% 
  summarise_at(vars(Ht_left), list(Avg = mean)) %>%
  ggplot(aes(x= start, y=Avg, color = site.type)) +
  geom_point() +
  geom_vline(xintercept = 420) +
  geom_vline(xintercept = 1080) +
  xlab("Minute of the Day")+
  ylab("Temporal Entropy")+
  labs(color = "Site Type") +
  theme(text=element_text(family="serif", size = 14))+
  theme(legend.position="none")

pg4<-alldata %>% group_by(start,site.type) %>% 
  summarise_at(vars(Hf_left), list(Avg = mean)) %>%
  ggplot(aes(x= start, y=Avg, color = site.type)) +
  geom_point() +
  geom_vline(xintercept = 420) +
  geom_vline(xintercept = 1080) +
  xlab("Minute of the Day")+
  ylab("Freuquency Entropy")+
  labs(color = "Site Type") +
  theme(text=element_text(family="serif", size = 14))+
  theme(legend.position="none")

pg5<-alldata %>% group_by(start,site.type) %>% 
  summarise_at(vars(H_left), list(Avg = mean)) %>%
  ggplot(aes(x= start, y=Avg, color = site.type)) +
  geom_point() +
  geom_vline(xintercept = 420) +
  geom_vline(xintercept = 1080) +
  xlab("Minute of the Day")+
  ylab("Entropy Index")+
  labs(color = "Site Type") +
  theme(text=element_text(family="serif", size = 14))+
  theme(legend.position="none")

pg6<-alldata %>% group_by(start,site.type) %>% 
  summarise_at(vars(npic_left), list(Avg = mean)) %>%
  ggplot(aes(x= start, y=Avg, color = site.type)) +
  geom_point() +
  geom_vline(xintercept = 420) +
  geom_vline(xintercept = 1080) +
  xlab("Minute of the Day")+
  ylab("Number of Frequency Peaks") +
  labs(color = "Site Type") +
  theme(text=element_text(family="serif", size = 14))+
  theme(legend.position="none")

pg7<-alldata %>% group_by(start,site.type) %>% 
  summarise_at(vars(ACI_left), list(Avg = mean)) %>%
  ggplot(aes(x= start, y=Avg, color = site.type)) +
  geom_point() +
  geom_vline(xintercept = 420) +
  geom_vline(xintercept = 1080) +
  xlab("Minute of the Day")+
  ylab("Acoustic Complextiy Index")+
  ylim(1500, 1700) +
  labs(color = "Site Type") +
  theme(text=element_text(family="serif", size = 14))+
  theme(legend.position="none")

pg8<-alldata %>% group_by(start,site.type) %>% 
  summarise_at(vars(AEI_villa_left), list(Avg = mean)) %>%
  ggplot(aes(x= start, y=Avg, color = site.type)) +
  geom_point() +
  geom_vline(xintercept = 420) +
  geom_vline(xintercept = 1080) +
  xlab("Minute of the Day") +
  ylab("Acoustic Eveness Index")+
  labs(color = "Site Type") +
  theme(text=element_text(family="serif", size = 14))+
  theme(legend.position="none")

pg9<-alldata %>% group_by(start,site.type) %>% 
  summarise_at(vars(M_left), list(Avg = mean)) %>%
  ggplot(aes(x= start, y=Avg, color = site.type)) +
  geom_point() +
  geom_vline(xintercept = 420) +
  geom_vline(xintercept = 1080) + 
  ylim(0.000000000007, 0.000000000009) +
  xlab("Minute of the Day") +
  ylab("Median of Amplitude Envelope")+
  labs(color = "Site Type") +
  theme(text=element_text(family="serif", size = 14))+
  theme(legend.position="none")

pg10<-alldata %>% group_by(start,site.type) %>% 
  summarise_at(vars(NDSI_left), list(Avg = mean)) %>%
  ggplot(aes(x= start, y=Avg, color = site.type)) +
  geom_point() +
  geom_vline(xintercept = 420) +
  geom_vline(xintercept = 1080) +
  xlab("Minute of the Day") +
  ylab("Normalised Differernce Soundscape Index")+
  labs(color = "Site Type") +
  theme(text=element_text(family="serif", size = 14))+
  theme(legend.position="none")

pg11<-alldata %>% group_by(start,site.type) %>% 
  summarise_at(vars(ADI_left), list(Avg = mean)) %>%
  ggplot(aes(x= start, y=Avg, color = site.type)) +
  geom_point() +
  geom_vline(xintercept = 420) +
  geom_vline(xintercept = 1080) +
  xlab("Minute of the Day")+
  ylab("Acoustic Diversity Index") +
  labs(color = "Site Type") +
  theme(text=element_text(family="serif", size = 14))


grid.arrange(pg1, pg2, pg3, pg4, pg5, pg6, pg7, pg8, pg9, pg10, pg11, ncol=3, vp=viewport(width=0.9, height=0.9))

# QUICK PCA WITH ALL INDICES and all mins

PCAtest <- prcomp(rfdata[, c(2, 9)], scale. = T, center = T)
ggbiplot(PCAtest, ellipse = TRUE, groups = rfdata$site.type, var.axes = T)

# PCA with all indices and avg for each site

aggall <- aggregate(cbind(AcouOccupancy_left, Bioac_left, Ht_left, Hf_left, H_left, ACI_left, AEI_villa_left, M_left, NDSI_left, ADI_left, npic_left) ~ Site + site.type, data = rfdata, mean, na.rm = TRUE)
PCAaggall <- prcomp(aggall[, c(3:13)], scale. = T, center = T)
ggbiplot(PCAaggall, ellipse = T, groups =aggall$site.type, var.axes = T)

# Try plotting PC1 on Boxplot 

valuesPCAagg <- as.data.frame(PCAaggall$x)
ggplot(valuesPCAagg, aes(x = aggall$site.type, y = PC1)) +  
  geom_boxplot() 

#run an anova on it

anovaagg <- aov(valuesPCAagg$PC1 ~ aggall$site.type)
summary(anovaagg)
plot(anovaagg)

tukey.daydusk <- TukeyHSD(anovaagg)
tukey.daydusk
#sucks

# AVERAGE DOWN INDICES BY TOM

dawn <- (rfdata$start < 420 & rfdata$start > 239)
day <- (rfdata$start > 419 & rfdata$start < 1080)
dusk <- (rfdata$start > 1079 & rfdata$start < 1260)
night <- (rfdata$start < 240 | rfdata$start > 1259)
dawndata <- subset(rfdata, dawn)
daydata <- subset(rfdata, day)
duskdata <- subset(rfdata, dusk)
nightdata <- subset(rfdata, night)

#Try PCA's based on averaging down per time of day

meansdawn <- aggregate(cbind(AcouOccupancy_left, Bioac_left, Ht_left, Hf_left, H_left, ACI_left, AEI_villa_left, M_left, NDSI_left, ADI_left, npic_left) ~ Site + site.type, data = dawndata, mean, na.rm = TRUE)
PCAalldawn <- prcomp(meansdawn[,3:13], scale. = T, center = T)
ggbiplot(PCAalldawn, ellipse =TRUE, groups = meansdawn$site.type, var.axes = T)

meansday <- aggregate(cbind(AcouOccupancy_left, Bioac_left, Ht_left, Hf_left, H_left, ACI_left, AEI_villa_left, M_left, NDSI_left, ADI_left, npic_left) ~ Site + site.type, data = daydata, mean, na.rm = TRUE)
PCAallday <- prcomp(meansday[,3:13], scale. = T, center = T)
ggbiplot(PCAallday, ellipse =TRUE, groups = meansday$site.type, var.axes = T)

meansdusk <- aggregate(cbind(AcouOccupancy_left, Bioac_left, Ht_left, Hf_left, H_left, ACI_left, AEI_villa_left, M_left, NDSI_left, ADI_left, npic_left) ~ Site + site.type, data = duskdata, mean, na.rm = TRUE)
PCAalldusk <- prcomp(meansdusk[,3:13], scale. = T, center = T)
ggbiplot(PCAalldusk, ellipse =TRUE, groups = meansdusk$site.type, var.axes = T)

meansnight <- aggregate(cbind(AcouOccupancy_left, Bioac_left, Ht_left, Hf_left, H_left, ACI_left, AEI_villa_left, M_left, NDSI_left, ADI_left, npic_left) ~ Site + site.type, data = nightdata, mean, na.rm = TRUE)
PCAallnight <- prcomp(meansnight[,3:13], scale. = T, center = T)
ggbiplot(PCAallnight, ellipse =TRUE, groups = meansnight$site.type, var.axes = T)

#Try a PCA with only day times 
PCAday <- prcomp(meansday[,c(3:13)], scale. = T, center = T)
ggbiplot(PCAday, ellipse = T, groups = meansday$site.type, var.axes = T)

#Boxplot of PC1

valuesPCAday <- as.data.frame(PCAday$x)
ggplot(valuesPCAday, aes(x=meansday$site.type, y= PC1)) +
  geom_boxplot()

#anova
anovaday <- aov(valuesPCAday$PC1~meansday$site.type)
summary(anovaday)
plot(anovaday)

tukey.day <- TukeyHSD(anovaday)
tukey.day

############################
#Try a PCA with only day and dusk times
dayduskmeans <- cbind(meansday, meansdusk[,3:13])
PCAdaydusk <- prcomp(dayduskmeans[,3:24], scale. = T, center = T)
ggbiplot(PCAdaydusk, ellipse = T, groups = dayduskmeans$site.type, var.axes = T) +
  labs(color= "Site Type") +
  xlab("Principal Component 1 (33.9%)") +
  ylab("Principal Component 2 (17.5%)") +
  theme_classic() +
  theme(text=element_text(family="serif", size = 14))

#this looks good plot PC 1 on a boxplot

valuesPCA <- as.data.frame(PCAdaydusk$x)
ggplot(valuesPCA, aes(x = dayduskmeans$site.type, y = PC1, fill = dayduskmeans$site.type)) +  
  geom_boxplot() +
  xlab("Site Type") +
  ylab("Principal Component 1 Values") +
  theme_classic() +
  theme(legend.position="none", text=element_text(family="serif", size = 14))
 

#anova 
anovadaydusk <- aov(valuesPCA$PC2 ~ dayduskmeans$site.type)
summary(anovadaydusk)
plot(anovadaydusk)

tukey.daydusk <- TukeyHSD(anovadaydusk)
tukey.daydusk

#Try PCA for only Bioac

aggBioac <- dawndata %>% group_by(site.type, Site) %>% dplyr::summarise(avgBioac_dawn = mean(Bioac_left))
aggBioacday <- daydata %>% group_by(site.type, Site) %>% dplyr::summarise(avgBioac_day = mean(Bioac_left))
aggBioacdusk <- duskdata %>% group_by(site.type, Site) %>% dplyr::summarise(avgBioac_dusk = mean(Bioac_left))
aggBioacnight <- nightdata %>% group_by(site.type, Site) %>% dplyr::summarise(avgBioac_night = mean(Bioac_left))
aggBioac$avgBioac_day <- aggBioacday$avgBioac_day
aggBioac$avgBioac_dusk <- aggBioacdusk$avgBioac_dusk
aggBioac$avgBioac_night <- aggBioacnight$avgBioac_night

BioPCA <- prcomp(aggBioac[, c(3:6)], scale. = T, center = T)
pca2 <- ggbiplot(BioPCA, ellipse =TRUE, groups = aggBioac$site.type, var.axes = T)


#For NDSI

aggNDSI <- dawndata %>% group_by(site.type, Site) %>% dplyr::summarise(avgNDSI_dawn = mean(NDSI_left))
aggNDSIday <- daydata %>% group_by(site.type, Site) %>% dplyr::summarise(avgNDSI_day = mean(NDSI_left))
aggNDSIdusk <- duskdata %>% group_by(site.type, Site) %>% dplyr::summarise(avgNDSI_dusk = mean(NDSI_left))
aggNDSInight <- nightdata %>% group_by(site.type, Site) %>% dplyr::summarise(avgNDSI_night = mean(NDSI_left))
aggNDSI$avgNDSI_day <- aggNDSIday$avgNDSI_day
aggNDSI$avgNDSI_dusk <- aggNDSIdusk$avgNDSI_dusk
aggNDSI$avgNDSI_night <- aggNDSInight$avgNDSI_night

NDSIPCA <- prcomp(aggNDSI[, c(3:6)], scale. = T, center = T)
pca9 <- ggbiplot(NDSIPCA, ellipse =TRUE, groups = aggNDSI$site.type, var.axes = T)

#For both

NDSIBioac <- cbind(aggNDSI, aggBioac)
NDSIBioac <- NDSIBioac[ , -c(7, 8)]

twoPCA <- prcomp(NDSIBioac[, c(3:10)], scale. = T, center = T)
ggbiplot(twoPCA, ellipse =TRUE, groups = NDSIBioac$site.type...1, var.axes = T)

#For both but only day and dusk
test <- NDSIBioac[, -c(3, 6, 7, 10)]
twoPCAdaydusk <- prcomp(test[, c(3:6)], scale. = T, center = T)
ggbiplot(twoPCAdaydusk, ellipse = T, groups = test$site.type...1, var.axes = T)

#For ACI

aggACI <- dawndata %>% group_by(site.type, Site) %>% dplyr::summarise(avgACI_dawn = mean(ACI_left))
aggACIday <- daydata %>% group_by(site.type, Site) %>% dplyr::summarise(avgACI_day = mean(ACI_left))
aggACIdusk <- duskdata %>% group_by(site.type, Site) %>% dplyr::summarise(avgACI_dusk = mean(ACI_left))
aggACInight <- nightdata %>% group_by(site.type, Site) %>% dplyr::summarise(avgACI_night = mean(ACI_left))
aggACI$avgACI_day <- aggACIday$avgACI_day
aggACI$avgACI_dusk <- aggACIdusk$avgACI_dusk
aggACI$avgACI_night <- aggACInight$avgACI_night

ACIPCA <- prcomp(aggACI[, c(3:6)], scale. = T, center = T)
pca6 <- ggbiplot(ACIPCA, ellipse =TRUE, groups = aggACI$site.type, var.axes = T)

#For all three 

threetopvar <- cbind(NDSIBioac, aggACI)
threetopvar <- threetopvar[ , -c(11, 12)]

threePCA <- prcomp(threetopvar[, c(3:14)], scale. = T, center = T)
ggbiplot(threePCA, ellipse =TRUE, groups = threetopvar$site.type...1, var.axes = T)

#Try a PCA using the Range of values instead of the values

rangevalue <- function(x) {
  max(x) - min(x)
}
rangedata <- alldata %>% group_by(site.type, Site, file) %>% dplyr::summarise(across(c(AcouOccupancy_left, Bioac_left, Ht_left, Hf_left, H_left, ACI_left, AEI_villa_left, M_left, NDSI_left, ADI_left, npic_left), rangevalue))
rangedata <- rangedata %>% group_by(site.type, Site) %>% dplyr::summarise(across(c(AcouOccupancy_left, Bioac_left, Ht_left, Hf_left, H_left, ACI_left, AEI_villa_left, M_left, NDSI_left, ADI_left, npic_left), mean))

rangePCA <- prcomp(rangedata[, c(4, 8, 11)], scale. = T, center = T)
ggbiplot(rangePCA, ellipse = TRUE, groups = rangedata$site.type, var.axes = T)

#Using the SD?
sddata <- alldata %>% group_by(site.type, Site) %>% dplyr::summarise(across(c(AcouOccupancy_left, Bioac_left, Ht_left, Hf_left, H_left, ACI_left, AEI_villa_left, M_left, NDSI_left, ADI_left, npic_left), sd))
sdPCA <- prcomp(sddata[, 3:13], scale. = T, center = T)
ggbiplot(sdPCA, ellipse = T, groups=sddata$site.type, var.axes = T)

#Averages and SDs of top three variables

avgtopthree <- alldata %>% group_by(site.type, Site) %>% dplyr::summarise(across(c(Bioac_left, ACI_left, NDSI_left), mean))
sdtopthree <- alldata %>% group_by(site.type, Site) %>% dplyr::summarise(across(c(Bioac_left, ACI_left, NDSI_left), sd))
sdavg <- cbind(avgtopthree, sdtopthree)
sdavg <- sdavg[, -c(6, 7)]

sdavgPCA <- prcomp(sdavg[, c(3:8)], center = T, scale. = T)
ggbiplot(sdavgPCA, ellipse = TRUE, groups = sdavg$site.type...1, var.axes = T)

#Just AccOcou

aggAcouOccupancy <- dawndata %>% group_by(site.type, Site) %>% dplyr::summarise(avgAcouOccupancy_dawn = mean(AcouOccupancy_left))
aggAcouOccupancyday <- daydata %>% group_by(site.type, Site) %>% dplyr::summarise(avgAcouOccupancy_day = mean(AcouOccupancy_left))
aggAcouOccupancydusk <- duskdata %>% group_by(site.type, Site) %>% dplyr::summarise(avgAcouOccupancy_dusk = mean(AcouOccupancy_left))
aggAcouOccupancynight <- nightdata %>% group_by(site.type, Site) %>% dplyr::summarise(avgAcouOccupancy_night = mean(AcouOccupancy_left))
aggAcouOccupancy$avgAcouOccupancy_day <- aggAcouOccupancyday$avgAcouOccupancy_day
aggAcouOccupancy$avgAcouOccupancy_dusk <- aggAcouOccupancydusk$avgAcouOccupancy_dusk
aggAcouOccupancy$avgAcouOccupancy_night <- aggAcouOccupancynight$avgAcouOccupancy_night

AcouOccupancyPCA <- prcomp(aggAcouOccupancy[, c(3:6)], scale. = T, center = T)
pca1 <- ggbiplot(AcouOccupancyPCA, ellipse =TRUE, groups = aggAcouOccupancy$site.type, var.axes = T)

#JUST Ht
aggHt <- dawndata %>% group_by(site.type, Site) %>% dplyr::summarise(avgHt_dawn = mean(Ht_left))
aggHtday <- daydata %>% group_by(site.type, Site) %>% dplyr::summarise(avgHt_day = mean(Ht_left))
aggHtdusk <- duskdata %>% group_by(site.type, Site) %>% dplyr::summarise(avgHt_dusk = mean(Ht_left))
aggHtnight <- nightdata %>% group_by(site.type, Site) %>% dplyr::summarise(avgHt_night = mean(Ht_left))
aggHt$avgHt_day <- aggHtday$avgHt_day
aggHt$avgHt_dusk <- aggHtdusk$avgHt_dusk
aggHt$avgHt_night <- aggHtnight$avgHt_night

HtPCA <- prcomp(aggHt[, c(3:6)], scale. = T, center = T)
pca3 <- ggbiplot(HtPCA, ellipse =TRUE, groups = aggHt$site.type, var.axes = T)

#JUST Hf

aggHf <- dawndata %>% group_by(site.type, Site) %>% dplyr::summarise(avgHf_dawn = mean(Hf_left))
aggHfday <- daydata %>% group_by(site.type, Site) %>% dplyr::summarise(avgHf_day = mean(Hf_left))
aggHfdusk <- duskdata %>% group_by(site.type, Site) %>% dplyr::summarise(avgHf_dusk = mean(Hf_left))
aggHfnight <- nightdata %>% group_by(site.type, Site) %>% dplyr::summarise(avgHf_night = mean(Hf_left))
aggHf$avgHf_day <- aggHfday$avgHf_day
aggHf$avgHf_dusk <- aggHfdusk$avgHf_dusk
aggHf$avgHf_night <- aggHfnight$avgHf_night

HfPCA <- prcomp(aggHf[, c(3:6)], scale. = T, center = T)
pca4 <- ggbiplot(HfPCA, ellipse =TRUE, groups = aggHf$site.type, var.axes = T)

#JUST H

aggH <- dawndata %>% group_by(site.type, Site) %>% dplyr::summarise(avgH_dawn = mean(H_left))
aggHday <- daydata %>% group_by(site.type, Site) %>% dplyr::summarise(avgH_day = mean(H_left))
aggHdusk <- duskdata %>% group_by(site.type, Site) %>% dplyr::summarise(avgH_dusk = mean(H_left))
aggHnight <- nightdata %>% group_by(site.type, Site) %>% dplyr::summarise(avgH_night = mean(H_left))
aggH$avgH_day <- aggHday$avgH_day
aggH$avgH_dusk <- aggHdusk$avgH_dusk
aggH$avgH_night <- aggHnight$avgH_night

HPCA <- prcomp(aggH[, c(3:6)], scale. = T, center = T)
pca5 <- ggbiplot(HPCA, ellipse =TRUE, groups = aggH$site.type, var.axes = T)

#JUST AEI

aggAEI_villa <- dawndata %>% group_by(site.type, Site) %>% dplyr::summarise(avgAEI_villa_dawn = mean(AEI_villa_left))
aggAEI_villaday <- daydata %>% group_by(site.type, Site) %>% dplyr::summarise(avgAEI_villa_day = mean(AEI_villa_left))
aggAEI_villadusk <- duskdata %>% group_by(site.type, Site) %>% dplyr::summarise(avgAEI_villa_dusk = mean(AEI_villa_left))
aggAEI_villanight <- nightdata %>% group_by(site.type, Site) %>% dplyr::summarise(avgAEI_villa_night = mean(AEI_villa_left))
aggAEI_villa$avgAEI_villa_day <- aggAEI_villaday$avgAEI_villa_day
aggAEI_villa$avgAEI_villa_dusk <- aggAEI_villadusk$avgAEI_villa_dusk
aggAEI_villa$avgAEI_villa_night <- aggAEI_villanight$avgAEI_villa_night

AEI_villaPCA <- prcomp(aggAEI_villa[, c(3:6)], scale. = T, center = T)
pca7 <- ggbiplot(AEI_villaPCA, ellipse =TRUE, groups = aggAEI_villa$site.type, var.axes = T)

#JUST M

aggM <- dawndata %>% group_by(site.type, Site) %>% dplyr::summarise(avgM_dawn = mean(M_left))
aggMday <- daydata %>% group_by(site.type, Site) %>% dplyr::summarise(avgM_day = mean(M_left))
aggMdusk <- duskdata %>% group_by(site.type, Site) %>% dplyr::summarise(avgM_dusk = mean(M_left))
aggMnight <- nightdata %>% group_by(site.type, Site) %>% dplyr::summarise(avgM_night = mean(M_left))
aggM$avgM_day <- aggMday$avgM_day
aggM$avgM_dusk <- aggMdusk$avgM_dusk
aggM$avgM_night <- aggMnight$avgM_night

MPCA <- prcomp(aggM[, c(3:6)], scale. = T, center = T)
pca8 <- ggbiplot(MPCA, ellipse =TRUE, groups = aggM$site.type, var.axes = T)

#JUST ADI

aggADI <- dawndata %>% group_by(site.type, Site) %>% dplyr::summarise(avgADI_dawn = mean(ADI_left))
aggADIday <- daydata %>% group_by(site.type, Site) %>% dplyr::summarise(avgADI_day = mean(ADI_left))
aggADIdusk <- duskdata %>% group_by(site.type, Site) %>% dplyr::summarise(avgADI_dusk = mean(ADI_left))
aggADInight <- nightdata %>% group_by(site.type, Site) %>% dplyr::summarise(avgADI_night = mean(ADI_left))
aggADI$avgADI_day <- aggADIday$avgADI_day
aggADI$avgADI_dusk <- aggADIdusk$avgADI_dusk
aggADI$avgADI_night <- aggADInight$avgADI_night

ADIPCA <- prcomp(aggADI[, c(3:6)], scale. = T, center = T)
pca10 <- ggbiplot(ADIPCA, ellipse =TRUE, groups = aggADI$site.type, var.axes = T)

#Just npic 

aggnpic <- dawndata %>% group_by(site.type, Site) %>% dplyr::summarise(avgnpic_dawn = mean(npic_left))
aggnpicday <- daydata %>% group_by(site.type, Site) %>% dplyr::summarise(avgnpic_day = mean(npic_left))
aggnpicdusk <- duskdata %>% group_by(site.type, Site) %>% dplyr::summarise(avgnpic_dusk = mean(npic_left))
aggnpicnight <- nightdata %>% group_by(site.type, Site) %>% dplyr::summarise(avgnpic_night = mean(npic_left))
aggnpic$avgnpic_day <- aggnpicday$avgnpic_day
aggnpic$avgnpic_dusk <- aggnpicdusk$avgnpic_dusk
aggnpic$avgnpic_night <- aggnpicnight$avgnpic_night

npicPCA <- prcomp(aggnpic[, c(3:6)], scale. = T, center = T)
pca11 <- ggbiplot(npicPCA, ellipse =TRUE, groups = aggnpic$site.type, var.axes = T)

# Plot all single index PCA's with TOM breakdown

grid.arrange(pca1, pca2, pca3, pca4, pca5, pca6, pca7, pca8, pca9, pca10, pca11, ncol=4)


# Try creating columns of minutes and rows of sites 
sitesandnames <- alldata[,c(14, 15)]
sitesandnames <- sitesandnames %>% distinct()

#All min of BioAc during four times of day (AccOcou doesn't work)

dawnBio <- unstack(dawndata, Bioac_left ~ Site)
dawnBio <- lapply(dawnBio, "[", seq(min(lengths(dawnBio))))
dawnBio <- as.data.frame(t(bind_rows(dawnBio)))

pcadawnBio <- prcomp(dawnBio, center = T, scale. = T)
dbpca <- ggbiplot(pcadawnBio, ellipse = T, groups = sitesandnames$site.type, var.axes = F)

dayBio <- unstack(daydata, Bioac_left ~ Site)
dayBio <- lapply(dayBio, "[", seq(min(lengths(dayBio))))
dayBio <- as.data.frame(t(bind_rows(dayBio)))

pcadayBio <- prcomp(daybio, center = T, scale. = T)
daybpca <- ggbiplot(pcadayBio, ellipse = T, groups = sitesandnames$site.type, var.axes = F)

duskBio <- unstack(duskdata, Bioac_left ~ Site)
duskBio <- lapply(duskBio, "[", seq(min(lengths(duskBio))))
duskBio <- as.data.frame(t(bind_rows(duskBio)))

pcaduskBio <- prcomp(duskBio, center = T, scale. = T)
duskbpca <- ggbiplot(pcaduskBio, ellipse = T, groups = sitesandnames$site.type, var.axes = F)

nightBio <- unstack(nightdata, Bioac_left ~ Site)
nightBio <- lapply(nightBio, "[", seq(min(lengths(nightBio))))
nightBio <- as.data.frame(t(bind_rows(nightBio)))

pcanightBio <- prcomp(nightBio, center = T, scale. = T)
nbpca <- ggbiplot(pcanightBio, ellipse = T, groups = sitesandnames$site.type, var.axes = F)

grid.arrange(dbpca, daybpca, duskbpca, nbpca, ncol=2)

# Now with Ht

dawnHt <- unstack(dawndata, Ht_left ~ Site)
dawnHt <- lapply(dawnHt, "[", seq(min(lengths(dawnHt))))
dawnHt <- as.data.frame(t(bind_rows(dawnHt)))

pcadawnHt <- prcomp(dawnHt, center = T, scale. = T)
dawnhtpca <- ggbiplot(pcadawnHt, ellipse = T, groups = sitesandnames$site.type, var.axes = F)

dayHt <- unstack(daydata, Ht_left ~ Site)
dayHt <- lapply(dayHt, "[", seq(min(lengths(dayHt))))
dayHt <- as.data.frame(t(bind_rows(dayHt)))

pcadayHt <- prcomp(dayHt, center = T, scale. = T)
dayhtpca <- ggbiplot(pcadayHt, ellipse = T, groups = sitesandnames$site.type, var.axes = F)

duskHt <- unstack(duskdata, Ht_left ~ Site)
duskHt <- lapply(duskHt, "[", seq(min(lengths(duskHt))))
duskHt <- as.data.frame(t(bind_rows(duskHt)))

pcaduskHt <- prcomp(duskHt, center = T, scale. = T)
duskhtpca <- ggbiplot(pcaduskHt, ellipse = T, groups = sitesandnames$site.type, var.axes = F)

nightHt <- unstack(nightdata, Ht_left ~ Site)
nightHt <- lapply(nightHt, "[", seq(min(lengths(nightHt))))
nightHt <- as.data.frame(t(bind_rows(nightHt)))

pcanightHt <- prcomp(nightHt, center = T, scale. = T)
nighthtpca <- ggbiplot(pcanightHt, ellipse = T, groups = sitesandnames$site.type, var.axes = F)

grid.arrange(dawnhtpca, dayhtpca, duskhtpca, nighthtpca, ncol= 2)

#For Hf

dawnHf <- unstack(dawndata, Hf_left ~ Site)
dawnHf <- lapply(dawnHf, "[", seq(min(lengths(dawnHf))))
dawnHf <- as.data.frame(t(bind_rows(dawnHf)))

pcadawnHf <- prcomp(dawnHf, center = T, scale. = T)
dawnhfpca <- ggbiplot(pcadawnHf, ellipse = T, groups = sitesandnames$site.type, var.axes = F)

dayHf <- unstack(daydata, Hf_left ~ Site)
dayHf <- lapply(dayHf, "[", seq(min(lengths(dayHf))))
dayHf <- as.data.frame(t(bind_rows(dayHf)))

pcadayHf <- prcomp(dayHf, center = T, scale. = T)
dayhfpca <- ggbiplot(pcadayHf, ellipse = T, groups = sitesandnames$site.type, var.axes = F)

duskHf <- unstack(duskdata, Hf_left ~ Site)
duskHf <- lapply(duskHf, "[", seq(min(lengths(duskHf))))
duskHf <- as.data.frame(t(bind_rows(duskHf)))

pcaduskHf <- prcomp(duskHf, center = T, scale. = T)
duskhfpca <- ggbiplot(pcaduskHf, ellipse = T, groups = sitesandnames$site.type, var.axes = F)

nightHf <- unstack(nightdata, Hf_left ~ Site)
nightHf <- lapply(nightHf, "[", seq(min(lengths(nightHf))))
nightHf <- as.data.frame(t(bind_rows(nightHf)))

pcanightHf <- prcomp(nightHf, center = T, scale. = T)
nhfpca <- ggbiplot(pcanightHf, ellipse = T, groups = sitesandnames$site.type, var.axes = F)

grid.arrange(dawnhfpca, dayhfpca, duskhfpca, nhfpca, ncol=2)

#For H

dawnH <- unstack(dawndata, H_left ~ Site)
dawnH <- lapply(dawnH, "[", seq(min(lengths(dawnH))))
dawnH <- as.data.frame(t(bind_rows(dawnH)))

pcadawnH <- prcomp(dawnH, center = T, scale. = T)
dawnHpca <- ggbiplot(pcadawnH, ellipse = T, groups = sitesandnames$site.type, var.axes = F)

dayH <- unstack(daydata, H_left ~ Site)
dayH <- lapply(dayH, "[", seq(min(lengths(dayH))))
dayH <- as.data.frame(t(bind_rows(dayH)))

pcadayH <- prcomp(dayH, center = T, scale. = T)
dayHpca <- ggbiplot(pcadayH, ellipse = T, groups = sitesandnames$site.type, var.axes = F)

duskH <- unstack(duskdata, H_left ~ Site)
duskH <- lapply(duskH, "[", seq(min(lengths(duskH))))
duskH <- as.data.frame(t(bind_rows(duskH)))

pcaduskH <- prcomp(duskH, center = T, scale. = T)
duskHpca <- ggbiplot(pcaduskH, ellipse = T, groups = sitesandnames$site.type, var.axes = F)

nightH <- unstack(nightdata, H_left ~ Site)
nightH <- lapply(nightH, "[", seq(min(lengths(nightH))))
nightH <- as.data.frame(t(bind_rows(nightH)))

pcanightH <- prcomp(nightH, center = T, scale. = T)
nHpca <- ggbiplot(pcanightH, ellipse = T, groups = sitesandnames$site.type, var.axes = F)

grid.arrange(dawnHpca, dayHpca, duskHpca, nHpca, ncol=2)

#ACI
dawnACI <- unstack(dawndata, ACI_left ~ Site)
dawnACI <- lapply(dawnACI, "[", seq(min(lengths(dawnACI))))
dawnACI <- as.data.frame(t(bind_rows(dawnACI)))

pcadawnACI <- prcomp(dawnACI, center = T, scale. = T)
dawnACIpca <- ggbiplot(pcadawnACI, ellipse = T, groups = sitesandnames$site.type, var.axes = F)

dayACI <- unstack(daydata, ACI_left ~ Site)
dayACI <- lapply(dayACI, "[", seq(min(lengths(dayACI))))
dayACI <- as.data.frame(t(bind_rows(dayACI)))

pcadayACI <- prcomp(dayACI, center = T, scale. = T)
dayACIpca <- ggbiplot(pcadayACI, ellipse = T, groups = sitesandnames$site.type, var.axes = F)

duskACI <- unstack(duskdata, ACI_left ~ Site)
duskACI <- lapply(duskACI, "[", seq(min(lengths(duskACI))))
duskACI <- as.data.frame(t(bind_rows(duskACI)))

pcaduskACI <- prcomp(duskACI, center = T, scale. = T)
duskACIpca <- ggbiplot(pcaduskACI, ellipse = T, groups = sitesandnames$site.type, var.axes = F)

nightACI <- unstack(nightdata, ACI_left ~ Site)
nightACI <- lapply(nightACI, "[", seq(min(lengths(nightACI))))
nightACI <- as.data.frame(t(bind_rows(nightACI)))

pcanightACI <- prcomp(nightACI, center = T, scale. = T)
nACIpca <- ggbiplot(pcanightACI, ellipse = T, groups = sitesandnames$site.type, var.axes = F)

grid.arrange(dawnACIpca, dayACIpca, duskACIpca, nACIpca, ncol=2)

# AEI villa 

dawnAEI_villa <- unstack(dawndata, AEI_villa_left ~ Site)
dawnAEI_villa <- lapply(dawnAEI_villa, "[", seq(min(lengths(dawnAEI_villa))))
dawnAEI_villa <- as.data.frame(t(bind_rows(dawnAEI_villa)))

pcadawnAEI_villa <- prcomp(dawnAEI_villa, center = T, scale. = T)
dawnAEI_villapca <- ggbiplot(pcadawnAEI_villa, ellipse = T, groups = sitesandnames$site.type, var.axes = F)

dayAEI_villa <- unstack(daydata, AEI_villa_left ~ Site)
dayAEI_villa <- lapply(dayAEI_villa, "[", seq(min(lengths(dayAEI_villa))))
dayAEI_villa <- as.data.frame(t(bind_rows(dayAEI_villa)))

pcadayAEI_villa <- prcomp(dayAEI_villa, center = T, scale. = T)
dayAEI_villapca <- ggbiplot(pcadayAEI_villa, ellipse = T, groups = sitesandnames$site.type, var.axes = F)

duskAEI_villa <- unstack(duskdata, AEI_villa_left ~ Site)
duskAEI_villa <- lapply(duskAEI_villa, "[", seq(min(lengths(duskAEI_villa))))
duskAEI_villa <- as.data.frame(t(bind_rows(duskAEI_villa)))

pcaduskAEI_villa <- prcomp(duskAEI_villa, center = T, scale. = T)
duskAEI_villapca <- ggbiplot(pcaduskAEI_villa, ellipse = T, groups = sitesandnames$site.type, var.axes = F)

nightAEI_villa <- unstack(nightdata, AEI_villa_left ~ Site)
nightAEI_villa <- lapply(nightAEI_villa, "[", seq(min(lengths(nightAEI_villa))))
nightAEI_villa <- as.data.frame(t(bind_rows(nightAEI_villa)))

pcanightAEI_villa <- prcomp(nightAEI_villa, center = T, scale. = T)
nAEI_villapca <- ggbiplot(pcanightAEI_villa, ellipse = T, groups = sitesandnames$site.type, var.axes = F)

grid.arrange(dawnAEI_villapca, dayAEI_villapca, duskAEI_villapca, nAEI_villapca, ncol=2)

# M
dawnM <- unstack(dawndata, M_left ~ Site)
dawnM <- lapply(dawnM, "[", seq(min(lengths(dawnM))))
dawnM <- as.data.frame(t(bind_rows(dawnM)))

pcadawnM <- prcomp(dawnM, center = T, scale. = T)
dawnMpca <- ggbiplot(pcadawnM, ellipse = T, groups = sitesandnames$site.type, var.axes = F)

dayM <- unstack(daydata, M_left ~ Site)
dayM <- lapply(dayM, "[", seq(min(lengths(dayM))))
dayM <- as.data.frame(t(bind_rows(dayM)))

pcadayM <- prcomp(dayM, center = T, scale. = T)
dayMpca <- ggbiplot(pcadayM, ellipse = T, groups = sitesandnames$site.type, var.axes = F)

duskM <- unstack(duskdata, M_left ~ Site)
duskM <- lapply(duskM, "[", seq(min(lengths(duskM))))
duskM <- as.data.frame(t(bind_rows(duskM)))

pcaduskM <- prcomp(duskM, center = T, scale. = T)
duskMpca <- ggbiplot(pcaduskM, ellipse = T, groups = sitesandnames$site.type, var.axes = F)

nightM <- unstack(nightdata, M_left ~ Site)
nightM <- lapply(nightM, "[", seq(min(lengths(nightM))))
nightM <- as.data.frame(t(bind_rows(nightM)))

pcanightM <- prcomp(nightM, center = T, scale. = T)
nMpca <- ggbiplot(pcanightM, ellipse = T, groups = sitesandnames$site.type, var.axes = F)

grid.arrange(dawnMpca, dayMpca, duskMpca, nMpca, ncol=2)

#NDSI 

dawnNDSI <- unstack(dawndata, NDSI_left ~ Site)
dawnNDSI <- lapply(dawnNDSI, "[", seq(min(lengths(dawnNDSI))))
dawnNDSI <- as.data.frame(t(bind_rows(dawnNDSI)))

pcadawnNDSI <- prcomp(dawnNDSI, center = T, scale. = T)
dawnNDSIpca <- ggbiplot(pcadawnNDSI, ellipse = T, groups = sitesandnames$site.type, var.axes = F)

dayNDSI <- unstack(daydata, NDSI_left ~ Site)
dayNDSI <- lapply(dayNDSI, "[", seq(min(lengths(dayNDSI))))
dayNDSI <- as.data.frame(t(bind_rows(dayNDSI)))

pcadayNDSI <- prcomp(dayNDSI, center = T, scale. = T)
dayNDSIpca <- ggbiplot(pcadayNDSI, ellipse = T, groups = sitesandnames$site.type, var.axes = F)

duskNDSI <- unstack(duskdata, NDSI_left ~ Site)
duskNDSI <- lapply(duskNDSI, "[", seq(min(lengths(duskNDSI))))
duskNDSI <- as.data.frame(t(bind_rows(duskNDSI)))

pcaduskNDSI <- prcomp(duskNDSI, center = T, scale. = T)
duskNDSIpca <- ggbiplot(pcaduskNDSI, ellipse = T, groups = sitesandnames$site.type, var.axes = F)

nightNDSI <- unstack(nightdata, NDSI_left ~ Site)
nightNDSI <- lapply(nightNDSI, "[", seq(min(lengths(nightNDSI))))
nightNDSI <- as.data.frame(t(bind_rows(nightNDSI)))

pcanightNDSI <- prcomp(nightNDSI, center = T, scale. = T)
nNDSIpca <- ggbiplot(pcanightNDSI, ellipse = T, groups = sitesandnames$site.type, var.axes = F)

grid.arrange(dawnNDSIpca, dayNDSIpca, duskNDSIpca, nNDSIpca, ncol=2)

#ADI  

dawnADI <- unstack(dawndata, ADI_left ~ Site)
dawnADI <- lapply(dawnADI, "[", seq(min(lengths(dawnADI))))
dawnADI <- as.data.frame(t(bind_rows(dawnADI)))

pcadawnADI <- prcomp(dawnADI, center = T, scale. = T)
dawnADIpca <- ggbiplot(pcadawnADI, ellipse = T, groups = sitesandnames$site.type, var.axes = F)

dayADI <- unstack(daydata, ADI_left ~ Site)
dayADI <- lapply(dayADI, "[", seq(min(lengths(dayADI))))
dayADI <- as.data.frame(t(bind_rows(dayADI)))

pcadayADI <- prcomp(dayADI, center = T, scale. = T)
dayADIpca <- ggbiplot(pcadayADI, ellipse = T, groups = sitesandnames$site.type, var.axes = F)

duskADI <- unstack(duskdata, ADI_left ~ Site)
duskADI <- lapply(duskADI, "[", seq(min(lengths(duskADI))))
duskADI <- as.data.frame(t(bind_rows(duskADI)))

pcaduskADI <- prcomp(duskADI, center = T, scale. = T)
duskADIpca <- ggbiplot(pcaduskADI, ellipse = T, groups = sitesandnames$site.type, var.axes = F)

nightADI <- unstack(nightdata, ADI_left ~ Site)
nightADI <- lapply(nightADI, "[", seq(min(lengths(nightADI))))
nightADI <- as.data.frame(t(bind_rows(nightADI)))

pcanightADI <- prcomp(nightADI, center = T, scale. = T)
nADIpca <- ggbiplot(pcanightADI, ellipse = T, groups = sitesandnames$site.type, var.axes = F)

grid.arrange(dawnADIpca, dayADIpca, duskADIpca, nADIpca, ncol=2)

#npic 

dawnnpic <- unstack(dawndata, npic_left ~ Site)
dawnnpic <- lapply(dawnnpic, "[", seq(min(lengths(dawnnpic))))
dawnnpic <- as.data.frame(t(bind_rows(dawnnpic)))

pcadawnnpic <- prcomp(dawnnpic, center = T, scale. = T)
dawnnpicpca <- ggbiplot(pcadawnnpic, ellipse = T, groups = sitesandnames$site.type, var.axes = F)

daynpic <- unstack(daydata, npic_left ~ Site)
daynpic <- lapply(daynpic, "[", seq(min(lengths(daynpic))))
daynpic <- as.data.frame(t(bind_rows(daynpic)))

pcadaynpic <- prcomp(daynpic, center = T, scale. = T)
daynpicpca <- ggbiplot(pcadaynpic, ellipse = T, groups = sitesandnames$site.type, var.axes = F)

dusknpic <- unstack(duskdata, npic_left ~ Site)
dusknpic <- lapply(dusknpic, "[", seq(min(lengths(dusknpic))))
dusknpic <- as.data.frame(t(bind_rows(dusknpic)))

pcadusknpic <- prcomp(dusknpic, center = T, scale. = T)
dusknpicpca <- ggbiplot(pcadusknpic, ellipse = T, groups = sitesandnames$site.type, var.axes = F)

nightnpic <- unstack(nightdata, npic_left ~ Site)
nightnpic <- lapply(nightnpic, "[", seq(min(lengths(nightnpic))))
nightnpic <- as.data.frame(t(bind_rows(nightnpic)))

pcanightnpic <- prcomp(nightnpic, center = T, scale. = T)
nnpicpca <- ggbiplot(pcanightnpic, ellipse = T, groups = sitesandnames$site.type, var.axes = F)

grid.arrange(dawnnpicpca, daynpicpca, dusknpicpca, nnpicpca, ncol=2)


#RANDOM FOREST

set.seed(120)  # Setting seed

rfdata <- alldata

split <- sample.split(rfdata, SplitRatio = 0.7)
split

train <- subset(rfdata, split == "TRUE")
test <- subset(rfdata, split == "FALSE")


classifier_RF <- randomForest(x = train[,1:12],
                             y = as.factor(train$site.type),
                             ntree = 100)

save(classifier_RF, file = "C://Users/giaco/Documents/Master Thesis/Classifier_RF.RData")
classifier_RF

y_pred <- predict(classifier_RF, newdata = test[, 1:12])       

confusion_mtx <- table(test$site.type, y_pred)
confusion_mtx
confmtx<-as.data.frame(confusion_mtx)
colnames(confmtx) <- c("SiteType", "PredictedSiteType", "Frequency")
confmtx$PredictedSiteType <- factor(confmtx$PredictedSiteType, levels=rev(levels(confmtx$PredictedSiteType)))


ggplot(confmtx, aes(x = PredictedSiteType, y= SiteType, fill= percentage)) + 
  geom_tile() +
  geom_text(aes(label = paste(percent, Frequency, sep = '\n'))) +
  ylab("Site Type") +
  xlab("Predicted Site Type")+
  scale_x_discrete(position="top", limits = rev) +
  scale_y_discrete(limits=rev) +
  labs(fill = "Percent")
  


plot(classifier_RF)

importance(classifier_RF)
varImpPlot(classifier_RF)


#######################
#LDA Analysis

p_load(klaR)
p_load(psych)
p_load(MASS)
p_load(ggord)
p_load(devtools)
p_load(ggord)

indexdata <- alldata[ , c(1:12, 15)]
set.seed(123)
ind <- sample(2, nrow(tempdata),
              replace = TRUE,
              prob = c(0.6, 0.4))
training <- tempdata[ind==1,]
testing <- tempdata[ind==2,]

linear <- lda(site.type~., training)

p <- predict(linear, testing)
ldahist(data = p$x[,1], g = training[,-8]$site.type)
ldahist(data = p$x[,2], g = training[,-8]$site.type)

ggbiplot(linear, ellipse = T, groups = training$site.type, var.axes = T)





