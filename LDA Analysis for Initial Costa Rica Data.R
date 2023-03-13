#####
#We load in an already clean data frame
#If data frame is missing or damaged check the R script: "CleanedCostaRica.R"
#####

alldata <- read.csv("C://Users/giaco/Documents/Master Thesis/CleanedDataCostaRica.csv")
alldata$site.type <- factor(alldata$site.type, levels = c("Reference Forest", "Conservation", "Reforestation", "Pasture"))


library(pacman)
p_load(ggplot2)
p_load(dplyr)
p_load(tidyverse)
p_load(ggbiplot)
p_load(rlist)
p_load(gridExtra)
p_load(caTools)
p_load(randomForest)
p_load(broom)
p_load(ggpubr)
p_load(caret)
p_load(MASS)
p_load(devtools)
p_load(mvtnorm)

#####
#Make Divisions for time of day and then aggregate them by the means of each index for each site
#####

dawn <- (alldata$start < 420 & alldata$start > 239)
day <- (alldata$start > 419 & alldata$start < 1080)
dusk <- (alldata$start > 1079 & alldata$start < 1260)
night <- (alldata$start < 240 | alldata$start > 1259)
dawndata <- subset(alldata, dawn)
daydata <- subset(alldata, day)
duskdata <- subset(alldata, dusk)
nightdata <- subset(alldata, night)
meansdawn <- aggregate(cbind(AcouOccupancy_left, Bioac_left, Ht_left, Hf_left, H_left, ACI_left, AEI_villa_left, M_left, NDSI_left, ADI_left, npic_left) ~ Site + site.type, data = dawndata, mean, na.rm = TRUE)
meansday <- aggregate(cbind(AcouOccupancy_left, Bioac_left, Ht_left, Hf_left, H_left, ACI_left, AEI_villa_left, M_left, NDSI_left, ADI_left, npic_left) ~ Site + site.type, data = daydata, mean, na.rm = TRUE)
meansdusk <- aggregate(cbind(AcouOccupancy_left, Bioac_left, Ht_left, Hf_left, H_left, ACI_left, AEI_villa_left, M_left, NDSI_left, ADI_left, npic_left) ~ Site + site.type, data = duskdata, mean, na.rm = TRUE)
meansnight <- aggregate(cbind(AcouOccupancy_left, Bioac_left, Ht_left, Hf_left, H_left, ACI_left, AEI_villa_left, M_left, NDSI_left, ADI_left, npic_left) ~ Site + site.type, data = nightdata, mean, na.rm = TRUE)

#####
#The best division we found before was using all averaged indices for each site, during only day and dusk
# Here is that PCA and ANOVA
#####

dayduskmeans <- cbind(meansday, meansdusk[,3:13])
PCAdaydusk <- prcomp(dayduskmeans[,3:24], scale. = T, center = T)
ggbiplot(PCAdaydusk, ellipse = T, groups = dayduskmeans$site.type, var.axes = T) +
  labs(color= "Site Type") +
  xlab("Principal Component 1 (33.9%)") +
  ylab("Principal Component 2 (17.5%)") +
  theme_classic() +
  theme(text=element_text(family="serif", size = 14))

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


#####
# Aggregate the data by minute of the day, such that each site type has 1440 observations (an average for that time of day) for each index regardess of site origin
#####

meansmin <- aggregate(cbind(AcouOccupancy_left, Bioac_left, Ht_left, Hf_left, H_left, ACI_left, AEI_villa_left, M_left, NDSI_left, ADI_left, npic_left) ~ start + site.type, data = alldata, mean, na.rm = TRUE)

#PCA 
PCAbymin <- prcomp(meansmin[, c(5, 14)], scale. = T, center = T)
ggbiplot(PCAbymin, ellipse = TRUE, groups = meansmin$site.type, var.axes = T)

#anova 
valuesPCAbymin <- as.data.frame(PCAbymin$x)
anovabymin <- aov(valuesPCAbymin$PC2 ~meansmin$site.type)
summary(anovabymin)
plot(anovabymin)

tukey.daydusk <- TukeyHSD(anovabymin)
tukey.daydusk

#PC1 on a boxplot
ggplot(valuesPCAbymin, aes(x = meansmin$site.type, y = PC1, fill = meansmin$site.type)) +  
  geom_boxplot() +
  xlab("Site Type") +
  ylab("Principal Component 1 Values") +
  theme_classic() +
  theme(legend.position="none", text=element_text(family="serif", size = 14))

#LDA
set.seed(123)
split <- sample.split(meansmin, SplitRatio = 0.7)
split

train <- subset(meansmin[ , -1], split == "TRUE")
test <- subset(meansmin[ , -1], split == "FALSE")

preproc.parameter <- train %>% 
  preProcess(method = c("center", "scale"))

# Transform the data using the estimated parameters
train.transform <- preproc.parameter %>% predict(train)
test.transform <- preproc.parameter %>% predict(test)


model <- lda(site.type~., data = train[ , -9])

predictions <- model %>% predict(test[ , -9])
mean(predictions$class==test$site.type)

p <- predict(model, test[ , -9])
ldahist(data = p$x[,1], g = train$site.type)
ldahist(data = p$x[,2], g = train$site.type)

ggbiplot(model, ellipse = T, groups = train$site.type, var.axes = T)

#QDA

qmodel <- qda(site.type~., data = train)
qmodel

predictions_QDA = data.frame(predict(model_QDA, test))

predictions_QDA = cbind(test, predictions_QDA)

predictions_QDA %>%
  count(class, Direction)

predictions_QDA %>%
  summarize(score = mean(class == Direction))
