pg1<- alldata %>% group_by(start,site.type,Site) %>% 
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


pg2<-alldata %>% group_by(start,site.type,Site) %>% 
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

pg3<-alldata %>% group_by(start,site.type,Site) %>% 
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

pg4<-alldata %>% group_by(start,site.type,Site) %>% 
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

pg5<-alldata %>% group_by(start,site.type,Site) %>% 
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

pg6<-alldata %>% group_by(start,site.type,Site) %>% 
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

pg7<-alldata %>% group_by(start,site.type,Site) %>% 
  summarise_at(vars(ACI_left), list(Avg = mean)) %>%
  ggplot(aes(x= start, y=Avg, color = site.type)) +
  geom_point() +
  geom_vline(xintercept = 420) +
  geom_vline(xintercept = 1080) +
  xlab("Minute of the Day")+
  ylab("Acoustic Complextiy Index") +
  labs(color = "Site Type") +
  theme(text=element_text(family="serif", size = 14))+
  theme(legend.position="none")

pg8<-alldata %>% group_by(start,site.type,Site) %>% 
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

pg9<-alldata %>% group_by(start,site.type,Site) %>% 
  summarise_at(vars(M_left), list(Avg = mean)) %>%
  ggplot(aes(x= start, y=Avg, color = site.type)) +
  geom_point() +
  geom_vline(xintercept = 420) +
  geom_vline(xintercept = 1080) +
  xlab("Minute of the Day") +
  ylab("Median of Amplitude Envelope")+
  labs(color = "Site Type") +
  theme(text=element_text(family="serif", size = 14))+
  theme(legend.position="none")

pg10<-alldata %>% group_by(start,site.type,Site) %>% 
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

pg11<-alldata %>% group_by(start,site.type,Site) %>% 
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

 ###### Means per site and site type per Period of Day
meansdawn <- aggregate(cbind(AcouOccupancy_left, Bioac_left, Ht_left, Hf_left, H_left, ACI_left, AEI_villa_left, M_left, NDSI_left, ADI_left, npic_left) ~ Site + site.type, data = dawndata, mean, na.rm = TRUE)
meansdawn$POD <- "Dawn"
meansday <- aggregate(cbind(AcouOccupancy_left, Bioac_left, Ht_left, Hf_left, H_left, ACI_left, AEI_villa_left, M_left, NDSI_left, ADI_left, npic_left) ~ Site + site.type, data = daydata, mean, na.rm = TRUE)
meansday$POD <- "Day"
meansdusk <- aggregate(cbind(AcouOccupancy_left, Bioac_left, Ht_left, Hf_left, H_left, ACI_left, AEI_villa_left, M_left, NDSI_left, ADI_left, npic_left) ~ Site + site.type, data = duskdata, mean, na.rm = TRUE)
meansdusk$POD <- "Dusk"
meansnight <- aggregate(cbind(AcouOccupancy_left, Bioac_left, Ht_left, Hf_left, H_left, ACI_left, AEI_villa_left, M_left, NDSI_left, ADI_left, npic_left) ~ Site + site.type, data = nightdata, mean, na.rm = TRUE)
meansnight$POD <- "Night"

PODdata <- rbind(meansdawn, meansday, meansdusk, meansnight)

reforestPOD <- (PODdata$site.type == "Reforestation")
consPOD <- (PODdata$site.type == "Conservation")
referencePOD <- (PODdata$site.type == "Reference Forest")
pastPOD <- (PODdata$site.type == "Pasture")

reforestPODdata <- subset(PODdata, reforestPOD)
consPODdata <- subset(PODdata, consPOD)
referencePODdata <- subset(PODdata, referencePOD)
pastPODdata <- subset(PODdata, pastPOD)

PODaccou1 <- ggplot(data=reforestPODdata, aes(x=POD, y=AcouOccupancy_left, group = Site, color = Site)) +
  geom_line() +
  geom_point() +
  ggtitle("Reforestation") +
  theme(legend.position="none")
PODaccou2 <- ggplot(data=consPODdata, aes(x=POD, y=AcouOccupancy_left, group = Site, color = Site)) +
  geom_line( ) +
  geom_point() +
  ggtitle("Conservation")+
  theme(legend.position="none")
PODaccou3 <- ggplot(data=referencePODdata, aes(x=POD, y=AcouOccupancy_left, group = Site, color = Site)) +
  geom_line( ) +
  geom_point() +
  ggtitle("Reference") +
  theme(legend.position="none")
PODaccou4 <- ggplot(data=pastPODdata, aes(x=POD, y=AcouOccupancy_left, group = Site, color = Site)) +
  geom_line( ) +
  geom_point() +
  ggtitle("Pasture") +
  theme(legend.position="none")
grid.arrange(PODaccou1, PODaccou2, PODaccou3, PODaccou4,  ncol=2, vp=viewport(width=0.9, height=0.9))

PODBioac1 <- ggplot(data=reforestPODdata, aes(x=POD, y=Bioac_left, group = Site, color = Site)) +
  geom_line( ) +
  geom_point() +
  ggtitle("Reforestation") +
  theme(legend.position="none")
PODBioac2 <- ggplot(data=consPODdata, aes(x=POD, y=Bioac_left, group = Site, color = Site)) +
  geom_line( ) +
  geom_point() +
  ggtitle("Conservation") +
  theme(legend.position="none")
PODBioac3 <- ggplot(data=referencePODdata, aes(x=POD, y=Bioac_left, group = Site, color = Site)) +
  geom_line( ) +
  geom_point() +
  ggtitle("Reference") +
  theme(legend.position="none")
PODBioac4 <- ggplot(data=pastPODdata, aes(x=POD, y=Bioac_left, group = Site, color = Site)) +
  geom_line( ) +
  geom_point() +
  ggtitle("Pasture") +
  theme(legend.position="none")
grid.arrange(PODBioac1, PODBioac2, PODBioac3, PODBioac4,  ncol=2, vp=viewport(width=0.9, height=0.9))

PODHt1 <- ggplot(data=reforestPODdata, aes(x=POD, y=Ht_left, group = Site, color = Site)) +
  geom_line( ) +
  geom_point() +
  ggtitle("Reforestation") +
  theme(legend.position="none")
PODHt2 <- ggplot(data=consPODdata, aes(x=POD, y=Ht_left, group = Site, color = Site)) +
  geom_line( ) +
  geom_point() +
  ggtitle("Conservation") +
  theme(legend.position="none")
PODHt3 <- ggplot(data=referencePODdata, aes(x=POD, y=Ht_left, group = Site, color = Site)) +
  geom_line( ) +
  geom_point() +
  ggtitle("Reference") +
  theme(legend.position="none")
PODHt4 <- ggplot(data=pastPODdata, aes(x=POD, y=Ht_left, group = Site, color = Site)) +
  geom_line( ) +
  geom_point() +
  ggtitle("Pasture") +
  theme(legend.position="none")
grid.arrange(PODHt1, PODHt2, PODHt3, PODHt4,  ncol=2, vp=viewport(width=0.9, height=0.9))

PODHf1 <- ggplot(data=reforestPODdata, aes(x=POD, y=Hf_left, group = Site, color = Site)) +
  geom_line( ) +
  geom_point() +
  ggtitle("Reforestation") +
  theme(legend.position="none")
PODHf2 <- ggplot(data=consPODdata, aes(x=POD, y=Hf_left, group = Site, color = Site)) +
  geom_line( ) +
  geom_point() +
  ggtitle("Conservation") +
  theme(legend.position="none")
PODHf3 <- ggplot(data=referencePODdata, aes(x=POD, y=Hf_left, group = Site, color = Site)) +
  geom_line( ) +
  geom_point() +
  ggtitle("Reference") +
  theme(legend.position="none")
PODHf4 <- ggplot(data=pastPODdata, aes(x=POD, y=Hf_left, group = Site, color = Site)) +
  geom_line( ) +
  geom_point() +
  ggtitle("Pasture") +
  theme(legend.position="none")
grid.arrange(PODHf1, PODHf2, PODHf3, PODHf4,  ncol=2, vp=viewport(width=0.9, height=0.9))

PODH1 <- ggplot(data=reforestPODdata, aes(x=POD, y=H_left, group = Site, color = Site)) +
  geom_line( ) +
  geom_point() +
  ggtitle("Reforestation") +
  theme(legend.position="none")
PODH2 <- ggplot(data=consPODdata, aes(x=POD, y=H_left, group = Site, color = Site)) +
  geom_line( ) +
  geom_point() +
  ggtitle("Conservation") +
  theme(legend.position="none")
PODH3 <- ggplot(data=referencePODdata, aes(x=POD, y=H_left, group = Site, color = Site)) +
  geom_line( ) +
  geom_point() +
  ggtitle("Reference") +
  theme(legend.position="none")
PODH4 <- ggplot(data=pastPODdata, aes(x=POD, y=H_left, group = Site, color = Site)) +
  geom_line( ) +
  geom_point() +
  ggtitle("Pasture") +
  theme(legend.position="none")
grid.arrange(PODH1, PODH2, PODH3, PODH4,  ncol=2, vp=viewport(width=0.9, height=0.9))

PODACI1 <- ggplot(data=reforestPODdata, aes(x=POD, y=ACI_left, group = Site, color = Site)) +
  geom_line( ) +
  geom_point() +
  ggtitle("Reforestation") +
  theme(legend.position="none")
PODACI2 <- ggplot(data=consPODdata, aes(x=POD, y=ACI_left, group = Site, color = Site)) +
  geom_line( ) +
  geom_point() +
  ggtitle("Conservation") +
  theme(legend.position="none")
PODACI3 <- ggplot(data=referencePODdata, aes(x=POD, y=ACI_left, group = Site, color = Site)) +
  geom_line( ) +
  geom_point() +
  ggtitle("Reference") +
  theme(legend.position="none")
PODACI4 <- ggplot(data=pastPODdata, aes(x=POD, y=ACI_left, group = Site, color = Site)) +
  geom_line( ) +
  geom_point() +
  ggtitle("Pasture") +
  theme(legend.position="none")
grid.arrange(PODACI1, PODACI2, PODACI3, PODACI4,  ncol=2, vp=viewport(width=0.9, height=0.9))

PODAEI_villa1 <- ggplot(data=reforestPODdata, aes(x=POD, y=AEI_villa_left, group = Site, color = Site)) +
  geom_line( ) +
  geom_point() +
  ggtitle("Reforestation") +
  theme(legend.position="none")
PODAEI_villa2 <- ggplot(data=consPODdata, aes(x=POD, y=AEI_villa_left, group = Site, color = Site)) +
  geom_line( ) +
  geom_point() +
  ggtitle("Conservation") +
  theme(legend.position="none")
PODAEI_villa3 <- ggplot(data=referencePODdata, aes(x=POD, y=AEI_villa_left, group = Site, color = Site)) +
  geom_line( ) +
  geom_point() +
  ggtitle("Reference") +
  theme(legend.position="none")
PODAEI_villa4 <- ggplot(data=pastPODdata, aes(x=POD, y=AEI_villa_left, group = Site, color = Site)) +
  geom_line( ) +
  geom_point() +
  ggtitle("Pasture") +
  theme(legend.position="none")
grid.arrange(PODAEI_villa1, PODAEI_villa2, PODAEI_villa3, PODAEI_villa4,  ncol=2, vp=viewport(width=0.9, height=0.9))

PODM1 <- ggplot(data=reforestPODdata, aes(x=POD, y=M_left, group = Site, color = Site)) +
  geom_line( ) +
  geom_point() +
  ggtitle("Reforestation") +
  theme(legend.position="none")
PODM2 <- ggplot(data=consPODdata, aes(x=POD, y=M_left, group = Site, color = Site)) +
  geom_line( ) +
  geom_point() +
  ggtitle("Conservation") +
  theme(legend.position="none")
PODM3 <- ggplot(data=referencePODdata, aes(x=POD, y=M_left, group = Site, color = Site)) +
  geom_line( ) +
  geom_point() +
  ggtitle("Reference") +
  theme(legend.position="none")
PODM4 <- ggplot(data=pastPODdata, aes(x=POD, y=M_left, group = Site, color = Site)) +
  geom_line( ) +
  geom_point() +
  ggtitle("Pasture") +
  theme(legend.position="none")
grid.arrange(PODM1, PODM2, PODM3, PODM4,  ncol=2, vp=viewport(width=0.9, height=0.9))


PODNDSI1 <- ggplot(data=reforestPODdata, aes(x=POD, y=NDSI_left, group = Site, color = Site)) +
  geom_line( ) +
  geom_point() +
  ggtitle("Reforestation") +
  theme(legend.position="none")
PODNDSI2 <- ggplot(data=consPODdata, aes(x=POD, y=NDSI_left, group = Site, color = Site)) +
  geom_line( ) +
  geom_point() +
  ggtitle("Conservation") +
  theme(legend.position="none")
PODNDSI3 <- ggplot(data=referencePODdata, aes(x=POD, y=NDSI_left, group = Site, color = Site)) +
  geom_line( ) +
  geom_point() +
  ggtitle("Reference") +
  theme(legend.position="none")
PODNDSI4 <- ggplot(data=pastPODdata, aes(x=POD, y=NDSI_left, group = Site, color = Site)) +
  geom_line( ) +
  geom_point() +
  ggtitle("Pasture") +
  theme(legend.position="none")
grid.arrange(PODNDSI1, PODNDSI2, PODNDSI3, PODNDSI4,  ncol=2, vp=viewport(width=0.9, height=0.9))

PODADI1 <- ggplot(data=reforestPODdata, aes(x=POD, y=ADI_left, group = Site, color = Site)) +
  geom_line( ) +
  geom_point() +
  ggtitle("Reforestation") +
  theme(legend.position="none")
PODADI2 <- ggplot(data=consPODdata, aes(x=POD, y=ADI_left, group = Site, color = Site)) +
  geom_line( ) +
  geom_point() +
  ggtitle("Conservation") +
  theme(legend.position="none")
PODADI3 <- ggplot(data=referencePODdata, aes(x=POD, y=ADI_left, group = Site, color = Site)) +
  geom_line( ) +
  geom_point() +
  ggtitle("Reference") +
  theme(legend.position="none")
PODADI4 <- ggplot(data=pastPODdata, aes(x=POD, y=ADI_left, group = Site, color = Site)) +
  geom_line( ) +
  geom_point() +
  ggtitle("Pasture") +
  theme(legend.position="none")
grid.arrange(PODADI1, PODADI2, PODADI3, PODADI4,  ncol=2, vp=viewport(width=0.9, height=0.9))

PODnpic1 <- ggplot(data=reforestPODdata, aes(x=POD, y=npic_left, group = Site, color = Site)) +
  geom_line( ) +
  geom_point() +
  ggtitle("Reforestation") +
  theme(legend.position="none")
PODnpic2 <- ggplot(data=consPODdata, aes(x=POD, y=npic_left, group = Site, color = Site)) +
  geom_line( ) +
  geom_point() +
  ggtitle("Conservation") +
  theme(legend.position="none")
PODnpic3 <- ggplot(data=referencePODdata, aes(x=POD, y=npic_left, group = Site, color = Site)) +
  geom_line( ) +
  geom_point() +
  ggtitle("Reference") +
  theme(legend.position="none")
PODnpic4 <- ggplot(data=pastPODdata, aes(x=POD, y=npic_left, group = Site, color = Site)) +
  geom_line( ) +
  geom_point() +
  ggtitle("Pasture") +
  theme(legend.position="none")
grid.arrange(PODnpic1, PODnpic2, PODnpic3, PODnpic4,  ncol=2, vp=viewport(width=0.9, height=0.9))


#### Means per site type per Period of day


meansdawn <- aggregate(cbind(AcouOccupancy_left, Bioac_left, Ht_left, Hf_left, H_left, ACI_left, AEI_villa_left, M_left, NDSI_left, ADI_left, npic_left) ~ site.type, data = dawndata, mean, na.rm = TRUE)
meansdawn$POD <- "Dawn"
meansday <- aggregate(cbind(AcouOccupancy_left, Bioac_left, Ht_left, Hf_left, H_left, ACI_left, AEI_villa_left, M_left, NDSI_left, ADI_left, npic_left) ~ site.type, data = daydata, mean, na.rm = TRUE)
meansday$POD <- "Day"
meansdusk <- aggregate(cbind(AcouOccupancy_left, Bioac_left, Ht_left, Hf_left, H_left, ACI_left, AEI_villa_left, M_left, NDSI_left, ADI_left, npic_left) ~ site.type, data = duskdata, mean, na.rm = TRUE)
meansdusk$POD <- "Dusk"
meansnight <- aggregate(cbind(AcouOccupancy_left, Bioac_left, Ht_left, Hf_left, H_left, ACI_left, AEI_villa_left, M_left, NDSI_left, ADI_left, npic_left) ~ site.type, data = nightdata, mean, na.rm = TRUE)
meansnight$POD <- "Night"

PODdata <- rbind(meansdawn, meansday, meansdusk, meansnight)

reforestPOD <- (PODdata$site.type == "Reforestation")
consPOD <- (PODdata$site.type == "Conservation")
referencePOD <- (PODdata$site.type == "Reference Forest")
pastPOD <- (PODdata$site.type == "Pasture")

reforestPODdata <- subset(PODdata, reforestPOD)
consPODdata <- subset(PODdata, consPOD)
referencePODdata <- subset(PODdata, referencePOD)
pastPODdata <- subset(PODdata, pastPOD)

PODaccou1 <- ggplot(data=reforestPODdata, aes(x=POD, y=AcouOccupancy_left, group = 1)) +
  geom_line() +
  geom_point(linetype = "dashed") +
  ggtitle("Reforestation")
PODaccou2 <- ggplot(data=consPODdata, aes(x=POD, y=AcouOccupancy_left, group = 1)) +
  geom_line(linetype = "dashed") +
  geom_point() +
  ggtitle("Conservation")
PODaccou3 <- ggplot(data=referencePODdata, aes(x=POD, y=AcouOccupancy_left, group = 1)) +
  geom_line(linetype = "dashed") +
  geom_point() +
  ggtitle("Reference")
PODaccou4 <- ggplot(data=pastPODdata, aes(x=POD, y=AcouOccupancy_left, group = 1)) +
  geom_line(linetype = "dashed") +
  geom_point() +
  ggtitle("Pasture")
grid.arrange(PODaccou1, PODaccou2, PODaccou3, PODaccou4,  ncol=2, vp=viewport(width=0.9, height=0.9))

PODBioac1 <- ggplot(data=reforestPODdata, aes(x=POD, y=Bioac_left, group = 1)) +
  geom_line(linetype = "dashed") +
  geom_point() +
  ggtitle("Reforestation")
PODBioac2 <- ggplot(data=consPODdata, aes(x=POD, y=Bioac_left, group = 1)) +
  geom_line(linetype = "dashed") +
  geom_point() +
  ggtitle("Conservation")
PODBioac3 <- ggplot(data=referencePODdata, aes(x=POD, y=Bioac_left, group = 1)) +
  geom_line(linetype = "dashed") +
  geom_point() +
  ggtitle("Reference")
PODBioac4 <- ggplot(data=pastPODdata, aes(x=POD, y=Bioac_left, group = 1)) +
  geom_line(linetype = "dashed") +
  geom_point() +
  ggtitle("Pasture")
grid.arrange(PODBioac1, PODBioac2, PODBioac3, PODBioac4,  ncol=2, vp=viewport(width=0.9, height=0.9))

PODHt1 <- ggplot(data=reforestPODdata, aes(x=POD, y=Ht_left, group = 1)) +
  geom_line(linetype = "dashed") +
  geom_point() +
  ggtitle("Reforestation")
PODHt2 <- ggplot(data=consPODdata, aes(x=POD, y=Ht_left, group = 1)) +
  geom_line(linetype = "dashed") +
  geom_point() +
  ggtitle("Conservation")
PODHt3 <- ggplot(data=referencePODdata, aes(x=POD, y=Ht_left, group = 1)) +
  geom_line(linetype = "dashed") +
  geom_point() +
  ggtitle("Reference")
PODHt4 <- ggplot(data=pastPODdata, aes(x=POD, y=Ht_left, group = 1)) +
  geom_line(linetype = "dashed") +
  geom_point() +
  ggtitle("Pasture")
grid.arrange(PODHt1, PODHt2, PODHt3, PODHt4,  ncol=2, vp=viewport(width=0.9, height=0.9))

PODHf1 <- ggplot(data=reforestPODdata, aes(x=POD, y=Hf_left, group = 1)) +
  geom_line(linetype = "dashed") +
  geom_point() +
  ggtitle("Reforestation")
PODHf2 <- ggplot(data=consPODdata, aes(x=POD, y=Hf_left, group = 1)) +
  geom_line(linetype = "dashed") +
  geom_point() +
  ggtitle("Conservation")
PODHf3 <- ggplot(data=referencePODdata, aes(x=POD, y=Hf_left, group = 1)) +
  geom_line(linetype = "dashed") +
  geom_point() +
  ggtitle("Reference")
PODHf4 <- ggplot(data=pastPODdata, aes(x=POD, y=Hf_left, group = 1)) +
  geom_line(linetype = "dashed") +
  geom_point() +
  ggtitle("Pasture")
grid.arrange(PODHf1, PODHf2, PODHf3, PODHf4,  ncol=2, vp=viewport(width=0.9, height=0.9))

PODH1 <- ggplot(data=reforestPODdata, aes(x=POD, y=H_left, group = 1)) +
  geom_line(linetype = "dashed") +
  geom_point() +
  ggtitle("Reforestation")
PODH2 <- ggplot(data=consPODdata, aes(x=POD, y=H_left, group = 1)) +
  geom_line(linetype = "dashed") +
  geom_point() +
  ggtitle("Conservation")
PODH3 <- ggplot(data=referencePODdata, aes(x=POD, y=H_left, group = 1)) +
  geom_line(linetype = "dashed") +
  geom_point() +
  ggtitle("Reference")
PODH4 <- ggplot(data=pastPODdata, aes(x=POD, y=H_left, group = 1)) +
  geom_line(linetype = "dashed") +
  geom_point() +
  ggtitle("Pasture")
grid.arrange(PODH1, PODH2, PODH3, PODH4,  ncol=2, vp=viewport(width=0.9, height=0.9))

PODACI1 <- ggplot(data=reforestPODdata, aes(x=POD, y=ACI_left, group = 1)) +
  geom_line(linetype = "dashed") +
  geom_point() +
  ggtitle("Reforestation")
PODACI2 <- ggplot(data=consPODdata, aes(x=POD, y=ACI_left, group = 1)) +
  geom_line(linetype = "dashed") +
  geom_point() +
  ggtitle("Conservation")
PODACI3 <- ggplot(data=referencePODdata, aes(x=POD, y=ACI_left, group = 1)) +
  geom_line(linetype = "dashed") +
  geom_point() +
  ggtitle("Reference")
PODACI4 <- ggplot(data=pastPODdata, aes(x=POD, y=ACI_left, group = 1)) +
  geom_line(linetype = "dashed") +
  geom_point() +
  ggtitle("Pasture")
grid.arrange(PODACI1, PODACI2, PODACI3, PODACI4,  ncol=2, vp=viewport(width=0.9, height=0.9))

PODAEI_villa1 <- ggplot(data=reforestPODdata, aes(x=POD, y=AEI_villa_left, group = 1)) +
  geom_line(linetype = "dashed") +
  geom_point() +
  ggtitle("Reforestation")
PODAEI_villa2 <- ggplot(data=consPODdata, aes(x=POD, y=AEI_villa_left, group = 1)) +
  geom_line(linetype = "dashed") +
  geom_point() +
  ggtitle("Conservation")
PODAEI_villa3 <- ggplot(data=referencePODdata, aes(x=POD, y=AEI_villa_left, group = 1)) +
  geom_line(linetype = "dashed") +
  geom_point() +
  ggtitle("Reference")
PODAEI_villa4 <- ggplot(data=pastPODdata, aes(x=POD, y=AEI_villa_left, group = 1)) +
  geom_line(linetype = "dashed") +
  geom_point() +
  ggtitle("Pasture")
grid.arrange(PODAEI_villa1, PODAEI_villa2, PODAEI_villa3, PODAEI_villa4,  ncol=2, vp=viewport(width=0.9, height=0.9))

PODM1 <- ggplot(data=reforestPODdata, aes(x=POD, y=M_left, group = 1)) +
  geom_line(linetype = "dashed") +
  geom_point() +
  ggtitle("Reforestation")
PODM2 <- ggplot(data=consPODdata, aes(x=POD, y=M_left, group = 1)) +
  geom_line(linetype = "dashed") +
  geom_point() +
  ggtitle("Conservation")
PODM3 <- ggplot(data=referencePODdata, aes(x=POD, y=M_left, group = 1)) +
  geom_line(linetype = "dashed") +
  geom_point() +
  ggtitle("Reference")
PODM4 <- ggplot(data=pastPODdata, aes(x=POD, y=M_left, group = 1)) +
  geom_line(linetype = "dashed") +
  geom_point() +
  ggtitle("Pasture")
grid.arrange(PODM1, PODM2, PODM3, PODM4,  ncol=2, vp=viewport(width=0.9, height=0.9))


PODNDSI1 <- ggplot(data=reforestPODdata, aes(x=POD, y=NDSI_left, group = 1)) +
  geom_line(linetype = "dashed") +
  geom_point() +
  ggtitle("Reforestation")
PODNDSI2 <- ggplot(data=consPODdata, aes(x=POD, y=NDSI_left, group = 1)) +
  geom_line(linetype = "dashed") +
  geom_point() +
  ggtitle("Conservation")
PODNDSI3 <- ggplot(data=referencePODdata, aes(x=POD, y=NDSI_left, group = 1)) +
  geom_line(linetype = "dashed") +
  geom_point() +
  ggtitle("Reference")
PODNDSI4 <- ggplot(data=pastPODdata, aes(x=POD, y=NDSI_left, group = 1)) +
  geom_line(linetype = "dashed") +
  geom_point() +
  ggtitle("Pasture")
grid.arrange(PODNDSI1, PODNDSI2, PODNDSI3, PODNDSI4,  ncol=2, vp=viewport(width=0.9, height=0.9))

PODADI1 <- ggplot(data=reforestPODdata, aes(x=POD, y=ADI_left, group = 1)) +
  geom_line(linetype = "dashed") +
  geom_point() +
  ggtitle("Reforestation")
PODADI2 <- ggplot(data=consPODdata, aes(x=POD, y=ADI_left, group = 1)) +
  geom_line(linetype = "dashed") +
  geom_point() +
  ggtitle("Conservation")
PODADI3 <- ggplot(data=referencePODdata, aes(x=POD, y=ADI_left, group = 1)) +
  geom_line(linetype = "dashed") +
  geom_point() +
  ggtitle("Reference")
PODADI4 <- ggplot(data=pastPODdata, aes(x=POD, y=ADI_left, group = 1)) +
  geom_line(linetype = "dashed") +
  geom_point() +
  ggtitle("Pasture")
grid.arrange(PODADI1, PODADI2, PODADI3, PODADI4,  ncol=2, vp=viewport(width=0.9, height=0.9))

PODnpic1 <- ggplot(data=reforestPODdata, aes(x=POD, y=npic_left, group = 1)) +
  geom_line(linetype = "dashed") +
  geom_point() +
  ggtitle("Reforestation")
PODnpic2 <- ggplot(data=consPODdata, aes(x=POD, y=npic_left, group = 1)) +
  geom_line(linetype = "dashed") +
  geom_point() +
  ggtitle("Conservation")
PODnpic3 <- ggplot(data=referencePODdata, aes(x=POD, y=npic_left, group = 1)) +
  geom_line(linetype = "dashed") +
  geom_point() +
  ggtitle("Reference")
PODnpic4 <- ggplot(data=pastPODdata, aes(x=POD, y=npic_left, group = 1)) +
  geom_line(linetype = "dashed") +
  geom_point() +
  ggtitle("Pasture")
grid.arrange(PODnpic1, PODnpic2, PODnpic3, PODnpic4,  ncol=2, vp=viewport(width=0.9, height=0.9))
