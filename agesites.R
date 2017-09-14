rm(list = ls())


library (dplR)
library (Hmisc)

# Read in Metadata
library(readr)
meta <- read_csv("~/Documents/Data Assimilation RA/vod_trw/Core_data_01202014.csv")
View(meta)

## PITH ESTIMATES OF TREES
#### subset metafile for those files that have complete pith estimates
meta <- data.frame(cbind(meta$TreeID,meta$pith.yr,meta$outer.measured))
colnames(meta) <- c("TreeID", "pith.yr", "outer.measured")
meta[1014,3] <- 2014
meta <- na.omit(meta) 
trees <- duplicated(meta$TreeID)
meta <- meta[trees,]
meta <- data.frame(meta)
meta$outer.measured <- as.numeric(as.character(meta$outer.measured))
meta$pith.yr <- as.numeric(as.character(meta$pith.yr))
meta$age <-  meta$outer.measured - meta$pith.yr + 1

meta$site <- substr(meta$TreeID,0,2)
VL1 <- grep("VL",meta$site)
VL1 <- meta[VL1,] #VL1$age

VU1 <- grep("VU",meta$site)
VU1 <- meta[VU1,] #VU1$age

Valles <- rbind(VL1,VU1)

AC1 <- grep("AC",meta$site)
AC1 <- meta[AC1,] 

SR1 <- grep("SR",meta$site)
SR1 <- meta[SR1,] 

OO1 <- grep("OO",meta$site)
OO1 <- meta[OO1,] 

MO1 <- grep("MO",meta$site)
MO1 <- meta[MO1,] 

MA1 <- grep("MA",meta$site)
MA1 <- meta[MA1,] 

DL1 <- grep("DL",meta$site)
DL1 <- meta[DL1,] 

DH1 <- grep("DH",meta$site)
DH1 <- meta[DH1,] 

Duke <- rbind(DL1,DH1)

MM1 <- grep("MM",meta$site)
MM1 <- meta[MM1,] 

niwB <- read.table("metadata_niwot_plotB.txt",header = T)
niwC <- read.table("metadata_niwot_plotC.txt",header = T)

NIW <- matrix(NA,nrow = 289,ncol = 1)
NIW[1:144,] <- niwB$age
NIW[145:288,] <- niwC$age
NIW <- na.omit(NIW) 

# HARVARD 

HA <- read.table("TowerAllPlotsLogsheet.txt",header = T) #PITH is estimate to pith from inner year
HA <- HA[!duplicated(HA$TREE), ]
HA$OUTER_YEAR <- as.numeric(as.character(HA$OUTER_YEAR))
HA$INNER_YEAR <- as.numeric(as.character(HA$INNER_YEAR))
HA$PITH <- as.numeric(as.character(HA$PITH))
HA$age <-  HA$OUTER_YEAR - (HA$INNER_YEAR-HA$PITH) + 1

# Histogram of Tree Ages by site
par(mfrow = c(5, 2))
hist(AC1$age,main = "Austin-Cary, N = 63", xlab = "Age")
hist(Duke$age,main = "Duke Hardwood + Loblolly, N = 111", xlab = "Age")
hist(HA$age,main = "Harvard TP1+TP2, N = 87", xlab = "Age")
hist(MA1$age,main = "Michigan Ameriflux UMBS, N = 125", xlab = "Age")
hist(MM1$age,main = "Morgan-Monroe, N = 48", xlab = "Age")
hist(MO1$age,main = "Missouri Ozarks, N = 73", xlab = "Age")
hist(NIW,main = "Niwot Ridge B+C, N = 250", xlab = "Age")
hist(OO1$age,main = "Oak Openings, N = 74", xlab = "Age")
hist(SR1$age,main = "Savannah River, N = 76", xlab = "Age") #,xlim = c(0,120),ylim = c(0,60))
hist(Valles$age,main = "Valles Upper + Lower, N = 158", xlab = "Age")
dev.off() 

#individual
hist(AC1$age,breaks= 10, main = "Austin-Cary, N = 63", xlab = "Age")#,xlim = c(0,250),ylim = c(0,50))
hist(Duke$age, breaks= 10, main = "Duke Hardwood + Loblolly, N = 111", xlab = "Age")
hist(HA$age,breaks= 10,main = "Harvard TP1+TP2, N = 87", xlab = "Age")
hist(MA1$age,breaks= 10,main = "Michigan Ameriflux UMBS, N = 125", xlab = "Age")
hist(MM1$age,breaks= 10,main = "Morgan-Monroe, N = 48", xlab = "Age")
hist(MO1$age,breaks= 10,main = "Missouri Ozarks, N = 73", xlab = "Age")
hist(NIW,breaks= 10,main = "Niwot Ridge B+C, N = 250", xlab = "Age")
hist(OO1$age,breaks= 10,main = "Oak Openings, N = 74", xlab = "Age")
hist(SR1$age,breaks= 10,main = "Savannah River, N = 76", xlab = "Age") #,xlim = c(0,120),ylim = c(0,60))
hist(Valles$age,breaks= 10,main = "Valles Upper + Lower, N = 158", xlab = "Age")

# Violin Histograms

#merge columns together
allsites <- data.frame(matrix(NA, nrow = 250, ncol = 10))
allsites[,1] <-  AC1$age
nm <- cbind( z1, c(z2, rep(NA,length(z1)-length(z2))) )

NIW <- data.frame(NIW)
allsites <- cbind(c(AC1$age, rep(NA,length(NIW)-length(AC1$age))), Duke$age,HA$age, MA1$age, MM1$age,MO1$age,NIW,OO1$age, SR1$age, Valles$age)

allsites <- cbind(c(AC1$age, rep(NA,250-length(AC1$age))), 
                  c(Duke$age, rep(NA,250-length(Duke$age))),
                  c(HA$age, rep(NA,250-length(HA$age))),
                  c(MA1$age, rep(NA,250-length(MA1$age))),
                  c(MM1$age, rep(NA,250-length(MM1$age))),
                  c(MO1$age, rep(NA,250-length(MO1$age))),
                  NIW,
                  c(OO1$age, rep(NA,250-length(OO1$age))),
                  c(SR1$age, rep(NA,250-length(SR1$age))),
                  c(Valles$age, rep(NA,250-length(Valles$age))))
colnames(allsites) <- c("Austin-Cary, N = 63",  "Duke Hardwood + Loblolly, N = 111", "Harvard TP1+TP2, N = 87", "Michigan Ameriflux UMBS, N = 125","Morgan-Monroe, N = 48", "Missouri Ozarks, N = 73","Niwot Ridge B+C, N = 250","Oak Openings, N = 74", "Savannah River, N = 76","Valles Upper + Lower, N = 158")

library(reshape2)
library (Hmisc)
sitesmelt <- melt(allsites)
p <- ggplot(sitesmelt, aes(x = variable, y = value)) + geom_violin()
p + theme_bw() + geom_boxplot(width=0.8) + theme(axis.text.x = element_text(size = 20,face = "bold",angle=60,hjust = 1),axis.title=element_text(size=20,face="bold")) + theme(axis.text.y = element_text(size = 14,face = "bold"))+ scale_y_continuous(name = "Tree Age",breaks=seq(0,280,20)) + scale_x_discrete(name = "Site and Number of Trees") +



#add grids back in


##################################

# Howland Does not have Metadata on TREE AGE, so can do an estimate with cores.

########################
## RAW RING WIDTH

## Howland

HOW1 <- read.rwl("HOW_PIST.rw")
HOW2 <- read.rwl("HOW_ACRU.rw")
HOW3 <- read.rwl("HOW_ABBA.rw")
HOW4 <- read.rwl("HOW_PCRU.rw")
HOW5 <- read.rwl("HOW_BEAL.rw")
HOW6 <- read.rwl("HOW_TSCA.rw")
HOW7 <- read.rwl("HOW_THOC.rw") #CAN'T READ IN




# subset the raw ring width files to estimate age of trees (no pith estimates)
alltrees <- read.rwl("RWL_all_trees.rwl", "tucson")

# split the site/plot/tree/core string into site/plot and tree/core to then select the "A" core and remove duplicate ages
x <-  summary(alltrees)
siteplot <- substr(x$series, 0, 3)
treecore <- data.frame(substr(x$series, 4, 8))
coreA <- grep("A",treecore[,1])
alltrees2 <- alltrees[,coreA]
x <-  summary(alltrees2)
siteplot <- data.frame(substr(x$series, 0, 2))
treecore <- data.frame(substr(x$series, 4, 8))
# produce a subset of alltrees, called alltrees2, to use for ages

VL <- grep("VL",siteplot[,1])
VL <- alltrees2[,VL]

VU <- grep("VU",siteplot[,1])
VU <- alltrees2[,VU]

MM <- grep("MM",siteplot[,1])
MM <- alltrees2[,MM]

OO <- grep("OO",siteplot[,1])
OO <- alltrees2[,OO]

MO <- grep("MO",siteplot[,1])
MO <- alltrees2[,MO]

MA <- grep("MA",siteplot[,1])
MA <- alltrees2[,MA]

SR <- grep("SR",siteplot[,1])
SR <- alltrees2[,SR]

DL <- grep("DL",siteplot[,1])
DL <- alltrees2[,DL]

AC <- grep("AC",siteplot[,1])
AC <- alltrees2[,AC]

DH <- grep("DH",siteplot[,1])
DH <- alltrees2[,DH]




## Harvard
TP1 <- read.rwl("TP_PIST.rwl")
TP2 <- read.rwl("TP_QURU.rwl")
TP3 <- read.rwl("TP_PIAB.rwl")
TP4 <- read.rwl("TP_FAGR.rwl")
TP5 <- read.rwl("TP_BELE.rwl")
TP6 <- read.rwl("TP_TSCA.rwl")
TP7 <- read.rwl("TP_ACRU.rwl")
TP8 <- read.rwl("TP_BEAL.rwl")

par(mfrow = c(4, 2))
hist(summary(TP1)$year, breaks = 10,main = "TP1, N = 109", xlab = "Age",xlim = c(0,120),ylim = c(0,60))
hist(summary(TP2)$year, breaks = 10,main = "TP2, N = 119", xlab = "Age",xlim = c(0,120),ylim = c(0,60))
hist(summary(TP3)$year, breaks = 10,main = "TP3, N = 87", xlab = "Age",xlim = c(0,120),ylim = c(0,60))
hist(summary(TP4)$year, breaks = 10,main = "TP4, N = 107", xlab = "Age",xlim = c(0,120),ylim = c(0,60))
hist(summary(TP5)$year, breaks = 10,main = "TP5, N = 82", xlab = "Age",xlim = c(0,120),ylim = c(0,60))
hist(summary(TP6)$year, breaks = 10,main = "TP6, N = 92", xlab = "Age",xlim = c(0,120),ylim = c(0,60))
hist(summary(TP7)$year, breaks = 10,main = "TP7, N = 118", xlab = "Age",xlim = c(0,120),ylim = c(0,60))
hist(summary(TP8)$year, breaks = 10,main = "TP8, N = 110", xlab = "Age",xlim = c(0,120),ylim = c(0,60))

## Plot Histogram

par(mfrow = c(2, 2))
hist(summary(VLF)$year, breaks = 10,main = "VLF, N = 95", xlab = "Age",xlim = c(0,120),ylim = c(0,60))
hist(summary(VUF)$year,breaks = 20, main = "VUF, N = 77", xlab = "Age",xlim = c(0,120),ylim = c(0,60))
hist(summary(MM)$year,breaks = 20, main = "MorganMonroe, N = 125", xlab = "Age",xlim = c(0,120),ylim = c(0,60))
hist(summary(OO)$year,breaks = 10, main = "OakOpenings, N = 127", xlab = "Age",xlim = c(0,120),ylim = c(0,60))
dev.off()

hist(NIW,breaks = 20, main = "Niwot Ridge B+C, N = 288", xlab = "Age")#,xlim = c(0,120),ylim = c(0,60))
