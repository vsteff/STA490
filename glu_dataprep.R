###############################################################################
####################### Data Preparation Glu 2014 Data ########################
##########                                                           ##########
########## Verena Steffen, Master Student in Biostatistics; May 2016 ##########
##########                                                           ##########
########## University of Zurich STA490 Statistical Consulting FS2016 ##########
##########                                                           ##########
##########   for Lia Belly and Alexander Leichtle at Unispital Bern  ##########
##########                                                           ##########
##########               supervised by Christos Nakas                ##########
###############################################################################
### needs following files:
## Glucose_2014_anon.csv
## Wardsclean.csv

rm(list=ls())

### Set wd
setwd("/home/vreni/Dropbox/Zürich/Uni Zürich Biostatistik Master/4_FS_2016/STA490_StatisticalConsulting")
setwd("/home/c/vsteff/STA490_StatisticalConsulting")

### Load data
glu <- read.csv("Glucose monitoring UZH/Glucose_2014_anon.csv", sep=";", stringsAsFactors=FALSE)

### Clean and formate data
colnames(glu)
colnames(glu) <- c("Lab", "Time", "Sex", "Glu", "Cr.ABL", "CR", "eGFR", 
                   "eGFR.EPI", "HbA1c.DCCT", "HbA1c.IFCC", "ASAT", "ALAT", 
                   "CHOL", "TG", "HDL", "LDL", "LDL.calc", "YOB", "ANR.anon", 
                   "PID.anon", "Ward.anon")  # simplify names
glu$Lab[which(glu$Lab == "Zentrum f\x9fr Labormedizin")] <- "Klinische Chemie"
glu$Lab[which(glu$Lab == "Klinische Chemie")] <- "ClinChem"
glu$Lab[which(glu$Lab == "Point-of-Care-Diagnostik")] <- "POC"
glu$Lab <- as.factor(glu$Lab)  # convert to factor
glu$Lab[c(which(glu$Lab == "H\x8amostase"), which(glu$Lab == "Morphologie"))] <- NA
glu$Lab <- factor(glu$Lab)  # convert to factor with less levels
glu$Time <- as.POSIXct(glu$Time, format = "%d.%m.%Y %H:%M")  # convert to date format
glu$Sex[which(glu$Sex == "")] <- glu$Sex[which(glu$Sex == "U")] <- NA  # unknown gender to NA
glu$Sex <- as.factor(glu$Sex)  # convert to factor
glu$Glu <- as.numeric(glu$Glu)  # convert to numeric
glu$Cr.ABL <- as.numeric(glu$Cr.ABL)
glu$CR <- as.numeric(glu$CR)
glu$eGFR <- as.numeric(glu$eGFR)
glu$eGFR.EPI <- as.numeric(glu$eGFR.EPI)
glu$HbA1c.DCCT <- as.numeric(glu$HbA1c.DCCT)
glu$HbA1c.IFCC <- as.numeric(glu$HbA1c.IFCC)
glu$ASAT <- as.numeric(glu$ASAT)
glu$ALAT <- as.numeric(glu$ALAT)
glu$CHOL <- as.numeric(glu$CHOL)
glu$TG <- as.numeric(glu$TG)
glu$HDL <- as.numeric(glu$HDL)
glu$LDL <- as.numeric(glu$LDL)
glu$LDL.calc <- as.numeric(glu$LDL.calc)
glu$YOB <- as.integer(glu$YOB)
glu$ANR.anon <- as.factor(glu$ANR.anon)  # convert to factor
glu$PID.anon <- as.factor(glu$PID.anon)  # convert to factor
glu$Ward.anon <- as.factor(glu$Ward.anon)  # convert to factor
library("data.table")
glu <- as.data.table(glu)
# There are duplicate rows in the data frame
glu <- unique(glu)  # now 587973 instead of 810437 rows
# Remove NAs in Glu variable
glu <- glu[!is.na(glu$Glu),]  # now 587973 instead of 587973 rows

### Calculate Age
## Assume all patients are below 100 years old # Assume 0-14 --> 2000 - 2014
# Change 15-99 to 1915 to 1999
glu$YOB[which(glu$YOB > 14)] <- 
  as.numeric(paste0(19, glu$YOB[which(glu$YOB > 14)]))
# Change 0-9 to 2000-2009
glu$YOB[which(glu$YOB < 10)] <- 
  as.numeric(paste0(200, glu$YOB[which(glu$YOB < 10)]))
# Change 10-14 to 2010-2014
glu$YOB[which(glu$YOB >= 10 & glu$YOB <= 14)] <- 
  as.numeric(paste0(20, glu$YOB[which(glu$YOB >= 10 & glu$YOB <= 14)]))
range(glu$YOB, na.rm = TRUE)
# Add Age column
glu$Age <- rep(2014, nrow(glu)) - glu$YOB

### Exclude some patients
# 1. Lab = Morphologie (NA)  # 3
a <- which(is.na(glu$Lab))
# 2. Glucose values > 70 & <= 0  # 3
b <- c(which(glu$Glu > 70), which(glu$Glu <= 0))
# 3. NA data in Sex  # 1,117
c <- which(is.na(glu$Sex))
# 4. Truncate Age (exclude patients < 3 years)  # 19,209
d <- c(which(glu$Age < 3), which(glu$Age > 99), which(is.na(glu$YOB)))
# 6. data not from 2014  # 118,805
library(chron)
e <- c(which(month.day.year(glu$Time)$year != 14), which(is.na(glu$Time)))
## exclude
exc <- c(a, b, c, d, e)
glu <- glu[-unique(exc), ]
glu <- glu[-which.max(glu$Time), ]
glu <- glu[-which.max(glu$Time), ]
glu <- glu[-which.max(glu$Time), ]
glu[which.max(glu$Time), ]
# because there were three 2015 values not detected by month.day.year()

### Simplify anonymous factor variables
## ANR
glu$ANR <- glu$ANR.anon <- factor(glu$ANR.anon)
levels(glu$ANR) <- 1:length(levels(glu$ANR))
## PID
glu$PID <- glu$PID.anon <- factor(glu$PID.anon)
levels(glu$PID) <- 1:length(levels(glu$PID))
## Ward
glu$Ward.number <- glu$Ward.anon <- factor(glu$Ward.anon)
levels(glu$Ward.number) <- 1:length(levels(glu$Ward.number))
# Decrypt Ward.anon
wardsclean <- read.csv("Glucose monitoring UZH/Wardsclean.csv")
colnames(wardsclean) <- c("X", "Ward", "Ward.anon")
wardsclean <- as.data.table(wardsclean[, 2:3])
glu <- merge(glu, wardsclean, by="Ward.anon", all=TRUE)
glu <- glu[-which(is.na(glu$Ward.number)), ]  # remove "extra" wards that are not in dataset but were merged
# Correct special character
glu$Ward <- factor(gsub("\xd9", "ue", as.character(glu$Ward)))

### Order data after PID and Time
glu <- glu[order(glu$PID, glu$Time), ]

### Inpatient definition
# Add column truncated date for inpatient definition
library("lubridate")
glu$Time.trunc <- floor_date(glu$Time, unit = "day")
glu$Time.trunc <- as.chron(glu$Time.trunc)
# Define inpatient factor
# Inpatient = at least two successive Glu values on different days
glu$Inpatient <- rep(NA, nrow(glu))
# loop
npatients <- length(unique(glu$PID))  # 41428
for (i in 1:npatients) {  # i: patient
  pat.time <- as.data.frame(glu)[which(as.numeric(glu$PID) == i), "Time.trunc"]

  j <- 1  # j: time point of patient i

  while (j <= (length(pat.time))) {
    if (length(pat.time) == 1) {
      glu$Inpatient[as.numeric(glu$PID) == i][j] <- 0  # "no"
    }
    else {
      if (j == 1) {
        if (diff(c(pat.time[j], pat.time[j+1])) > 1) {
          glu$Inpatient[as.numeric(glu$PID) == i][j] <- 0  # "no"
        }
        else {
          glu$Inpatient[as.numeric(glu$PID) == i][j] <- 1  # "yes"
        }
      }
      else if (j == length(pat.time)) {
        if (diff(c(pat.time[j-1], pat.time[j])) > 1) {
          glu$Inpatient[as.numeric(glu$PID) == i][j] <- 0  # "no"
        }
        else {
          glu$Inpatient[as.numeric(glu$PID) == i][j] <- 1  # "yes"
        }
      }
      else {
        if (diff(c(pat.time[j-1], pat.time[j])) <= 1 | diff(c(pat.time[j], pat.time[j+1])) <= 1) {
          glu$Inpatient[as.numeric(glu$PID) == i][j] <- 1  # "yes"
        }
        else {
          glu$Inpatient[as.numeric(glu$PID) == i][j] <- 0  # "no"
        }
      }
    }
    j <- j+1
  }
}  # takes about 3.5 hours
glu$Time.trunc <- NULL  # do not need it anymore
glu$Inpatient <- factor(glu$Inpatient, labels = c(0,1))

### (High) variability
# Calculate patient CV (%)
CV <- function(mean, sd) (sd/mean)*100
glu$cv <- rep(0, nrow(glu))
for (i in 1:npatients) {  # i: patient
    pat.glu <- as.data.frame(glu)[which(as.numeric(glu$PID) == i), "Glu"]
    pat.inp <- as.data.frame(glu)[which(as.numeric(glu$PID) == i), "Inpatient"]
    glu[which(as.numeric(glu$PID) == i), ]$cv <- ifelse(pat.inp == TRUE, CV(mean(pat.glu), sd(pat.glu)), NA)
}  # takes about 2.6 hours

# Add column for high variability inpatients
glu$high.var20 <- ifelse(glu$cv > 20, 1, 0)

### Diabetic patient yes/no
# new way: per patient
glu$diabetic <- rep(NA, nrow(glu))
for (i in 1:npatients) {
  if (sum(glu[as.numeric(glu$PID) == i, "Glu"] >= 11.1 |
          glu[as.numeric(glu$PID) == i, "HbA1c.IFCC"] >= 48 |
          glu[as.numeric(glu$PID) == i, "HbA1c.DCCT"] >= 6.5,
          na.rm = TRUE) >=1) {
    glu[as.numeric(glu$PID) == i, "diabetic"] <- 1
  }
  else {
    glu[as.numeric(glu$PID) == i, "diabetic"] <- 0
  }
}
glu$diabetic <- factor(glu$diabetic, labels = c(0, 1))

### Glycemic control status
# Define status according to glycemic control ranges
glu$glyc.control <- ifelse(glu$Glu < 2.2,
   yes = "severe hypoglycemia",
   no = ifelse(glu$Glu < 4,
               yes = "hypoglycemia",
               no = ifelse(glu$Glu < 7.8,
                           yes = "euglycemia",
                           no = ifelse(glu$Glu < 10.1,
                                       yes = "mild hyperglycemia",
                                       no = ifelse(glu$Glu < 15,
                                                   yes = "pronounced hyperglycemia",
                                                   no = "severe hyperglycemia")))))
glu$glyc.control <- factor(glu$glyc.control)

### Flag public holidays or Sundays
# Flag public holidays
Sys.setlocale("LC_TIME", "en_US.UTF-8")
glu$day <- factor(weekdays(glu$Time))
public.holidays.bern2014 <- c("14-01-01",  # Neujahr
                              "14-01-02",  # Berchtoldstag
                              "14-04-18",  # Karfreitag
                              "14-04-21",  # Ostermontag
                              "14-05-29",  # Auffahrt
                              "14-06-09",  # Pfingstmontag
                              "14-08-01",  # Nationalfeiertag Schweiz
                              "14-12-25",  # Weihnachten
                              "14-12-26"   # Stephanstag
                              )
ind.ph <- NULL
for (i in (1:length(public.holidays.bern2014))) {
  ind.ph <- c(ind.ph, which(startsWith(as.character(glu$Time),
                                   public.holidays.bern2014[i])))
}
glu$holiday <- ifelse(glu$day == "Sunday", TRUE, FALSE)
glu$holiday[ind.ph] <- TRUE



### After long computations: save workspace
# save(glu, file = "STA490/glu_clean.RData")
load("STA490/glu_clean.RData")
summary(glu)
