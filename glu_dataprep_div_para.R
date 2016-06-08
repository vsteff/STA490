###############################################################################
####################### Data Preparation Glu 2014 Data ########################
##########                                                           ##########
########## Verena Steffen, Master Student in Biostatistics; May 2016 ##########
##########                                                           ##########
########## University of Zurich STA490 Statistical Consulting FS2016 ##########
##########                                                           ##########
##########   for Lia Bally and Alexander Leichtle at Unispital Bern  ##########
##########                                                           ##########
##########               supervised by Christos Nakas                ##########
###############################################################################
### needs following files:
## Glu_Div_Para_14_15_anon.csv
## Wardsclean.csv

rm(list=ls())

### Set wd
setwd("/home/vreni/Dropbox/Zürich/Uni Zürich Biostatistik Master/4_FS_2016/STA490_StatisticalConsulting")
setwd("/home/c/vsteff/STA490_StatisticalConsulting")

### Load data
glu.full <- read.csv("Glucose monitoring UZH/Glu_Div_Para_14_15_anon.csv", sep=";", stringsAsFactors=FALSE)

### Clean and formate data
colnames(glu.full)
colnames(glu.full) <- c("Lab", "Time", "Sex", "Glu", "Cr.ABL", "CR", "eGFR", 
                   "eGFR.EPI", "HbA1c.DCCT", "HbA1c.IFCC", "ASAT", "ALAT", 
                   "CHOL", "TG", "HDL", "LDL", "LDL.calc", "YOB", 
                   "PID.anon", "Ward.anon")  # simplify names
glu.full$Lab[which(glu.full.full$Lab == "Zentrum f\x9fr Labormedizin")] <- "Klinische Chemie"
glu.full$Lab[which(glu.full$Lab == "Klinische Chemie")] <- "ClinChem"
glu.full$Lab[which(glu.full$Lab == "Point-of-Care-Diagnostik")] <- "POC"
glu.full$Lab <- as.factor(glu.full$Lab)  # convert to factor
glu.full$Lab[c(which(glu.full$Lab == "H\x8amostase"), which(glu.full$Lab == "Morphologie"))] <- NA
glu.full$Lab <- factor(glu.full$Lab)  # convert to factor with less levels
glu.full$Time <- as.POSIXct(glu.full$Time, format = "%d.%m.%Y %H:%M")  # convert to date format
glu.full$Sex[which(glu.full$Sex == "")] <- glu.full$Sex[which(glu.full$Sex == "U")] <- NA  # unknown gender to NA
glu.full$Sex <- as.factor(glu.full$Sex)  # convert to factor
glu.full$Glu <- as.numeric(glu.full$Glu)  # convert to numeric
glu.full$Cr.ABL <- as.numeric(glu.full$Cr.ABL)
glu.full$CR <- as.numeric(glu.full$CR)
glu.full$eGFR <- as.numeric(glu.full$eGFR)
glu.full$eGFR.EPI <- as.numeric(glu.full$eGFR.EPI)
glu.full$HbA1c.DCCT <- as.numeric(glu.full$HbA1c.DCCT)
glu.full$HbA1c.IFCC <- as.numeric(glu.full$HbA1c.IFCC)
glu.full$ASAT <- as.numeric(glu.full$ASAT)
glu.full$ALAT <- as.numeric(glu.full$ALAT)
glu.full$CHOL <- as.numeric(glu.full$CHOL)
glu.full$TG <- as.numeric(glu.full$TG)
glu.full$HDL <- as.numeric(glu.full$HDL)
glu.full$LDL <- as.numeric(glu.full$LDL)
glu.full$LDL.calc <- as.numeric(glu.full$LDL.calc)
glu.full$YOB <- as.integer(glu.full$YOB)
glu.full$PID.anon <- as.factor(glu.full$PID.anon)  # convert to factor
glu.full$Ward.anon <- as.factor(glu.full$Ward.anon)  # convert to factor
library("data.table")
glu.full <- as.data.table(glu.full)
# There are duplicate rows in the data frame
glu.full <- unique(glu.full)  # now 587099 instead of 810437 rows
# Remove NAs in Glu variable
# glu.full <- glu.full[!is.na(glu.full$Glu),]  # now 444165 instead of 587099 rows

### Calculate Age
## Assume all patients are below 100 years old # Assume 0-14 --> 2000 - 2014
# Change 15-99 to 1915 to 1999
glu.full$YOB[which(glu.full$YOB > 14)] <- 
  as.numeric(paste0(19, glu.full$YOB[which(glu.full$YOB > 14)]))
# Change 0-9 to 2000-2009
glu.full$YOB[which(glu.full$YOB < 10)] <- 
  as.numeric(paste0(200, glu.full$YOB[which(glu.full$YOB < 10)]))
# Change 10-14 to 2010-2014
glu.full$YOB[which(glu.full$YOB >= 10 & glu.full$YOB <= 14)] <- 
  as.numeric(paste0(20, glu.full$YOB[which(glu.full$YOB >= 10 & glu.full$YOB <= 14)]))
range(glu.full$YOB, na.rm = TRUE)
# Add Age column
glu.full$Age <- rep(2014, nrow(glu.full)) - glu.full$YOB

### Exclude some patients
# 1. Lab = Morphologie (NA)  # 3
a <- which(is.na(glu.full$Lab))
# 2. Glucose values > 70 & <= 0  # 3
b <- c(which(glu.full$Glu > 70), which(glu.full$Glu <= 0))
# 3. NA data in Sex  # 1,112
c <- which(is.na(glu.full$Sex))
# 4. Truncate Age (exclude patients < 3 years)  # 18,953
d <- c(which(glu.full$Age < 3), which(glu.full$Age > 99), which(is.na(glu.full$YOB)))
# 6. data not from 2014  # 118,781
library(chron)
e <- c(which(month.day.year(glu.full$Time)$year != 14), which(is.na(glu.full$Time)))
## exclude
exc <- c(a, b, c, d, e)
glu.full <- glu.full[-unique(exc), ]
glu.full <- glu.full[-which.max(glu.full$Time), ]
glu.full <- glu.full[-which.max(glu.full$Time), ]
glu.full <- glu.full[-which.max(glu.full$Time), ]
# because there were three 2015 values not detected by month.day.year()

### Simplify anonymous factor variables
## PID
glu.full$PID <- glu.full$PID.anon <- factor(glu.full$PID.anon)
levels(glu.full$PID) <- 1:length(levels(glu.full$PID))
## Ward
glu.full$Ward.number <- glu.full$Ward.anon <- factor(glu.full$Ward.anon)
levels(glu.full$Ward.number) <- 1:length(levels(glu.full$Ward.number))
# Decrypt Ward.anon
wardsclean <- read.csv("Glucose monitoring UZH/Wardsclean.csv")
colnames(wardsclean) <- c("X", "Ward", "Ward.anon")
wardsclean <- as.data.table(wardsclean[, 2:3])
glu.full <- merge(glu.full, wardsclean, by="Ward.anon", all=TRUE)
glu.full <- glu.full[-which(is.na(glu.full$Ward.number)), ]  # remove "extra" wards that are not in dataset but were merged
# Correct special character
glu.full$Ward <- factor(gsub("\xd9", "ue", as.character(glu.full$Ward)))

### Order data after PID and Time
glu.full <- glu.full[order(glu.full$PID, glu.full$Time), ]

### Inpatient definition
# Add column truncated date for inpatient definition
library("lubridate")
glu.full$Time.trunc <- floor_date(glu.full$Time, unit = "day")
glu.full$Time.trunc <- as.chron(glu.full$Time.trunc)
# Define inpatient factor
# Inpatient = at least two successive Glu values on different days
glu.full$Inpatient <- rep(NA, nrow(glu.full))
# loop
npatients <- length(unique(glu.full$PID))  # 41428
for (i in 1:npatients) {  # i: patient
  pat.time <- as.data.frame(glu.full)[which(as.numeric(glu.full$PID) == i), "Time.trunc"]

  j <- 1  # j: time point of patient i

  while (j <= (length(pat.time))) {
    if (length(pat.time) == 1) {
      glu.full$Inpatient[as.numeric(glu.full$PID) == i][j] <- 0  # "no"
    }
    else {
      if (j == 1) {
        if (diff(c(pat.time[j], pat.time[j+1])) > 1) {
          glu.full$Inpatient[as.numeric(glu.full$PID) == i][j] <- 0  # "no"
        }
        else {
          glu.full$Inpatient[as.numeric(glu.full$PID) == i][j] <- 1  # "yes"
        }
      }
      else if (j == length(pat.time)) {
        if (diff(c(pat.time[j-1], pat.time[j])) > 1) {
          glu.full$Inpatient[as.numeric(glu.full$PID) == i][j] <- 0  # "no"
        }
        else {
          glu.full$Inpatient[as.numeric(glu.full$PID) == i][j] <- 1  # "yes"
        }
      }
      else {
        if (diff(c(pat.time[j-1], pat.time[j])) <= 1 | diff(c(pat.time[j], pat.time[j+1])) <= 1) {
          glu.full$Inpatient[as.numeric(glu.full$PID) == i][j] <- 1  # "yes"
        }
        else {
          glu.full$Inpatient[as.numeric(glu.full$PID) == i][j] <- 0  # "no"
        }
      }
    }
    j <- j+1
  }
}  # takes about 3.5 hours
glu.full$Time.trunc <- NULL  # do not need it anymore
glu.full$Inpatient <- factor(glu.full$Inpatient, labels = c(0,1))

### (High) variability
# Calculate patient CV (%)
CV <- function(mean, sd) (sd/mean)*100
glu.full$cv <- rep(0, nrow(glu.full))
for (i in 1:npatients) {  # i: patient
    pat.glu.full <- as.data.frame(glu.full)[which(as.numeric(glu.full$PID) == i), "Glu"]
    pat.inp <- as.data.frame(glu.full)[which(as.numeric(glu.full$PID) == i), "Inpatient"]
    glu.full[which(as.numeric(glu.full$PID) == i), ]$cv <- ifelse(pat.inp == TRUE, CV(mean(pat.glu.full), sd(pat.glu.full)), NA)
}  # takes about 2.6 hours

# Add column for high variability inpatients
glu.full$high.var20 <- ifelse(glu.full$cv > 20, 1, 0)

### Diabetic patient yes/no
# new way: per patient
glu.full$diabetic <- rep(NA, nrow(glu.full))
for (i in 1:npatients) {
  if (sum(glu.full[as.numeric(glu.full$PID) == i, "Glu"] >= 11.1 |
          glu.full[as.numeric(glu.full$PID) == i, "HbA1c.IFCC"] >= 48 |
          glu.full[as.numeric(glu.full$PID) == i, "HbA1c.DCCT"] >= 6.5,
          na.rm = TRUE) >=1) {
    glu.full[as.numeric(glu.full$PID) == i, "diabetic"] <- 1
  }
  else {
    glu.full[as.numeric(glu.full$PID) == i, "diabetic"] <- 0
  }
}
glu.full$diabetic <- factor(glu.full$diabetic, labels = c(0, 1))

### Glycemic control status
# Define status according to glycemic control ranges
glu.full$glyc.control <- ifelse(glu.full$Glu < 2.2,
   yes = "severe hypoglycemia",
   no = ifelse(glu.full$Glu < 4,
               yes = "hypoglycemia",
               no = ifelse(glu.full$Glu < 7.8,
                           yes = "euglycemia",
                           no = ifelse(glu.full$Glu < 10.1,
                                       yes = "mild hyperglycemia",
                                       no = ifelse(glu.full$Glu < 15,
                                                   yes = "pronounced hyperglycemia",
                                                   no = "severe hyperglycemia")))))
glu.full$glyc.control <- factor(glu.full$glyc.control)

### Flag public holidays or Sundays
# Flag public holidays
Sys.setlocale("LC_TIME", "en_US.UTF-8")
glu.full$day <- factor(weekdays(glu.full$Time))
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
  ind.ph <- c(ind.ph, which(startsWith(as.character(glu.full$Time),
                                   public.holidays.bern2014[i])))
}
glu.full$holiday <- ifelse(glu.full$day == "Sunday", TRUE, FALSE)
glu.full$holiday[ind.ph] <- TRUE



### After long computations: save workspace
# save(glu.full, file = "STA490/glu.full_clean.RData")
load("STA490/glu.full_clean.RData")
summary(glu.full)
