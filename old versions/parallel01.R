# How long does it take?
inpat <- function(B) {
    for (i in 1:B) {  # i: patient
        pat.time <- glu[which(as.numeric(glu$PID) == i), "Time.trunc"]
        
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
    }
}

npatients <- length(unique(glu$PID))  # 41428

B <- c(10, 50, 100, 300, 500)
t <- rep(NA, length(B))

for (b in 1:length(B)) {
    t[b] <- system.time(inpat(B = B[b]))[3]
}

plot(t ~ B)
m1 <- lm(t ~ B)
summary(m1)
predict(m1, data.frame(B = 325401))/60/60  # hours
# Test loop parallel


# Define inpatient factor
# Inpatient = at least two successive Glu values on different days
glu$Inpatient <- rep(NA, nrow(glu))
# loop
npatients <- length(levels(glu$PID))  # 43296
library(foreach)
library(doMC)
library(rbenchmark)
registerDoMC(cores = 4)

foreach(i = 1:100, .combine = "c") %dopar% {  # i: patient
    
    pat.time <- glu[which(as.numeric(glu$PID) == i), "Time.trunc"]
    
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
}

head(glu[, c("PID", "Time", "Inpatient")], 20)

# access single patient
# i <- 3794
# i <- 12
head(glu[as.numeric(glu$PID) == i, c("Time", "Inpatient")], 50)
