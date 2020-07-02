### Author: Ricky G Fernando
###
### Steps:
### 0. Preparation: library calling, read CSVs
### 1. Data inspection and data cleaning
### 2. Clustering analysis
### 3. Predictive analysis
### 3.1 Squared error
### 3.2 A new model
### 3.3 Calculate Markov Model (Time series)

### 0. Preparation
### Calling required libraries
library(ggplot2)
library(data.table)

# Set working directory
setwd("./data")

titles = c("Timestamp","MaxCurr", "EffCurr", "Screen1","Screen2", "Device")

# Read csv file and store it to dataset
screen1.raw       = read.csv(file="test_data1536128609.98_screen1.csv", header=FALSE, sep=",")
screen2.raw       = read.csv(file="test_data1536128871.84_screen2.csv", header=FALSE, sep=",")
device.raw        = read.csv(file="test_data1536129261.77_device253.csv", header=FALSE, sep=",")
mainInput         = read.csv(file="test_data1536129670.55_main.csv", header=FALSE, sep=",")

colnames(screen1.raw) = c("Timestamp","MaxCurr", "EffCurr")
colnames(screen2.raw) = c("Timestamp","MaxCurr", "EffCurr")
colnames(device.raw) = c("Timestamp","MaxCurr", "EffCurr")

screen1.clean     = screen1.raw
screen2.clean     = screen2.raw
device.clean      = device.raw



### 1. Data inspection and data cleaning

### 1.1. Data inspection
hist(screen1.raw$MaxCurr, main="Histogram Screen1", xlab=NA, ylab=NA)
hist(screen2.raw$MaxCurr, main="Histogram Screen2", xlab=NA, ylab=NA)
hist(device.raw$MaxCurr, main="Histogram Device", xlab=NA, ylab=NA)

plot(screen1.raw$MaxCurr,screen1.raw$EffCurr, main="Plot Screen1")

### In plot screen2, we saw suspicious data point at the top right
plot(screen2.raw$MaxCurr,screen2.raw$EffCurr, main="Plot Screen2")

### In plot device, we saw possible outliers at the top right
plot(device.raw$MaxCurr,device.raw$EffCurr, main="Plot Device")


### 1.2. Data Cleaning

### 1.2.1 screen2 data
qnt <- quantile(screen2.raw$MaxCurr, probs=c(.25, .75), na.rm = T)
caps <- quantile(screen2.raw$MaxCurr, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(screen2.raw$MaxCurr, na.rm = T)

### data looks fine
screen2.raw[screen2.raw$MaxCurr < (qnt[1] - H),]
screen2.raw[screen2.raw$MaxCurr > (qnt[2] + H),]

### 1.2.2. device data
### In Device data, we detect outliers
### Without treatment, a center will mainly focus on the outliers
### Treatment:
### 1. modify the value of bottom outliers to 5% quantile
### 2. modify the value of top outliers to 95% quantile
### 
### Please view kmDevice.raw and examine the Cluster means and Clustering vector
### and compare it with kmDevice (clean version) below
hist(device.raw$MaxCurr, main="With outliers", xlab=NA, ylab=NA)
kmDevice.raw     = kmeans(device.raw[,2:3],2,nstart=20)
kmDevice.raw

qnt <- quantile(device.clean$MaxCurr, probs=c(.25, .75), na.rm = T)
caps <- quantile(device.clean$MaxCurr, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(device.clean$MaxCurr, na.rm = T)
device.clean[device.clean$MaxCurr < (qnt[1] - H),] <- caps[1]
device.clean[device.clean$MaxCurr > (qnt[2] + H),] <- caps[2]
hist(device.clean$MaxCurr, main="Without outliers", xlab=NA, ylab=NA)


### 2. Clustering analysis
### Method: K-Means

### 2.1 Clustering screen1 data
set.seed(42)
kmScreen1     = kmeans(screen1.raw[,2:3],3,nstart=20)
kmScreen1
screen1.clean[,4]   = kmScreen1$cluster
screen1.clean[screen1.clean[,4]==1,4] = "Off"
screen1.clean[screen1.clean[,4]==2,4] = "On"
screen1.clean[screen1.clean[,4]==3,4] = "Idle"
screen1.clean[,5]   = "Off"
screen1.clean[,6]   = "Off"
colnames(screen1.clean) = titles
screen1.clean

### 2.2 Clustering screen2 data
set.seed(42)
kmScreen2     = kmeans(screen2.raw[,2:3],3,nstart=20)
kmScreen2
screen2.clean[,4]   = "Off"
screen2.clean[,5]   = kmScreen2$cluster
screen2.clean[screen2.clean[,5]==1,5] = "On"
screen2.clean[screen2.clean[,5]==2,5] = "Idle"
screen2.clean[screen2.clean[,5]==3,5] = "Off"
screen2.clean[,6]   = "Off"
colnames(screen2.clean) = titles
screen2.clean

### 2.3 Clustering device data
set.seed(42)
kmDevice     = kmeans(device.clean[,2:3],2,nstart=20)
kmDevice
device.clean[,4]   = "Off"
device.clean[,5]   = "Off"
device.clean[,6]   = kmDevice$cluster
device.clean[device.clean[,6]==1,6] = "Off"
device.clean[device.clean[,6]==2,6] = "On"
colnames(device.clean) = titles
device.clean





### 3. Predictive analysis



### 3.1 Squared error
### Method:
### We calculate the mean for 8 conditions:
### 1. Only screen1 On
### 2. Only screen1 Off
### 3. Only screen1 Idle
### 4. Only screen2 On
### 5. Only screen2 Off
### 6. Only screen2 Idle
### 7. Only device On
### 8. Only device Off
### And then sum the mean for all permutations (18 conditions)
### 1. screen1 off, screen2 off, device off
### 2. screen1 off, screen2 off, device on
### 3. screen1 off, screen2 on, device off
### 4-18.
###
### The Prediction function will then sum the squared error
### for Maximum Current and Effective Current
### of each input against each permutations
### And then we pick a condition with the least sum of squared error
results = data.frame(matrix(ncol=6, nrow=nrow(mainInput)))
results[,1] = mainInput[,1]
results[,2] = mainInput[,2]
results[,3] = mainInput[,3]
colnames(results) = titles

meanMaxCurr = c(mean(screen1.clean[screen1.clean[,4]=="Off",2]),
                mean(screen2.clean[screen2.clean[,5]=="Off",2]),
                mean(device.clean[device.clean[,6]=="Off",2]),
                mean(screen1.clean[screen1.clean[,4]=="On",2]),
                mean(screen2.clean[screen2.clean[,5]=="On",2]),
                mean(device.clean[device.clean[,6]=="On",2]),
                mean(screen1.clean[screen1.clean[,4]=="Idle",2]),
                mean(screen2.clean[screen2.clean[,5]=="Idle",2]),
                NA)

meanEffCurr = c(mean(screen1.clean[screen1.clean[,4]=="Off",3]),
                mean(screen2.clean[screen2.clean[,5]=="Off",3]),
                mean(device.clean[device.clean[,6]=="Off",3]),
                mean(screen1.clean[screen1.clean[,4]=="On",3]),
                mean(screen2.clean[screen2.clean[,5]=="On",3]),
                mean(device.clean[device.clean[,6]=="On",3]),
                mean(screen1.clean[screen1.clean[,4]=="Idle",3]),
                mean(screen2.clean[screen2.clean[,5]=="Idle",3]),
                NA)


meanMaxCurr.df = data.frame(matrix(data=meanMaxCurr,ncol=3,nrow=3))
rownames(meanMaxCurr.df) = c("Screen1","Screen2","Device")
colnames(meanMaxCurr.df) = c("Off","On","Idle")

meanEffCurr.df = data.frame(matrix(data=meanEffCurr,ncol=3,nrow=3))
rownames(meanEffCurr.df) = c("Screen1","Screen2","Device")
colnames(meanEffCurr.df) = c("Off","On","Idle")


stateScreen1 = c("Off","On","Idle")
stateScreen2 = c("Off","On","Idle")
stateDevice = c("Off","On")

powerConsumption.MaxCurr = list()
powerConsumption.EffCurr = list()
for(stateS1 in stateScreen1) {
  powerConsumption.MaxCurr[[stateS1]] = list()
  powerConsumption.EffCurr[[stateS1]] = list()
  for(stateS2 in stateScreen2) {
    powerConsumption.MaxCurr[[stateS1]][[stateS2]] = list()
    powerConsumption.EffCurr[[stateS1]][[stateS2]] = list()
    for(stateD in stateDevice) {
      powerConsumption.MaxCurr[[stateS1]][[stateS2]][[stateD]] = meanMaxCurr.df["Screen1",as.character(stateS1)] + meanMaxCurr.df["Screen2",as.character(stateS2)] + meanMaxCurr.df["Device",as.character(stateD)]
      powerConsumption.EffCurr[[stateS1]][[stateS2]][[stateD]] = meanEffCurr.df["Screen1",as.character(stateS1)] + meanEffCurr.df["Screen2",as.character(stateS2)] + meanEffCurr.df["Device",as.character(stateD)]
    }
  }
}

predictStates = function(x,y) {
  tempResult = c(NA,NA,NA)
  lowestError = 2147483647
  tempError = 0
  for(stateS1 in stateScreen1) {
    for(stateS2 in stateScreen2) {
      for(stateD in stateDevice) {
        tempError = (x - powerConsumption.MaxCurr[[stateS1]][[stateS2]][[stateD]])^2 + (y - powerConsumption.EffCurr[[stateS1]][[stateS2]][[stateD]])^2
        if(tempError < lowestError) {
          lowestError = tempError
          tempResult = c(stateS1,stateS2,stateD)
        }
      }
    }
  }
  return(tempResult)
}

### Set initial condition
mainInput[,4] = NA
mainInput[,5] = NA
mainInput[,6] = NA
colnames(mainInput) = titles

### Start prediction row by row
for (row in 1:nrow(mainInput)) {
# for (row in 1:10) {
  mainInput[row,c(4:6)] = predictStates(mainInput[row,2],mainInput[row,3])
}

### write the result to external file
write.csv(mainInput,file="result.csv")




### 3.2 A new model
### From the data, the mean when device is "Off" is 8.7
### From screen1 and screen2 data, we can see that if all devices are Off, the current will be 0.
### So we'd like to change the state for all non-zero current to "On" and zero current to "Off"

device.clean.v2 = device.clean
device.clean.v2[device.clean.v2[,2]>0,6] = "On"
device.clean.v2[device.clean.v2[,2]==0,6] = "Off"

meanMaxCurr.v2 = c(mean(screen1.clean[screen1.clean[,4]=="Off",2]),
                mean(screen2.clean[screen2.clean[,5]=="Off",2]),
                mean(device.clean.v2[device.clean.v2[,6]=="Off",2]),
                mean(screen1.clean[screen1.clean[,4]=="On",2]),
                mean(screen2.clean[screen2.clean[,5]=="On",2]),
                mean(device.clean.v2[device.clean.v2[,6]=="On",2]),
                mean(screen1.clean[screen1.clean[,4]=="Idle",2]),
                mean(screen2.clean[screen2.clean[,5]=="Idle",2]),
                NA)

meanEffCurr.v2 = c(mean(screen1.clean[screen1.clean[,4]=="Off",3]),
                mean(screen2.clean[screen2.clean[,5]=="Off",3]),
                mean(device.clean.v2[device.clean.v2[,6]=="Off",3]),
                mean(screen1.clean[screen1.clean[,4]=="On",3]),
                mean(screen2.clean[screen2.clean[,5]=="On",3]),
                mean(device.clean.v2[device.clean.v2[,6]=="On",3]),
                mean(screen1.clean[screen1.clean[,4]=="Idle",3]),
                mean(screen2.clean[screen2.clean[,5]=="Idle",3]),
                NA)

varMaxCurr.v2 = c(var(screen1.clean[screen1.clean[,4]=="Off",2]),
                  var(screen2.clean[screen2.clean[,5]=="Off",2]),
                  var(device.clean.v2[device.clean.v2[,6]=="Off",2]),
                  var(screen1.clean[screen1.clean[,4]=="On",2]),
                  var(screen2.clean[screen2.clean[,5]=="On",2]),
                  var(device.clean.v2[device.clean.v2[,6]=="On",2]),
                  var(screen1.clean[screen1.clean[,4]=="Idle",2]),
                  var(screen2.clean[screen2.clean[,5]=="Idle",2]),
                  NA)

varEffCurr.v2 = c(var(screen1.clean[screen1.clean[,4]=="Off",3]),
                  var(screen2.clean[screen2.clean[,5]=="Off",3]),
                  var(device.clean.v2[device.clean.v2[,6]=="Off",3]),
                  var(screen1.clean[screen1.clean[,4]=="On",3]),
                  var(screen2.clean[screen2.clean[,5]=="On",3]),
                  var(device.clean.v2[device.clean.v2[,6]=="On",3]),
                  var(screen1.clean[screen1.clean[,4]=="Idle",3]),
                  var(screen2.clean[screen2.clean[,5]=="Idle",3]),
                  NA)


meanMaxCurr.v2.df = data.frame(matrix(data=meanMaxCurr.v2,ncol=3,nrow=3))
rownames(meanMaxCurr.v2.df) = c("Screen1","Screen2","Device")
colnames(meanMaxCurr.v2.df) = c("Off","On","Idle")

meanEffCurr.v2.df = data.frame(matrix(data=meanEffCurr.v2,ncol=3,nrow=3))
rownames(meanEffCurr.v2.df) = c("Screen1","Screen2","Device")
colnames(meanEffCurr.v2.df) = c("Off","On","Idle")

varMaxCurr.v2.df = data.frame(matrix(data=varMaxCurr.v2,ncol=3,nrow=3))
rownames(varMaxCurr.v2.df) = c("Screen1","Screen2","Device")
colnames(varMaxCurr.v2.df) = c("Off","On","Idle")

varEffCurr.v2.df = data.frame(matrix(data=varEffCurr.v2,ncol=3,nrow=3))
rownames(varEffCurr.v2.df) = c("Screen1","Screen2","Device")
colnames(varEffCurr.v2.df) = c("Off","On","Idle")


stateScreen1 = c("Off","On","Idle")
stateScreen2 = c("Off","On","Idle")
stateDevice = c("Off","On")

powerConsumption.MaxCurr.v2 = list()
powerConsumption.EffCurr.v2 = list()
for(stateS1 in stateScreen1) {
  powerConsumption.MaxCurr.v2[[stateS1]] = list()
  powerConsumption.EffCurr.v2[[stateS1]] = list()
  for(stateS2 in stateScreen2) {
    powerConsumption.MaxCurr.v2[[stateS1]][[stateS2]] = list()
    powerConsumption.EffCurr.v2[[stateS1]][[stateS2]] = list()
    for(stateD in stateDevice) {
      powerConsumption.MaxCurr.v2[[stateS1]][[stateS2]][[stateD]] = meanMaxCurr.v2.df["Screen1",as.character(stateS1)] + meanMaxCurr.v2.df["Screen2",as.character(stateS2)] + meanMaxCurr.v2.df["Device",as.character(stateD)]
      powerConsumption.EffCurr.v2[[stateS1]][[stateS2]][[stateD]] = meanEffCurr.v2.df["Screen1",as.character(stateS1)] + meanEffCurr.v2.df["Screen2",as.character(stateS2)] + meanEffCurr.v2.df["Device",as.character(stateD)]
    }
  }
}

mainInput.v2 = mainInput

for (row in 1:nrow(mainInput.v2)) {
  # for (row in 1:10) {
  mainInput.v2[row,c(4:6)] = predictStates(mainInput.v2[row,2],mainInput.v2[row,3])
}


write.csv(mainInput.v2,file="result.v2.csv")


### 3.3 Calculate with Markov Model
pScreen1 = list()
pScreen2 = list()
pDevice  = list()

pScreen1[["Off"]]   = nrow(screen1.clean[screen1.clean[,4]=="Off",])/nrow(screen1.clean)
pScreen1[["On"]]    = nrow(screen1.clean[screen1.clean[,4]=="On",])/nrow(screen1.clean)
pScreen1[["Idle"]]  = nrow(screen1.clean[screen1.clean[,4]=="Idle",])/nrow(screen1.clean)
pScreen2[["Off"]]   = nrow(screen2.clean[screen2.clean[,5]=="Off",])/nrow(screen2.clean)
pScreen2[["On"]]    = nrow(screen2.clean[screen2.clean[,5]=="On",])/nrow(screen2.clean)
pScreen2[["Idle"]]  = nrow(screen2.clean[screen2.clean[,5]=="Idle",])/nrow(screen2.clean)
pDevice[["Off"]]    = nrow(device.clean[device.clean[,6]=="Off",])/nrow(device.clean)
pDevice[["On"]]     = nrow(device.clean[device.clean[,6]=="On",])/nrow(device.clean)



pCondScreen1        = list()
pCondScreen2        = list()
for(stateNow in c("Off","On","Idle")) {
  pCondScreen1[[stateNow]] = list()
  pCondScreen2[[stateNow]] = list()
  for(statePrev in c("Off","On","Idle")) {
    pCondScreen1[[stateNow]][[statePrev]] = nrow(screen1.clean[screen1.clean[,4]==stateNow & shift(screen1.clean[,4])==statePrev,]) / (nrow(screen1.clean)-1)
    pCondScreen2[[stateNow]][[statePrev]] = nrow(screen2.clean[screen2.clean[,5]==stateNow & shift(screen2.clean[,5])==statePrev,]) / (nrow(screen2.clean)-1)
  }
}


pCondDevice         = list()
for(stateNow in c("Off","On")) {
  pCondDevice[[stateNow]] = list()
  for(statePrev in c("Off","On")) {
    pCondDevice[[stateNow]][[statePrev]] = nrow(device.clean[device.clean[,6]==stateNow & shift(device.clean[,6])==statePrev,]) / (nrow(device.clean)-1)
  }
}

calcProbabilities = function(x) {
  prob.screen1    = 1
  prob.screen2    = 1
  prob.device     = 1
  
  ### Prob screen1, screen2, device
  for(row in 1:nrow(x)) {
    if(row==1) {
      prob.screen1    = pScreen1[[as.character(x[row,1])]]
      prob.screen2    = pScreen2[[as.character(x[row,2])]]
      prob.device     = pDevice[[as.character(x[row,3])]]
    } else {
      prob.screen1    = prob.screen1 * pCondScreen1[[as.character(x[row,1])]][[as.character(x[row-1,1])]]
      prob.screen2    = prob.screen2 * pCondScreen2[[as.character(x[row,2])]][[as.character(x[row-1,2])]]
      prob.device     = prob.device  * pCondDevice[[as.character(x[row,3])]][[as.character(x[row-1,3])]]
    }
  }
  
  totalProb = prob.screen1 * prob.screen2 * prob.device
  
  return(totalProb)
  
}



print(calcProbabilities(mainInput.v2[,4:6])>calcProbabilities(mainInput[,4:6]))
### This means our first model has the highest probability






