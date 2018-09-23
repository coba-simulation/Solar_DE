#################################################################################
#                                                                               #
#                               Build a model                                   #
#                         Here is an approach to model                          #
#                         performance in sepcific month                         #
#                                                                               #
#################################################################################
#
### Close all open plot windows ###
graphics.off()
rm(list=ls())


# load packages needed
libraries = c("caret", "rpart", "AUC","ROCR", "plm", "pglm", "nnet", "graphics", "rgdal")
lapply(libraries, function(x)if(!(x %in% installed.packages())){install.packages(x)})
lapply(libraries, require, quietly = TRUE, character.only = TRUE)


# suppress scientific notation
options(scipen=999)

# load helperfunctions
source("C:/Users/bra/Dropbox/MK/exchange/R/scripts/Fog project/02_Scripts/PrepData&HelpFun.R")

# set working directory
setwd("C:/Users/bra/Dropbox/MK/exchange/R/scripts/Fog project/03_Data")

# load data # specify in folder Test_Train_Data which file to use. 
input.train <- data.frame(read.csv("Test_Train_Data/20160601-20170228_Trainset.csv", header=TRUE, sep=";", dec=",", fill=T))
input.test  <- data.frame(read.csv("Test_Train_Data/20170301-20170430_Testset.csv", header=TRUE, sep=";", dec=",", fill=T))



#################################################################################
#                          get spatial information                              #
#################################################################################

### SPV data // shapfile for germany

# load shapefile and extract a dataframe with the including PLZ
DEU12.shape   <- readOGR(dsn="PLZ",layer = "plz-2stellig") #will load the shapefile to your dataset.
PLZ.shapefile <- as.data.frame(DEU12.shape@data$plz)
names(PLZ.shapefile) <- "PLZ"

# get produced spv-energy according to plz code
SPV.plz <- read.csv("PLZ/SPV_plz-2stellig.csv", header=TRUE, sep=";", dec=",", fill=T)
PLZ.new <- merge(PLZ.shapefile, SPV.plz, by ="PLZ", all.x=TRUE)

# order by production
x <- PLZ.new[with(PLZ.new, order(prod)),]

# Create a function to generate a continuous color palette
rbPal <- colorRampPalette(c('yellow','red'))
# assign colours depending on installed capacity per km^2
x$col <- rbPal(100)[as.numeric(cut(x$prod, breaks=100))]

# reorder alphabetic and extract colours
PLZ.new <- x[with(x, order(PLZ)),]

# vector with colours for each plz-code in shapefile. 
# na values represent no information
myColours <- PLZ.new$col

### geo data (weather stations data)
geo.idx <- read.csv("geoData.csv", header=TRUE, sep=";", dec=",", fill=T)
lon <- geo.idx[,c(3)]
lat <- geo.idx[,c(2)]
alt <- geo.idx[,c(4)]


# set up plot layout (bra: uzywac ostroznie - dobor parametrow nie jest trywialny!)
layout(matrix(c(1,1,2,2), 2), width = c(3.5,1), height = c(1,1))
par(mar=c(0.5, 4.5, 0.5, 0.5))
# german map data 
plot(DEU12.shape, col = myColours) #to get an overview
# plot weatherstations as blue points
points(lon,lat, asp = F, col="blue", pch=16)
# put text next to point with altitude
text(lon,lat, labels=alt, pos=1)

# legend 
par(mar=c(4, 0, 1, 1))
plot(c(0,2),c(0,1),type = 'n', axes = F,xlab = '', ylab = '')
text(x = 1, y = seq(0,0.8,l=10), labels = round((seq(min(x$prod, na.rm = TRUE), max(x$prod, na.rm = TRUE), l = 10))/10)*10)
text(x = 1, y = 0.9, labels = "kWh / kWpeak", cex = 1.4)
legend_image <- as.raster(matrix(x$col, ncol = 1))
rasterImage(legend_image[c(84:1)], 0, 0, 0.6,0.8)

# close graphic device and reset graphical parameters
dev.off()

# plot without legend
# plot(DEU12.shape, col=myColours, main = paste("SPV Production and Weatherstations")) #to get an overview

# clean some variables
rm( SPV.plz, alt, lon, lat)


#################################################################################
#                         Some descriptive statistics                           #
#################################################################################

# prepare data and extract them: (prepare.dataset() is self implemented function 
#                                 that breaks down RRA and RR information on hourly
#                                 level and produces as output a list objects, that
#                                 contains two data.frame objects. one with information
#                                 on visibility and one with binary variable fog/no fog)

input.train  <- prepare.dataset(input.train)
df.train.fog <- input.train$cs.fog
input.test   <- prepare.dataset(input.test)
df.test.fog  <- input.test$cs.fog
head(df.test.fog)
str(input.test)

write.csv2(x = df.test.fog, file="test-Input_Data.csv", row.names=TRUE, quote=FALSE,na="")
#write.csv2(x = df.train.fog, file="test-Input_Data_2.csv", row.names=TRUE, quote=FALSE,na="")



# clean some variables
rm(input.test, input.train)
head(df.test.fog);head( df.train.fog)

## select only those with no missings
data.fog.na  <- na.omit(df.train.fog)
data.test.na <- na.omit(df.test.fog)
summary(data.fog.na); dim(data.fog.na);  length(which(data.fog.na$fog == TRUE))/length(which(data.fog.na$fog == FALSE))
summary(data.test.na);dim(data.test.na); length(which(data.test.na$fog == TRUE))/length(which(data.test.na$fog == FALSE))



#################################################################################
#                                                                               #
#                        Cross Sectional approach                               #
#                             Build a model                                     #
#                                                                               #
#                 Visibility as imited dependent variable                       #
#                                                                               #
#                         unbalanced setting                                    #
#################################################################################

# seperate into a train and testset
trainData <- data.fog.na
testData  <- data.test.na
# create a variable giving information if fog/no fog occured, necessary for cut-off
train.fog <- as.numeric(trainData$fog) - 1
test.fog  <- as.numeric(testData$fog) - 1

## Fit models on unbalanced set
## LOGIT
lr.model.ub <- glm(fog ~ ., data=trainData, family="binomial")
# cutoff
lr.cut.ub   <- opt.cut(predict(lr.model.ub, trainData, type="response"),train.fog)[3,]

## DECISION TREE
dt.model.ub <- rpart(fog ~., data = trainData)
dt.cut.ub   <- 0.5

## Fast Forward Neural Network
nn.model.ub <- nnet(fog~., data = trainData, size=10)
nn.cut.ub   <- opt.cut(predict(nn.model.ub, trainData),train.fog)[3,]


#################################################################################
#                         Apply models to Testset                               #
#################################################################################

## LOGIT UNBALANCED
lr.y.ub  <- predict(lr.model.ub,  newdata=testData, type="response")
# plot NEWDAT
plot(density(lr.y.ub), main = paste0("Density - Logit: cut-off = ", round(lr.cut.ub, 2)))
abline(v=lr.cut.ub, col="red")
# brier
lr.brier.ub <- brier(test.fog, lr.y.ub)
lr.cftable.ub <- table(as.numeric(ifelse(lr.y.ub >lr.cut.ub, TRUE, FALSE)), test.fog)
lr.correct.ub <- sum(diag(lr.cftable.ub)/length(test.fog))
lr.brier.ub; lr.cftable.ub; lr.correct.ub

## DECISION TREE UNBALANCED
dt.y.ub  <- predict(dt.model.ub,  newdata=testData, type="prob")[,2]
plot(density(dt.y.ub), main = paste0("Density - Decision Tree: cut-off = ",dt.cut.ub) )
abline(v=dt.cut.ub, col="red")
dt.brier.ub <- brier(test.fog, dt.y.ub)
dt.cftable.ub <- table(as.numeric(ifelse(dt.y.ub >dt.cut.ub, TRUE, FALSE)), test.fog)
dt.correct.ub <- sum(diag(dt.cftable.ub)/length(test.fog))
dt.brier.ub; dt.cftable.ub; dt.correct.ub


## FF NEURAL NETWORK  UNBALANCED
nn.y.ub  <- predict(nn.model.ub,  newdata=testData)
plot(density(nn.y.ub), main=paste0("Density - FFN Network: cut-off = ", round(nn.cut.ub,2)))
abline(v=nn.cut.ub, col="red")
nn.brier.ub <- brier(test.fog, nn.y.ub)
nn.cftable.ub <- table(as.numeric(ifelse(nn.y.ub >nn.cut.ub, TRUE, FALSE)), test.fog)
nn.correct.ub <- sum(diag(nn.cftable.ub)/length(test.fog))
nn.brier.ub; nn.cftable.ub; nn.correct.ub


#################################################################################
#                                                                               #
#                       Investigate Fog Prediction                              #
#                 Deeper investigation of results from above                    #
#                                                                               #
#               Compute key figures for each and every station                  #
#                                                                               #
#                                                                               #
#################################################################################
# first investigate fog occurence / data without missings, 
summary(data.test.na); str(data.test.na)
summary(df.test.fog); str(df.test.fog)

# break information down on weather stations
# list with data for each station
station.list <- list()
idx.length   <- dim(df.test.fog)[1]
stations     <- length(namesvec) # number of stations in sample
idx.step     <- idx.length/stations
start        <- seq(from = 1, to = idx.length, by = idx.step)
end          <- seq(from = idx.step, to = idx.length, by = idx.step)




# loop to create a list containig values station specific
for (i in 1:length(start)){
  station.list[[i]] <- data.frame(df.test.fog[c(start[i]:end[i]),])
}
names(station.list) <- namesvec
head(station.list[[1]])



# some descriptive statistics
mat.descriptive <- matrix(rep(NA, 72*3), nrow= 72)
rownames(mat.descriptive) <- namesvec
colnames(mat.descriptive) <- c("observations", "obs without missings", "occurence of fog in no missing set")

for (i in 1:stations){
  mat.descriptive[i,1] <- dim(station.list[[i]])[1]
  mat.descriptive[i,2] <- dim(na.omit(station.list[[i]]))[1]
  mat.descriptive[i,3] <- sum(na.omit(station.list[[i]])$fog==TRUE)
}

# predict probabilities for each station with unbalanced lr
lr.prob.ub <- lapply(station.list, predict, object = lr.model.ub, type = "response")
# Translate True/False
lr.fog.ub <-  lapply(lr.prob.ub, function(x) ifelse(x > lr.cut.ub, TRUE, FALSE))


lr.results.ub <- matrix(rep(NA,72*12), nrow = 72)
rownames(lr.results.ub) <- namesvec
colnames(lr.results.ub) <- c("correct NoFog (TN)",  "wrong predicted NoFog (FN)" , "wrong predicted Fog (FP)", "correct Fog (TP)", "sensitivity", "specificity","precision","npv", "idx.1", "idx.2", "idx.3","idx.4")
for ( i in 1:length(station.list)){
  TN    <- length(which(station.list[[i]]$fog==FALSE & lr.fog.ub[[i]]==FALSE))
  FN    <- length(which(station.list[[i]]$fog==TRUE  & lr.fog.ub[[i]]==FALSE))
  FP    <- length(which(station.list[[i]]$fog==FALSE & lr.fog.ub[[i]]==TRUE))
  TP    <- length(which(station.list[[i]]$fog==lr.fog.ub[[i]]  & lr.fog.ub[[i]]==TRUE))
  
  lr.results.ub[i,1]  <- TN
  lr.results.ub[i,2]  <- FN
  lr.results.ub[i,3]  <- FP
  lr.results.ub[i,4]  <- TP
  lr.results.ub[i,5]  <- sens <- TP/(TP+FN)
  lr.results.ub[i,6]  <- spec <- TN/(TN+FP)
  lr.results.ub[i,7]  <- prec <- TP/(TP+FP)
  lr.results.ub[i,8]  <- npv  <- TN/(TN+FN)
  lr.results.ub[i,9]  <- sens*spec*prec*npv
  lr.results.ub[i,10] <- sens*spec*prec*npv*(TP+FN)
  lr.results.ub[i,11] <- sens*spec
  lr.results.ub[i,12] <- prec*npv
  
}

write.csv2(lr.results.ub, file="Evaluation_by_Station_LOGIT.csv", row.names=TRUE, quote=FALSE)
apply(lr.results.ub, 2, sum)




# predict probabilities for each station with unbalanced nn
nn.prob.ub <- lapply(station.list, predict, object=nn.model.ub)
# Translate True False
nn.fog.ub <-  lapply(nn.prob.ub, function(x) ifelse(x > nn.cut.ub, TRUE, FALSE))


nn.results.ub <- matrix(rep(NA,72*12),nrow=72)
rownames(nn.results.ub) <- namesvec
colnames(nn.results.ub) <- c("correct NoFog (TN)",  "wrong predicted NoFog (FN)" , "wrong predicted Fog (FP)", "correct Fog (TP)", "sensitivity", "specificity","precision","npv", "idx.1", "idx.2","idx.3","idx.4")
for ( i in 1:length(station.list))
{
  TN    <- length(which(station.list[[i]]$fog==FALSE & nn.fog.ub[[i]]==FALSE))
  FN    <- length(which(station.list[[i]]$fog==TRUE  & nn.fog.ub[[i]]==FALSE))
  FP    <- length(which(station.list[[i]]$fog==FALSE & nn.fog.ub[[i]]==TRUE))
  TP    <- length(which(station.list[[i]]$fog==nn.fog.ub[[i]]  & nn.fog.ub[[i]]==TRUE))
  
  nn.results.ub[i,1]  <- TN
  nn.results.ub[i,2]  <- FN
  nn.results.ub[i,3]  <- FP
  nn.results.ub[i,4]  <- TP
  nn.results.ub[i,5]  <- sens <- TP/(TP+FN)
  nn.results.ub[i,6]  <- spec <- TN/(TN+FP)
  nn.results.ub[i,7]  <- prec <- TP/(TP+FP)
  nn.results.ub[i,8]  <- npv  <- TN/(TN+FN)
  nn.results.ub[i,9]  <- sens*spec*prec*npv
  nn.results.ub[i,10] <- sens*spec*prec*npv*(TP+FN)
  nn.results.ub[i,11]  <- sens*spec
  nn.results.ub[i,12]  <- prec*npv
}


write.csv(nn.results.ub, file="Evaluation_by_Station_FFNN.csv", row.names=TRUE, quote=FALSE)
apply(nn.results.ub, 2, sum)



# predict probabilities for each station with unbalanced decision tree
dt.prob.ub <- lapply(station.list, predict, object=dt.model.ub)
# Translate True False
dt.fog.ub <-  lapply(dt.prob.ub, function(x) ifelse(x[,2] > dt.cut.ub, TRUE, FALSE))


# since decision tree does not produce a NA in case of missing values but instead imputes some vale for the NA,
# results would be different to above, where the sample was corrected from missings. So here replace predictions by NA
# in case it is na in the prediction of the logistic regression which predicts NA in case on variable is NA
# set na correct
na <- lapply(lr.fog.ub, is.na)
for (i in 1:72){
  idx.na <- which(na[[i]]==TRUE)
  dt.fog.ub[[i]][idx.na] <- NA
}

dt.results.ub <- matrix(rep(NA,72*12),nrow=72)
rownames(dt.results.ub) <- namesvec
colnames(dt.results.ub) <- c("correct NoFog (TN)",  "wrong predicted NoFog (FN)" , "wrong predicted Fog (FP)", "correct Fog (TP)", "sensitivity", "specificity","precision","npv", "idx.1", "idx.2","idx.3","idx.4")
for ( i in 1:length(station.list)){
  TN    <- length(which(station.list[[i]]$fog==FALSE & dt.fog.ub[[i]]==FALSE))
  FN    <- length(which(station.list[[i]]$fog==TRUE  & dt.fog.ub[[i]]==FALSE))
  FP    <- length(which(station.list[[i]]$fog==FALSE & dt.fog.ub[[i]]==TRUE))
  TP    <- length(which(station.list[[i]]$fog==dt.fog.ub[[i]]  & dt.fog.ub[[i]]==TRUE))
  
  dt.results.ub[i,1]   <- TN
  dt.results.ub[i,2]   <- FN
  dt.results.ub[i,3]   <- FP
  dt.results.ub[i,4]   <- TP
  dt.results.ub[i,5]   <- sens <- TP/(TP+FN)
  dt.results.ub[i,6]   <- spec <- TN/(TN+FP)
  dt.results.ub[i,7]   <- prec <- TP/(TP+FP)
  dt.results.ub[i,8]   <- npv  <- TN/(TN+FN)
  dt.results.ub[i,9]   <- sens*spec*prec*npv
  dt.results.ub[i,10]  <- sens*spec*prec*npv*(TP+FN)
  dt.results.ub[i,11]  <- sens*spec
  dt.results.ub[i,12]  <- prec*npv
}


apply(dt.results.ub, 2, sum)


write.csv(dt.results.ub, file="Evaluation_by_Station_DECISION-TREE.csv", row.names=TRUE, quote=FALSE)

# save indices from each model in the geo.idx dataframe
geo.idx$lr.idx.1 <- lr.results.ub[,9]
geo.idx$lr.idx.2 <- lr.results.ub[,10]
geo.idx$lr.idx.3 <- lr.results.ub[,11]
geo.idx$lr.idx.4 <- lr.results.ub[,12]

geo.idx$nn.idx.1 <- nn.results.ub[,9]
geo.idx$nn.idx.2 <- nn.results.ub[,10]
geo.idx$nn.idx.3 <- nn.results.ub[,11]
geo.idx$nn.idx.4 <- nn.results.ub[,12]

geo.idx$dt.idx.1 <- dt.results.ub[,9]
geo.idx$dt.idx.2 <- dt.results.ub[,10]
geo.idx$dt.idx.3 <- dt.results.ub[,11]
geo.idx$dt.idx.4 <- dt.results.ub[,12]

head(geo.idx)


for (i in 5:16){
  # plot germany in colours according to spv production
  # plot weatherstations according to their index
  png(file=paste("C:/Users/MKRJSO/Dropbox/exchange/R/scripts/Fog project/05_Graphs/",colnames(geo.idx)[i],".png", sep=""), width=1000, height = 1090, pointsize = 16, res=100)
 
  # set up plot layout
  layout(matrix(c(1,1,2,2), 2), width = c(3.5,1), height = c(1,1))
  par(mar=c(0.5, 4.5, 0.5, 0.5))
  # german map data 
  plot(DEU12.shape, col = myColours) #to get an overview

  # find corresponding optimal weather station, sort over computed index in descending order best hit.idx stations
  hit.idx = 18
  lon <- geo.idx[order(-geo.idx[,i])[c(1:hit.idx)],c(3)]
  lat <- geo.idx[order(-geo.idx[,i])[c(1:hit.idx)],c(2)]
  alt <- geo.idx[order(-geo.idx[,i])[c(1:hit.idx)],c(4)]
  # plot weatherstations as blue points
  points(lon,lat, asp = F, col="blue", pch=16)
  # put text next to point with altitude
  text(lon,lat, labels=alt, pos=1)
  
  # legend 
  par(mar=c(4, 0, 1, 1))
  plot(c(0,2),c(0,1),type = 'n', axes = F,xlab = '', ylab = '')
  text(x = 1, y = seq(0,0.8,l=10), labels = round((seq(min(x$prod, na.rm = TRUE), max(x$prod, na.rm = TRUE), l = 10))/10)*10)
  text(x = 1, y = 0.9, labels = "kWh / kWpeak", cex = 1.4)
  legend_image <- as.raster(matrix(x$col, ncol = 1))
  rasterImage(legend_image[c(84:1)], 0, 0, 0.6,0.8)
  title(paste0("SPV Production and Weatherstations ",colnames(geo.idx)[i]), outer = TRUE, line = -3.5 )
  dev.off()
}


# store information in a csv file
write.csv2(geo.idx[,c(1,5:16)], file="geo.idx.models.csv", row.names = FALSE, quote = FALSE, na = "")
#write.csv2(geo.idx[,c(1:16)], file="geo.idx.models.complete.csv", row.names = FALSE, quote = FALSE, na = "")

#################################################################################
#                            Model Consistency                                  #
#################################################################################


# get true/false predictions for whole sample
test.lr <- ifelse(lr.y.ub >lr.cut.ub, TRUE, FALSE)
test.nn <- ifelse(nn.y.ub >nn.cut.ub, TRUE, FALSE)
test.dt <- ifelse(dt.y.ub >dt.cut.ub, TRUE, FALSE)

# compute confusion tables, as we have done above
table(test.lr, test.fog)
table(test.nn, test.fog)

# confusion table between logit and nn
table(test.lr, test.nn)
# confusion table between logit and dt
table(test.lr, test.dt)
# confusion table between nn and dt
table(test.nn, test.dt)


TT   <- length(which(test.fog == TRUE  & test.lr == TRUE  & test.nn == TRUE))
FF   <- length(which(test.fog == FALSE & test.lr == FALSE & test.nn == FALSE))
PFW  <- length(which(test.fog == FALSE & test.lr == TRUE  & test.nn == TRUE))
PNFW <- length(which(test.fog == TRUE  & test.lr == FALSE & test.nn == FALSE))
mat.1 <-matrix(c(FF,  PFW, PNFW,TT),nrow=2)
colnames(mat.1) <- c("Observed No Fog", "Observed Fog")
rownames(mat.1) <- c("predicted No Fog (both)", "predicted Fog (both)");mat.1


# include decision tree into framework
# first, analyze how identical the three models perform
TT   <- length(which(test.dt == TRUE  & test.lr == TRUE  & test.nn == TRUE ))
FF   <- length(which(test.dt == FALSE & test.lr == FALSE & test.nn == FALSE))
PFW  <- length(which(test.dt == FALSE & test.lr == TRUE  & test.nn == TRUE))
PNFW <- length(which(test.dt == TRUE  & test.lr == FALSE & test.nn == FALSE))
mat.2 <-matrix(c(FF,  PFW, PNFW,TT),nrow=2)
colnames(mat.2) <- c("DT No Fog", "DT Fog")
rownames(mat.2) <- c("predicted No Fog (both)", "predicted Fog (both)");mat.2
sum(mat.2)

# include observed values and compare with intersection of all the hree models
TT   <- length(which(test.fog==TRUE  & test.lr == TRUE  & test.nn == TRUE & test.dt ==TRUE))
FF   <- length(which(test.fog==FALSE & test.lr == FALSE & test.nn == FALSE & test.dt == FALSE))
PFW  <- length(which(test.fog==FALSE & test.lr == TRUE  & test.nn == TRUE & test.dt == TRUE))
PNFW <- length(which(test.fog==TRUE  & test.lr == FALSE & test.nn == FALSE & test.dt == FALSE))
mat.3 <-matrix(c(FF,  PFW, PNFW,TT),nrow=2)
colnames(mat.3) <- c("Observed No Fog", "Observed Fog")
rownames(mat.3) <- c("All models: No Fog", "All models: Fog");mat.3
sum(mat.3)



#################################################################################
#                            Model Consistency                                  #
#                       Individual Weatherstations                              #
#################################################################################

## individual station consistency
c.names <- c("Consistent LR&NN", "Consisitent 3 Models", "inconsistent 3 Models", "precision DT", "neg.pred.Value", "sensitivity DT","specificity","idx.pred","idx.correct")
consistency.ub <- matrix(rep(NA,72*length(c.names)),nrow=72)
rownames(consistency.ub) <- namesvec
colnames(consistency.ub) <- c.names
for ( i in 1:length(station.list)){
 # i = 1
  # logit and neural network which are identical / which not
  cons.nn.lr    <- length(which(lr.fog.ub[[i]] == FALSE & nn.fog.ub[[i]] == FALSE | lr.fog.ub[[i]] == TRUE  & nn.fog.ub[[i]] == TRUE ))
  incons.nn.lr  <- length(which(lr.fog.ub[[i]] == TRUE  & nn.fog.ub[[i]] == FALSE | lr.fog.ub[[i]] == FALSE & nn.fog.ub[[i]] == TRUE ))
  # number of observations
  obs.lr        <- length(na.omit(lr.fog.ub[[i]]))
  obs.nn        <- length(na.omit(nn.fog.ub[[i]]))

  
  # include decision tree and compare with intersection of both models before
  cons.3m       <- length(which(lr.fog.ub[[i]]==FALSE & nn.fog.ub[[i]]==FALSE & dt.fog.ub[[i]] == FALSE | lr.fog.ub[[i]]==TRUE  & nn.fog.ub[[i]]==TRUE & dt.fog.ub[[i]]==TRUE ))
  obs.dt        <- length(na.omit(dt.fog.ub[[i]]))
  obs.min       <- min(obs.lr, obs.nn, obs.dt)
  incons.3m     <- obs.min - cons.3m
  
  # look how often fog occurs at individual station 
  fog.occur  <- length(which(station.list[[i]]$fog == TRUE))
  # in case all values of station i are NA, set fog.occur to NA
  fog.occur  <- ifelse(sum(is.na(station.list[[i]]$fog)) == length(station.list[[i]]$fog),NA,fog.occur)
  
  # how many predicted fog with dt
  fog.dt <- length(which(dt.fog.ub[[i]]==TRUE))
  # how many interesect:
  TP.dt  <- length(which(dt.fog.ub[[i]]==TRUE  & station.list[[i]]$fog==TRUE ))
  FP.dt  <- length(which(dt.fog.ub[[i]]==TRUE  & station.list[[i]]$fog==FALSE))
  FN.dt  <- length(which(dt.fog.ub[[i]]==FALSE & station.list[[i]]$fog==TRUE))
  TN.dt  <- length(which(dt.fog.ub[[i]]==FALSE & station.list[[i]]$fog==FALSE))
  prec   <- TP.dt/(TP.dt+FP.dt)
  nnpv   <- TN.dt/(TN.dt+FN.dt)
  idx.prediction <- prec*nnpv
  sens   <- TP.dt/(TP.dt+FN.dt)
  spec  <- TN.dt/(TN.dt+FP.dt)
  idx.correct <- sens*spec
  
  # fill dataframe with information
  consistency.ub[i,1]   <- cons.nn.lr/obs.min
  consistency.ub[i,2]   <- cons.3m/obs.min
  consistency.ub[i,3]   <- incons.3m
  consistency.ub[i,4]   <- prec
  consistency.ub[i,5]   <- nnpv
  consistency.ub[i,6]   <- sens
  consistency.ub[i,7]   <- spec
  consistency.ub[i,8]   <- idx.prediction*cons.3m/obs.min
  consistency.ub[i,9]   <- idx.correct*cons.3m/obs.min
  
}
sum(is.na(!consistency.ub[,1]))
which(is.na(consistency.ub[,1]))
str(consistency.ub)

# oreder table by some criteria
consistency.ub[order(-consistency.ub[,8]),]

# add geo information to table 
consistency.ub <- data.frame(geo.idx[,2:4],consistency.ub)

# identify NA and 0 cases
n.station <- dim(consistency.ub)[1]-sum(is.na(!consistency.ub[,12]) | consistency.ub[,12]==0)

# plot of each and every criteria a map of germany with "best" stations according to certain index.
for (i in 11:12){
  # plot germany in colours according to spv production
  # plot weatherstations according to their index
  png(file=paste("C:/Users/MKRJSO/Dropbox/exchange/R/scripts/Fog project/05_Graphs/",colnames(consistency.ub)[i],".consistent.png", sep=""), width=1000, height = 1090, pointsize = 16, res=100)
  
  # set up plot layout
  layout(matrix(c(1,1,2,2), 2), width = c(3.5,1), height = c(1,1))
  par(mar=c(0.5, 4.5, 0.5, 0.5))
  # german map data 
  plot(DEU12.shape, col = myColours) #to get an overview
 
  # find corresponding optimal weather station, sort over computed index in descending order
  lon <- consistency.ub[order(-consistency.ub[,i])[c(1:n.station)],c(2)]
  lat <- consistency.ub[order(-consistency.ub[,i])[c(1:n.station)],c(1)]
  alt <- consistency.ub[order(-consistency.ub[,i])[c(1:n.station)],c(3)]
  # plot weatherstations as blue points
  points(lon,lat, asp = F, col="blue", pch=16)
  # put text next to point with altitude
  text(lon,lat, labels=alt, pos=1)
  
  # legend 
  par(mar=c(4, 0, 1, 1))
  plot(c(0,2),c(0,1),type = 'n', axes = F,xlab = '', ylab = '')
  text(x = 1, y = seq(0,0.8,l=10), labels = round((seq(min(x$prod, na.rm = TRUE), max(x$prod, na.rm = TRUE), l = 10))/10)*10)
  text(x = 1, y = 0.9, labels = "kWh / kWpeak", cex = 1.4)
  legend_image <- as.raster(matrix(x$col, ncol = 1))
  rasterImage(legend_image[c(84:1)], 0, 0, 0.6,0.8)
  title(paste0("SPV Production and Weatherstations consistent.",colnames(consistency.ub)[i]), outer = TRUE, line = -3.5 )
  dev.off()
}


# store information in a csv file
write.csv2(consistency.ub[,-c(1:2)], file="consistency.idx.models.csv", row.names=TRUE, quote=FALSE,na="")









