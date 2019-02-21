########## GWLR

library(GWmodel)
library(sp)
library(spgwr)
library(dummies)

completeFun <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}

# Read in data and 
data = read.csv("C:/Users/aditi.chemparathy/Documents/NAhouses.csv",header=TRUE)

# Training data - remove all rows with "NA"s
newdata <- data[complete.cases(data), ]

# Test data - get rows with NAs in dependent variable
test_data <- data[which(is.na(data$houseValue)), ]
test_data <- completeFun(test_data, "income")
test_data <- completeFun(test_data, "population")
test_data <- completeFun(test_data, "households")

#One-hot encode the categorical variable(s)
df <- dummy.data.frame(newdata, names=c("houseAge"), sep="_")
df <- newdata
#Transform data into SpatialPointsDataFrame
coordinates(df) <- ~longitude+latitude
coordinates(test_data) <- ~longitude+latitude
proj4string(df) <- CRS('+proj=longlat +datum=WGS84') #lambert coordinates
proj4string(test_data) <- CRS('+proj=longlat +datum=WGS84') #lambert coordinates

#Create diagonal distance matrix
DM_points<-gw.dist(dp.locat=coordinates(df))
DM_predict_dist <- gw.dist(dp.locat=coordinates(df),rp.locat = coordinates(test_data))

# #Calculate optimal BW through CV
# Sys.time()
# bw.f2<-bw.ggwr(houseAge_1~income+population+households,data=df,dMat = DM_points,adaptive=TRUE,family="binomial",longlat=TRUE)
# Sys.time()

#GWLR - 
#dependant variable = houseAge_1 
#independant variable = income + houseValue
#regression points are at each grid cell
res <- gwr.predict(houseValue~income+population+households
                           ,bw=20
                           ,adaptive=TRUE
                           ,data=df
                           ,predictdata = test_data
                           ,dMat1=DM_predict_dist
                           ,dMat2=DM_points
                           ,longlat = TRUE)

Sys.time()

predictions <- as.data.frame(res$SDF$prediction)
colnames(predictions)[colnames(predictions)=="res$SDF$prediction"] <- "predicted_houseValues"
test_data_frame <- as.data.frame(test_data)
results <- cbind(test_data_frame,predictions)
results <- results[which(results$predicted_houseValues >= 0), ]
coordinates(results) <- ~longitude+latitude

plot(as.data.frame(results$predicted_houseValues))
points(as.data.frame(df$houseValue),col="red")


