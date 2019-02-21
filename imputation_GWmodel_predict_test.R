########## GWLR

library(GWmodel)
library(sp)
library(spgwr)
library(dummies)

# Read in data and remove all rows with "NA"s
data = read.csv("C:/Users/aditi.chemparathy/Documents/NAhouses.csv",header=TRUE)
newdata <- data[complete.cases(data), ]

#One-hot encode the categorical variable(s)
df <- dummy.data.frame(newdata, names=c("houseAge"), sep="_")

#Transform data into SpatialPointsDataFrame
coordinates(df) <- ~longitude+latitude
proj4string(df) <- CRS('+proj=longlat +datum=WGS84') #lambert coordinates

#Create diagonal distance matrix
DM_points<-gw.dist(dp.locat=coordinates(df))

#Create Spatial grid
bb <- bbox(df)
cs <- c(0.5,0.5) # cell size in lat/long
cc <- bb[, 1] + (cs/2)  # cell offset
cd <- ceiling(diff(t(bb))/cs)  # number of cells per direction
grd <- SpatialGrid(GridTopology(cellcentre.offset=cc, cellsize=cs, cells.dim=cd))
sp_grd <- SpatialGridDataFrame(grd,
                               data=data.frame(id=1:prod(cd)),
                               proj4string=CRS(proj4string(df)))

#Create diagonal distance matrix of grid
DM_grid<-gw.dist(dp.locat = coordinates(df),rp.locat = coordinates(sp_grd))

#Plot data on top of grid
library("lattice")
spplot(sp_grd,
       panel = function(...) {
         panel.gridplot(...,border="black")
         sp.points(df, cex=1.5)
       })

# #Calculate optimal BW through CV
# Sys.time()
# bw.f2<-bw.ggwr(houseAge_1~income+population+households,data=df,dMat = DM_points,adaptive=TRUE,family="binomial",longlat=TRUE)
# Sys.time()

#GWLR - 
#dependant variable = houseAge_1 
#independant variable = income + houseValue
#regression points are at each grid cell
res.binomial <- ggwr.basic(houseValue~income+population+households
                           ,bw=20
                           ,adaptive=TRUE
                           ,data=df
                           ,regression.points=sp_grd
                           ,dMat=DM_grid
                           ,dMat1=DM_points
                           ,longlat = TRUE)
#,family="binomial")
Sys.time()

image(res.binomial$SDF, 'population')
plot(df, add=TRUE)

####Prediction

#Get data with NA houseValue
test_data <- data[which(is.na(data$houseValue)), ]



