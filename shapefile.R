#################################################################################
#                               Solar_DE                                        #
#################################################################################

### Close all open plot windows ###
graphics.off()
rm(list=ls())

# load packages needed
libraries = c("rgdal")
lapply(libraries, function(x)if(!(x %in% installed.packages())){install.packages(x)})
lapply(libraries, require, quietly = TRUE, character.only = TRUE)


# suppress scientific notation
options(scipen=999)

# specify path
path = "C:/Users/Johannes/Documents/GitHub/Solar_DE"


### SPV data // shapfile for germany
# download shapefile from GDAM NUTS
# some infos about:
# https://ryouready.wordpress.com/2009/11/16/infomaps-using-r-visualizing-german-unemployment-rates-by-color-on-a-map/

# load shapefile and extract a dataframe with the including PLZ
DEU12.shape   <- readOGR(dsn="01_Data",layer = "plz-2stellig") #will load the shapefile to your dataset.
PLZ.shapefile <- as.data.frame(DEU12.shape@data$plz)
names(PLZ.shapefile) <- "PLZ"

# get produced spv-energy according to plz code
# Source for Energy Production: https://www.pv-ertraege.de/cgi-bin/pvdaten/src/region_uebersichten_auswahl.pl/kl
SPV.plz <- read.csv("01_Data/SPV_plz-2stellig.csv", header=TRUE, sep=";", dec=",", fill=T)
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

# geo data
# Source for geoData: https://www.ogimet.com/
geo.idx <- read.csv("01_Data/geoData.csv", header=TRUE, sep=";", dec=",", fill=T)
lon <- geo.idx[,c(3)]
lat <- geo.idx[,c(2)]
alt <- geo.idx[,c(4)]

# store german map as png file
png(file=paste(path,"/Solar_map.png", sep=""), width=1000, height = 1090, pointsize = 16, res=100)

# set up plot layout
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

# clean some variables
rm(x, SPV.plz, alt, lon, lat)
