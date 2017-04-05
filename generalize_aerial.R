##Written by Jason Karl (USDA-ARS) and Jeffrey Gillan (New Mexico State University)

## This R code takes topographic elevation data from an erosion bridge and UAS-based DEM and matches them every 10 cm along a 6.1 m transect. 
## This is necessary because the DEM data has 5 cm cells while the erosion bridge data has measurements taken every 10 cm
## The output is a CSV file with the erosion bridge elevation and DEM elevation paired and next to each other in separate columns. The excess DEM measurements are removed. 


## Load data
### Inputs are two CSV files: one for erosion bridge data, and second for photogrammetry data
### Erosion bridge file has three columns:
###    "x" = Position along the transect, "dist" = Distance from bridge to ground,  "rel_elev" = soil elevation relative to the starting pin elevation
### Photogrammetry file has two columns:
###    "x" = Position along the transect, "rel_elev" = soil elevation relative to the starting pin of the transect
### The CSV files should have a header row with the field names, but the actual names given to the fields don't matter.
### The order of the fields in the CSV file DOES matter, though.

setwd('F:/jeffs_documents/Gravelly Ridge TRAC project/R_profile_match')

##read in DEM and erosion bridge transect elevation data
aerial <- read.csv('aerial_utm2015_18-5.csv', header=T)
eb <- read.csv('eb_utm2015_18-5.csv', header=T)


## Plot original data for comparison
par(mfrow=c(2,1))
plot(aerial[,1],aerial[,2],pch=20,col="white", xlab="Distance from pin (m)", ylab="Relative elevation (m)", main="Original Data")
abline(0,0)
lines(aerial[,1],aerial[,2], col="blue")
lines(eb[,1],eb[,3],col="orange")

## generalize photogrammetry dataset to resolution of erosion bridge
gen_aerial <- function(aerial, eb) {
  f1 <- function(d) aerial[which.min(abs(aerial[,1] - d)),1]
  f2 <- function(e) aerial[which.min(abs(aerial[,1] - e)),2]
  dist <- sapply(eb[,1],f1)
  ele <- sapply(eb[,1],f2)
  return(cbind(dist,ele))
}

###write to csv file the elevation pairs of the erosion bridge and generalized aerial
gen <- gen_aerial(aerial,eb)
library(BlandAltmanLeh)
blandaltman=bland.altman.stats(eb[,3], gen[,2])
write.csv(blandaltman$groups, file="18_5_aerial_utm2015_pairs.csv")

