## ---------------------------
##
## Script name: Data Preparation
##
## Purpose of script: 
## Data preparation for the paper:
## Sonnenschein, T, Scheider, S., Zheng, S. (2021). THE REBIRTH OF URBAN SUBCENTERS - How Subway Expansion Impacts the Spatial Structure 
## and Mix of Amenities in European Cities. Environment and Planning B: Urban Analytics and City Science.
## 
## - Creating a spatial dataset of 100mx100m cells within a Nightlight defined Urban Area
## - For these spatial units creating attributes of 
##          1) accesibility (distance to metrostations), 
##          2) opening dates of the closest metrostations, 
##          3) the WorldPop population density,
##          4) the number of Foursquare amenities
##          5) the entropy of Foursquare amenity types
##          6) the similarity-based diversity of amenity types
##          7) the number of amenities by metacategory
##          8) the amenity subcenters of the cities (cluster centroids)
##          9) the amenity density of the closest amenity subcenter
##          10) the distance to the closest amenity subcenter
## - This prepared dataset will serve as an input for the regressions formulated 
##   in the Data Analysis script to analyse the impact of subway expansion on 
##   the density, diversity and heterogeneity of the amenity mix in surrounding neighborhoods
##
## Author: Tabea Sonnenschein
##
## Date Created: 2020-04-11
##
## Copyright (c) Tabea Sonnenschein, 2020
## Email: t.s.sonnenschein@uu.nl
##
## ------------------------------------------------
## Note: The script should be run through for each city, but one city at a time.
## ------------------------------------------------

pkgs = c("maptools","rgdal","sp", "sf", "jpeg", "data.table", "purrr",   
          "ggplot2", "lattice",  "raster",  "spatialEco", "rjson", "jsonlite","EconGeo", 
          "rstan", "boot",  "concaveman", "data.tree", "DiagrammeR", "networkD3", "rgexf", "tidytree")
sapply(pkgs, require, character.only = T) #load 
rm(pkgs)

dataFolder= "C:/Dokumente/Master Thesis/Master_these _ Latest/Data"
city= "Warsaw"
city= "Rome"
city= "Barcelona"
city= "Vienna"
city= "Budapest"
city= "Milan"
city= "Helsinki"
city= "Stuttgart"
city= "Sofia"

if( city== "Rome" | city == "Budapest"){
  Opening_year = 2014
} else if (city== "Warsaw" | city == "Sofia" | city == "Milan"){
  Opening_year = 2015
} else if (city== "Barcelona"| city== "Stuttgart"){
  Opening_year = 2016
} else if (city== "Vienna"| city == "Helsinki"){
  Opening_year = 2017
}
post_3_years = Opening_year + 3

###########################################################################
########################## Reading the Foursquare data ####################
###########################################################################

setwd(paste(dataFolder,"/Foursquare/", city, "/inferred_openings", sep = ""))
Fsq =read.csv(paste(city, "_all.csv", sep = ""), header = T)
coordinates(Fsq)= ~lon+lat
proj4string(Fsq)=CRS("+proj=longlat +datum=WGS84") 
writeOGR(Fsq, dsn=paste(dataFolder,"/Foursquare/", city, sep = "") ,layer= paste(city, "_Fsq", sep = ""),driver="ESRI Shapefile")
Fsq = readOGR(dsn=paste(dataFolder,"/Foursquare/", city, sep = "") ,layer= paste(city, "_Fsq", sep = ""))
plot(Fsq, main = paste(city, "Foursquare Venues"))


#######################################################################################################
####### getting the Nightlight defined urban area from Nightlight Satelite imagery ###################
#######################################################################################################

setwd(paste(dataFolder, "/", city, "/Nightlight", sep = ""))

Nightlight = raster("LuoJia1-01_LR201810085879_20181007201544_HDR_0030_gec.tif") #Warsaw
Nightlight = raster("LuoJia1-01_LR201806207826_20180619220046_HDR_0000_gec.tif") #Barcelona
Nightlight = raster("LuoJia1-01_LR201808179892_20180816204230_HDR_0029_gec.tif") #Budapest
Nightlight = raster("LuoJia1-01_LR201810113188_20181010195139_HDR_0051_gec.tif") #Helsinki
Nightlight = raster("LuoJia1-01_LR201806304569_20180629210830_HDR_0035_gec.tif") #Milan
Nightlight = raster("LuoJia1-01_LR201811174649_20181116212909_HDR_0033_gec.tif") #Stuttgart
Nightlight = raster("LuoJia1-01_LR201808294726_20180828203503_HDR_0019_gec.tif") #Sofia
Nightlight = raster("LuoJia1-01_LR201811174649_20181116212714_HDR_0010_gec.tif") #Rome
Nightlight = raster("LuoJia1-01_LR201808017650_20180731205134_HDR_0022_gec.tif") #Vienna


x_coord = c(max(Fsq@coords[,1]), max(Fsq@coords[,1]), min(Fsq@coords[,1]), min(Fsq@coords[,1]))
y_coord = c(min(Fsq@coords[,2]), max(Fsq@coords[,2]), max(Fsq@coords[,2]), min(Fsq@coords[,2]))
pol = cbind(x_coord, y_coord)
poly = Polygon(pol)
ps = Polygons(list(poly),1)
Clip_extent = SpatialPolygons(list(ps))
plot(Clip_extent)

Nightlight = crop(Nightlight, extent(Clip_extent))
summary(Nightlight)
plot(Nightlight)

jpeg(paste(city, "_Nightlight_hist.jpeg", sep = ""))
Nightlight_hist = hist(as.integer(Nightlight),
     main = paste("Distribution of Nightlight values", city),
     breaks = 500, xlim = c(0,60000), 
     xlab = "Light Gradient", ylab = "Frequency",
     col = "springgreen")
dev.off() 

## determine the cutoff value out of the distribution --> should be the infliction value
cutoff_value = 1000
NLight_def_urbanarea = Nightlight > cutoff_value
plot(NLight_def_urbanarea)

UrbanArea = rasterToPolygons(NLight_def_urbanarea,fun=function(x){x == 1}, dissolve= F)
UrbanArea  = aggregate(UrbanArea, dissolve = T)
UrbanArea = disaggregate(UrbanArea)
UrbanArea$area = areaPolygon(UrbanArea)
biggest = which(UrbanArea$area == max(UrbanArea$area))
UrbanArea =UrbanArea[biggest,]
jpeg(paste(city,"_UrbanArea.jpeg", sep = ""))
plot(UrbanArea)
dev.off() 

plot(UrbanArea, pch = 20, col = "purple")
plot(Fsq, add = T)

writeOGR(UrbanArea, dsn=paste(dataFolder, "/", city, sep = ""),layer= paste(city,"_UrbanArea", sep = ""),driver="ESRI Shapefile")
UrbanArea = readOGR(dsn=paste(dataFolder, "/", city, sep = ""),layer= paste(city,"_UrbanArea", sep = ""))


############################################################################################
############################# Creating regular point grid in the urban area #########################
############################################################################################

polygoncoord = as.data.frame(UrbanArea@polygons[[1]]@Polygons[[1]]@coords)
names(polygoncoord) = c("x", "y")
max_coord_x = max(polygoncoord$x)
min_coord_x = min(polygoncoord$x)
max_coord_y = max(polygoncoord$y)
min_coord_y = min(polygoncoord$y)
necessary_coord_x = as.integer((max_coord_x - min_coord_x)/ 0.001)
necessary_coord_y = as.integer((max_coord_y - min_coord_y)/ 0.001)
xcoordinates = c(min_coord_x)
for(i in 1:necessary_coord_x){
  xcoordinates = append(xcoordinates, (min_coord_x + (0.001*i)))
}
ycoordinates = c(min_coord_y)
for(i in 1:necessary_coord_y){
  ycoordinates = append(ycoordinates, (min_coord_y + (0.001*i)))
}

Centroids = matrix(data = 0, nrow = (length(xcoordinates)*length(ycoordinates)), ncol =  3)
Centroids = as.data.frame(Centroids)
colnames(Centroids) = c("id", "x", "y")
s=0
for(i in 1:length(xcoordinates)){
  for(n in 1:length(ycoordinates)){
    Centroid_data$x[(s+n)] = xcoordinates[i]
    Centroid_data$y[(s+n)] = ycoordinates[n]
  }
  s = s + n
}
coordinates(Centroids)= ~x+y
proj4string(Centroids)=CRS("+proj=longlat +datum=WGS84")
plot(Centroids)

Centroids = point.in.poly(Centroids, UrbanArea, sp = T)
Centroids = Centroids[!is.na(Centroids$area),]
Centroids$ORIG_FID = c(seq(1:nrow(Centroids)))
plot(Centroids)


setwd(paste(dataFolder, "/", city, sep = ""))
writeOGR(Centroids, dsn=paste(dataFolder, "/", city, sep = ""),layer= paste(city,"centroids", sep = ""),driver="ESRI Shapefile")
Centroids = readOGR(dsn=paste(dataFolder, "/", city, sep = ""),layer= paste(city,"centroids", sep = ""))


######################################################################################################
################## Joining the points with Worldpop data and the metrostation shapefile #########
######################################################################################################
setwd(paste(dataFolder, "/", city, "/WorldPop", sep = ""))

Pop_pre = raster("pol_ppp_2012.tif") #Warsaw
Pop_post = raster("pol_ppp_2018.tif") #Warsaw
Pop_pre = raster("esp_ppp_2012.tif") #Barcelona
Pop_post = raster("esp_ppp_2018.tif") #Barcelona
Pop_pre = raster("ita_ppp_2012.tif") #Rome
Pop_post = raster("ita_ppp_2018.tif") #Rome
Pop_pre = raster("ita_ppp_2012.tif") #Milan
Pop_post = raster("ita_ppp_2018.tif") #Milan
Pop_pre = raster("deu_ppp_2012.tif") #Stuttgart    ## need to get the data
Pop_post = raster("deu_ppp_2018.tif") #Stuttgart
Pop_pre = raster("bgr_ppp_2012.tif") #Sofia
Pop_post = raster("bgr_ppp_2018.tif") #Sofia
Pop_pre = raster("aut_ppp_2012.tif") #Vienna
Pop_post = raster("aut_ppp_2018.tif") #Vienna
Pop_pre = raster("fin_ppp_2012.tif") #Helsinki
Pop_post = raster("fin_ppp_2018.tif") #Helsinki  ## need to get the data
Pop_pre = raster("hun_ppp_2012.tif") #Budapest
Pop_post = raster("hun_ppp_2018.tif") #Budapest

x_coord = c(max(Fsq@coords[,1]), max(Fsq@coords[,1]), min(Fsq@coords[,1]), min(Fsq@coords[,1]))
y_coord = c(min(Fsq@coords[,2]), max(Fsq@coords[,2]), max(Fsq@coords[,2]), min(Fsq@coords[,2]))
pol = cbind(x_coord, y_coord)
poly = Polygon(pol)
ps = Polygons(list(poly),1)
Clip_extent = SpatialPolygons(list(ps))
plot(Clip_extent)

Pop_pre = as.integer(crop(Pop_pre, extent(Clip_extent)))
Pop_post = as.integer(crop(Pop_post, extent(Clip_extent)))
writeRaster(Pop_pre, paste(city, "_pop_pre.tif", sep = ""), format = "GTiff")
writeRaster(Pop_post, paste(city, "_pop_post.tif", sep = ""), format = "GTiff")
Pop_pre = rasterToPolygons(Pop_pre, dissolve= F)
Pop_post = rasterToPolygons(Pop_post, dissolve= F)
Pop_pre$pop_pre = Pop_pre$layer
Pop_post$pop_post = Pop_post$layer

Centroids = point.in.poly(Centroids, Pop_pre, sp = T, duplicate = T)
Centroids = point.in.poly(Centroids, Pop_post, sp = T, duplicate = T)

remove(Pop_pre, Pop_post)
Centroids$popchange = Centroids$pop_post - Centroids$pop_pre

###
setwd(paste(dataFolder, "/", city, "/Metrostations", sep = ""))

SubwayStations =read.csv(paste(city,"_Metrostations.csv", sep = ""), header = T)
coordinates(SubwayStations)= ~lon+lat
proj4string(SubwayStations)=CRS("+proj=longlat +datum=WGS84") # set it to UTM

plot(UrbanArea)
plot(SubwayStations, pch = 20, col = "purple", add = TRUE)

d = pointDistance(Centroids, SubwayStations, lonlat=TRUE)
r = apply(d, 1, which.min)
Centroid_data = as.data.frame(Centroids)
Centroid_data$metroid = r
SubwayStations =read.csv(paste(city,"_Metrostations.csv", sep = ""), header = T)
SubwayStations$metroid = c(seq(1,nrow(SubwayStations)))
Centroid_data = merge(Centroid_data, SubwayStations, by= "metroid", all = T)
Centroid_data = Centroid_data[!is.na(Centroid_data$ORIG_FID),]
Centroid_data = Centroid_data[order(Centroid_data$ORIG_FID),]
for(i in 1:nrow(Centroid_data)){
  Centroid_data$NEAR_DIST[i] = d[i, Centroid_data$metroid[i]]
}

setwd(paste(dataFolder, "/", city, sep = ""))
write.csv(as.data.frame(Centroid_data), paste(city,"centroid_data.csv", sep = ""))
Centroid_data = read.csv(paste(city,"centroid_data.csv", sep = ""), header = T)

##############################################################################################################
################ aggregating population data in the 300m buffers of the respective centroid #################
##############################################################################################################
Centroids = readOGR(dsn=paste(dataFolder, "/", city, sep = ""),layer= paste(city,"centroids", sep = ""))
Centroids = spTransform(Centroids, CRS("+proj=longlat +datum=WGS84" ))

make_GeodesicBuffer = function(pts, width) {
  ### A) Construct buffers as points at given distance and bearing
  # a vector of bearings (fallows a circle)
  dg = seq(from = 0, to = 360, by = 5)
  
  # Construct equidistant points defining circle shapes (the "buffer points")
  buff.XY = geosphere::destPoint(p = pts, 
                                  b = rep(dg, each = length(pts)), 
                                  d = width)
  
  ### B) Make SpatialPolygons
  # group (split) "buffer points" by id
  buff.XY = as.data.frame(buff.XY)
  id  = rep(1:length(pts), times = length(dg))
  lst = split(buff.XY, id)
  
  # Make SpatialPolygons out of the list of coordinates
  poly   = lapply(lst, sp::Polygon, hole = FALSE)
  polys  = lapply(list(poly), sp::Polygons, ID = NA)
  spolys = sp::SpatialPolygons(Srl = polys, 
                                proj4string = CRS(as.character("+proj=longlat +ellps=WGS84 +datum=WGS84")))
  # Disaggregate (split in unique polygons)
  spolys = sp::disaggregate(spolys)
  return(spolys)
}

Centroids_buff300m  = make_GeodesicBuffer(Centroids, width=300)
Centroids_buff300m$ORIG_FID_buff = Centroids$ORIG_FID

setwd(paste(dataFolder, "/", city, sep = ""))
Centroid_data = read.csv(paste(city,"centroid_data.csv", sep = ""), header = T)
Centroid = Centroid_data[,c("ORIG_FID", "centroid_lat", "centroid_lon", "pop_2012", "pop_2018")]
coordinates(Centroid) =~ centroid_lat + centroid_lon
proj4string(Centroid)=CRS("+proj=longlat +datum=WGS84")
Centroid = point.in.poly(Centroid, Centroids_buff300m, sp = T, duplicate = T)


nrow(Centroid)/nrow(Centroid_data)
for(i in 1:nrow(Centroid_data)){
  Centroid_data$pop_pre_buffer[Centroid_data$ORIG_FID == i] = sum(Centroid$pop_pre[Centroid$ORIG_FID_buff == i])
  Centroid_data$pop_post_buffer[Centroid_data$ORIG_FID == i] = sum(Centroid$pop_post[Centroid$ORIG_FID_buff == i])
}

setwd(paste(dataFolder, "/", city, sep = ""))
write.csv(as.data.frame(Centroid_data), paste(city,"centroid_data.csv", sep = ""))
Centroid_data = read.csv(paste(city,"centroid_data.csv", sep = ""), header = T)


##############################################################################################################
################ creating buffers around the centroids and full joining it with the Fsq Data #################
##############################################################################################################

Centroids = readOGR(dsn=paste(dataFolder, "/", city, sep = ""),layer= paste(city,"centroids", sep = ""))
Centroids = spTransform(Centroids, CRS("+proj=longlat +datum=WGS84" ))
plot(Centroids)

Fsq = readOGR(dsn=paste(dataFolder,"/Foursquare/", city, sep = "") ,layer= paste(city, "_Fsq.csv", sep = ""))
names(Fsq) = c("id", "categoryna", "categoryid", "venuename",  "venueid", "Latitude", "Longitude", "Metacatego", "openyear", "openmonths", "NA")

plot(Centroids_buff300m)
plot(SubwayStations, pch = 20, col = "purple", add = TRUE)
plot(Fsq, add = T, col = "red")

setwd(paste(dataFolder, "/", city, sep = ""))
Centroids_buff300m_df = point.in.poly(Fsq, Centroids_buff300m, sp = T, duplicate = T)
write.csv(as.data.frame(Centroids_buff300m_df), paste(city, "venuehexagonbuffers_tot.csv", sep = ""), row.names = FALSE)


#############################################################################################################################
#################### Transforming into bufferunit - venuecategory matrix for before and after the station openings ##########
############################################################################################################################

Centroids_buff300m_df = read.csv(paste(city, "venuehexagonbuffers_tot.csv", sep = ""), header = TRUE)
Centroids_buff300m_df = unique(subset(Centroids_buff300m_df, select = -c(Field1, Metacatego, coords.x1, coords.x2)))

venuecategories = unique(subset(Centroids_buff300m_df, select = c("categoryid", "categoryna")))
categorynames = venuecategories$categoryid

columnnames = paste("cat_" , categorynames, sep = "")
print(columnnames)

gen_venuecat_matrix = function(Centroids, columnnames, Centroids_buff300m_df, max_year){
  venuecat_matrix = matrix(data = 0, nrow = nrow(Centroids), ncol = length(columnnames))
  colnames(venuecat_matrix) = columnnames
  subset = Centroids_buff300m_df[Centroids_buff300m_df$creationye < max_year,]
  subset = subset[!is.na(subset_pre$ORIG_FID),]
  for(i in 1:length(categorynames)){
    x = subset[subset$categoryid == categorynames[i],]
    y = x %>% count(ORIG_FID)
    venuecat_matrix[y$ORIG_FID, i] = y$n
  }
  return(venuecat_matrix)
}

venuecat_matrix_pre = gen_venuecat_matrix(Centroids = Centroids, columnnames = columnnames, Centroids_buff300m_df= Centroids_buff300m_df, max_year = Opening_year)
write.csv(venuecat_matrix_pre, paste(city, "venuecat_matrix_pre", Opening_year, ".csv", sep = ""), row.names = FALSE)

venuecat_matrix_post = gen_venuecat_matrix(Centroids = Centroids, columnnames = columnnames, Centroids_buff300m_df= Centroids_buff300m_df, max_year = post_3_years)
write.csv(venuecat_matrix_post, paste(city, "venuecat_matrix_pre", post_3_years, ".csv", sep = ""), row.names = FALSE)


#########################################################################################################
################### density and basic Shannon Wiener entropy ############################################
#########################################################################################################
setwd(paste(dataFolder, "/", city, sep = ""))

entropy = function(mat) {
  freqs = mat/rowSums (mat)
  entropy = - rowSums (freqs * log2(freqs+0.000000001))
  entropy = round (entropy, digits = 3)
  return (entropy)
}

venuecat_matrix_pre = read.csv(paste(city, "venuecat_matrix_pre", Opening_year, ".csv", sep = ""), header = T)
venuecat_matrix_post = read.csv(paste(city, "venuecat_matrix_pre", post_3_years, ".csv", sep = ""), header = T)

venuecat_matrix_pre$entropy = entropy(venuecat_matrix_pre)
venuecat_matrix_pre$rowsum = rowSums(venuecat_matrix_pre[, 1:length(columnnames)])
Centroid_data$entropy_pre = venuecat_matrix_pre$entropy[Centroid_data$ORIG_FID]
Centroid_data$rowsum_pre = venuecat_matrix_pre$rowsum[Centroid_data$ORIG_FID]
Centroid_data$entropy_pre[Centroid_data$entropy_pre == "NaN"] = 0

venuecat_matrix_post$entropy = entropy(venuecat_matrix_post)
venuecat_matrix_post$rowsum = rowSums(venuecat_matrix_post[, 1:length(columnnames)])
Centroid_data$entropy_post = venuecat_matrix_post$entropy[Centroid_data$ORIG_FID]
Centroid_data$rowsum_post = venuecat_matrix_post$rowsum[Centroid_data$ORIG_FID]
Centroid_data$entropy_post[Centroid_data$entropy_post == "NaN"] = 0

Centroid_data$entropy_diff = (Centroid_data$entropy_post - Centroid_data$entropy_pre)
Centroid_data$density_increase = (Centroid_data$rowsum_post - Centroid_data$rowsum_pre)

setwd(paste(dataFolder, "/", city, sep = ""))
write.csv(as.data.frame(Centroid_data), paste(city,"centroid_data.csv", sep = ""))

#######################################################################################################
########################## creating a venue similarity matrix #########################################
########################################################################################################

library(igraph)
library(networkD3)
library(rgexf)
library(tidytree)
library(data.tree)
library(DiagrammeR)

setwd(dataFolder)
venue_cat_edges = read.csv("venue categories_edge list_ids.csv", header = TRUE)
venue_cat_nodes = read.csv("venue_cat_nodes.csv", header = TRUE)

cat_taxonomy= graph_from_data_frame(d=venue_cat_edges, vertices = venue_cat_nodes, directed = FALSE)
plot.igraph(cat_taxonomy)
venue_cat_degrees= degree(cat_taxonomy)
write.csv(as.data.frame(venue_cat_degrees), "degrees venue categories.csv", row.names=TRUE)

jpeg("Venue Categories Degrees.jpeg")
plot(density(venue_cat_degrees, from = 0, to= 150, na.rm = TRUE), main = "Foursquare Venue Category Degree centrality")
dev.off()

dist= distances(cat_taxonomy, v = V(cat_taxonomy), to = V(cat_taxonomy), mode = "all", weights = NULL, algorithm = "automatic")
write.csv(as.data.frame(dist), "distance venue categories.csv", row.names=TRUE)

setwd(dataFolder)
venuesimilarity = read.csv("distance venue categories.csv", header = TRUE)
venuesimilarity$categoryna = venuesimilarity$X
venuecategories = read.csv("venuecategories.csv", header = T)
colnames(venuecategories) = c("categoryna", "categoryid")
venuecategories = unique(venuecategories)

venuecategories$categoryna = as.character(venuecategories$categoryna)
venuesimilarity2 = merge(venuesimilarity, venuecategories, by = "categoryna", all.x = TRUE)
rowcolumnselection = venuesimilarity2$rownum[!is.na(venuesimilarity2$categoryid)]
totvenuesimilarity = venuesimilarity2[!is.na(venuesimilarity2$categoryid), ]
rowcolumnselection = append(rowcolumnselection, c(-2, 939))
totvenuesimilarity = totvenuesimilarity[,(rowcolumnselection+3)]

totvenuesimilarity$columnnames= paste("cat_" , totvenuesimilarity$categoryid, sep = "")

columnnames = totvenuesimilarity$columnnames
columnnames = append(columnnames, c("categoryna", "categoryid", "columnnames"))
colnames(totvenuesimilarity) = columnnames

write.csv(totvenuesimilarity, "totvenuesimilarity.csv", row.names = F)


#########################################################################################################
########################## Calculating Multifunctionality ###############################################
#########################################################################################################

## The function SimilarityBasedDiversity takes the 
## 1) "venuecat_matrix", which is a matrix of counts of amenities per venue category for each spatial unit, and
## 2) "totvenuesimilarity", which is a similarity measure based on shortest path matrix of all category nodes (McInnes BT, Pedersen T, Li Y, Melton GB and Pakhomov SV (2014) U-path: An undirected path-based measure of semantic similarity. AMIA - Annual Symposium proceedings. AMIA Symposium 2014: 882-891.)
## and calculates a similarity-adjusted diversity measurement. 
## The algorithm works by redistributing a nominal distribution over categories (venuecat_matrix) 
## according to the similarity measure between categories (given by totvenuesimilarity)
## and then measure Shannon entropy over this new distribution.

SimilarityBasedDiversity = function(venuecat_matrix, totvenuesimilarity){
  SBD_df = venuecat_matrix
  SBD_df[, c("ncat", "entropysum", "rowentropy")] = ""
  ComputationMatrix = venuecat_matrix
  colnameset <- colnames(venuecat_matrix)
  entropy <- function(mat) {
    freqs <- mat/rowSums (mat)
    entropy <- - rowSums (freqs * log2(freqs+0.000000001))
    entropy <- round (entropy, digits = 3)
    return (entropy)
  }
  for(n in 1:nrow(venuecat_matrix)){
    localvenuesubset <- which(venuecat_matrix[n,1:ncol(venuecat_matrix)] != 0)
    if(length(localvenuesubset) > 1){
      colnameset_local <- colnameset[localvenuesubset]
      localvenuesubset_entries <- venuecat_matrix[n, localvenuesubset]
      similarity_rows <- which(totvenuesimilarity$columnnames %in% colnameset_local[1:length(colnameset_local)])
      similarity_columns <- which(colnames(totvenuesimilarity) %in%  colnameset_local[1:length(colnameset_local)])
      for(g in 1:length(localvenuesubset)){
        ComputationMatrix[n,localvenuesubset[g]] <-  localvenuesubset_entries[g] * (sum(totvenuesimilarity[similarity_rows[g], similarity_columns]))
        for(p in 1:length(localvenuesubset)){
          if(p != g){
            ComputationMatrix[n,localvenuesubset[p]] <- (localvenuesubset_entries[p] * (1- totvenuesimilarity[similarity_rows[g], similarity_columns[p]]))
          }
        }
        SBD_df[n, localvenuesubset[g]] <- entropy(ComputationMatrix[n,])
      }
      SBD_df$ncat[n] <- length(localvenuesubset)
      SBD_df$entropysum[n] <- sum(SBD_df[n, 1:ncol(venuecat_matrix)])
      SBD_df$rowentropy[n] <- as.numeric(SBD_df$entropysum[n])/ as.numeric(SBD_df$ncat[n])
    }
    else if(length(localvenuesubset) == 1){
      SBD_df$ncat[n] <- length(localvenuesubset)
      SBD_df$entropysum[n] <- 0
      SBD_df$rowentropy[n] <- 0
    }
    if(n %% 100 == 0){
      print(paste("calculated ", n, " of ", nrow(venuecat_matrix)))
    }
  }
  return(SBD_df)
}

# reading shortest path matrix of all category nodes
setwd(dataFolder)
totvenuesimilarity <- read.csv("totvenuesimilarity.csv", header=TRUE)
max(totvenuesimilarity[1:937, 1:937]) ## maximum shortest path is 9

# redistribute for 0 to be maximum dissimilarity and 1 maximum similarity (for standardization)
totvenuesimilarity[1:937, 1:937] <-(1-(totvenuesimilarity[1:937, 1:937]/9))

# reading the matrix of counts of amenities per venue category for each spatial unit
setwd(paste(dataFolder, "/", city, sep = ""))
venuecat_matrix_pre = read.csv(paste(city, "venuecat_matrix_pre", Opening_year, ".csv", sep = ""), header = T)
venuecat_matrix_post = read.csv(paste(city, "venuecat_matrix_pre", post_3_years, ".csv", sep = ""), header = T)

# make sure that the dataset is clean, so no rownumber is in first column (apply below code if it is)
# venuecat_matrix <- subset(venuecat_matrix, select = -c(X))

# applying the algorithm
dissimilardiversity_matrix_pre = SimilarityBasedDiversity(venuecat_matrix = venuecat_matrix_pre, totvenuesimilarity = totvenuesimilarity)
dissimilardiversity_matrix_post = SimilarityBasedDiversity(venuecat_matrix = venuecat_matrix_post, totvenuesimilarity = totvenuesimilarity)

write.csv(dissimilardiversity_matrix_pre, paste(city, "dissimilardiversity_matrix_", Opening_year, ".csv", sep = ""), row.names = F)
write.csv(dissimilardiversity_matrix_pre, paste(city, "dissimilardiversity_matrix_", post_3_years, ".csv", sep = ""), row.names = F)

Centroid_data$rowentropy_post = dissimilardiversity_matrix_post$rowentropy[Centroid_data$ORIG_FID]
Centroid_data$rowentropy_pre = dissimilardiversity_matrix_pre$rowentropy[Centroid_data$ORIG_FID]
Centroid_data$rowentropy_post[is.na(Centroid_data$rowentropy_post)] = 0
Centroid_data$rowentropy_pre[is.na(Centroid_data$rowentropy_pre)] = 0
Centroid_data$multifunctionality_diff = (Centroid_data$rowentropy_post - Centroid_data$rowentropy_pre)

setwd(paste(dataFolder, "/", city, sep = ""))
write.csv(as.data.frame(Centroid_data), paste(city,"centroid_data.csv", sep = ""))

######################################################################################################################################
########Defining the city center based on maximum amenity density and calculate the points Euclidean distance to center###############
######################################################################################################################################

Centroid_data$center_x = Centroid_data$coords.x1[which(Centroid_data$rowsum_post == max(na.omit(Centroid_data$rowsum_post)))]
Centroid_data$center_y = Centroid_data$coords.x2[which(Centroid_data$rowsum_post == max(na.omit(Centroid_data$rowsum_post)))]

Centroid_data$centroid_dist_center = spDists(as.matrix(Centroid_data[, c("coords.x1", "coords.x2")]), as.matrix(Centroid_data[1,c("center_x", "center_y")]), longlat = T)
Centroid_data$centroid_dist_center = Centroid_data$centroid_dist_center *1000 # in meters


######################################################################################################
################## Rearranging the venuecategorymatrix to typecounts of Metacategory #################
######################################################################################################

setwd(dataFolder)
### This is a edge list of metacategories and all their subcategories
metacategory_assignment = read.csv("metacategory_assignment.csv", header = T)

Food_colnames = metacategory_assignment[which(metacategory_assignment$metacategory == "Food"), c("columnnames")]
Shops_colnames = metacategory_assignment[which(metacategory_assignment$metacategory == "Shop & Service"), c("columnnames")]
ArtsEnter_colnames = metacategory_assignment[which(metacategory_assignment$metacategory == "Arts & Entertainment"), c("columnnames")]
Nightlife_colnames = metacategory_assignment[which(metacategory_assignment$metacategory == "Nightlife Spot"), c("columnnames")]
Recreation_colnames = metacategory_assignment[which(metacategory_assignment$metacategory == "Recreation"), c("columnnames")]
Proff_colnames = metacategory_assignment[which(metacategory_assignment$metacategory == "Professional Places"), c("columnnames")]


######## regroup the counts of venuecategories into the Metacategories before and after the new metro stations opened
setwd(paste(dataFolder, "/", city, sep = ""))

venuecat_matrix_pre = read.csv(paste(city, "venuecat_matrix_pre", Opening_year, ".csv", sep = ""), header = T)
venuecat_matrix_post = read.csv(paste(city, "venuecat_matrix_pre", post_3_years, ".csv", sep = ""), header = T)

venuecat_matrix_pre$nr_Food_pre = 0
venuecat_matrix_pre$nr_ShopsServ_pre = 0
venuecat_matrix_pre$nr_ArtsEnter_pre = 0
venuecat_matrix_pre$nr_Nightlife_pre = 0
venuecat_matrix_pre$nr_Proff_pre = 0
venuecat_matrix_pre$nr_Recreation_pre = 0

matrix_colnames = colnames(venuecat_matrix_pre)
for(i in 1:ncol(venuecat_matrix_pre)){
  if(is.element(matrix_colnames[i], Food_colnames)){
    venuecat_matrix_pre$nr_Food_pre = (venuecat_matrix_pre$nr_Food_pre + venuecat_matrix_pre[, i])
  }
  else if(is.element(matrix_colnames[i], Shops_colnames)){
    venuecat_matrix_pre$nr_ShopsServ_pre = (venuecat_matrix_pre$nr_ShopsServ_pre + venuecat_matrix_pre[, i])
  }
  else if(is.element(matrix_colnames[i], ArtsEnter_colnames)){
    venuecat_matrix_pre$nr_ArtsEnter_pre = (venuecat_matrix_pre$nr_ArtsEnter_pre + venuecat_matrix_pre[, i])
  }
  else if(is.element(matrix_colnames[i], Nightlife_colnames)){
    venuecat_matrix_pre$nr_Nightlife_pre = (venuecat_matrix_pre$nr_Nightlife_pre + venuecat_matrix_pre[, i])
  }
  else if(is.element(matrix_colnames[i], Proff_colnames)){
    venuecat_matrix_pre$nr_Proff_pre = (venuecat_matrix_pre$nr_Proff_pre + venuecat_matrix_pre[, i])
  }
  else if(is.element(matrix_colnames[i], Recreation_colnames)){
    venuecat_matrix_pre$nr_Recreation_pre = (venuecat_matrix_pre$nr_Recreation_pre + venuecat_matrix_pre[, i])
  }
}


Centroid_data$nr_Food_pre_300mbuff = venuecat_matrix_pre$nr_Food_pre[Centroid_data$ORIG_FID] 
Centroid_data$nr_ShopsServ_pre_300mbuff = venuecat_matrix_pre$nr_ShopsServ_pre[Centroid_data$ORIG_FID] 
Centroid_data$nr_ArtsEnter_pre_300mbuff = venuecat_matrix_pre$nr_ArtsEnter_pre[Centroid_data$ORIG_FID] 
Centroid_data$nr_Nightlife_pre_300mbuff = venuecat_matrix_pre$nr_Nightlife_pre[Centroid_data$ORIG_FID] 
Centroid_data$nr_Proff_pre_300mbuff = venuecat_matrix_pre$nr_Proff_pre[Centroid_data$ORIG_FID] 
Centroid_data$nr_Recreation_pre_300mbuff = venuecat_matrix_pre$nr_Recreation_pre[Centroid_data$ORIG_FID] 

venuecat_matrix_post$nr_Food_post = 0
venuecat_matrix_post$nr_ShopsServ_post = 0
venuecat_matrix_post$nr_ArtsEnter_post = 0
venuecat_matrix_post$nr_Nightlife_post = 0
venuecat_matrix_post$nr_Proff_post = 0
venuecat_matrix_post$nr_Recreation_post = 0

matrix_colnames = colnames(venuecat_matrix_post)
for(i in 1:ncol(venuecat_matrix_post)){
  if(is.element(matrix_colnames[i], Food_colnames)){
    venuecat_matrix_post$nr_Food_post = (venuecat_matrix_post$nr_Food_post + venuecat_matrix_post[, i])
  }
  else if(is.element(matrix_colnames[i], Shops_colnames)){
    venuecat_matrix_post$nr_ShopsServ_post = (venuecat_matrix_post$nr_ShopsServ_post + venuecat_matrix_post[, i])
  }
  else if(is.element(matrix_colnames[i], ArtsEnter_colnames)){
    venuecat_matrix_post$nr_ArtsEnter_post = (venuecat_matrix_post$nr_ArtsEnter_post + venuecat_matrix_post[, i])
  }
  else if(is.element(matrix_colnames[i], Nightlife_colnames)){
    venuecat_matrix_post$nr_Nightlife_post = (venuecat_matrix_post$nr_Nightlife_post + venuecat_matrix_post[, i])
  }
  else if(is.element(matrix_colnames[i], Proff_colnames)){
    venuecat_matrix_post$nr_Proff_post = (venuecat_matrix_post$nr_Proff_post + venuecat_matrix_post[, i])
  }
  else if(is.element(matrix_colnames[i], Recreation_colnames)){
    venuecat_matrix_post$nr_Recreation_post = (venuecat_matrix_post$nr_Recreation_post + venuecat_matrix_post[, i])
  }
}


Centroid_data$nr_Food_post_300mbuff = venuecat_matrix_post$nr_Food_post[Centroid_data$ORIG_FID]  
Centroid_data$nr_ShopsServ_post_300mbuff = venuecat_matrix_post$nr_ShopsServ_post[Centroid_data$ORIG_FID] 
Centroid_data$nr_ArtsEnter_post_300mbuff = venuecat_matrix_post$nr_ArtsEnter_post[Centroid_data$ORIG_FID] 
Centroid_data$nr_Nightlife_post_300mbuff = venuecat_matrix_post$nr_Nightlife_post[Centroid_data$ORIG_FID] 
Centroid_data$nr_Proff_post_300mbuff = venuecat_matrix_post$nr_Proff_post[Centroid_data$ORIG_FID] 
Centroid_data$nr_Recreation_post_300mbuff = venuecat_matrix_post$nr_Recreation_post[Centroid_data$ORIG_FID] 

Centroid_data$nr_Food_prepost_300mbuff = Centroid_data$nr_Food_post_300mbuff - Centroid_data$nr_Food_pre_300mbuff
Centroid_data$nr_ShopsServ_prepost_300mbuff = Centroid_data$nr_ShopsServ_post_300mbuff - Centroid_data$nr_ShopsServ_pre_300mbuff 
Centroid_data$nr_ArtsEnter_prepost_300mbuff = Centroid_data$nr_ArtsEnter_post_300mbuff - Centroid_data$nr_ArtsEnter_pre_300mbuff
Centroid_data$nr_Nightlife_prepost_300mbuff = Centroid_data$nr_Nightlife_post_300mbuff - Centroid_data$nr_Nightlife_pre_300mbuff
Centroid_data$nr_Proff_prepost_300mbuff = Centroid_data$nr_Proff_post_300mbuff  - Centroid_data$nr_Proff_pre_300mbuff 
Centroid_data$nr_Recreation_prepost_300mbuff = Centroid_data$nr_Recreation_post_300mbuff - Centroid_data$nr_Recreation_pre_300mbuff 

setwd(paste(dataFolder, "/", city, sep = ""))
write.csv(as.data.frame(Centroid_data), paste(city,"centroid_data.csv", sep = ""))

###############################################################################################################
###################### Computing Street network accessability to the center as control variable ###############
###############################################################################################################
setwd(paste(dataFolder, "/", city, sep = ""))
Fsq = Fsq[!is.na(Fsq$lat), ]
kmeans_clusters = as.data.frame(matrix(data = 0, ncol = 2, nrow = 50))
colnames(kmeans_clusters) = c("cluster_number", "tot_withinss")
kmeans_clusters$cluster_number = c(seq(1,50))
k.max = 50
for(k in 1:k.max){
  kmeans_clusters$tot_withinss[k] = kmeans(cbind(Fsq$lat, Fsq$lon), centers = k )$tot.withinss
}

jpeg(paste(city, "k-means Clustering - Optimal Cluster Number.jpeg"), width = 8, height = 7, units = "in", res = 1000)
plot(kmeans_clusters$cluster_number, kmeans_clusters$tot_withinss,
     type="b", pch = 7, frame = FALSE, 
     main = paste(city, "k-means Clustering - Optimal Cluster Number"),
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")
dev.off()

##select the elbow number
km = kmeans(cbind(Fsq$lat, Fsq$lon), centers = 11)
jpeg(paste(city, "k-means Clustering Cluster-Map.jpeg"), width = 8, height = 8, units = "in", res = 1000)
plot(Fsq$lat, Fsq$lon, col = km$cluster, pch = 20, 
    main = paste(city, "k-means Clustering (Optimal Cluster Number = 11)"))
dev.off()
nrow(km$centers)
install.packages("concaveman")
library(concaveman)

Fsq$cluster = km$cluster
pnts = Fsq[Fsq$cluster == 1,] %>%
st_as_sf(coords = c("lon", "lat"), crs = 4326)
polygon = concaveman(pnts)
polygon$clusterid = 1

for(i in 2:nrow(km$centers)){
  pnts = Fsq[Fsq$cluster == i,] %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326)
  polygon2 = concaveman(pnts)
  polygon2$clusterid = i
  polygon = rbind(polygon, polygon2)
}

polygon = as_Spatial(polygon)
proj4string(polygon)=CRS("+proj=longlat +datum=WGS84") # set it to UTM

setwd(paste(dataFolder, "/", city, sep = ""))
Centroid_data = read.csv(paste(city,"centroid_data.csv", sep = ""), header = T)
coordinates(Centroid_data)= ~centroid_lat+ centroid_lon
proj4string(Centroid_data)=CRS("+proj=longlat +datum=WGS84") # set it to UTM
Centroid_data = point.in.poly(Centroid_data, polygon, sp = T, duplicate = F)
subcenters_data = as.data.frame(km$centers)
Centroid_data$rowsum_post[is.na(Centroid_data$rowsum_post)] = 0


subcenter = Centroid_data$pid1
Centroid_data = read.csv("Centroid_data.csv", header = TRUE)
Centroid_data$subcenterid = subcenter

subcenters = matrix(data=NA, nrow = nrow(km$centers), ncol = 5)
subcenters = as.data.frame(subcenters)
colnames(subcenters) = c("subcenterid", "subcenter_ORIG_FID", "subcenter_lat", "subcenter_lon", "subcenter_amen_density")
for(i in 1:nrow(km$centers)){
  subcentral_subset =Centroid_data[Centroid_data$subcenterid == i,]
  subcentral_subset = subcentral_subset[!is.na(subcentral_subset$ORIG_FID),]
  subcenters[i,1] = as.numeric(i)
  subcenters[i,2] = subcentral_subset$ORIG_FID[which(subcentral_subset$rowsum_pre == max(subcentral_subset$rowsum_pre))][1]
  subcenters[i,3] = subcentral_subset$centroid_lat[subcentral_subset$ORIG_FID == subcenters[i,2]]
  subcenters[i,4] = subcentral_subset$centroid_lon[subcentral_subset$ORIG_FID == subcenters[i,2]]
  subcenters[i,5] = subcentral_subset$rowsum_pre[subcentral_subset$ORIG_FID == subcenters[i,2]]
}


centroids = Centroid_data[,c( "centroid_lon", "centroid_lat")]
rownames(centroids) = c(Centroid_data$ORIG_FID)
colnames(centroids) = c("lon", "lat")
subcenters_data = subcenters[,c("subcenter_lon", "subcenter_lat")]
colnames(subcenters_data) = c("lon", "lat")
dis_matrix = pointDistance( centroids, subcenters_data, lonlat=TRUE)
dis_matrix = as.data.frame(dis_matrix)
colnames(dis_matrix)=row.names(subcenters_data)
row.names(dis_matrix)=row.names(centroids)
for(i in 1:nrow(dis_matrix)){
  dis_matrix$subcenterID[i] = which(dis_matrix[i, 1:nrow(km$centers)] == min(dis_matrix[i, 1:nrow(km$centers)]))
}
Centroid_data$subcenterID = dis_matrix$subcenterID
subcenters_data$subcenterID = c(seq(1,nrow(subcenters_data)))
colnames(subcenters_data) = c("subcenter_lon", "subcenter_lat", "subcenterID")
Centroid_data = merge(Centroid_data, subcenters_data, by = "subcenterID", all.x = T)


write.csv(Centroid_data, "Centroid_data.csv")

###traveltime	tt	Indicates whether the travel time information should be provided in summary entries
###costfactor	cf	Indicates whether the CostFactor information should be returned in summary entries.
###distance	di	Indicates whether distance information should be returned in summary entries.
###walking traveltime and distance are equivalent, probably based on the assumption that it takes 1 second to walk 1 meter

Accessability_matrix = subset(Centroid_data, select = c(ORIG_FID, centroid_lon, centroid_lat, subcenter_lon, subcenter_lat))
names(Accessability_matrix) = c("ORIG_FID", "centroid_lat", "centroid_lon", "center_x", "center_y")

Accessability_matrix$traveltime = ""
Accessability_matrix$streetdistance = ""

Accessability_matrix$centroid_lat = as.numeric(Accessability_matrix$centroid_lat)
Accessability_matrix$center_y = as.numeric(Accessability_matrix$center_y)

apikey = 'ZFoIDwMr_B2_gQtL9ojl89gCuOhSi1tZA8Af3sUOT1Q'
apikey = 'rdpNQnG-8sphTD7RkU154p4Zs6TOGEulfr0ap0m5jMA'
apikey = '3w3-gzPjO4fkeQYA94-hKMaj9zEEDBZCLKZD_diWcT4'

TransportMode = 'bicycle'
TransportMode = 'pedestrian'

TransportMode = 'car'
for(m in 1:nrow(Accessability_matrix)){
  url_geocod =  paste("https://matrix.route.ls.hereapi.com/routing/7.2/calculatematrix.xml?apiKey=", as.character(apikey),
                       "&start0=geo!", as.character(Accessability_matrix$centroid_lat[m]), ",", as.character(Accessability_matrix$centroid_lon[m]),
                       "&start1=geo!",  as.character(Accessability_matrix$centroid_lat[m+1]), ",", as.character(Accessability_matrix$centroid_lon[m+1]), 
                       "&start2=geo!",  as.character(Accessability_matrix$centroid_lat[m+2]), ",", as.character(Accessability_matrix$centroid_lon[m+2]), 
                       "&start3=geo!",  as.character(Accessability_matrix$centroid_lat[m+3]), ",", as.character(Accessability_matrix$centroid_lon[m+3]), 
                       "&start4=geo!",  as.character(Accessability_matrix$centroid_lat[m+4]), ",", as.character(Accessability_matrix$centroid_lon[m+4]), 
                       "&start5=geo!",  as.character(Accessability_matrix$centroid_lat[m+5]), ",", as.character(Accessability_matrix$centroid_lon[m+5]), 
                       "&start6=geo!",  as.character(Accessability_matrix$centroid_lat[m+6]), ",", as.character(Accessability_matrix$centroid_lon[m+6]), 
                       "&start7=geo!",  as.character(Accessability_matrix$centroid_lat[m+7]), ",", as.character(Accessability_matrix$centroid_lon[m+7]), 
                       "&start8=geo!",  as.character(Accessability_matrix$centroid_lat[m+8]), ",", as.character(Accessability_matrix$centroid_lon[m+8]), 
                       "&start9=geo!",  as.character(Accessability_matrix$centroid_lat[m+9]), ",", as.character(Accessability_matrix$centroid_lon[m+9]), 
                       "&destination0=geo!",  as.character(Accessability_matrix$center_x[m]), ",", as.character(Accessability_matrix$center_y[m]), 
                       "&mode=fastest;", as.character(TransportMode),";traffic:default",
                       "&summaryAttributes=distance,traveltime",
                       sep = "")
  webpage = read_html(as.character(url_geocod))
  Accessability_matrix$traveltime[m] = webpage[9]
  Accessability_matrix$streetdistance[m] =webpage[8]
  Accessability_matrix$traveltime[m+1] = webpage[14]
  Accessability_matrix$streetdistance[m+1] =webpage[13]
  Accessability_matrix$traveltime[m+2] = webpage[19]
  Accessability_matrix$streetdistance[m+2] =webpage[18]
  Accessability_matrix$traveltime[m+3] = webpage[24]
  Accessability_matrix$streetdistance[m+3] =webpage[23]
  Accessability_matrix$traveltime[m+4] = webpage[29]
  Accessability_matrix$streetdistance[m+4] =webpage[28]
  Accessability_matrix$traveltime[m+5] = webpage[34]
  Accessability_matrix$streetdistance[m+5] =webpage[33]
  Accessability_matrix$traveltime[m+6] = webpage[39]
  Accessability_matrix$streetdistance[m+6] =webpage[38]
  Accessability_matrix$traveltime[m+7] = webpage[44]
  Accessability_matrix$streetdistance[m+7] =webpage[43]
  Accessability_matrix$traveltime[m+8] = webpage[49]
  Accessability_matrix$streetdistance[m+8] =webpage[48]
  Accessability_matrix$traveltime[m+9] = webpage[54]
  Accessability_matrix$streetdistance[m+9] =webpage[53]
  m = m+9
}

write.csv(Accessability_matrix, paste(city, "accessability_matrix.csv", sep = ""))

colnames(subcenters) =  c("subcenterID" ,"subcenter_ORIG_FID", "subcenter_lat", "subcenter_lon", "subcenter_amen_density")
subcenters = subcenters[,c("subcenterID", "subcenter_amen_density")]
Centroid_data = merge(Centroid_data, subcenters, by = "subcenterID")
Centroid_data = merge(Centroid_data, Accessability_matrix, by = "ORIG_FID", all.x = T)

write.csv(Centroid_data, "Centroid_data.csv")
