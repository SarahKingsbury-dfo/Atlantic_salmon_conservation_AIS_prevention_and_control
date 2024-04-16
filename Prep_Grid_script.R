require(sf)
require(rvest)
require(rnaturalearth)
require(mregions)
require(robis)
require(rgbif)
require(rinat)
if(!require("esri2sf")) devtools::install_github("yonghah/esri2sf")
require(raster)
require(fasterize)
require(gdistance)
require(spocc)
require(readxl)
require(tmap)
require(randomForest)
require(ROCR)
require(rgeos)
require(stringr)
library(RColorBrewer)
require(tidyverse)

#setup geographic area
proj <- "+proj=lcc +lon_0=-63.1 +lat_1=43.8 +lat_2=46.9 +lat_0=45.3 +datum=WGS84 +units=m +no_defs"
ll <- "+proj=longlat +datum=WGS84"
sf::sf_use_s2(FALSE) # because of "Evaluation error: Found 1 feature with invalid spherical geometry." otherwise


if(!file.exists("tempdata/atlprov.rds")){
  dir.create("tempdata")
  Canada <-  rnaturalearth::ne_states(country="Canada",returnclass = "sf") 

  
  NS<-Canada%>% 
    filter(postal %in% "NS")%>% 
    st_transform(proj)
  
  rm(Canada)
  
  saveRDS(NS, "tempdata/NS_polygon.rds")
}

NS<- readRDS("tempdata/NS_polygon.rds")
#setting up NS's grid

#Sarah T's code from the freshwater app
if(!file.exists("tempdata/NS/primaryws.shp")){
  download.file("https://data.novascotia.ca/api/geospatial/569x-2wnq?method=export&format=Shapefile","1_10,000 Nova Scotia Primary Watersheds.zip")
  unzip("1_10,000 Nova Scotia Primary Watersheds.zip",exdir="Watersheds/NS")
}

#NS Primary Watersheds in polygon forms
primaryws <- st_read("Watersheds/NS/geo_export_bcae59bd-380a-4273-87ea-47e93db8e162.shp")%>%
  filter(river!="COSTAL ISLAND")

primaryws$river<-sub('[.]','_', make.names(primaryws$river, unique=TRUE))


primary_ws<- dplyr:: select(primaryws, river, geometry)


grid<-primary_ws%>%
  st_transform(proj)%>%
  mutate(g=as.numeric(row.names(.)))
grid<- st_buffer(grid,0)

#State of Salmon layer
raw_salmon<-st_read("tempdata/salmon/ALL_FINAL_SALMON_RIVERS_DO_NOT_SHARE.shp")%>%
  st_transform(proj)

#salmon HSM layer
model<-readRDS("plots/model14a_predictions.rds")%>%
  st_transform(proj)

#Salmon LGB Stocking program

stocking<-st_read("tempdata/LGB_SalmonReleases_CBF_2016_2020/LGB_SalmonReleases_CBF_2016_2020.gdb")%>%
  st_transform(proj)

stocking_detect<-st_join(stocking, grid, join=st_intersects)

stocking_detect$g <- st_intersects(stocking_detect, grid)%>%
  unlist()

grid<- stocking_detect%>%
  as.data.frame()%>%
  group_by(g)%>%
  summarize(sal_stocking=length(unique(SiteCode)))%>%
  left_join(grid,.,by="g")%>%
  mutate(sal_stocking=replace_na(sal_stocking,0),
    sal_stocking=(sal_stocking-min(sal_stocking))/max(sal_stocking-min(sal_stocking)))

#AIS names
page <- "https://laws-lois.justice.gc.ca/eng/regulations/SOR-2015-121/FullText.html" %>% 
  read_html()

# extract part 2
part2 <- (page %>% 
            html_table(header = TRUE))[[1]]%>%
  `colnames<-`(.[1, ]) %>%
  .[-1, ]

# extract part 3
part3 <- (page %>% 
            html_table(header = TRUE))[[2]]%>%
  `colnames<-`(.[1, ]) %>%
  .[-1, ]

unlisted <- read.csv("unlisted_AIS.csv",check.names=FALSE)

indigenoussp <- c("Petromyzon marinus",
                  "Acipenser brevirostrum",
                  "Alosa pseudoharengus",
                  "Osmerus mordax",
                  "Apeltes quadracus",
                  "Gasterosteus aculeatus",
                  "Gasterosteus wheatlandi",
                  "Morone saxatilis and hybrids",
                  "Morone americana and hybrids",
                  "Lepomis gibbosus",
                  "Perca flavescens")

AISnames <- bind_rows("AISR Part 2"=part2,
                      "AISR Part 3"=part3,
                      "Not Listed in AISR"=unlisted,
                      .id="list_status") %>% 
  filter(!(`Scientific Name` %in% indigenoussp))

#Loading AIS species distribution information


#Chinese mystery snails
# accessed from: https://figshare.com/articles/dataset/Chinese_Mystery_Snail_Project_Master_Database/12295463?accessType=DOWNLOAD



CMS <-  read_excel("tempdata/MASTER MARITIME LAKE FILE (2).xlsx") %>%
  mutate(longitude=as.numeric(longitude),
         latitude=as.numeric(latitude)) %>% 
  filter(!is.na(longitude)&!is.na(latitude)) %>% 
  filter(CMS>=1)%>%
  st_as_sf(coords=c("longitude","latitude"),crs=ll) %>%
  st_transform(proj)

# Provincial survey data-AIS Fishes
# if(!file.exists("tempdata/fishes.csv")){
#   download.file("https://data.novascotia.ca/api/views/jgyj-d4fh/rows.csv?accessType=DOWNLOAD",destfile = "tempdata/fishes.csv")
#   
# }

#Reading in NSDFA data and adding Scientific Names
fishes <- read_csv("tempdata/fishes.csv") %>%
  st_as_sf(coords=c("EASTING","NORTHING"),crs="+proj=utm +zone=20 +ellps=WGS84 +datum=WGS84 +units=m +no_defs") %>%
  st_transform(proj)

fishes_sci<-fishes%>%
  dplyr::mutate(
    Scientific_Name=dplyr::case_when(`SPECIES COMMON NAME`=="Brook Trout" ~"Salmo fontinalis",
                                     `SPECIES COMMON NAME` =="White Perch" ~"Morone americana",
                                     `SPECIES COMMON NAME`== "Yellow Perch"~"Perca flavescens",
                                     `SPECIES COMMON NAME` =="Brown Bullhead"~"Ameiurus nebulosus",
                                     `SPECIES COMMON NAME` =="American Eel"~"Anguilla rostrata",
                                     `SPECIES COMMON NAME` =="White Sucker"~"Catostomus commersonii",
                                     `SPECIES COMMON NAME` =="Golden Shiner"~"Notemigonus crysoleucas",
                                     `SPECIES COMMON NAME` =="Banded Killifish" ~"Fundulus diaphanus",
                                     `SPECIES COMMON NAME` =="Smallmouth Bass"~"Micropterus dolomieu",
                                     `SPECIES COMMON NAME` =="Creek Chub"~"Semotilus atromaculatus",
                                     `SPECIES COMMON NAME` =="Lake Whitefish"~"Coregonus clupeaformis",
                                     `SPECIES COMMON NAME` =="Tiger Trout"~"Salmo trutta × Salvelinus fontinalis",
                                     `SPECIES COMMON NAME` =="Brown Trout"~"Salmo trutta" ,
                                     `SPECIES COMMON NAME` =="Nine Spine Stickleback" ~ "Pungitius pungitius",
                                     `SPECIES COMMON NAME` =="Striped Bass" ~"Morone saxatilis",
                                     `SPECIES COMMON NAME` =="Atlantic Salmon"~"Salmo salar",
                                     `SPECIES COMMON NAME` =="Lake Chub"~"Couesius plumbeus",
                                     `SPECIES COMMON NAME` =="Three Spine Stickleback"~"Gasterosteus aculeatus",
                                     `SPECIES COMMON NAME` =="Common Shiner"~"Luxilus cornutus",
                                     `SPECIES COMMON NAME` =="Alewife (Gaspereau)"~"Alosa pseudoharengus",
                                     `SPECIES COMMON NAME` =="Blacknose Shiner"~"Notropis heterolepis",
                                     `SPECIES COMMON NAME` =="Mummichog"~"Fundulus heteroclitus",
                                     `SPECIES COMMON NAME` =="Atlantic Silverside"~"Menidia menidia",
                                     `SPECIES COMMON NAME` =="Chain Pickerel"~ "Esox niger",
                                     `SPECIES COMMON NAME` =="Four Spine Stickleback"~"Apeltes quadracus",
                                     `SPECIES COMMON NAME` =="Tomcod"~"Microgadus tomcod",
                                     `SPECIES COMMON NAME` =="Black Nose Dace"~"Rhinichthys atratulus",
                                     `SPECIES COMMON NAME` =="American Shad"~"Alosa sapidissima",
                                     `SPECIES COMMON NAME` =="Goldfish"~"Carassius auratus",
                                     `SPECIES COMMON NAME` =="Northern Redbelly Dace"~"Chrosomus eos",
                                     `SPECIES COMMON NAME` =="Fall Fish"~"Semotilus corporalis",
                                     `SPECIES COMMON NAME` =="Atlantic Whitefish"~"Coregonus huntsmani",
                                     `SPECIES COMMON NAME` =="Lake Trout"~"Salvelinus namaycush")
    
  )%>%
  #find the AIS fishes in the NSDFA database
  filter(Scientific_Name %in% AISnames$`Scientific Name`)%>%
  st_transform(proj)

#other iNaturalists Data
# datadir <- file.path("occurences")
# 
# 
# files <- file.info(file.path(datadir,list.files(path = datadir, pattern="occ"))) %>% 
#   mutate(filename=basename(row.names(.)),
#          temp=gsub("occ_","",gsub(".rds","",filename))) %>%
#   separate(temp,c("grid","year","month","day"),"_") %>% 
#   filter(size>678) %>% 
#   dplyr::select(filename,grid,year,month,day)
# 
# 
# latlong <- readRDS(file.path(datadir,files$filename[1])) %>% st_crs()
# 
# occ <- lapply(files$filename, function(x) readRDS(file.path(datadir,x)) %>% 
#                 mutate(filename=x) %>% 
#                 as.data.table()) %>% 
#   rbindlist() %>%
#   mutate(geometry=st_sfc(geometry,crs=latlong)) %>% 
#   left_join(files,by="filename") %>% 
#   st_as_sf()

bbcoords <- (c(st_geometry(grid)) %>% 
               st_convex_hull() %>% 
               st_transform(ll) %>% 
               st_bbox() %>% 
               as.numeric())[c(2,1,4,3)]

slow_inat <- function(sp){
  Sys.sleep(3)
  message(paste("getting: ",sp,"at: ",Sys.time()))
  occ <- try(rinat::get_inat_obs(taxon_name = sp,
                                 bounds = bbcoords, maxresults = 10000))
  
  if(class(occ)=="try-error"){
    data.frame()
  } else {
    occ
  }
}

iNatocc <- map(AISnames$`Scientific Name`,
               slow_inat) %>% 
  bind_rows()
# as.data.frame()%>%
# st_as_sf(coords=c("longitude","latitude"),crs=ll) %>%
# st_transform(proj)
# 
# iNatocc<-iNatocc%>%
#   as.data.frame()%>%
#   st_as_sf(coords=c("longitude","latitude"),crs=ll) %>%
#   st_transform(proj)

#putting all the freshwater detections together

AIS <- bind_rows(
  iNatocc %>% 
    st_as_sf(coords = c("longitude","latitude"),crs=ll) %>% 
    mutate(provenance = paste0("iNaturalist (",quality_grade,")"),
           author = user_login,
           scientificName = scientific_name,
           eventDate = observed_on) %>% 
    dplyr::select(scientificName,eventDate,author,provenance,url),
  
  fishes_sci%>%
    st_as_sf(coords = c("longitude","latitude"),crs=ll) %>% 
    mutate(provenance = "Nova Scotia Department of Fisheries and Aquaculture",
           url="https://data.novascotia.ca/Fishing-and-Aquaculture/Nova-Scotia-Sportfish-Angler-Records/7rxg-qfam",
           scientificName = Scientific_Name,
           eventDate=`CAPTURE DATE`)%>% 
    dplyr::select(scientificName,eventDate,provenance,url),
  
  CMS%>%
    mutate(scientificName="Cipangopaludina chinensis")
  
)%>%
  st_transform(proj)
  #st_set_crs(st_crs(grid))

#combine AIS layer with the grid where points and watershed polygons intersect
AIS_detection<-st_join(AIS, grid, join=st_intersects)

AIS_detection$g <- st_intersects(AIS_detection, grid)%>%
  unlist()

grid<- AIS_detection%>%
  as.data.frame()%>%
  group_by(g)%>%
  summarize(detections=length(unique(scientificName)))%>%
  left_join(grid,.,by="g")

#Dispersal Vectors
#Boat Launches--this is a missing metric. We only know of boat launches within parks
#For this analysis, Parks will be considered a protected area

launch <- read_csv("tempdata/BoatLaunches.csv") %>% 
  st_as_sf(coords=c("Lon","Lat"),crs=ll) %>%
  st_transform(proj)

BL<- launch %>%
  filter(st_intersects(st_combine(grid),.,sparse=FALSE)[1,]) 

BL$g<- st_intersects(BL,grid)%>%
  unlist()

grid<- BL%>%
  as.data.frame() %>% 
  group_by(g) %>% 
  summarize(BoatLaunch=length(unique(Name)))%>%
  left_join(grid,.,by="g")


#Add SAR Distribution Data
#SARA Crit Hab from ESRI Rest using Remi Daigle Code
# url= "https://gisp.dfo-mpo.gc.ca/arcgis/rest/services/FGP/DFO_SARA_CriticalHabitat/MapServer" 
# geometry=paste0(st_bbox(NS),collapse = ",")
# layer="0"
# where="1=1"
# url <- httr::parse_url(url)
# url$path <- paste(url$path, layer, "query", sep = "/")
# url$query <- list(geometryType = "esriGeometryEnvelope",
#                   geometry = geometry,
#                   where = where,
#                   outFields = "*",
#                   returnGeometry = "true",
#                   f = "geojson")
# request <- httr::build_url(url)
# 
# SARA_crithab <- sf::st_read(request)%>%
#   st_transform(proj)

#SARA Dist from ESRI Rest using a modified version of Remi's code
#I was unable to use a URL list to grab both layers in one go
# 
# url2= "https://gisp.dfo-mpo.gc.ca/arcgis/rest/services/FGP/DFO_SARA_Distribution/MapServer" 
# url2 <- httr::parse_url(url2)
# url2$path <- paste(url2$path, layer, "query", sep = "/")
# url2$query <- list(geometryType = "esriGeometryEnvelope",
#                    geometry = geometry,
#                    where = where,
#                    outFields = "*",
#                    returnGeometry = "true",
#                    f = "geojson")
# request <- httr::build_url(url2)
# 
# SARA <- rbind(esri2sf::esri2sf("https://gisp.dfo-mpo.gc.ca/arcgis/rest/services/FGP/DFO_SARA_Distribution/MapServer/0"),
#                                     esri2sf::esri2sf("https://gisp.dfo-mpo.gc.ca/arcgis/rest/services/FGP/DFO_SARA_CriticalHabitat/MapServer/0"))%>%
#   st_transform(proj)
# 
# SARA_Dist <- sf::st_read(request)%>%
#   st_transform(proj)
# 
# mapserverUrl <- httr::parse_url("https://gisp.dfo-mpo.gc.ca/arcgis/rest/services/FGP/DFO_SARA_Distribution/MapServer/")
# # select the layer:
# mapserverUrl$path <- paste(mapserverUrl$path, "0", "query", sep = "/")
# # set the query parameters:
# mapserverUrl$query <- list(outFields = "*",
#                            resultOffset = 2000,
#                            resultRecordCount= 1000,
#                            where = "1=1",
#                            returnGeometry = "true",
#                            f = "geojson")
# httr::build_url(mapserverUrl)

SARA_crithab<-st_read("tempdata/SAR/CriticalHabitat/CriticalHabitat_HabitatEssentiel/CriticalHabitat_FGP.gdb", layer="DFO_SARA_CritHab_2022_FGP_EN_Maritimes")%>%
  st_transform(proj)

#SARA Distribution data file was too large for my computer to open. This should be added back in once a new machine is available. 

# SARA_Dist<-st_read("tempdata/SAR/Distribution/Distribution_Repartition/Distribution_FGP.gdb", layer="DFO_SARA_Dist_2022_FGP_EN")%>%
#   st_transform(proj)

impactedsp <- c("Coregonus huntsmani","Salmo salar","Lampsilis cariosa","Alasmidonta varicosa")

# SARareas <- bind_rows(SARA_crithab %>% 
#                         filter(Scientific_Name %in% impactedsp),
#                       SARA_Dist %>% 
#                         filter(Scientific_Name %in% impactedsp & !(Scientific_Name %in% SARA_crithab$Scientific_Name)))

SARareas <- SARA_crithab %>% 
                        filter(Scientific_Name %in% impactedsp)%>%
  rename("geometry"="Shape")



SARtotals <- SARareas %>% 
 # mutate(area=as.numeric(st_area(st_geometry(.)))) %>% 
  as.data.frame() %>% 
  group_by(Scientific_Name) %>% 
  summarize(totalarea=sum(Area_Km2))

# colnames(SARareas)[colnames(SARareas)=="Shape"]<-"geometry"
# 
# colnames(SARtotals)[colnames(SARtotals)=="Shape"]<-"geometry"

#Row 14 had an invalid geometry 
SARareas<-SARareas[-c(14),]

SAR <- SARareas%>% 
  left_join(.,SARtotals,by="Scientific_Name") %>% 
  mutate(proportion=Area_Km2/totalarea)%>%
  st_make_valid()
#sf::st_buffer(dist=0)


SARproportional<-st_intersection(grid, SAR %>% st_transform(proj))%>%
  as.data.frame() %>% 
  group_by(g) %>% 
  summarize(SARproportion=sum(proportion))



grid <- grid %>%
  left_join(SARproportional,by = "g")


#Ecologically and Biologically Significant Areas
if(!file.exists("tempdata/DFO_EBSA.zip")){
  download.file("https://pacgis01.dfo-mpo.gc.ca/FGPPublic/EBSA/DFO_EBSA.zip","tempdata/DFO_EBSA.zip")
  unzip("tempdata/DFO_EBSA.zip",exdir="tempdata")
}

EBSAareas <- st_read("tempdata/DFO_EBSA/DFO_EBSA.shp",quiet = TRUE) %>% 
  st_transform(proj)


EBSAtotals <- EBSAareas %>% 
  mutate(area=as.numeric(st_area(st_geometry(.)))) %>% 
  as.data.frame() %>% 
  group_by(Name) %>% 
  summarize(totalarea=sum(area)) 


EBSA <- EBSAareas%>% 
  left_join(.,EBSAtotals,by="Name") %>% 
  mutate(area=as.numeric(st_area(st_geometry(.))),
         proportion=area/totalarea)



EBSAproportional<-st_intersection(grid, EBSA %>% st_transform(proj))%>%
  as.data.frame() %>% 
  group_by(g) %>% 
  summarize(EBSAproportion=sum(proportion))



grid <- grid%>%
  left_join(EBSAproportional, by="g")


#Indigenous Reserve Lands may offer protection to salmon
Reserves<- st_read("tempdata/indigenous/AL_TA_NS_2_127_eng.shp", quiet=TRUE)%>%
  st_transform(proj)


ResTotals<- Reserves %>%
  mutate(area=as.numeric(st_area(st_geometry(.))))%>%
  as.data.frame()%>%
  group_by(ALCODE)%>%
  summarize(totalarea=sum(area))

NSRes<-Reserves%>%
  left_join(.,ResTotals, by="ALCODE") %>%
  mutate(area=as.numeric(st_area(st_geometry(.))),
         proportion=area/totalarea)
NSRes<- st_buffer(NSRes,0)

NSResP <-st_intersection(grid, NSRes %>% st_transform(proj))%>%
  as.data.frame()%>%
  group_by(g)%>%
  summarize(NSResproportion=sum(proportion))


grid<- grid%>%
  left_join(NSResP, by="g")


#Impacts on native fish

#!Need to filter out AIS!Therefore, removed AIS fish from species preference ranking. 
SpeciesPrefernce <- read_csv("tempdata/SpeciesPrefernce.csv")

Native_fish <- bind_rows(
  fishes %>%
    mutate(lowersp=tolower(`SPECIES COMMON NAME`)) %>% 
    left_join(.,SpeciesPrefernce%>%mutate(lowersp=tolower(Common_Name)),
              by="lowersp") %>% 
    filter(!is.na(Common_Name))%>%
    st_transform(proj))


NFish_Detect<- bind_rows(
  fishes %>%
    filter(st_intersects(st_combine(grid),.,sparse=FALSE)[1,])) 


NFish_Detect$g <- st_intersects(NFish_Detect,grid) %>% 
  unlist()

grid <- NFish_Detect%>%
  as.data.frame()%>%
  group_by(g)%>%
  summarize(nativeFish=length(unique(`SPECIES COMMON NAME`)))%>%
  left_join(grid,.,by="g")


##Recreational Fishing represents a pathway for AIS introduction and an additional pressure on salmon
#already in percentage, just need to geographical bind

CountyFishing <- read_csv("tempdata/RecFish/CountyFishing.csv")

#!Join NS county polygon layer to county fishing data!
#county boundary files--polygon accessed from https://nsgi.novascotia.ca/gdd/

NSCounties<-st_read("tempdata/RecFish/County_Polygons.shp", quiet=TRUE)%>%
  st_transform(proj)


RecreationalFishing<- bind_rows(
  NSCounties %>%
    mutate(lowersp=tolower(NAME))%>%
    left_join(.,CountyFishing%>%mutate(lowersp=tolower(County)),
              by="lowersp") %>%
    filter(Resident_NonResident_Angler=="resident")%>%
    st_transform(proj)
)


RecFishTotal<-RecreationalFishing %>%
  mutate(days=as.numeric(Number_Days_Fished ))%>%
  as.data.frame()%>%
  group_by(County)%>%
  summarize(totaldays=sum(days))


RecFishP<- RecreationalFishing%>%
  left_join(., RecFishTotal, by="County")%>%
  mutate(days=as.numeric(Number_Days_Fished ),
         proportion=(days/totaldays))

RecFish <-st_intersection(grid, RecFishP %>% st_transform(proj))%>%
  as.data.frame()%>%
  group_by(g)%>%
  summarize(RecFishproportion=sum(proportion))

grid<- grid%>%
  left_join(RecFish, by="g")

#Provincial Parks offer some protection to salmon by controling the habitat surrounding waterbodies
NSParks <- st_read("tempdata/parks/geo_export_d177ae48-78e8-4f88-a6cb-f8f8817f9234.shp", quiet=TRUE)%>%
  st_transform(proj)


NSParksTotals<- NSParks %>%
  mutate(area=as.numeric(ha_gis))%>%
  as.data.frame()%>%
  group_by(objectid)%>%
  summarize(totalarea=sum(area))

Parks<-NSParks%>%
  left_join(.,NSParksTotals, by="objectid") %>%
  mutate(area=as.numeric(ha_gis),
         proportion=area/totalarea)
Parks<- st_buffer(Parks,0)

NSParksP <-st_intersection(grid, Parks %>% st_transform(proj))%>%
  as.data.frame()%>%
  group_by(g)%>%
  summarize(NSParksproportion=sum(proportion))

grid<- grid%>%
  left_join(NSParksP, by="g")

#salmon survey results (raw data) with modified final status of:
#lost=0, artifically stocked=0.2, high risk=0.4, medium risk=0.6, low risk=0.8, not at risk=1, unknown=NA

salmon_status<-raw_salmon%>% 
  filter(Region %in% c("Nova Scotia"))%>%
  mutate(salmon_stat=case_when(STATUS_FIN=="Lost"~0,
                               STATUS_FIN=="Low Risk" ~0.8,
                               STATUS_FIN=="Moderate Risk"~0.6,
                               STATUS_FIN=="High Risk"~0.4,
                               STATUS_FIN=="Artificially Sustained"~0.2,
                               STATUS_FIN=="Not at Risk" ~1),
         salmon_stat=as.numeric(salmon_stat))%>%
  select(salmon_stat, geometry)%>%
  st_transform(proj)%>%
  st_intersection(grid)

grid <- salmon_status%>%
  as.data.frame()%>%
  group_by(g)%>%
  summarize(salmon_stat_avg=mean(salmon_stat, na.rm=TRUE))%>%
  left_join(grid,.,by="g")


#Adding the predicted suitability

salmon_prediction<-model%>%
  select(predicted_suitable, geometry)%>%
  st_transform(proj)%>%
  st_intersection(grid)

grid <- salmon_prediction%>%
  as.data.frame()%>%
  group_by(g)%>%
  summarize(habitat_suitability_avg=mean(predicted_suitable, na.rm=TRUE))%>%
  left_join(grid,.,by="g")

#Total Road Density (km/km2)
roads<-model%>%
  select(TotRd_Dens, geometry)%>%
  st_transform(proj)%>%
  st_intersection(grid)

grid <- roads%>%
  as.data.frame()%>%
  group_by(g)%>%
  summarize(road_desnity_avg=mean(TotRd_Dens, na.rm=TRUE))%>%
  left_join(grid,.,by="g")


#climate projections from Christine Stortini

climate<-read_excel("tempdata/pressures/CS_FinalIndexScores_Dec29_short.xlsx", sheet=1)%>%
  filter(species=="Salmon")%>%
  st_as_sf(coords=c("Long_centr","Lat_centr"),crs=ll) %>%
  st_transform(proj)

Temp_vel<-climate%>%
  select(vel_t, geometry)%>%
  mutate(vel_t=as.numeric(vel_t))%>%
  filter(vel_t != "NA")%>%
  #as.data.frame()%>%
  st_set_crs(st_crs(grid))

Temp_vel_bind<-Temp_vel%>%
  filter(st_intersects(st_combine(grid),.,sparse=FALSE)[1,])
#st_intersection(grid, Temp_vel%>% st_transform(proj))
#st_join(Temp_vel, grid, join=st_intersects)

Temp_vel_bind$g <- st_intersects(Temp_vel_bind, grid)%>%
  unlist()

grid<- Temp_vel_bind%>%
  as.data.frame()%>%
  group_by(g)%>%
  summarize(Temp_Vel_avg=mean(vel_t, na.rm=TRUE))%>%
  left_join(grid,.,by="g")

#Road - Stream Crossings Density (#/km2)
X_stream<-model%>%
  select(Xing_Densi, geometry)%>%
  st_transform(proj)%>%
  st_intersection(grid)

grid <- X_stream%>%
  as.data.frame()%>%
  group_by(g)%>%
  summarize(X_stream_avg=mean(Xing_Densi, na.rm=TRUE))%>%
  left_join(grid,.,by="g")

#Aquatic barrier density (#km2)
AqBarrier<-model%>%
  select(AqBarrier_, geometry)%>%
  st_transform(proj)%>%
  st_intersection(grid)

grid <- AqBarrier%>%
  as.data.frame()%>%
  group_by(g)%>%
  summarize(AqBarrier_avg=mean(AqBarrier_, na.rm=TRUE))%>%
  left_join(grid,.,by="g")

#Percent area composed of urban/developed land
urban<-model%>%
  select(Percent_Ur, geometry)%>%
  st_transform(proj)%>%
  st_intersection(grid)

grid <- urban%>%
  as.data.frame()%>%
  group_by(g)%>%
  summarize(urban_land_avg=mean(Percent_Ur, na.rm=TRUE))%>%
  left_join(grid,.,by="g")

#Percent area composed of clearcuts (between 2008-2017)
clear_cut<-model%>%
  select(Percent_Cu, geometry)%>%
  st_transform(proj)%>%
  st_intersection(grid)

grid <- clear_cut%>%
  as.data.frame()%>%
  group_by(g)%>%
  summarize(clear_cut_avg=mean(Percent_Cu, na.rm=TRUE))%>%
  left_join(grid,.,by="g")


#Percent area composed of cropland and pasture
agri<-model%>%
  select(Percent_Ag, geometry)%>%
  st_transform(proj)%>%
  st_intersection(grid)

grid <- agri%>%
  as.data.frame()%>%
  group_by(g)%>%
  summarize(agri_avg=mean(Percent_Ag, na.rm=TRUE))%>%
  left_join(grid,.,by="g")


#Percent area of impervious surface
Impv<-model%>%
  select(Percent_Im, geometry)%>%
  st_transform(proj)%>%
  st_intersection(grid)

grid <- Impv%>%
  as.data.frame()%>%
  group_by(g)%>%
  summarize(Impv_avg=mean(Percent_Im, na.rm=TRUE))%>%
  left_join(grid,.,by="g")

#Annual nitrogen leaching (kg/ha)
N_Leaching<-model%>%
  select(N_Leach, geometry)%>%
  st_transform(proj)%>%
  st_intersection(grid)

grid <- N_Leaching%>%
  as.data.frame()%>%
  group_by(g)%>%
  summarize(N_Leach_avg=mean(N_Leach, na.rm=TRUE))%>%
  left_join(grid,.,by="g")


#Annual phosphorus leaching (kg/ha)
P_Leaching<-model%>%
  select(P_Leach, geometry)%>%
  st_transform(proj)%>%
  st_intersection(grid)

grid <- P_Leaching%>%
  as.data.frame()%>%
  group_by(g)%>%
  summarize(P_Leach_avg=mean(P_Leach, na.rm=TRUE))%>%
  left_join(grid,.,by="g")


#Annual pesticide leaching (g/ha)
pesticide<-model%>%
  select(PSTCD_Leac, geometry)%>%
  st_transform(proj)%>%
  st_intersection(grid)

grid <- pesticide%>%
  as.data.frame()%>%
  group_by(g)%>%
  summarize(pesticide_avg=mean(PSTCD_Leac, na.rm=TRUE))%>%
  left_join(grid,.,by="g")

#Density of metal, nutrient, and organic pollution point-sources (#/km2)
pollution<-model%>%
  select(PSP_Source, geometry)%>%
  st_transform(proj)%>%
  st_intersection(grid)

grid <- pollution%>%
  as.data.frame()%>%
  group_by(g)%>%
  summarize(pollution_avg=mean(PSP_Source, na.rm=TRUE))%>%
  left_join(grid,.,by="g")


#Normalize each column in grid
names(grid)

direction<- data.frame (metric=c("detections", "BoatLaunch",  "Temp_Vel_avg", "SARproportion",
                                 "EBSAproportion","NSResproportion","nativeFish", "RecFishproportion", 
                                 "NSParksproportion","salmon_stat_avg", "habitat_suitability_avg", "road_desnity_avg", 
                                 "X_stream_avg", "AqBarrier_avg","urban_land_avg", "clear_cut_avg", 
                                 "agri_avg", "Impv_avg", "N_Leach_avg", "P_Leach_avg", 
                                 "pesticide_avg", "pollution_avg", "sal_stocking"), 
                        d=c(-1,-1,-1,1,
                            1,1,1,-1,
                            1,1,1,-1,
                            -1,-1,-1,-1,
                            -1,-1,-1,-1,
                            -1,-1, -1))


summary <- grid %>% 
  as.data.frame() %>% 
  #removed everything that was already analyzed via RFM. Did not want to double count anything. 
  select(-geometry, -river, -Temp_Vel_avg, -road_desnity_avg, 
         -X_stream_avg, -AqBarrier_avg,-urban_land_avg, -clear_cut_avg, 
         -agri_avg, -Impv_avg, -N_Leach_avg, -P_Leach_avg, 
         -pesticide_avg, -pollution_avg, -sal_stocking) %>% 
  pivot_longer(cols=-g,names_to="metric", values_to = "value") %>% 
  left_join(direction,by="metric") %>%
mutate(value=replace_na(value,0),
       norm=value*d) %>% 
  group_by(metric) %>% 
  mutate(norm=(norm-min(norm))/max(norm-min(norm))) %>% 
  ungroup() %>% 
  group_by(g) %>% 
  summarise(mean_priority=mean(norm))

scores<-grid %>%
  left_join(summary,by = "g")%>%
  st_as_sf()

saveRDS(scores, "plots/scores.rds")
