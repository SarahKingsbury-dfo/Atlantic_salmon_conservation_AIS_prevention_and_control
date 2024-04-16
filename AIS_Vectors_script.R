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
library(viridis)
if(!require(geomtextpath)) devtools::install_github("AllanCameron/geomtextpath")
library(wesanderson)
require(tidyverse)

#set-up geographics
proj <- "+proj=lcc +lon_0=-63.1 +lat_1=43.8 +lat_2=46.9 +lat_0=45.3 +datum=WGS84 +units=m +no_defs"
ll <- "+proj=longlat +datum=WGS84"
sf::sf_use_s2(FALSE) # because of "Evaluation error: Found 1 feature with invalid spherical geometry." otherwise


if(!file.exists("tempdata/atlprov.rds")){
  dir.create("tempdata")
  Canada <-  rnaturalearth::ne_states(country="Canada",returnclass = "sf") 
  
  atlprov <- Canada %>% 
    filter(postal %in% c("NS","NB","PE"))%>% 
    st_transform(proj)
  
  NS<-Canada%>% 
    filter(postal %in% "NS")%>% 
    st_transform(proj)
  
  rm(Canada)
  saveRDS(atlprov,"tempdata/atlprov.rds")
  saveRDS(NS, "tempdata/NS_polygon.rds")
}

atlprov <- readRDS("tempdata/atlprov.rds")
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

#establish grid
grid<-primary_ws%>%
  st_transform(proj)%>%
  mutate(g=as.numeric(row.names(.)))
grid<- st_buffer(grid,0)

#Now add in pet store, water garden, and nursery location files
Pet_Garden <-  read_excel("tempdata/NS_Pet_Garden_Locations.xlsx") %>%
  mutate(Longitude=as.numeric(Longitude),
         Latitude=as.numeric(Latitude)) %>% 
  filter(!is.na(Longitude)&!is.na(Latitude)) %>% 
  st_as_sf(coords=c("Longitude","Latitude"),crs=ll) %>%
  st_transform(proj)

Pet_stores<-Pet_Garden%>%
  filter(Category=="Pet shop")

Pet_Store_grid<-st_join(Pet_stores, grid, join=st_intersects)

Pet_Store_grid$g <- st_intersects(Pet_Store_grid, grid)%>%
  unlist()

grid<- Pet_Store_grid%>%
  as.data.frame()%>%
  group_by(g)%>%
  summarize(No.Pet.Stores=length(unique(Name)))%>%
  left_join(grid,.,by="g")

Garden_Centers<-Pet_Garden%>%
  filter(Category=="Garden Centre or Nursery")

Garden_grid<-st_join(Garden_Centers, grid, join=st_intersects)

Garden_grid$g <- st_intersects(Garden_grid, grid)%>%
  unlist()

grid<- Garden_grid%>%
  as.data.frame()%>%
  group_by(g)%>%
  summarize(No.Garden.Centtres=length(unique(Name)))%>%
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

#Vector Summary with normalized scores

summary_vectors <- grid %>% 
  as.data.frame() %>% 
  #removed everything that was already analyzed via RFM. Did not want to double count anything. 
  dplyr::select(-geometry) %>% 
  pivot_longer(cols=-c(g, river),names_to="metric", values_to = "value") %>% 
  mutate(value=replace_na(value,0),
         norm=value) %>% 
  group_by(metric) %>% 
  mutate(norm=(norm-min(norm))/max(norm-min(norm)))%>%
  pivot_wider(names_from=metric, values_from = c(value, norm))%>%
  ungroup()%>%
  mutate(CDV_PetStore=1,132,
         CDV_WaterGarden_Nurseries=90,
         CDV_RecFishing=672,
         CDV_UncleanedGear=1,190,
         PIV_PetSore=805,
         PIV_WaterGarden_Nurseries=552,
         PIV_RecFishing=999,
         PIV_UncleanedGear=834,
         SI_f_PetStore=0.957,
         SI_f_WaterGarden_Nurseries=0.317,
         SI_f_RecFishing=0.826,
         SI_f_UncleanedGear=1,
         TP_PetStore=1,
         TP_WaterGarden_Nurserie=4,
         TP_RecFishing=6,
         TP_UncleanedGear=13)%>%
  group_by(g)%>%
  mutate(TV_f_PetStore=(TP_PetStore*norm_No.Pet.Stores)/6.5,
         TV_f_WaterGarden_Nurseries=(TP_WaterGarden_Nurserie*norm_No.Garden.Centtres)/6.5,
         TV_f_RecFishing=(TP_RecFishing*norm_RecFishproportion)/6.5,
         TV_f_UncleanedGear=(TP_UncleanedGear*norm_BoatLaunch)/6.5,
         RV_PetStore=((2*SI_f_PetStore)+TV_f_PetStore)/3,
         RV_WaterGarden_Nurserie=((2*SI_f_WaterGarden_Nurseries)+TV_f_WaterGarden_Nurseries)/3,
         RV_RecFishing=((2*SI_f_RecFishing)+TV_f_RecFishing)/3,
         RV_UncleanedGear=((2*SI_f_UncleanedGear)+TV_f_UncleanedGear)/3)

#export summary_vector as csv
write.csv(summary_vectors, "summary_vectors_by_watershed.csv", row.names = TRUE)

#plot the outcome
RV_Summary_Vector<-summary_vectors%>%
  dplyr::select(river, g, RV_PetStore, RV_WaterGarden_Nurserie, RV_RecFishing, RV_UncleanedGear)%>%
  pivot_longer(cols=-c(g, river), names_to="vector", values_to = "value")

plot<-ggplot(RV_Summary_Vector, aes(x=river, y=value, col=vector))+
  geom_point(size=2)+
  theme(axis.text.x=element_text(angle=90),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "grey"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "grey"))+
  labs(y="Risk Score", x="Watershed Name")+
  scale_color_viridis(discrete=TRUE, name="Vector", 
                      labels=c("Pet Store", "Rectreaional Fishing","Unclean Boats & Gear", "Water Garden Center & Nurseries"))

ggsave(filename="plots/vector_by_watershed.pdf", plot=plot, unit="cm",width=35,height=20,  dpi=600, device = pdf)
