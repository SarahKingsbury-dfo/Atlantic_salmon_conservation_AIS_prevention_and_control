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
require(ggplot2)
require(patchwork)
require(tidyverse)

# Get open data

## Province
proj <- "+proj=lcc +lon_0=-63.1 +lat_1=43.8 +lat_2=46.9 +lat_0=45.3 +datum=WGS84 +units=m +no_defs"
ll <- "+proj=longlat +datum=WGS84"
sf::sf_use_s2(FALSE) # because of "Evaluation error: Found 1 feature with invalid spherical geometry." otherwise


if(!file.exists("tempdata/atlprov.rds")){
  dir.create("tempdata")
  Canada <-  rnaturalearth::ne_states(country="Canada",returnclass = "sf") 
  
  atlprov <- Canada %>% 
    filter(postal %in% c("NS","NB","PE","NL","QC"))%>% 
    st_transform(proj)
  
  rm(Canada)
  saveRDS(atlprov,"tempdata/atlprov.rds")
}

atlprov <- readRDS("tempdata/atlprov.rds")

NS <- atlprov %>% 
  filter(postal=="NS")

## Watersheds
if(!file.exists("tempdata/nsws.rds")){
  download.file("https://nsgi.novascotia.ca/WSF_DDS/DDS.svc/DownloadFile?tkey=fhrTtdnDvfytwLz6&id=82",
                destfile = "tempdata/WTR_NS_Watersheds_10k_UT83.zip", mode='wb')
  
  unzip("tempdata/WTR_NS_Watersheds_10k_UT83.zip",exdir="tempdata")
  
  nsws <- read_sf("tempdata/NS_Watersheds_10k/PRIMARY_POLY_CSRS_UTM_Z20.shp") %>% 
    filter(!RIVER=="COSTAL ISLAND") %>% 
    st_transform(proj) %>% 
    group_by(RIVER) %>% 
    summarise()
  
  saveRDS(nsws,"tempdata/nsws.rds")
}

nsws <- readRDS("tempdata/nsws.rds")

ggplot(nsws)+
  geom_sf(aes(fill=RIVER))

## Aquatic Invasive Species
### Listed AIS names
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

# print out part 3
part3

### Unlisted AIS names
unlisted <- read.csv("unlisted_AIS.csv",check.names=FALSE)

## AIS occurrences
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

bbtxt <- st_geometry(nsws) %>% 
  st_bbox() %>% 
  st_as_sfc() %>% 
  st_transform(ll) %>% 
  st_as_text()


### OBIS
OBISocc <- robis::occurrence(scientificname = AISnames$`Scientific Name`,
                             geometry = bbtxt)


### GBIF
GBIFocc <- spocc::occ(query = AISnames$`Scientific Name`,
                      from = 'gbif',
                      geometry = bbtxt)%>%
  occ2df()%>%
  mutate(Species=case_when(name %in% c("Aequipecten irradians (Lamarck, 1819)","Argopecten irradians (Lamarck, 1819)") ~ "Argopecten irradians",
                           name %in% c("Ascidiella aspersa (MÃ¼ller, 1776)", "Ascidiella aspersa (Müller, 1776)") ~ "Ascidiella aspersa",
                           name %in% c("BOLD:AAA7687","BOLD:ACL8382","Carcinus maenas (Linnaeus, 1758)") ~ "Carcinus maenas",
                           name %in% c("Botrylloides violaceus Oka, 1927") ~ "Botrylloides violaceus",
                           name %in% c("Botryllus schlosseri (Pallas, 1766)") ~ "Botryllus schlosseri",
                           name %in% c("Bulimus tentaculatus (Linnaeus, 1758)")~"Bulimus tentaculatus",
                           name %in% c("Bythinia tentaculata Linnaeus, 1758" )~"Bythinia tentaculata",
                           name %in% c("Cabomba caroliniana A.Gray" )~"Cabomba caroliniana ", 
                           name %in% c("Caprella mutica Schurin, 1935","BOLD:AAE7686") ~ "Caprella mutica",
                           name %in% c("Carassius auratus (Linnaeus, 1758)" )~"Carassius auratus",
                           name %in% c("Carcinus maenas (Linnaeus, 1758)")~"Carcinus maenas",
                           name %in% c("Ciona intestinalis (Linnaeus, 1767)","Ciona intestinalis tenella (Stimpson, 1852)","Ciona tenella (Stimpson, 1852)") ~"Ciona intestinalis",
                           name %in% c("Cipangopaludina chinensis (Gray, 1833)")~"Cipangopaludina chinensis",
                           name %in% c("Codium fragile (Suringar) Hariot","Codium fragile subsp. fragile","Codium fragile subsp. tomentosoides (Goor) P.C.Silva","Codium fragile tomentosoides") ~ "Codium fragile",
                           name %in% c("Cyprinus rubrofuscus Lacepède, 1803")~"Cyprinus rubrofuscus",
                           name %in% c("Didemnum vexillum Kott, 2002") ~ "Didemnum vexillum",
                           name %in% c("Diplosoma listerianum (Milne Edwards, 1841)")~"Diplosoma listerianum",
                           name %in% c("Dorosoma cepedianum (Lesueur, 1818)")~ "Dorosoma cepedianum",
                           name %in% c("Esox lucius Linnaeus, 1758" )~"Esox lucius ",
                           name %in% c("Esox niger Lesueur, 1818")~"Esox niger",
                           name %in% c("Hemigrapsus sanguineus (De Haan, 1835)","Hemigrapsus sanguineus (de Haan, 1835)") ~ "Hemigrapsus sanguineus",
                           name %in% c("Membranipora membranacea (Linnaeus, 1767)", "Flustra membranacea Linnaeus, 1767" ) ~ "Membranipora membranacea",
                           name %in% c("Micropterus dolomieui Lacepède, 1802","Micropterus dolomieu Lacepède, 1802", "Micropterus dolomieu dolomieu", "Micropterus dolomieui" )~"Micropterus dolomieui",
                           name %in% c("Micropterus salmoides (Lacepède, 1802)" )~"Micropterus salmoides",
                           name %in% c("Nymphoides peltata (S.G.Gmel.) Kuntze")~"Nymphoides peltata",
                           name %in% c("Oncorhynchus mykiss (Walbaum, 1792)") ~ "Oncorhynchus mykiss",
                           name %in% c("Orconectes limosus (Rafinesque, 1817)", "Faxonius limosus (Rafinesque, 1817)" )~ "Faxonius limosus",
                           name %in% c("Ostrea edulis (Linnaeus, 1767)","Ostrea edulis Linnaeus, 1758") ~ "Ostrea edulis",
                           name %in% c("Polyodon spathula (Walbaum, 1792)")~"Polyodon spathula",
                           name %in% c("Sander vitreus (Mitchill, 1818)")~"Sander vitreus",
                           name %in% c("Styela clava Herdman, 1881") ~ "Styela clava",
                           name %in% c("Valvata piscinalis (O.F.Müller, 1774)" )~"Valvata piscinalis",
                           name %in% c("Sphaerium corneum (Linnaeus, 1758)")~"Sphaerium corneum ",
                           TRUE ~ name))
### iNaturalist
# spocc does not work here because it makes too many queries
# so I created a function that does the same thing but slower!
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

### NSDFA
# Provincial survey data-AIS Fishes
if(!file.exists("tempdata/fishes.csv")){
  download.file("https://data.novascotia.ca/api/views/jgyj-d4fh/rows.csv?accessType=DOWNLOAD",destfile = "tempdata/fishes.csv")
  
}

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
  filter(Scientific_Name %in% AISnames$`Scientific Name`)

### AIS Dataset Integration
#Combining all AIS occurrence records into one database
AIS <- bind_rows(
  OBISocc %>% 
    st_as_sf(coords = c("decimalLongitude","decimalLatitude"),crs=ll) %>% 
    mutate(provenance = "OBIS",
           url = paste0("https://obis.org/dataset/",dataset_id),
           author = institutionCode) %>% 
    dplyr::select(scientificName,eventDate,author,provenance,url),
  
  GBIFocc %>% 
    #filter(datasetKey == "50c9509d-22c7-4a22-a47d-8c48425ef4a7") %>%  # this step removes the iNaturalist Observations from GBIF
    st_as_sf(coords = c("longitude","latitude"),crs=ll) %>% 
    mutate(provenance = "GBIF",
           url = paste0("https://gbif.org/dataset/", key),
           #author = paste0("https://gbif.org/publisher/",publishingOrgKey),
           scientificName = Species,
           eventDate=as.character(date)) %>% 
    dplyr::select(scientificName,eventDate,provenance,url),
  
  iNatocc %>% 
    st_as_sf(coords = c("longitude","latitude"),crs=ll) %>% 
    mutate(provenance = paste0("iNaturalist (",quality_grade,")"),
           author = user_login,
           scientificName = scientific_name,
           eventDate = observed_on) %>% 
    dplyr::select(scientificName,eventDate,author,provenance,url),
  
  fishes_sci%>%
    mutate(provenance = "Nova Scotia Department of Fisheries and Aquaculture",
           url="https://data.novascotia.ca/Fishing-and-Aquaculture/Nova-Scotia-Sportfish-Angler-Records/7rxg-qfam",
           scientificName = Scientific_Name,
           eventDate=`CAPTURE DATE`)%>% 
    dplyr::select(scientificName,eventDate,provenance,url)%>% 
    st_transform(ll)
)%>% 
  st_as_sf() %>% 
  st_transform(proj)

# Mapping
subregions <- nsws %>% 
  rename(subregion = RIVER)%>%
  dplyr::select(subregion)



AIS_subregion <- st_intersection(subregions,AIS)

AIS_summary <- AIS_subregion %>% 
  as.data.frame() %>% 
  group_by(subregion,scientificName) %>% 
  summarize(n=n()) %>% 
  left_join(subregions,by="subregion") %>% 
  ungroup() %>% 
  st_as_sf()

AIS_summary$scientificName %>% unique()

freshwater_sp<-AIS_summary%>%
  filter(scientificName %in% c("Micropterus dolomieu", "Cabomba caroliniana", "Cipangopaludina chinensis",
                               "Nymphoides peltata", "Esox niger", "Faxonius limosus", "Sphaerium corneum ", "Corbicula fluminea", "Carassius auratus", "Lythrum salicaria"))

species_names <- unique(freshwater_sp$scientificName)

species_plots <- list()

for (scientificName_ in species_names) { 
  species_plots[[scientificName_]] = ggplot(freshwater_sp%>% filter(scientificName == scientificName_))  +
    geom_sf()+
    geom_sf(data=subregions, fill='lightgrey',colour='black')+
    geom_sf(data=freshwater_sp%>% filter(scientificName == scientificName_), aes(fill=n))+
    labs(title = scientificName_)+
    theme_light()
  
  print(species_plots[[scientificName_]])
  ggsave(species_plots[[scientificName_]], file=paste0("plots/establishment_", scientificName_,".png"), dpi=300)
  
}

write.csv(AIS, "tempdata/AIS_data.csv")
