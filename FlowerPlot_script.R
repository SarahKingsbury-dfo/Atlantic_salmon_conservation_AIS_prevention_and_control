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
if(!require(geomtextpath)) devtools::install_github("AllanCameron/geomtextpath")
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

#code edited from: https://raw.githubusercontent.com/OHI-Science/arc/master/circle2016/plot_flower_local.R
#read-in summary of grid dataset
scores<-readRDS("plots/scores.rds")%>%
  as.data.frame()%>% 
  dplyr::select(-geometry) %>% 
  pivot_longer(cols=-c(river,g, mean_priority), names_to="metric", values_to = "value") %>% 
  #left_join(direction,by="metric") %>%
  mutate(value=replace_na(value,0),
         norm=value) %>% 
  group_by(metric) %>% 
  mutate(norm=((norm-min(norm))/max(norm-min(norm)))*100,
         score=norm,
         goal=metric) %>%
  group_by(river)


   
# labeling:: Index score for center labeling before join with conf
score_index <- scores %>%
  dplyr::select(river, score, g) %>%
  mutate(score = round(score))


# unique regions to plot
region_plots <- unique(scores$g)

# goals.csv configuration info----

# read in conf/goals.csv, start dealing with supra goals
conf <-  readr::read_csv("goals.csv")%>%
  mutate (name_supra=name)
goals_supra <- na.omit(unique(conf$parent))
supra_lookup <- conf %>%
  filter(goal %in% goals_supra) %>%
  dplyr::select(parent = goal, name_supra = name)

#goals_supra <- na.omit(unique(conf$parent))
## extract conf info for labeling
conf <- conf %>%
  dplyr::select(goal, order_color, order_hierarchy,
                weight, name_supra, name_flower, parent) %>%
  mutate(name_flower = gsub("\\n", "\n", name_flower, fixed = TRUE)) %>%
  arrange(order_hierarchy)

# join scores and conf ----
score_df <- scores %>%
  as.data.frame()%>%
  inner_join(conf, by="goal") %>%
  arrange(order_color)

# set up positions for the bar centers:
# cumulative sum of weights (incl current) minus half the current weight
score_df <- score_df %>%
  group_by(river) %>%
  mutate(pos   = sum(weight) - (cumsum(weight) - 0.5 * weight)) %>%
  mutate(pos_end = sum(weight)) %>%
  ungroup() %>%
  group_by(name_supra) %>%
  # calculate position of supra goals before any unequal weighting (ie for FP)
  mutate(pos_supra  = ifelse(!is.na(name_supra), mean(pos), NA)) %>%
  ungroup() %>%
  filter(weight != 0) %>%
  # set up for displaying NAs
  mutate(plot_NA = ifelse(is.na(score), 100, NA))


# create supra goal dataframe for position and labeling ----
supra <- score_df %>%
  mutate(parent = ifelse(is.na(parent), name_flower, parent)) %>%
  mutate(parent = paste0(parent, "\n")) %>%
  group_by(parent)%>%
  summarize(pos_supra=mean(pos))%>%
  ungroup()%>%
  dplyr::select(parent, pos_supra) %>%
  unique() %>%
  as.data.frame()

# calculate arc: stackoverflow.com/questions/38207390/making-curved-text-on-coord-polar ----
supra_df <- supra %>%
  #mutate(myAng = seq(-70, 250, length.out = dim(unique(supra))[1])) %>%
  mutate(myAng=seq(-20,340, length.out=4))%>%
  filter(!is.na(pos_supra))


# more labeling and parameters ----
goal_labels <- score_df %>%
  dplyr::select(goal, name_flower)


p_limits <- c(0, score_df$pos_end[1])
blank_circle_rad <- 42
light_line <- 'grey90'
white_fill <- 'white'
light_fill <- 'grey80'
med_line   <- 'grey50'
med_fill   <- 'grey52'
dark_line  <- 'grey20'
dark_fill  <- 'grey22'


# Mel's color palette ----
reds <-  grDevices::colorRampPalette(
  c("#FEE090","#FDAE61", "#F46D43","#D73027","#A50026"),
  space="Lab")(65)
blues <-  grDevices::colorRampPalette(
  c("#313695","#4575B4", "#74ADD1", "#ABD9E9", "#E0F3F8"))(35)
myPalette <-   c(blues, reds)

# filenaming for labeling and saving ----
region_names_all <- scores %>%
  as.data.frame()%>%
  dplyr::select(g, river)%>%
  mutate (region_id = as.character(g),
          region_name=as.character (river)) %>%
  mutate(flower_png = sprintf('%s/flower_%s.png',
                              "tempdata",
                              str_replace_all(region_name, ' ', '')))


#  write out filenames
readr::write_csv(region_names_all, 'tempdata/regions_figs.csv')
# 
#  move into for loop only with region_names to plot
region_names <- region_names_all %>%
  filter(g %in% region_plots) %>%  # filter only regions to plot
  distinct()                              # in case region_id 0 was included in regions_list.csv


# ggtheme_plot ----

ggtheme_plot <- function(base_size = 9) {
  theme(axis.ticks = element_blank(),
        text             = element_text(family = 'Helvetica', color = 'gray30', size = base_size),
        plot.title       = element_text(size = rel(3), hjust = 0.5, vjust=-2, face = 'bold'),
        panel.background = element_blank(),
        legend.position  = 'right',
        panel.border     = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = 'grey90', size = .25),
        # panel.grid.major = element_blank(),
        legend.key       = element_rect(colour = NA, fill = NA),
        axis.line        = element_blank()) # element_line(colour = "grey30", size = .5))
}

# loop through to save flower plot for each region ----
for (g in region_plots) { 
  

  # filter region info, setup to plot ----
  plot_df <- score_df [score_df$g==g,]
  plot_score_index <- score_index[score_index$g==g,] %>%
    #filter(river==scores$river[g])%>%
    as.data.frame()
  
  
  # fig_name to save
  fig_save <- region_names$flower_png[region_names$region_id==g]
  
  
  # labeling:: region name for title
  region_name <- region_names %>%
    dplyr::select(river)
  
  
  # recalculate pos with injected weights arrange by pos for proper ordering
  plot_df <- plot_df %>%
    mutate(pos = sum(weight) - (cumsum(weight) - 0.5 * weight)) %>%
    arrange(pos)
  
  
  
  # set up basic plot parameters ----
  plot_obj <- ggplot(data = plot_df,
                     aes(x = pos, y = score, fill = score, width = weight))
  
  # sets up the background/borders to the external boundary (100%) of plot
  plot_obj <- plot_obj +
    geom_bar(aes(y = 100),
             stat = 'identity', color = light_line, fill = white_fill, size = .2) +
    geom_errorbar(aes(x = pos, ymin = 100, ymax = 100, width = weight),
                  size = 0.5, color =light_line , show.legend = NA)
  
  # establish the basics of the flower plot
  plot_obj <- plot_obj +
    # plot the actual scores on top of background/borders:
    geom_bar(stat = 'identity', color = dark_line, size = .2)+
    # emphasize edge of petal
    geom_errorbar(aes(x = pos, ymin = score, ymax = score),
                  size = 0.5, color = dark_line, show.legend = NA) +
    # plot zero as a baseline:
    geom_errorbar(aes(x = pos, ymin = 0, ymax = 0),
                  size = 0.5, color = dark_line, show.legend = NA) +
    # turn linear bar chart into polar coordinates start at 90 degrees (pi*.5)
    coord_polar(start = pi * 0.5) +
    #set petal colors to the red-yellow-blue color scale:
    scale_fill_gradientn(colours=myPalette, na.value="black",
                         limits = c(0, 100)) +
    # use weights to assign widths to petals:
    scale_x_continuous(labels = plot_df$goal, 
      breaks = plot_df$pos, limits = p_limits) +
    scale_y_continuous(limits = c(-blank_circle_rad,
                                  ifelse(first(goal_labels == TRUE) |
                                           is.data.frame(goal_labels),
                                         150, 100)))
  
  # add center number and title
  
  plot_obj <- plot_obj +
    geom_text(data = scores[scores$g==g,],
              inherit.aes = FALSE,
              aes(label = round(mean_priority*100)),
              x = 0, y = -blank_circle_rad,
              hjust = .5, vjust = .5,
              size = 10,
              color = dark_line)+
    labs(title=score_df$river[g])
  
  # clean up the theme
  plot_obj <- plot_obj +
    ggtheme_plot() +
    theme(panel.grid.major = element_blank(),
          axis.line  = element_blank(),
          axis.text  = element_blank(),
          axis.title = element_blank())

  ## position supra arc and names. x is angle, y is distance from center
  supra_rad  <- 140 ## supra goal radius from center
  
  # add goal names
  plot_obj <- plot_obj +
    geom_text(aes(label = name_supra, 
                  x = pos, y = 120),
              hjust = .5, vjust = .5,
              size = 3,
              color = dark_line)
  
  
  # display/save options: print to graphics, save to file
  print(plot_obj)
  
  ggsave(filename = fig_save, plot = plot_obj, device = "png",
         height = 6, width = 8, units = 'in', dpi = 150)
  
}


