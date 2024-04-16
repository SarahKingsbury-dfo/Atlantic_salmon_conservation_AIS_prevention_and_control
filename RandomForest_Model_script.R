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
library(corrplot)
require(superheat)
library(visdat)
library(UpSetR)
library(naniar)       
require(tidyverse)

#setup geographic area
proj <- "+proj=lcc +lon_0=-63.1 +lat_1=43.8 +lat_2=46.9 +lat_0=45.3 +datum=WGS84 +units=m +no_defs"
ll <- "+proj=longlat +datum=WGS84"
sf::sf_use_s2(FALSE) # because of "Evaluation error: Found 1 feature with invalid spherical geometry." otherwise

atlprov <- readRDS("tempdata/atlprov.rds")
NS<- readRDS("tempdata/NS_polygon.rds")


ggplot(atlprov)+
  geom_sf()


#Addition of river classifications and structures for NB, NS, and PEI
#Must download the data to desktop as there is a multi-step process to accessing data. Can download data from: https://2c1forest.databasin.org/datasets/3fa5eb769b99496fad0c05c838c8823d/
#Data Citation: Millar, W., Olivero-Sheldon, A., Nussey, P. & Noseworthy, J. (2019). A Stream Classification for the Northern Appalachian–Acadian Region of Canada, Version 2.0. Fredericton, New Brunswick: Nature Conservancy of Canada, Atlantic Regional Office.

streams<-read_sf("tempdata/streams/commondata/stream_classification_v2/NCC_Stream_Classification_v2.shp")%>%
  st_transform(proj)
  
#Atlantic salmon survey data. Note: this data is internal to DFO and requires permission before publishing
raw_salmon<-st_read("tempdata/salmon/ALL_FINAL_SALMON_RIVERS_DO_NOT_SHARE.shp")%>%
  st_transform(proj)

salmon<-raw_salmon%>% 
  filter(Region %in% c("Nova Scotia", "Prince Edward Island", "New Brunswick"))%>%
  mutate(presence=case_when(STATUS_FIN=="Lost"~0,
                            STATUS_FIN=="Low Risk" ~1,
                            STATUS_FIN=="Moderate Risk"~1,
                            STATUS_FIN=="High Risk"~1,
                            STATUS_FIN=="Artificially Sustained"~1),
         presence=as.factor(presence))

#Overlapping salmon p/a with streams database grid

streams_grid<-st_join(streams, salmon, join=st_intersects, left=TRUE)
 
print(streams_grid$presence)

 ggplot()+
   geom_sf(data=streams_grid, mapping = aes(col=presence))+ 
   coord_sf()+
   theme_bw()

predSet<-streams_grid%>%
   dplyr::filter(is.na(presence))
 
trainSS<-streams_grid%>%
  dplyr::filter(presence !="NA")



#Random Forest Model of habitat suitability for salmon


names(trainSS)

trainSS<- trainSS%>%
  mutate(
    Hydro_ID =as.factor(Hydro_ID),
    gridcode =as.factor(gridcode),
    Size_Comp=as.factor(Size_Comp),
    Size_Simp=as.factor(Size_Simp),
    Grad_Simp=as.factor(Grad_Simp),
    Grad_Comp=as.factor(Grad_Comp),
    Alk=as.factor(Alk),
    Tidal=as.factor(Tidal),
    SimpCombo=as.factor(SimpCombo),
    IntmCombo=as.factor(IntmCombo),
    CmplxCombo=as.factor(CmplxCombo),
    Length_Km= as.numeric(Length_Km),
    Temp=as.factor(Temp),
    ID =as.factor(ID),
    name_db=as.factor( name_db),
    name_en_CG=as.factor(name_en_CG),
    name_fr_CG=as.factor(name_fr_CG),
    name_other=as.factor(name_other),
    NAME_LABEL=as.factor(NAME_LABEL),
    Country=as.factor(Country),
    Region=as.factor(Region),
    RiverName_=as.factor(RiverName_),
    AndersonSp=as.factor(AndersonSp),
    RiverName1=as.factor(RiverName1),
    Rivername2=as.factor(Rivername2),
    DISSOLVE=as.factor(DISSOLVE),
    GNIS_NAME_=as.factor(GNIS_NAME_),
    River_Name=as.factor(River_Name),
    Original_A=as.factor(Original_A),
    StockClass=as.factor(StockClass),
    StockClas1=as.factor(StockClas1),
    StockClas2=as.factor(StockClas2),
    STATUS_FIN=as.factor(STATUS_FIN),
    presence= as.factor(presence)
  )


predSet<- predSet%>%
  mutate(
    Hydro_ID =as.factor(Hydro_ID),
    gridcode =as.factor(gridcode),
    Size_Comp=as.factor(Size_Comp),
    Size_Simp=as.factor(Size_Simp),
    Grad_Simp=as.factor(Grad_Simp),
    Grad_Comp=as.factor(Grad_Comp),
    Alk=as.factor(Alk),
    Tidal=as.factor(Tidal),
    SimpCombo=as.factor(SimpCombo),
    IntmCombo=as.factor(IntmCombo),
    CmplxCombo=as.factor(CmplxCombo),
    Length_Km= as.numeric(Length_Km),
    Temp=as.factor(Temp),
    ID =as.factor(ID),
    name_db=as.factor( name_db),
    name_en_CG=as.factor(name_en_CG),
    name_fr_CG=as.factor(name_fr_CG),
    name_other=as.factor(name_other),
    NAME_LABEL=as.factor(NAME_LABEL),
    Country=as.factor(Country),
    Region=as.factor(Region),
    RiverName_=as.factor(RiverName_),
    AndersonSp=as.factor(AndersonSp),
    RiverName1=as.factor(RiverName1),
    Rivername2=as.factor(Rivername2),
    DISSOLVE=as.factor(DISSOLVE),
    GNIS_NAME_=as.factor(GNIS_NAME_),
    River_Name=as.factor(River_Name),
    Original_A=as.factor(Original_A),
    StockClass=as.factor(StockClass),
    StockClas1=as.factor(StockClas1),
    StockClas2=as.factor(StockClas2),
    STATUS_FIN=as.factor(STATUS_FIN),
    presence= as.factor(presence)
  )

colSums(is.na(trainSS))

colSums(is.na(predSet))

#set aside data from trainSS for training and another for testing.
#Split ratio 70:30 
set.seed(100)

sampleSet<-sample(nrow(trainSS), 0.7*nrow(trainSS), replace=FALSE)
TrainSet<- trainSS[sampleSet,]
TestSet<-trainSS[-sampleSet,]
#Create folder for plots and the like

dir.create("plots")

##Trying Simple Formula from NCC Data
#OOB=17.94%. Default tried mtry=2
model1<-randomForest(presence~Size_Simp+Grad_Simp+Alk+Length_Km+Temp+Tidal, data=TrainSet,
                     importance=TRUE, ntree=500)
model1

#attempting to imrpve error rate of model1 by changing the mtry. mtry=5 was the best for OOB
#16.4%
model2<-randomForest(presence~Size_Simp+Grad_Simp+Alk+Length_Km+Temp+Tidal, data=TrainSet,
                     importance=TRUE, ntree=500, mtry=3)
model2

#OOB=15.19%
model3<-randomForest(presence~Size_Simp+Grad_Simp+Alk+Length_Km+Temp+Tidal, data=TrainSet,
                     importance=TRUE, ntree=500, mtry=4)
model3

#OOB=13.7%
model4<-randomForest(presence~Size_Simp+Grad_Simp+Alk+Length_Km+Temp+Tidal, data=TrainSet,
                     importance=TRUE, ntree=500, mtry=5)
model4

#OOB=14.18%
model5<-randomForest(presence~Size_Simp+Grad_Simp+Alk+Length_Km+Temp+Tidal, data=TrainSet,
                     importance=TRUE, ntree=500, mtry=6)
model5

#checking valiable importance

png(filename=paste0("plots/model4_plot.png"))
varImpPlot(model4)
dev.off()


t4<-importance(model4)
write.csv(t4, file="plots/model4_table.csv",row.names = TRUE)
##Try Complex data from NCC
#OOB=16.92%
model6<-randomForest(presence~Size_Comp+Grad_Comp+Alk+Length_Km+Temp+Tidal, data=TrainSet,
                     importance=TRUE, ntree=500)
model6

#OOB=15.43%
model7<-randomForest(presence~Size_Comp+Grad_Comp+Alk+Length_Km+Temp+Tidal, data=TrainSet,
                     importance=TRUE, ntree=500, mtry=3)
model7

#OOB=13.91%
model8<-randomForest(presence~Size_Comp+Grad_Comp+Alk+Length_Km+Temp+Tidal, data=TrainSet,
                     importance=TRUE, ntree=500, mtry=4)
model8

#OOB=13.22%
model9<-randomForest(presence~Size_Comp+Grad_Comp+Alk+Length_Km+Temp+Tidal, data=TrainSet,
                     importance=TRUE, ntree=500, mtry=5)
model9

#OOB=13.74%
model10<-randomForest(presence~Size_Comp+Grad_Comp+Alk+Length_Km+Temp+Tidal, data=TrainSet,
                      importance=TRUE, ntree=500, mtry=6)
model10

#OOB 13.46%
model9a<-randomForest(presence~Size_Simp+Grad_Comp+Alk+Length_Km+Temp+Tidal, data=TrainSet,
                      importance=TRUE, ntree=500, mtry=5)
model9a

#OOB 13.24%
model9b<-randomForest(presence~Size_Comp+Grad_Simp+Alk+Length_Km+Temp+Tidal, data=TrainSet,
                      importance=TRUE, ntree=500, mtry=5)
model9b

png(filename=paste0("plots/model9_plot.png"))
varImpPlot(model9)
dev.off()


t9<-importance(model9)
write.csv(t9, file="plots/model9_table.csv",row.names = TRUE)


#validation step
#model1
predValid1<-predict(model1, TestSet)

mean(predValid1==TestSet$presence)

table(predValid1, TestSet$presence)

#model2
predValid2<-predict(model2, TestSet)

mean(predValid2==TestSet$presence)

table(predValid2, TestSet$presence)

#model3
predValid3<-predict(model3, TestSet)

mean(predValid3==TestSet$presence)

table(predValid3, TestSet$presence)

#model4
predValid4<-predict(model4, TestSet)

mean(predValid4==TestSet$presence)

table(predValid4, TestSet$presence)

#model5
predValid5<-predict(model5, TestSet)

mean(predValid5==TestSet$presence)

table(predValid5, TestSet$presence)

#model6
predValid6<-predict(model6, TestSet)

mean(predValid6==TestSet$presence)

table(predValid6, TestSet$presence)

#model7
predValid7<-predict(model7, TestSet)

mean(predValid7==TestSet$presence)

table(predValid7, TestSet$presence)

#model8
predValid8<-predict(model8, TestSet)

mean(predValid8==TestSet$presence)

table(predValid8, TestSet$presence)

#model9
predValid9<-predict(model9, TestSet)

mean(predValid9==TestSet$presence)

table(predValid9, TestSet$presence)

#model9a
predValid9a<-predict(model9a, TestSet)

mean(predValid9a==TestSet$presence)

table(predValid9a, TestSet$presence)

#model9b
predValid9b<-predict(model9b, TestSet)

mean(predValid9b==TestSet$presence)

table(predValid9b, TestSet$presence)

#model10
predValid10<-predict(model10, TestSet)

mean(predValid10==TestSet$presence)

table(predValid10, TestSet$presence)

#making prediction on remaining data

predSet<-rbind(trainSS[1,], predSet)
predSet<-predSet[-1,]

#model4 predictions
pred1<-predict(model4, newdata=predSet, type='prob')[,2]
pred1
plot(pred1)

write.csv(pred1, file="plots/model4_predictions.csv", row.names = TRUE)

#trying out model9 on the predication dataset
pred2<-predict(model9, newdata=predSet, type='prob')[,2]
pred2<-pred2%>%
  as.data.frame()
pred2$row_num<-seq.int(nrow(pred2))
predSet$row_num<-seq.int(nrow(predSet))

output<-left_join(predSet, pred2, by="row_num")

#writing a data table for the model9 predictions and mapping those
write.csv(output, "plots/model9_predictions.csv", row.names=TRUE)

names(output)[names(output)=='.']<-'predicted_presence'



png(filename = paste0("plots/model9_predictions_map.png"))

ggplot(output)+
  geom_sf(aes(col=predicted_presence))+
  scale_colour_gradient2(low="red", midpoint = 0.50, mid="white", high="blue", space = "Lab")+
  ggtitle("Predicted Suitable Habitat for Salmo salar based on Environmentals Only")+
  theme(panel.grid.major = element_line(color = gray(0.5), linetype = "dashed", 
                                        size = 0.5), panel.background = element_rect(fill = "aliceblue"))
dev.off()


#pressures: layers must be downloaded from NCC's website at: https://2c1forest.databasin.org/datasets/339f63ca00bf4e86aa1563d25de1185d/
#Citation for layer: Millar, W., Noseworthy, J. & Nussey, P. (2019). A Watershed Health Assessment for the Northern Appalachian–Acadian Region of Canada. Nature Conservancy of Canada. Atlantic Regional Office. Fredericton, New Brunswick.

pressures<- read_sf("tempdata/pressures/commondata/watershed_health_assessment/NCC_Watershed_Health_Assessment_2019.shp")%>%
  mutate(CHU_12_ID=as.character(CHUC_12_ID))%>%
  st_transform(proj)

raw_pressures<-read_csv("tempdata/pressures/NCC_WHA_Predictor_Variables.csv")%>%
  mutate(CHUC_12_ID=paste0("0", CHU_12_ID),
         CHU_12_ID=as.character(CHU_12_ID)
         #CHUC_12_ID=as.character(CHUC_12_ID)
         )

pressures<-pressures%>%
  left_join(., raw_pressures, by="CHUC_12_ID")
  #left_join(pressures, raw_pressures, by="CHUC_12_ID")

colSums(is.na(pressures))

#joining the pressures layer to the streams_grid to generate a dataset based on both ecosystems pressures and environmental characteristics
pressures_grid<-st_join(streams_grid, pressures, join=st_intersects, left=TRUE)

#Now splitting pressures_grid into training/validation set and prediction set

#removing rows with missing data, especially if missing climate velocity data
pres_trainSS<-pressures_grid%>%
  dplyr::filter(presence !="NA" & Clim_Veloc !="NA")

ggplot(atlprov)+
  geom_sf()+
  geom_sf(data=pres_trainSS, aes(col=presence))+
  theme_light()

pres_predSet<-pressures_grid%>%
  dplyr::filter(is.na(presence) & Clim_Veloc !="NA")

colSums(is.na(pres_trainSS))
colSums(is.na(pres_predSet))

#mutating the dataset for use in models

pres_trainSS<- pres_trainSS%>%
  mutate(
    Hydro_ID =as.factor(Hydro_ID),
    gridcode =as.factor(gridcode),
    Size_Comp=as.factor(Size_Comp),
    Size_Simp=as.factor(Size_Simp),
    Grad_Simp=as.factor(Grad_Simp),
    Grad_Comp=as.factor(Grad_Comp),
    Alk=as.factor(Alk),
    Tidal=as.factor(Tidal),
    SimpCombo=as.factor(SimpCombo),
    IntmCombo=as.factor(IntmCombo),
    CmplxCombo=as.factor(CmplxCombo),
    Length_Km= as.numeric(Length_Km),
    Temp=as.factor(Temp),
    ID.y =as.factor(ID.y),
    name_db=as.factor( name_db),
    name_en_CG=as.factor(name_en_CG),
    name_fr_CG=as.factor(name_fr_CG),
    name_other=as.factor(name_other),
    NAME_LABEL=as.factor(NAME_LABEL),
    Country=as.factor(Country),
    Region=as.factor(Region),
    RiverName_=as.factor(RiverName_),
    AndersonSp=as.factor(AndersonSp),
    RiverName1=as.factor(RiverName1),
    Rivername2=as.factor(Rivername2),
    DISSOLVE=as.factor(DISSOLVE),
    GNIS_NAME_=as.factor(GNIS_NAME_),
    River_Name=as.factor(River_Name),
    Original_A=as.factor(Original_A),
    StockClass=as.factor(StockClass),
    StockClas1=as.factor(StockClas1),
    StockClas2=as.factor(StockClas2),
    STATUS_FIN=as.factor(STATUS_FIN),
    presence= as.factor(presence),
    N_TotRd_De=as.numeric(N_TotRd_De),
    N_PRd_Den_=as.numeric(N_PRd_Den_),
    N_UPRd_Den=as.numeric(N_UPRd_Den),
    N_CL_EXC_N=as.numeric(N_CL_EXC_N),
    N_Xing_Den=as.numeric(N_Xing_Den),
    N_NN_Fish_=as.numeric(N_NN_Fish_),
    M_AqBarrie=as.numeric(M_AqBarrie),
    N_Pct_Urba=as.numeric(N_Pct_Urba),
    N_Pct_Crop=as.numeric(N_Pct_Crop),
    N_Pct_Cut_=as.numeric(N_Pct_Cut_),
    N_Pct_Past=as.numeric(N_Pct_Past),
    N_Pct_Ag_N=as.numeric(N_Pct_Ag_N),
    N_Pct_Impv=as.numeric(N_Pct_Impv),
    N_Pop_Den_=as.numeric(N_Pop_Den_),
    N_N_Leach_=as.numeric(N_N_Leach_),
    N_P_Leach_=as.numeric(N_P_Leach_),
    N_PSTCD_Le=as.numeric(N_PSTCD_Le),
    N_Clim_Vel=as.numeric(N_Clim_Vel),
    N_PSP_Sour=as.numeric(N_PSP_Sour),
    N_PSP_Meta=as.numeric(N_PSP_Meta),
    N_PSP_Nutr=as.numeric(N_PSP_Nutr),
    N_PSP_Orga=as.numeric(N_PSP_Orga),
    N_TDiff_Le=as.numeric(N_TDiff_Le),
    Area_SqKm.x=as.numeric(Area_SqKm.x)
  )

pres_predSet<- pres_predSet%>%
  mutate(
    Hydro_ID =as.factor(Hydro_ID),
    gridcode =as.factor(gridcode),
    Size_Comp=as.factor(Size_Comp),
    Size_Simp=as.factor(Size_Simp),
    Grad_Simp=as.factor(Grad_Simp),
    Grad_Comp=as.factor(Grad_Comp),
    Alk=as.factor(Alk),
    Tidal=as.factor(Tidal),
    SimpCombo=as.factor(SimpCombo),
    IntmCombo=as.factor(IntmCombo),
    CmplxCombo=as.factor(CmplxCombo),
    Length_Km= as.numeric(Length_Km),
    Temp=as.factor(Temp),
    ID.y =as.factor(ID.y),
    name_db=as.factor( name_db),
    name_en_CG=as.factor(name_en_CG),
    name_fr_CG=as.factor(name_fr_CG),
    name_other=as.factor(name_other),
    NAME_LABEL=as.factor(NAME_LABEL),
    Country=as.factor(Country),
    Region=as.factor(Region),
    RiverName_=as.factor(RiverName_),
    AndersonSp=as.factor(AndersonSp),
    RiverName1=as.factor(RiverName1),
    Rivername2=as.factor(Rivername2),
    DISSOLVE=as.factor(DISSOLVE),
    GNIS_NAME_=as.factor(GNIS_NAME_),
    River_Name=as.factor(River_Name),
    Original_A=as.factor(Original_A),
    StockClass=as.factor(StockClass),
    StockClas1=as.factor(StockClas1),
    StockClas2=as.factor(StockClas2),
    STATUS_FIN=as.factor(STATUS_FIN),
    presence= as.factor(presence),
    N_TotRd_De=as.numeric(N_TotRd_De),
    N_PRd_Den_=as.numeric(N_PRd_Den_),
    N_UPRd_Den=as.numeric(N_UPRd_Den),
    N_CL_EXC_N=as.numeric(N_CL_EXC_N),
    N_Xing_Den=as.numeric(N_Xing_Den),
    N_NN_Fish_=as.numeric(N_NN_Fish_),
    M_AqBarrie=as.numeric(M_AqBarrie),
    N_Pct_Urba=as.numeric(N_Pct_Urba),
    N_Pct_Crop=as.numeric(N_Pct_Crop),
    N_Pct_Cut_=as.numeric(N_Pct_Cut_),
    N_Pct_Past=as.numeric(N_Pct_Past),
    N_Pct_Ag_N=as.numeric(N_Pct_Ag_N),
    N_Pct_Impv=as.numeric(N_Pct_Impv),
    N_Pop_Den_=as.numeric(N_Pop_Den_),
    N_N_Leach_=as.numeric(N_N_Leach_),
    N_P_Leach_=as.numeric(N_P_Leach_),
    N_PSTCD_Le=as.numeric(N_PSTCD_Le),
    N_Clim_Vel=as.numeric(N_Clim_Vel),
    N_PSP_Sour=as.numeric(N_PSP_Sour),
    N_PSP_Meta=as.numeric(N_PSP_Meta),
    N_PSP_Nutr=as.numeric(N_PSP_Nutr),
    N_PSP_Orga=as.numeric(N_PSP_Orga),
    N_TDiff_Le=as.numeric(N_TDiff_Le),
    Area_SqKm.x=as.numeric(Area_SqKm.x)
  )
#set aside data from trainSS for training and another for testing.
#Split ratio 70:30 
set.seed(100)

sampleSet_pres<-sample(nrow(pres_trainSS), 0.7*nrow(pres_trainSS), replace=FALSE)
TrainSet_pres<- pres_trainSS[sampleSet_pres,]
TestSet_pres<-pres_trainSS[-sampleSet_pres,]

#Now we try a model and see what happens. :)

#OOB 0.42%
model11<-randomForest(presence~Size_Simp+
                        Grad_Simp+
                        Alk+
                        Length_Km+
                        Temp+
                        Tidal+
                        N_TotRd_De+
                        N_PRd_Den_+
                        N_UPRd_Den+
                        N_CL_EXC_N+
                        N_Xing_Den+
                        N_NN_Fish_+
                        M_AqBarrie+
                        N_Pct_Urba+
                        N_Pct_Crop+
                        N_Pct_Cut_+
                        N_Pct_Past+
                        N_Pct_Ag_N+
                        N_Pct_Impv+
                        N_Pop_Den_+
                        N_N_Leach_+
                        N_P_Leach_+
                        N_PSTCD_Le+
                        N_Clim_Vel+
                        N_PSP_Sour+
                        N_TDiff_Le+
                        Area_SqKm.x+
                        N_PSP_Meta+
                        N_PSP_Nutr+
                        N_PSP_Orga,
                      data=TrainSet_pres,
                     importance=TRUE, ntree=500)
model11


png(filename=paste0("plots/model11_plot.png"))
varImpPlot(model11)
dev.off()


t11<-importance(model11)
write.csv(t11, file="plots/model11_table.csv",row.names = TRUE)

#OOB 0.39%
model12<-randomForest(presence~Size_Simp+
                        Grad_Simp+
                        Alk+
                        Length_Km+
                        Temp+
                        Tidal+
                        N_TotRd_De+
                        N_PRd_Den_+
                        N_UPRd_Den+
                        N_CL_EXC_N+
                        N_Xing_Den+
                        N_NN_Fish_+
                        M_AqBarrie+
                        N_Pct_Urba+
                        N_Pct_Crop+
                        N_Pct_Cut_+
                        N_Pct_Past+
                        N_Pct_Ag_N+
                        N_Pct_Impv+
                        N_Pop_Den_+
                        N_N_Leach_+
                        N_P_Leach_+
                        N_PSTCD_Le+
                        N_Clim_Vel+
                        N_TDiff_Le+
                        Area_SqKm.x,
                      data=TrainSet_pres,
                      importance=TRUE, ntree=500)
model12

varImpPlot(model12)
importance(model12)

#OOB 0.41%
model13<-randomForest(presence~Size_Simp+
                        Grad_Simp+
                        Alk+
                        Length_Km+
                        Temp+
                        Tidal+
                        N_TotRd_De+
                        N_PRd_Den_+
                        N_UPRd_Den+
                        N_CL_EXC_N+
                        N_Xing_Den+
                        N_NN_Fish_+
                        M_AqBarrie+
                        N_Pct_Urba+
                        N_Pct_Crop+
                        N_Pct_Cut_+
                        N_Pct_Past+
                        N_Pct_Ag_N+
                        N_Pct_Impv+
                        N_Pop_Den_+
                        N_N_Leach_+
                        N_P_Leach_+
                        N_PSTCD_Le+
                        N_Clim_Vel+
                        N_PSP_Sour+
                        N_TDiff_Le+
                        Area_SqKm.x+
                        N_PSP_Meta+
                        N_PSP_Nutr+
                        N_PSP_Orga,
                      data=TrainSet_pres,
                      importance=TRUE, ntree=500, mtry=6)
model13
png(filename=paste0("plots/model13_plot.png"))
varImpPlot(model13)
dev.off()


t13<-importance(model13)
write.csv(t13, file="plots/model13_table.csv",row.names = TRUE)

#OOB 0.39%
model14<-randomForest(presence~Size_Simp+
                        Grad_Simp+
                        Alk+
                        Length_Km+
                        Temp+
                        Tidal+
                        N_TotRd_De+
                        N_PRd_Den_+
                        N_UPRd_Den+
                        N_CL_EXC_N+
                        N_Xing_Den+
                        N_NN_Fish_+
                        M_AqBarrie+
                        N_Pct_Urba+
                        N_Pct_Crop+
                        N_Pct_Cut_+
                        N_Pct_Past+
                        N_Pct_Ag_N+
                        N_Pct_Impv+
                        N_Pop_Den_+
                        N_N_Leach_+
                        N_P_Leach_+
                        N_PSTCD_Le+
                        N_Clim_Vel+
                        N_PSP_Sour+
                        N_TDiff_Le+
                        Area_SqKm.x+
                        N_PSP_Meta+
                        N_PSP_Nutr+
                        N_PSP_Orga,
                      data=TrainSet_pres,
                      importance=TRUE, ntree=500, mtry=10)
model14



#OOB 0.38%
model14a<-randomForest(presence~Size_Comp+
                        Grad_Comp+
                        Alk+
                        Length_Km+
                        Temp+
                        Tidal+
                        N_TotRd_De+
                        N_PRd_Den_+
                        N_UPRd_Den+
                        N_CL_EXC_N+
                        N_Xing_Den+
                        N_NN_Fish_+
                        M_AqBarrie+
                        N_Pct_Urba+
                        N_Pct_Crop+
                        N_Pct_Cut_+
                        N_Pct_Past+
                        N_Pct_Ag_N+
                        N_Pct_Impv+
                        N_Pop_Den_+
                        N_N_Leach_+
                        N_P_Leach_+
                        N_PSTCD_Le+
                        N_Clim_Vel+
                        N_PSP_Sour+
                        N_TDiff_Le+
                        Area_SqKm.x+
                        N_PSP_Meta+
                        N_PSP_Nutr+
                        N_PSP_Orga,
                      data=TrainSet_pres,
                      importance=TRUE, ntree=500, mtry=10)
model14a

png(filename=paste0("plots/model14a_plot.png"))
varImpPlot(model14a)
dev.off()
#OOB 0.4%
model15<-randomForest(presence~Size_Simp+
                        Grad_Simp+
                        Alk+
                        Length_Km+
                        Temp+
                        Tidal+
                        N_TotRd_De+
                        N_PRd_Den_+
                        N_UPRd_Den+
                        N_CL_EXC_N+
                        N_Xing_Den+
                        N_NN_Fish_+
                        M_AqBarrie+
                        N_Pct_Urba+
                        N_Pct_Crop+
                        N_Pct_Cut_+
                        N_Pct_Past+
                        N_Pct_Ag_N+
                        N_Pct_Impv+
                        N_Pop_Den_+
                        N_N_Leach_+
                        N_P_Leach_+
                        N_PSTCD_Le+
                        N_Clim_Vel+
                        N_PSP_Sour+
                        N_TDiff_Le+
                        Area_SqKm.x+
                        N_PSP_Meta+
                        N_PSP_Nutr+
                        N_PSP_Orga,
                      data=TrainSet_pres,
                      importance=TRUE, ntree=500, mtry=8)
model15

#OOB 0.41%
model16<-randomForest(presence~Size_Simp+
                        Grad_Simp+
                        Alk+
                        Length_Km+
                        Temp+
                        Tidal+
                        N_TotRd_De+
                        N_PRd_Den_+
                        N_UPRd_Den+
                        N_CL_EXC_N+
                        N_Xing_Den+
                        N_NN_Fish_+
                        M_AqBarrie+
                        N_Pct_Urba+
                        N_Pct_Crop+
                        N_Pct_Cut_+
                        N_Pct_Past+
                        N_Pct_Ag_N+
                        N_Pct_Impv+
                        N_Pop_Den_+
                        N_N_Leach_+
                        N_P_Leach_+
                        N_PSTCD_Le+
                        N_Clim_Vel+
                        N_PSP_Sour+
                        N_TDiff_Le+
                        Area_SqKm.x+
                        N_PSP_Meta+
                        N_PSP_Nutr+
                        N_PSP_Orga,
                      data=TrainSet_pres,
                      importance=TRUE, ntree=500, mtry=9)
model16

#OOB 0.41%
model17<-randomForest(presence~Size_Simp+
                        Grad_Simp+
                        Alk+
                        Length_Km+
                        Temp+
                        Tidal+
                        N_TotRd_De+
                        N_PRd_Den_+
                        N_UPRd_Den+
                        N_CL_EXC_N+
                        N_Xing_Den+
                        N_NN_Fish_+
                        M_AqBarrie+
                        N_Pct_Urba+
                        N_Pct_Crop+
                        N_Pct_Cut_+
                        N_Pct_Past+
                        N_Pct_Ag_N+
                        N_Pct_Impv+
                        N_Pop_Den_+
                        N_N_Leach_+
                        N_P_Leach_+
                        N_PSTCD_Le+
                        N_Clim_Vel+
                        N_PSP_Sour+
                        N_TDiff_Le+
                        Area_SqKm.x+
                        N_PSP_Meta+
                        N_PSP_Nutr+
                        N_PSP_Orga,
                      data=TrainSet_pres,
                      importance=TRUE, ntree=500, mtry=12)
model17

t14<-importance(model14)
write.csv(t14, file="plots/model14_table.csv",row.names = TRUE)

#Validation of models that include pressures
#model11
predValid11<-predict(model11, TestSet_pres)

mean(predValid11==TestSet_pres$presence)

table(predValid11, TestSet_pres$presence)

#model12
predValid12<-predict(model12, TestSet_pres)

mean(predValid12==TestSet_pres$presence)

table(predValid12, TestSet_pres$presence)

#model13
predValid13<-predict(model13, TestSet_pres)

mean(predValid13==TestSet_pres$presence)

table(predValid13, TestSet_pres$presence)

#model14
predValid14<-predict(model14, TestSet_pres)

mean(predValid14==TestSet_pres$presence)

table(predValid14, TestSet_pres$presence)

#model14a
predValid14a<-predict(model14a, TestSet_pres)

mean(predValid14a==TestSet_pres$presence)

table(predValid14a, TestSet_pres$presence)

#model15
predValid15<-predict(model15, TestSet_pres)

mean(predValid15==TestSet_pres$presence)

table(predValid15, TestSet_pres$presence)

#model16
predValid16<-predict(model16, TestSet_pres)

mean(predValid16==TestSet_pres$presence)

table(predValid16, TestSet_pres$presence)

#model17
predValid17<-predict(model17, TestSet_pres)

mean(predValid17==TestSet_pres$presence)

table(predValid17, TestSet_pres$presence)

#trying out model11 on the predication dataset
pred3<-predict(model11, newdata=pres_predSet, type='prob')[,2]
pred3<-pred3%>%
  as.data.frame()
pred3$row_num<-seq.int(nrow(pred3))
pres_predSet$row_num<-seq.int(nrow(pres_predSet))

pres_output<-left_join(pres_predSet, pred3, by="row_num")

#writing a data table for the model11 predictions and mapping those
write.csv(pres_output, "plots/model11_predictions.csv", row.names=TRUE)

names(pres_output)[names(pres_output)=='.']<-'predicted_suitable'


png(filename = paste0("plots/model11_predictions_map.png"))

ggplot(pres_output)+
  geom_sf(aes(col=predicted_presence))+
  scale_colour_gradient2(low="red", midpoint = 0.50, mid="white", high="blue", space = "Lab")+
  ggtitle("Predicted Suitable Habitat for Salmo salar based on Environmental Characteristics and Habitat Pressures")+
  theme(panel.grid.major = element_line(color = gray(0.5), linetype = "dashed", 
                                        size = 0.5), panel.background = element_rect(fill = "aliceblue"))
dev.off()

#trying out model14 on the predication dataset
pred4<-predict(model14a, newdata=pres_predSet, type='prob')[,2]
pred4<-pred4%>%
  as.data.frame()
pred4$row_num<-seq.int(nrow(pred4))
pres_predSet$row_num<-seq.int(nrow(pres_predSet))

pres_output2<-left_join(pres_predSet, pred4, by="row_num")

names(pres_output2)[names(pres_output2)=='.']<-'predicted_suitable'

#writing a data table for the model11 predictions and mapping those
write.csv(pres_output2, "plots/model14a_predictions.csv", row.names=TRUE)
saveRDS(pres_output2, "plots/model14a_predictions.rds")

#st_write(pres_output2, "plots/model14_shapefile.shp")




png(filename = paste0("plots/model14a_predictions_map.png"))

ggplot(pres_output2)+
  geom_sf(aes(col=predicted_suitable))+
  scale_colour_gradient2(low="red", midpoint = 0.50, mid="white", high="blue", space = "Lab")+
  ggtitle("Predicted Suitable Habitat for Salmo salar based on \nEnvironmental Characteristics and Habitat Pressures")+
  theme(panel.grid.major = element_line(color = gray(0.5), linetype = "dashed", 
                                        size = 0.5), panel.background = element_rect(fill = "aliceblue"))
dev.off()
#Checking Model sensitivities and accuracy

#model1
check1=predict(model1, TrainSet, type="prob")[,2]
perf1<-prediction(check1, TrainSet$presence)
roc.perf1=performance(perf1, measure = "tpr", x.measure = "fpr")

png(filename = paste0("plots/model1_roc.plot.png"))
plot(roc.perf1)
abline(a=0,b=1)
dev.off()

auc.perf1=performance(perf1, measure = "auc")
auc.perf1@y.values

#model2
check2=predict(model2, TrainSet, type="prob")[,2]
perf2<-prediction(check2, TrainSet$presence)
roc.perf2=performance(perf2, measure = "tpr", x.measure = "fpr")

png(filename = paste0("plots/model2_roc.plot.png"))
plot(roc.perf2)
abline(a=0,b=1)
dev.off()

auc.perf2=performance(perf2, measure = "auc")
auc.perf2@y.values

#model3
check3=predict(model3, TrainSet, type="prob")[,2]
perf3<-prediction(check3, TrainSet$presence)
roc.perf3=performance(perf3, measure = "tpr", x.measure = "fpr")

png(filename = paste0("plots/model3_roc.plot.png"))
plot(roc.perf3)
abline(a=0,b=1)
dev.off()

auc.perf3=performance(perf3, measure = "auc")
auc.perf3@y.values

#model4
check4=predict(model4, TrainSet, type="prob")[,2]
perf4<-prediction(check4, TrainSet$presence)
roc.perf4=performance(perf4, measure = "tpr", x.measure = "fpr")

png(filename = paste0("plots/model4_roc.plot.png"))
plot(roc.perf4)
abline(a=0,b=1)
dev.off()

auc.perf4=performance(perf4, measure = "auc")
auc.perf4@y.values

#model5
check5=predict(model5, TrainSet, type="prob")[,2]
perf5<-prediction(check5, TrainSet$presence)
roc.perf5=performance(perf5, measure = "tpr", x.measure = "fpr")

png(filename = paste0("plots/model5_roc.plot.png"))
plot(roc.perf5)
abline(a=0,b=1)
dev.off()

auc.perf5=performance(perf5, measure = "auc")
auc.perf5@y.values

#model6
check6=predict(model6, TrainSet, type="prob")[,2]
perf6<-prediction(check6, TrainSet$presence)
roc.perf6=performance(perf6, measure = "tpr", x.measure = "fpr")

png(filename = paste0("plots/model6_roc.plot.png"))
plot(roc.perf6)
abline(a=0,b=1)
dev.off()

auc.perf6=performance(perf6, measure = "auc")
auc.perf6@y.values

#model7
check7=predict(model7, TrainSet, type="prob")[,2]
perf7<-prediction(check7, TrainSet$presence)
roc.perf7=performance(perf7, measure = "tpr", x.measure = "fpr")

png(filename = paste0("plots/model7_roc.plot.png"))
plot(roc.perf7)
abline(a=0,b=1)
dev.off()

auc.perf7=performance(perf7, measure = "auc")
auc.perf7@y.values

#model8
check8=predict(model8, TrainSet, type="prob")[,2]
perf8<-prediction(check8, TrainSet$presence)
roc.perf8=performance(perf8, measure = "tpr", x.measure = "fpr")

png(filename = paste0("plots/model8_roc.plot.png"))
plot(roc.perf8)
abline(a=0,b=1)
dev.off()

auc.perf8=performance(perf8, measure = "auc")
auc.perf8@y.values

#model9
check9=predict(model9, TrainSet, type="prob")[,2]
perf9<-prediction(check9, TrainSet$presence)
roc.perf9=performance(perf9, measure = "tpr", x.measure = "fpr")

png(filename = paste0("plots/model9_roc.plot.png"))
plot(roc.perf9)
abline(a=0,b=1)
dev.off()

auc.perf9=performance(perf9, measure = "auc")
auc.perf9@y.values

#model9a
check9a=predict(model9a, TrainSet, type="prob")[,2]
perf9a<-prediction(check9a, TrainSet$presence)
roc.perf9a=performance(perf9a, measure = "tpr", x.measure = "fpr")

png(filename = paste0("plots/model9a_roc.plot.png"))
plot(roc.perf9a)
abline(a=0,b=1)
dev.off()

auc.perf9a=performance(perf9a, measure = "auc")
auc.perf9a@y.values

#model9b
check9b=predict(model9b, TrainSet, type="prob")[,2]
perf9b<-prediction(check9b, TrainSet$presence)
roc.perf9b=performance(perf9b, measure = "tpr", x.measure = "fpr")

png(filename = paste0("plots/model9b_roc.plot.png"))
plot(roc.perf9b)
abline(a=0,b=1)
dev.off()

auc.perf9b=performance(perf9b, measure = "auc")
auc.perf9b@y.values

#model10
check10=predict(model10, TrainSet, type="prob")[,2]
perf10<-prediction(check10, TrainSet$presence)
roc.perf10=performance(perf10, measure = "tpr", x.measure = "fpr")

png(filename = paste0("plots/model10_roc.plot.png"))
plot(roc.perf10)
abline(a=0,b=1)
dev.off()

auc.perf10=performance(perf10, measure = "auc")
auc.perf10@y.values

#model11
check11=predict(model11, TrainSet_pres, type="prob")[,2]
perf11<-prediction(check11, TrainSet_pres$presence)
roc.perf11=performance(perf11, measure = "tpr", x.measure = "fpr")

png(filename = paste0("plots/model11_roc.plot.png"))
plot(roc.perf11)
abline(a=0,b=1)
dev.off()

auc.perf11=performance(perf11, measure = "auc")
auc.perf11@y.values

#model12
check12=predict(model12, TrainSet_pres, type="prob")[,2]
perf12<-prediction(check12, TrainSet_pres$presence)
roc.perf12=performance(perf12, measure = "tpr", x.measure = "fpr")

png(filename = paste0("plots/model12_roc.plot.png"))
plot(roc.perf12)
abline(a=0,b=1)
dev.off()

auc.perf12=performance(perf12, measure = "auc")
auc.perf12@y.values

#model13
check13=predict(model13, TrainSet_pres, type="prob")[,2]
perf13<-prediction(check13, TrainSet_pres$presence)
roc.perf13=performance(perf13, measure = "tpr", x.measure = "fpr")

png(filename = paste0("plots/model13_roc.plot.png"))
plot(roc.perf13)
abline(a=0,b=1)
dev.off()

auc.perf13=performance(perf13, measure = "auc")
auc.perf13@y.values

#model14
check14=predict(model14, TrainSet_pres, type="prob")[,2]
perf14<-prediction(check14, TrainSet_pres$presence)
roc.perf14=performance(perf14, measure = "tpr", x.measure = "fpr")

png(filename = paste0("plots/model14_roc.plot.png"))
plot(roc.perf14)
abline(a=0,b=1)
dev.off()

auc.perf14=performance(perf14, measure = "auc")
auc.perf14@y.values

#model14a
check14a=predict(model14a, TrainSet_pres, type="prob")[,2]
perf14a<-prediction(check14a, TrainSet_pres$presence)
roc.perf14a=performance(perf14a, measure = "tpr", x.measure = "fpr")

png(filename = paste0("plots/model14a_roc.plot.png"))
plot(roc.perf14a)
abline(a=0,b=1)
dev.off()

auc.perf14a=performance(perf14a, measure = "auc")
auc.perf14a@y.values

#model15
check15=predict(model15, TrainSet_pres, type="prob")[,2]
perf15<-prediction(check15, TrainSet_pres$presence)
roc.perf15=performance(perf15, measure = "tpr", x.measure = "fpr")

png(filename = paste0("plots/model15_roc.plot.png"))
plot(roc.perf15)
abline(a=0,b=1)
dev.off()

auc.perf15=performance(perf15, measure = "auc")
auc.perf15@y.values

#model16
check16=predict(model16, TrainSet_pres, type="prob")[,2]
perf16<-prediction(check16, TrainSet_pres$presence)
roc.perf16=performance(perf16, measure = "tpr", x.measure = "fpr")

png(filename = paste0("plots/model16_roc.plot.png"))
plot(roc.perf16)
abline(a=0,b=1)
dev.off()

auc.perf16=performance(perf16, measure = "auc")
auc.perf16@y.values

#model17
check17=predict(model17, TrainSet_pres, type="prob")[,2]
perf17<-prediction(check17, TrainSet_pres$presence)
roc.perf17=performance(perf17, measure = "tpr", x.measure = "fpr")

png(filename = paste0("plots/model17_roc.plot.png"))
plot(roc.perf17)
abline(a=0,b=1)
dev.off()

auc.perf17=performance(perf17, measure = "auc")
auc.perf17@y.values

#A new take on the RFM using CABIN + salmon + pressures

###CABIN habitat data for Maritimes Region###
#there seems to be an issue with the CABIN data

CABIN<-read_excel("tempdata/CABIN.xlsx", sheet="cabin_habitat_data_mda01_1987-p")%>%
  dplyr::select(-VariableDescription, -MDL, -Computed, -Note, -Site, -Protocol, -JulianDay, -Year, -SampleNumber, -Status, -status, -QAQC, -Type, -Unit)%>%
  pivot_wider(id_cols = SiteVisitID,names_from = Variable, values_from =Value)%>%
  mutate(SiteVisitID=as.character(SiteVisitID))%>%
  as.data.frame()%>%
  replace(.=="NULL", "NA")

CABIN_Loc<-read_excel("tempdata/CABIN_Loc.xlsx") %>%
  dplyr::select(SiteVisitID, LocalBasinName, Latitude, Longitude, Year)%>%
  filter(Latitude>1)%>%
  mutate(Longitude=as.numeric(Longitude),
         Latitude=as.numeric(Latitude))%>% 
  filter(!is.na(Longitude)&!is.na(Latitude)) %>%
  as.data.frame()%>%
  st_as_sf(coords=c("Longitude", "Latitude"),crs=ll) %>%
  st_transform(proj)

#inner-joined cabin habitat data with location data to match the two datasets
Full_CABIN<-left_join(CABIN, CABIN_Loc, by="SiteVisitID")%>%
  as.data.frame()%>%
  st_as_sf()

#not working: seems that cabin and salmon don't overlay well together
# cab_grid<- st_join(Full_CABIN, salmon, join=st_intersects)
# colSums(is.na(cab_grid))

png(filename=paste0("plots/CABIN_overlay_plot.png"))
ggplot(atlprov)+
  geom_sf()+
  geom_sf(data=Full_CABIN)+
  geom_sf(data=salmon, mapping=aes(col=presence))+ 
  coord_sf()+
  theme_bw()
dev.off()

#Now join CABIN to pressures for grid
cab_grid<-st_join(pressures,Full_CABIN, join=st_intersects, left=TRUE)%>%
  st_transform(proj)

#colSums(is.na(cab_grid))

# salmon_cab<-salmon%>%
#   st_sfc(st_crs(ll))
salmon<-salmon%>%
  st_transform(proj)

# salmon%>%
# filter(st_intersects(st_combine(cab_grid),.,sparse=FALSE)[1,])
#cab_grid<-st_intersection(cab_grid, salmon)
cab_grid_sal<- 
st_join(salmon,cab_grid, join=st_intersects, left=TRUE)%>%
  select(geometry, presence, CHUC_12_ID, Region)%>%
  na.omit()

getmode <- function(presence) {
  uniqv <- unique(presence)
  uniqv[which.max(tabulate(match(presence, uniqv)))]
}

cab_grid_sal<-cab_grid_sal%>%
  group_by(CHUC_12_ID) %>% 
  summarise(mode_presence=getmode(presence))

cab_grid <- cab_grid_sal %>% 
  as.data.frame() %>% 
  select(-geometry)%>%
  left_join(cab_grid,.,by="CHUC_12_ID") 

cab_grid<-cab_grid%>%
  dplyr::select(-OBJECTID, -CHUC_12_NM, -ID, -Eco_Group,
                -Eco_Grp_NM, -Stress_1, -Stress_2, -Stress_3,
                -Grp_1_Rank, -Grp_2_Rank, -Grp_3_Rank, -Grp_4_Rank,
                -Grp_5_Rank, -Grp_6_Rank, -SUM,
                -CHU_12_NM, -CHU_10_ID, -CHU_10_NM, -CHU_8_ID, -CHU_8_NM,
                -CHU_6_ID, -CHU_6_NM, -CHU_4_ID, -CHU_4_NM, -CHU_2_ID, -CHU_2_NM,
                -LocalBasinName, -Year)

cab_grid_NS<-cab_grid%>%
  st_filter(NS)

sum_velocity_NS<-cab_grid_NS%>%
  as.data.frame()%>%
  rename("Velocity_Avg"="Velocity-Avg")%>%
  group_by(CHUC_12_ID)%>%
  summarize(Average_Water_Velocity=mean(unlist(Velocity_Avg)),
              Slope=mean(unlist(Slope)),
              Presence_of_Riffles=mean(unlist(`Reach-Riffles`)),
              Presence_of_Pools=mean(unlist(`Reach-Pools`)),
              Precent_Canopy_Coverage=mean(unlist(`Reach-%CanopyCoverage`)),
              Average_Water_Depth=mean(unlist(`Depth-Avg`)),
              Max_Annual_Water_Temp=mean(unlist(TempANNUALmax)),
              water_Temp=mean(unlist(`General-TempWater`)),
              Annual_Mean_Precipitation=mean(unlist(`Precipitation-AnnualMean`)),
              Average_Evelvation=mean(unlist(ElevationAvg)),
              pH=mean(unlist(`General-pH`)),
              Specific_Conductivity=mean(unlist(`General-SpCond`)),
              Dissolved_Oxygen=mean(unlist(`General-DO`)),
              Turbidity=mean(unlist(`General-Turbidity`)),
              Alkalinity=mean(unlist(`General-Alkalinity`)),
              Aluminum=mean(unlist(Al)),
            Mode_Salmon_Presence=mode_presence)




#Lets look at the missing data in our datasets starting with CABIN


sum_missing<- data.matrix(sum_velocity)

superheat(sum_missing, scale=T, heat.na.col = "white")

gg_miss_var(sum_velocity_NS, show_pct = FALSE)+ labs(y="Porportion of Missing Data")

sum_miss_CHUC<-sum_velocity%>%
  group_by(CHUC_12_ID)%>%
  miss_var_summary()

gg_miss_fct(x=sum_velocity_NS, fct=CHUC_12_ID)+
  theme(axis.text.x=element_blank())+
  labs(title="Missing Data in each Hydrographic Unit in Nova Scotia (CHUC_12_ID)")

vis_miss(sum_velocity_NS%>%
           ungroup()%>%
           select(-CHUC_12_ID))

gg_miss_upset(sum_velocity_NS, nsets=n_var_miss(sum_velocity_NS))

sum_velocity_NS_longer<- sum_velocity_NS%>%
  ungroup()%>%
  dplyr::select(-CHUC_12_ID)%>%
  summarise_all(list(~is.na(.)))%>%
  pivot_longer(everything(),
               names_to = "variables", values_to="missing") %>%
  count(variables, missing)%>%
  mutate(porpotion= (n/712)*100)

  ggplot(sum_velocity_NS_longer, aes(y=variables,x=n,fill=missing))+
  geom_col()+
  scale_fill_manual(values=c("skyblue3","gold"))+
  theme(axis.title.y=element_blank())
  
sum_velocity_NS_longer%>%
  filter(missing=="TRUE")%>%
  arrange(desc(porpotion))%>%
ggplot(aes(x=n, y=variables, size=porpotion))+
  geom_point(alpha=0.5, colour="#00AFBB")+
  scale_size(range=c(.5, 24), name="Percentage of Missing Data Per Variable")+
  theme_bw()+
  ylab("Variables Important for Atlantic Salmon Habitat Monitoring")+
  xlab("Number of Hydrographic Units in Nova Scotia Missing Data (n/712)")+
  labs(title = "Data Gaps from Steams and Rivers in Nova Scotia (per hydrographic unit)")

#Correlation plot to determine how parameters within the original dataset are correlated

head(pressures_grid)
names(pressures_grid)

pg<-pressures_grid%>%
  select(-Hydro_ID, -gridcode, -geometry, -ID.x, -name_db, -name_en_CG, -name_fr_CG, -name_other, -NAME_LABEL, -Country,
         -Region, -RiverName_, -AndersonSp, -Latitude_A, -Longitude_, -RiverName1, -Rivername2, -Latitude_D, -Longitude1,
         -GNIS_NAME_, -River_Name, -Original_A, -StockClass, -StockClas1, -StockClas2, -LAT_LON_DF, -OBJECTID, -CHUC_12_ID, -CHUC_12_NM,
         -ID.y, -WSI, -WSI_Rank, -Eco_Group, -Eco_Grp_NM, -Stress_1, -Stress_2, -Stress_3, -Grp_1_Rank, -Grp_2_Rank, -Grp_3_Rank, -Grp_4_Rank,
         -Grp_5_Rank, -Grp_6_Rank, -SUM, -CHU_12_ID.x, -CHU_12_ID.y, -CHU_12_NM, -CHU_10_ID, -CHU_10_NM, -CHU_8_ID, -CHU_8_NM, -CHU_6_ID,
         -CHU_6_NM, -CHU_4_ID, -CHU_4_NM, -CHU_2_NM, -CHU_2_ID, -Area_SqKm.y, -NB_Rank, -NS_Rank, -QC_Rank, -PE_Rank, -Raw_PCA1_S, -Raw_PCA2_S,
         -Raw_PCA3_S, -Size_Comp, -Grad_Simp, -Grad_Comp, -Alk, -Tidal, -SimpCombo, -IntmCombo, -CmplxCombo, -Length_Km, -Temp, -Size_Simp, -DISSOLVE,
         -STATUS_FIN, -TotRd_Dens, -PRd_Densit, -UPRd_Densi, -CL_EXC, -Xing_Densi, -AqBarrier_, -NN_Fish_Nu, -Percent_Ur, -Percent_Cr, -Percent_Cu, -Percent_Pa,
         -Percent_Ag, -Percent_Im, -Pop_Den, -N_Leach, -P_Leach, -PSTCD_Leac, -Clim_Veloc, -PSP_Source, -PSP_Metals, -PSP_Nutrie, -PSP_Organi,
         -TDiff_Len)%>%
  as.data.frame()

pg<-pg%>%
  dplyr::select(-geometry)%>%
  mutate(presence=as.numeric(presence))

#need to replace NAs with zero because otherwise there are too many NAs for a matrix
pg[is.na(pg)]=0

# mat : is a matrix of data
# ... : further arguments to pass to the native R cor.test function
cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}
# matrix of the p-value of the correlation
p.mat <- cor.mtest(pg)
head(p.mat[, 1:5])

# Specialized the insignificant value according to the significant level (i.e.p=0.05)
corrplot(cor(pg), type="lower", order="hclust", tl.col="black", tl.srt=45, tl.offset=0.4, tl.cex=0.8,
         p.mat=p.mat, sig.level=0.05)

#Other salmon Hab suitability scenarios, artifical stocking=absence

salmon_art<-raw_salmon%>% 
  filter(Region %in% c("Nova Scotia", "Prince Edward Island", "New Brunswick"))%>%
  mutate(presence=case_when(STATUS_FIN=="Lost"~0,
                            STATUS_FIN=="Low Risk" ~1,
                            STATUS_FIN=="Moderate Risk"~1,
                            STATUS_FIN=="High Risk"~1,
                            STATUS_FIN=="Artificially Sustained"~0),
         presence=as.factor(presence))

#Overlapping salmon p/a with streams database grid

streams_grid_art<-st_join(streams, salmon_art, join=st_intersects, left=TRUE)

pressures_grid_art<-st_join(streams_grid_art, pressures, join=st_intersects, left=TRUE)

#Now splitting pressures_grid into training/validation set and prediction set

#removing rows with missing data, especially if missing climate velocity data
pres_trainSS_art<-pressures_grid_art%>%
  dplyr::filter(presence !="NA" & Clim_Veloc !="NA")

pres_predSet_art<-pressures_grid_art%>%
  dplyr::filter(is.na(presence) & Clim_Veloc !="NA")


#mutating the dataset for use in models

pres_trainSS_art<- pres_trainSS_art%>%
  mutate(
    Hydro_ID =as.factor(Hydro_ID),
    gridcode =as.factor(gridcode),
    Size_Comp=as.factor(Size_Comp),
    Size_Simp=as.factor(Size_Simp),
    Grad_Simp=as.factor(Grad_Simp),
    Grad_Comp=as.factor(Grad_Comp),
    Alk=as.factor(Alk),
    Tidal=as.factor(Tidal),
    SimpCombo=as.factor(SimpCombo),
    IntmCombo=as.factor(IntmCombo),
    CmplxCombo=as.factor(CmplxCombo),
    Length_Km= as.numeric(Length_Km),
    Temp=as.factor(Temp),
    ID.y =as.factor(ID.y),
    name_db=as.factor( name_db),
    name_en_CG=as.factor(name_en_CG),
    name_fr_CG=as.factor(name_fr_CG),
    name_other=as.factor(name_other),
    NAME_LABEL=as.factor(NAME_LABEL),
    Country=as.factor(Country),
    Region=as.factor(Region),
    RiverName_=as.factor(RiverName_),
    AndersonSp=as.factor(AndersonSp),
    RiverName1=as.factor(RiverName1),
    Rivername2=as.factor(Rivername2),
    DISSOLVE=as.factor(DISSOLVE),
    GNIS_NAME_=as.factor(GNIS_NAME_),
    River_Name=as.factor(River_Name),
    Original_A=as.factor(Original_A),
    StockClass=as.factor(StockClass),
    StockClas1=as.factor(StockClas1),
    StockClas2=as.factor(StockClas2),
    STATUS_FIN=as.factor(STATUS_FIN),
    presence= as.factor(presence),
    N_TotRd_De=as.numeric(N_TotRd_De),
    N_PRd_Den_=as.numeric(N_PRd_Den_),
    N_UPRd_Den=as.numeric(N_UPRd_Den),
    N_CL_EXC_N=as.numeric(N_CL_EXC_N),
    N_Xing_Den=as.numeric(N_Xing_Den),
    N_NN_Fish_=as.numeric(N_NN_Fish_),
    M_AqBarrie=as.numeric(M_AqBarrie),
    N_Pct_Urba=as.numeric(N_Pct_Urba),
    N_Pct_Crop=as.numeric(N_Pct_Crop),
    N_Pct_Cut_=as.numeric(N_Pct_Cut_),
    N_Pct_Past=as.numeric(N_Pct_Past),
    N_Pct_Ag_N=as.numeric(N_Pct_Ag_N),
    N_Pct_Impv=as.numeric(N_Pct_Impv),
    N_Pop_Den_=as.numeric(N_Pop_Den_),
    N_N_Leach_=as.numeric(N_N_Leach_),
    N_P_Leach_=as.numeric(N_P_Leach_),
    N_PSTCD_Le=as.numeric(N_PSTCD_Le),
    N_Clim_Vel=as.numeric(N_Clim_Vel),
    N_PSP_Sour=as.numeric(N_PSP_Sour),
    N_PSP_Meta=as.numeric(N_PSP_Meta),
    N_PSP_Nutr=as.numeric(N_PSP_Nutr),
    N_PSP_Orga=as.numeric(N_PSP_Orga),
    N_TDiff_Le=as.numeric(N_TDiff_Le),
    Area_SqKm.x=as.numeric(Area_SqKm.x)
  )

pres_predSet_art<- pres_predSet_art%>%
  mutate(
    Hydro_ID =as.factor(Hydro_ID),
    gridcode =as.factor(gridcode),
    Size_Comp=as.factor(Size_Comp),
    Size_Simp=as.factor(Size_Simp),
    Grad_Simp=as.factor(Grad_Simp),
    Grad_Comp=as.factor(Grad_Comp),
    Alk=as.factor(Alk),
    Tidal=as.factor(Tidal),
    SimpCombo=as.factor(SimpCombo),
    IntmCombo=as.factor(IntmCombo),
    CmplxCombo=as.factor(CmplxCombo),
    Length_Km= as.numeric(Length_Km),
    Temp=as.factor(Temp),
    ID.y =as.factor(ID.y),
    name_db=as.factor( name_db),
    name_en_CG=as.factor(name_en_CG),
    name_fr_CG=as.factor(name_fr_CG),
    name_other=as.factor(name_other),
    NAME_LABEL=as.factor(NAME_LABEL),
    Country=as.factor(Country),
    Region=as.factor(Region),
    RiverName_=as.factor(RiverName_),
    AndersonSp=as.factor(AndersonSp),
    RiverName1=as.factor(RiverName1),
    Rivername2=as.factor(Rivername2),
    DISSOLVE=as.factor(DISSOLVE),
    GNIS_NAME_=as.factor(GNIS_NAME_),
    River_Name=as.factor(River_Name),
    Original_A=as.factor(Original_A),
    StockClass=as.factor(StockClass),
    StockClas1=as.factor(StockClas1),
    StockClas2=as.factor(StockClas2),
    STATUS_FIN=as.factor(STATUS_FIN),
    presence= as.factor(presence),
    N_TotRd_De=as.numeric(N_TotRd_De),
    N_PRd_Den_=as.numeric(N_PRd_Den_),
    N_UPRd_Den=as.numeric(N_UPRd_Den),
    N_CL_EXC_N=as.numeric(N_CL_EXC_N),
    N_Xing_Den=as.numeric(N_Xing_Den),
    N_NN_Fish_=as.numeric(N_NN_Fish_),
    M_AqBarrie=as.numeric(M_AqBarrie),
    N_Pct_Urba=as.numeric(N_Pct_Urba),
    N_Pct_Crop=as.numeric(N_Pct_Crop),
    N_Pct_Cut_=as.numeric(N_Pct_Cut_),
    N_Pct_Past=as.numeric(N_Pct_Past),
    N_Pct_Ag_N=as.numeric(N_Pct_Ag_N),
    N_Pct_Impv=as.numeric(N_Pct_Impv),
    N_Pop_Den_=as.numeric(N_Pop_Den_),
    N_N_Leach_=as.numeric(N_N_Leach_),
    N_P_Leach_=as.numeric(N_P_Leach_),
    N_PSTCD_Le=as.numeric(N_PSTCD_Le),
    N_Clim_Vel=as.numeric(N_Clim_Vel),
    N_PSP_Sour=as.numeric(N_PSP_Sour),
    N_PSP_Meta=as.numeric(N_PSP_Meta),
    N_PSP_Nutr=as.numeric(N_PSP_Nutr),
    N_PSP_Orga=as.numeric(N_PSP_Orga),
    N_TDiff_Le=as.numeric(N_TDiff_Le),
    Area_SqKm.x=as.numeric(Area_SqKm.x)
  )
#set aside data from trainSS for training and another for testing.
#Split ratio 70:30 
set.seed(100)

sampleSet_pres_art<-sample(nrow(pres_trainSS_art), 0.7*nrow(pres_trainSS_art), replace=FALSE)
TrainSet_pres_art<- pres_trainSS_art[sampleSet_pres_art,]
TestSet_pres_art<-pres_trainSS_art[-sampleSet_pres_art,]

#model14a adapted for new salmon status

#OOb 0.29%
model14a_art<-randomForest(presence~Size_Comp+
                         Grad_Comp+
                         Alk+
                         Length_Km+
                         Temp+
                         Tidal+
                         N_TotRd_De+
                         N_PRd_Den_+
                         N_UPRd_Den+
                         N_CL_EXC_N+
                         N_Xing_Den+
                         N_NN_Fish_+
                         M_AqBarrie+
                         N_Pct_Urba+
                         N_Pct_Crop+
                         N_Pct_Cut_+
                         N_Pct_Past+
                         N_Pct_Ag_N+
                         N_Pct_Impv+
                         N_Pop_Den_+
                         N_N_Leach_+
                         N_P_Leach_+
                         N_PSTCD_Le+
                         N_Clim_Vel+
                         N_PSP_Sour+
                         N_TDiff_Le+
                         Area_SqKm.x+
                         N_PSP_Meta+
                         N_PSP_Nutr+
                         N_PSP_Orga,
                       data=TrainSet_pres_art,
                       importance=TRUE, ntree=500, mtry=10)
model14a_art



png(filename=paste0("plots/model14a_art_plot.png"))
varImpPlot(model14a_art)
dev.off()

#model14a_art k-fold
predValid14a_art<-predict(model14a_art, TestSet_pres_art)

# 0.9965986
mean(predValid14a_art==TestSet_pres_art$presence)

table(predValid14a_art, TestSet_pres_art$presence)

#roc
check14a_art=predict(model14a_art, TrainSet_pres_art, type="prob")[,2]
perf14a_art<-prediction(check14a_art, TrainSet_pres_art$presence)
roc.perf14a_art=performance(perf14a_art, measure = "tpr", x.measure = "fpr")

png(filename = paste0("plots/model14a_art_roc.plot.png"))
plot(roc.perf14a_art)
abline(a=0,b=1)
dev.off()

#0.9999871
auc.perf14a_art=performance(perf14a_art, measure = "auc")
auc.perf14a_art@y.values

#trying out model14 on the predication dataset
pred4_art<-predict(model14a_art, newdata=pres_predSet_art, type='prob')[,2]
pred4_art<-pred4_art%>%
  as.data.frame()
pred4_art$row_num<-seq.int(nrow(pred4_art))
pres_predSet_art$row_num<-seq.int(nrow(pres_predSet_art))

pres_output2_art<-left_join(pres_predSet_art, pred4_art, by="row_num")

names(pres_output2_art)[names(pres_output2_art)=='.']<-'predicted_suitable'

#writing a data table for the model11 predictions and mapping those
write.csv(pres_output2_art, "plots/model14a_art_predictions.csv", row.names=TRUE)
saveRDS(pres_output2_art, "plots/model14a_art_predictions.rds")

#st_write(pres_output2, "plots/model14_shapefile.shp")




png(filename = paste0("plots/model14a_art_predictions_map.png"))

ggplot(pres_output2_art)+
  geom_sf(aes(col=predicted_suitable))+
  scale_colour_gradient2(low="red", midpoint = 0.50, mid="white", high="blue", space = "Lab")+
  ggtitle("Predicted Suitable Habitat for Salmo salar based on \nEnvironmental Characteristics and Habitat Pressures")+
  theme(panel.grid.major = element_line(color = gray(0.5), linetype = "dashed", 
                                        size = 0.5), panel.background = element_rect(fill = "aliceblue"))
dev.off()

#Other salmon Hab suitability scenarios, artificial stocking+ high risk=absence

salmon_high<-raw_salmon%>% 
  filter(Region %in% c("Nova Scotia", "Prince Edward Island", "New Brunswick"))%>%
  mutate(presence=case_when(STATUS_FIN=="Lost"~0,
                            STATUS_FIN=="Low Risk" ~1,
                            STATUS_FIN=="Moderate Risk"~1,
                            STATUS_FIN=="High Risk"~0,
                            STATUS_FIN=="Artificially Sustained"~0),
         presence=as.factor(presence))

#Overlapping salmon p/a with streams database grid

streams_grid_high<-st_join(streams, salmon_high, join=st_intersects, left=TRUE)

pressures_grid_high<-st_join(streams_grid_high, pressures, join=st_intersects, left=TRUE)

#Now splitting pressures_grid into training/validation set and prediction set

#removing rows with missing data, especially if missing climate velocity data
pres_trainSS_high<-pressures_grid_high%>%
  dplyr::filter(presence !="NA" & Clim_Veloc !="NA")

pres_predSet_high<-pressures_grid_high%>%
  dplyr::filter(is.na(presence) & Clim_Veloc !="NA")


#mutating the dataset for use in models

pres_trainSS_high<- pres_trainSS_high%>%
  mutate(
    Hydro_ID =as.factor(Hydro_ID),
    gridcode =as.factor(gridcode),
    Size_Comp=as.factor(Size_Comp),
    Size_Simp=as.factor(Size_Simp),
    Grad_Simp=as.factor(Grad_Simp),
    Grad_Comp=as.factor(Grad_Comp),
    Alk=as.factor(Alk),
    Tidal=as.factor(Tidal),
    SimpCombo=as.factor(SimpCombo),
    IntmCombo=as.factor(IntmCombo),
    CmplxCombo=as.factor(CmplxCombo),
    Length_Km= as.numeric(Length_Km),
    Temp=as.factor(Temp),
    ID.y =as.factor(ID.y),
    name_db=as.factor( name_db),
    name_en_CG=as.factor(name_en_CG),
    name_fr_CG=as.factor(name_fr_CG),
    name_other=as.factor(name_other),
    NAME_LABEL=as.factor(NAME_LABEL),
    Country=as.factor(Country),
    Region=as.factor(Region),
    RiverName_=as.factor(RiverName_),
    AndersonSp=as.factor(AndersonSp),
    RiverName1=as.factor(RiverName1),
    Rivername2=as.factor(Rivername2),
    DISSOLVE=as.factor(DISSOLVE),
    GNIS_NAME_=as.factor(GNIS_NAME_),
    River_Name=as.factor(River_Name),
    Original_A=as.factor(Original_A),
    StockClass=as.factor(StockClass),
    StockClas1=as.factor(StockClas1),
    StockClas2=as.factor(StockClas2),
    STATUS_FIN=as.factor(STATUS_FIN),
    presence= as.factor(presence),
    N_TotRd_De=as.numeric(N_TotRd_De),
    N_PRd_Den_=as.numeric(N_PRd_Den_),
    N_UPRd_Den=as.numeric(N_UPRd_Den),
    N_CL_EXC_N=as.numeric(N_CL_EXC_N),
    N_Xing_Den=as.numeric(N_Xing_Den),
    N_NN_Fish_=as.numeric(N_NN_Fish_),
    M_AqBarrie=as.numeric(M_AqBarrie),
    N_Pct_Urba=as.numeric(N_Pct_Urba),
    N_Pct_Crop=as.numeric(N_Pct_Crop),
    N_Pct_Cut_=as.numeric(N_Pct_Cut_),
    N_Pct_Past=as.numeric(N_Pct_Past),
    N_Pct_Ag_N=as.numeric(N_Pct_Ag_N),
    N_Pct_Impv=as.numeric(N_Pct_Impv),
    N_Pop_Den_=as.numeric(N_Pop_Den_),
    N_N_Leach_=as.numeric(N_N_Leach_),
    N_P_Leach_=as.numeric(N_P_Leach_),
    N_PSTCD_Le=as.numeric(N_PSTCD_Le),
    N_Clim_Vel=as.numeric(N_Clim_Vel),
    N_PSP_Sour=as.numeric(N_PSP_Sour),
    N_PSP_Meta=as.numeric(N_PSP_Meta),
    N_PSP_Nutr=as.numeric(N_PSP_Nutr),
    N_PSP_Orga=as.numeric(N_PSP_Orga),
    N_TDiff_Le=as.numeric(N_TDiff_Le),
    Area_SqKm.x=as.numeric(Area_SqKm.x)
  )

pres_predSet_high<- pres_predSet_high%>%
  mutate(
    Hydro_ID =as.factor(Hydro_ID),
    gridcode =as.factor(gridcode),
    Size_Comp=as.factor(Size_Comp),
    Size_Simp=as.factor(Size_Simp),
    Grad_Simp=as.factor(Grad_Simp),
    Grad_Comp=as.factor(Grad_Comp),
    Alk=as.factor(Alk),
    Tidal=as.factor(Tidal),
    SimpCombo=as.factor(SimpCombo),
    IntmCombo=as.factor(IntmCombo),
    CmplxCombo=as.factor(CmplxCombo),
    Length_Km= as.numeric(Length_Km),
    Temp=as.factor(Temp),
    ID.y =as.factor(ID.y),
    name_db=as.factor( name_db),
    name_en_CG=as.factor(name_en_CG),
    name_fr_CG=as.factor(name_fr_CG),
    name_other=as.factor(name_other),
    NAME_LABEL=as.factor(NAME_LABEL),
    Country=as.factor(Country),
    Region=as.factor(Region),
    RiverName_=as.factor(RiverName_),
    AndersonSp=as.factor(AndersonSp),
    RiverName1=as.factor(RiverName1),
    Rivername2=as.factor(Rivername2),
    DISSOLVE=as.factor(DISSOLVE),
    GNIS_NAME_=as.factor(GNIS_NAME_),
    River_Name=as.factor(River_Name),
    Original_A=as.factor(Original_A),
    StockClass=as.factor(StockClass),
    StockClas1=as.factor(StockClas1),
    StockClas2=as.factor(StockClas2),
    STATUS_FIN=as.factor(STATUS_FIN),
    presence= as.factor(presence),
    N_TotRd_De=as.numeric(N_TotRd_De),
    N_PRd_Den_=as.numeric(N_PRd_Den_),
    N_UPRd_Den=as.numeric(N_UPRd_Den),
    N_CL_EXC_N=as.numeric(N_CL_EXC_N),
    N_Xing_Den=as.numeric(N_Xing_Den),
    N_NN_Fish_=as.numeric(N_NN_Fish_),
    M_AqBarrie=as.numeric(M_AqBarrie),
    N_Pct_Urba=as.numeric(N_Pct_Urba),
    N_Pct_Crop=as.numeric(N_Pct_Crop),
    N_Pct_Cut_=as.numeric(N_Pct_Cut_),
    N_Pct_Past=as.numeric(N_Pct_Past),
    N_Pct_Ag_N=as.numeric(N_Pct_Ag_N),
    N_Pct_Impv=as.numeric(N_Pct_Impv),
    N_Pop_Den_=as.numeric(N_Pop_Den_),
    N_N_Leach_=as.numeric(N_N_Leach_),
    N_P_Leach_=as.numeric(N_P_Leach_),
    N_PSTCD_Le=as.numeric(N_PSTCD_Le),
    N_Clim_Vel=as.numeric(N_Clim_Vel),
    N_PSP_Sour=as.numeric(N_PSP_Sour),
    N_PSP_Meta=as.numeric(N_PSP_Meta),
    N_PSP_Nutr=as.numeric(N_PSP_Nutr),
    N_PSP_Orga=as.numeric(N_PSP_Orga),
    N_TDiff_Le=as.numeric(N_TDiff_Le),
    Area_SqKm.x=as.numeric(Area_SqKm.x)
  )
#set aside data from trainSS for training and another for testing.
#Split ratio 70:30 
set.seed(100)

sampleSet_pres_high<-sample(nrow(pres_trainSS_high), 0.7*nrow(pres_trainSS_high), replace=FALSE)
TrainSet_pres_high<- pres_trainSS_high[sampleSet_pres_high,]
TestSet_pres_high<-pres_trainSS_high[-sampleSet_pres_high,]

#model14a adapted for new salmon status

#OOb 0.29%
model14a_high<-randomForest(presence~Size_Comp+
                             Grad_Comp+
                             Alk+
                             Length_Km+
                             Temp+
                             Tidal+
                             N_TotRd_De+
                             N_PRd_Den_+
                             N_UPRd_Den+
                             N_CL_EXC_N+
                             N_Xing_Den+
                             N_NN_Fish_+
                             M_AqBarrie+
                             N_Pct_Urba+
                             N_Pct_Crop+
                             N_Pct_Cut_+
                             N_Pct_Past+
                             N_Pct_Ag_N+
                             N_Pct_Impv+
                             N_Pop_Den_+
                             N_N_Leach_+
                             N_P_Leach_+
                             N_PSTCD_Le+
                             N_Clim_Vel+
                             N_PSP_Sour+
                             N_TDiff_Le+
                             Area_SqKm.x+
                             N_PSP_Meta+
                             N_PSP_Nutr+
                             N_PSP_Orga,
                           data=TrainSet_pres_art,
                           importance=TRUE, ntree=500, mtry=10)
model14a_high



png(filename=paste0("plots/model14a_high_plot.png"))
varImpPlot(model14a_high)
dev.off()

#model14a_art k-fold
predValid14a_high<-predict(model14a_high, TestSet_pres_high)

# 0.5446712
mean(predValid14a_high==TestSet_pres_high$presence)

table(predValid14a_high, TestSet_pres_high$presence)

#roc
check14a_high=predict(model14a_high, TrainSet_pres_high, type="prob")[,2]
perf14a_high<-prediction(check14a_high, TrainSet_pres_high$presence)
roc.perf14a_high=performance(perf14a_high, measure = "tpr", x.measure = "fpr")

png(filename = paste0("plots/model14a_high_roc.plot.png"))
plot(roc.perf14a_high)
abline(a=0,b=1)
dev.off()

#0.694016
auc.perf14a_high=performance(perf14a_high, measure = "auc")
auc.perf14a_high@y.values

#trying out model14 on the predication dataset
pred4_high<-predict(model14a_high, newdata=pres_predSet_high, type='prob')[,2]
pred4_high<-pred4_high%>%
  as.data.frame()
pred4_high$row_num<-seq.int(nrow(pred4_high))
pres_predSet_high$row_num<-seq.int(nrow(pres_predSet_high))

pres_output2_high<-left_join(pres_predSet_high, pred4_high, by="row_num")

names(pres_output2_high)[names(pres_output2_high)=='.']<-'predicted_suitable'

#writing a data table for the model11 predictions and mapping those
write.csv(pres_output2_high, "plots/model14a_high_predictions.csv", row.names=TRUE)
saveRDS(pres_output2_high, "plots/model14a_high_predictions.rds")


png(filename = paste0("plots/model14a_high_predictions_map.png"))

ggplot(pres_output2_high)+
  geom_sf(aes(col=predicted_suitable))+
  scale_colour_gradient2(low="red", midpoint = 0.50, mid="white", high="blue", space = "Lab")+
  ggtitle("Predicted Suitable Habitat for Salmo salar based on \nEnvironmental Characteristics and Habitat Pressures")+
  theme(panel.grid.major = element_line(color = gray(0.5), linetype = "dashed", 
                                        size = 0.5), panel.background = element_rect(fill = "aliceblue"))
dev.off()

#Request from Sarah T. Overlay salmon status with watershed map
#adjusted salmon presence
ggplot(atlprov)+
  geom_sf()+
  geom_sf(data=primaryws)+
  geom_sf(data=salmon, aes(col=presence))+
  theme_light()

#unadjusted salmon pop categories
ggplot(NS)+
  geom_sf()+
  geom_sf(data=primaryws)+
  geom_sf(data=raw_salmon%>%
            filter(Region=="Nova Scotia"), 
          aes(col=STATUS_FIN))+
  scale_color_brewer(palette= "Set1")+
  theme_light()
