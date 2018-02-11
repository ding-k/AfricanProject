library(ggplot2)
library(reshape2)
library(Hmisc)
library(ggrepel)
require(CCA)
library(cluster)
library(fpc)
library(YaleToolkit)
options(warn=-1)
library(GGally)
library(boot)
library(leaps)

library(tidyr)

library(GGally)
#remove not useful columns in raw data table
data<-read.csv("WEF_Data_Compilation_ALL_SSA.csv") 
no_use<- names(data) %in% c("X")
data<-data[!no_use]

#make the parameter column as charactor so we can search the certain name in the column
data$Parameter.<-as.character(data$Parameter.)

##`. create df with the primary response variables: service variables and health outcome
svs_all_var <- data %>%
  # dplyr::rename(Parameter = Parameter.) %>% #change column name to exclude period
  dplyr::filter(Parameter. %in% c("Total population with access to safe drinking-water","Population with access to improved sanitation","Depth of the food deficit","Energy supply per capita")) %>%
  dplyr::select(-China,-USA,-`Sri.Lanka`,-India,-Bangladesh) #emit countries not in Africa
#emit countries not in Africa
row.names(svs_all_var)<-svs_all_var$Parameter.
##. Explore missing datasets 

svs_all_var<-svs_all_var[,-c(1,2,3,4)]
summary(svs_all_var)
sum(is.na(svs_all_var["Total population with access to safe drinking-water",]))
sum(is.na(svs_all_var["Population with access to improved sanitation",]))
sum(is.na(svs_all_var["Depth of the food deficit",]))
sum(is.na(svs_all_var["Energy supply per capita",]))
svs_all_var<-t(svs_all_var)
# name<-for (i in 1:nrow(svs_all_var) ){
#   for(j in 1:ncol(svs_all_var)){ifelse(complete.cases(svs_all_var[i,j])==FALSE,print(paste(row.names(svs_all_var)[i],colnames(svs_all_var)[j], sep = ",")),print(" "))}
# }
# 
# ##. Decide which countries to eliminate from analysis due to missing data
ac_dw_df <- data %>%
  # dplyr::rename(Parameter = Parameter.) %>% #change column name to exclude period
  #ac_dw,ppt_lt,tot_water_renew,var_interannual,var_sea,flood,drought,gdp,edu_ind,voi_ac,pol_sta,gov_ef,reg_qual,ru_law,con_corru,ru_percent
  dplyr::filter(Parameter. %in% c("Total population with access to safe drinking-water",
                                  "Long-term annual precip depth",
                                  "Total renewable water sources",
                                  "Interannual variability",
                                  "Seasonal variability",
                                  "Flood occurrence",
                                  "Drought frequency",
                                  "Gross domestic product (GDP)",
                                  "Education Index","Voice and Accountability",
                                  "Political Stability and Absence of Violence/Terrorism",
                                  "Government Effectiveness",
                                  "Regulatory Quality",
                                  "Rule of Law",
                                  "Control of Corruption",
                                  "Rural Population",
                                  "Population")) %>%
  dplyr::select(-Burundi, -Comoros, -Eritrea, -Congo..Dem..Rep., -Equatorial.Guinea, -Seychelles, -Somalia, -South.Sudan, -Somalia,-China,-USA,-`Sri.Lanka`,-India,-Bangladesh)
row.names(ac_dw_df)<-ac_dw_df$Parameter.
ac_dw_df<-ac_dw_df[,-c(1,2,3,4)]

ac_dw_df<- ac_dw_df %>%
  dplyr::select(-Cabo.Verde,-Mauritius,-Sao.Tome.and.Principe) #emit countries not in Africa

#make dataframe numeric
for (i in 1:ncol(ac_dw_df)){
  ac_dw_df[,i]=as.numeric(as.character(ac_dw_df[,i]))
}
#transpose to make df in row of countries and col of parameters
ac_dw_df<-as.data.frame(t(ac_dw_df))

#water availability variables pca

water_av_pca<-ac_dw_df %>%
  dplyr::select("Long-term annual precip depth",
                "Total renewable water sources",
                "Interannual variability",
                "Seasonal variability",
                "Flood occurrence",
                "Drought frequency")

pc_water<-prcomp(water_av_pca, scale= TRUE, center = TRUE)
summary(pc_water)
pc_water

#principal component analysis in governance variables
WBGI_pca_ac_dw<-ac_dw_df %>%
  dplyr::select("Voice and Accountability","Political Stability and Absence of Violence/Terrorism","Government Effectiveness","Regulatory Quality","Rule of Law","Control of Corruption")
pc_WBGI<-prcomp(WBGI_pca_ac_dw)
summary(pc_WBGI)
pc_WBGI
pc_water_GI_scores<-cbind.data.frame(pc_water$x,pc_WBGI$x)
row.names(pc_water_GI_scores)<-row.names(ac_dw_df)
colnames(pc_water_GI_scores)<-c("PC1_water","PC2_water","PC3_water","PC4_water","PC5_water","PC6_water","PC1_GI","PC2_GI","PC3_GI","PC4_GI","PC5_GI","PC6_GI")

pca_ac_dw_df<- ac_dw_df %>%
  dplyr::select("Total population with access to safe drinking-water","Gross domestic product (GDP)","Education Index","Rural Population","Population")%>%
  cbind.data.frame(pc_water_GI_scores)
ru_percent<-pca_ac_dw_df$`Rural Population`/pca_ac_dw_df$Population*100

#prove that the pca is conducted approapriately
# model1.1<-regsubsets(`Total population with access to safe drinking-water`~.-PC3_water-PC4_water-PC5_water-PC6_water-PC3_GI-PC4_GI-PC5_GI-PC6_GI,data= pca_ac_dw_df,nv=8,method = "forward")
model1.1<-lm(`Total population with access to safe drinking-water`~.-PC3_water-PC4_water-PC5_water-PC6_water-PC3_GI-PC4_GI-PC5_GI-PC6_GI,data= pca_ac_dw_df,nv=8,method = "forward")

summary(model1.1)
pca_ac_dw_df<-pca_ac_dw_df%>%
  dplyr::select("Total population with access to safe drinking-water","Gross domestic product (GDP)","Education Index","PC1_water","PC2_water","PC1_GI","PC2_GI")%>%
  cbind.data.frame(ru_percent)

model1.15<-regsubsets(`Total population with access to safe drinking-water`~.,data= pca_ac_dw_df, method = "forward")
summary(model1.15)
#outcome is that education index and pc2water is not significant
model1.2<-lm(`Total population with access to safe drinking-water`~.-PC2_water-`Education Index`,data= pca_ac_dw_df)
summary(model1.2)
model1.2
pred_ac_dw<-predict(model1.2,newdata = pca_ac_dw_df)
plot(pred_ac_dw,pca_ac_dw_df$`Total population with access to safe drinking-water`)

ggplot(pca_ac_dw_df,aes( x=`Total population with access to safe drinking-water`,y=pred_ac_dw,size=ru_percent))+ 
  geom_point(color="blue")+
  # geom_smooth(method=lm,formula= y~x,color="black")+
  geom_abline(slope = 1,color="red",linetype=2)+
  scale_x_continuous(limits = c(50, 100))+
  scale_y_continuous(limits =  c(50, 100))+
  labs(x="Total Population with Access to Safe Drinking Water (%)",y= "Model Prediction (%)") + 
  theme_bw()+
  theme(legend.position = "right")+
  theme(axis.title.x = element_text(size=30),
        axis.title.y = element_text(size = 30),
        axis.text.x = element_text(color="#993333", size=14),
        axis.text.y = element_text(color="#993333", size=14))+
  scale_size(range = c(5,10))+
  geom_text_repel(aes(`Total population with access to safe drinking-water`,pred_ac_dw, label = row.names(pca_ac_dw_df)),show.legend = FALSE)

biplot(pc_WBGI,ylim=c(-.58,.58),xlim=c(-.58,.58))
biplot(pc_water,ylim=c(-.45,.45),xlim=c(-.45,.45))
# ggbiplot(pc_WBGI, choices = c(1,2), obs.scale = 1, var.scale = 1)
# ggplot(pca_ac_dw_df,aes( x=PC1_GI,y=PC2_GI, color=row.names(pca_ac_dw_df)))+
#   geom_point()+
#   labs(x="PC1 of World Bank Governance Indicators",y= "PC2 of World Bank Governance Indicators") +
#   theme(legend.position = "none")+ 
#   geom_text_repel(aes(PC1_GI,PC2_GI, label = row.names(pca_ac_dw_df)))

################ac_sanitaion

#filter data and construct dataframe for ac_sani
ac_sani_df <- data %>%
  # dplyr::rename(Parameter = Parameter.) %>% #change column name to exclude period
  #ac_dw,ppt_lt,tot_water_renew,var_interannual,var_sea,flood,drought,gdp,edu_ind,voi_ac,pol_sta,gov_ef,reg_qual,ru_law,con_corru,ru_percent
  dplyr::filter(Parameter. %in% c("Population with access to improved sanitation","Long-term annual precip depth","Total renewable water sources","Interannual variability","Seasonal variability","Flood occurrence","Drought frequency","Gross domestic product (GDP)","Education Index","Voice and Accountability","Political Stability and Absence of Violence/Terrorism","Government Effectiveness","Regulatory Quality","Rule of Law","Control of Corruption","Rural Population","Population")) %>%
  dplyr::select(-Burundi, -Comoros, -Eritrea, -Congo..Dem..Rep., -Equatorial.Guinea, -Seychelles, -Somalia, -South.Sudan, -Somalia,-China,-USA,-`Sri.Lanka`,-India,-Bangladesh)
row.names(ac_sani_df)<-ac_sani_df$Parameter.
ac_sani_df<-ac_sani_df[,-c(1,2,3,4)]

ac_sani_df<- ac_sani_df %>%
  dplyr::select(-Cabo.Verde,-Mauritius,-Sao.Tome.and.Principe) #emit countries not in Africa

##. Create your df with predictors for access to sani alone


##. Explore missing datasets
summary(ac_sani_df) 

#make data frame numeric 
for (i in 1:ncol(ac_sani_df)){
  ac_sani_df[,i]=as.numeric(as.character(ac_sani_df[,i]))
}
ac_sani_df<-as.data.frame(t(ac_sani_df))

model2<-lm(`Population with access to improved sanitation`~.,data = ac_sani_df)
summary(model2)

ru_percent<-pca_ac_sani_df$`Rural Population`/pca_ac_sani_df$Population*100

##make pc of water and GI into dataframe
pca_ac_sani_df<- ac_sani_df %>%
  dplyr::select("Population with access to improved sanitation","Gross domestic product (GDP)","Education Index")%>%
  cbind.data.frame(pc_water_GI_scores) %>%
  cbind.data.frame(ru_percent)

#prove that the pca is conducted approapriately
model2.1<-lm(`Population with access to improved sanitation`~.-PC3_water-PC4_water-PC5_water-PC6_water-PC3_GI-PC4_GI-PC5_GI-PC6_GI,data= pca_ac_sani_df)
summary(model2.1)
#check off proceed to stepwise regression
model2.2<-regsubsets(`Population with access to improved sanitation`~.-PC3_water-PC4_water-PC5_water-PC6_water-PC3_GI-PC4_GI-PC5_GI-PC6_GI,data= pca_ac_sani_df,nv=8)
summary(model2.2)

model2.3<-lm(`Population with access to improved sanitation`~.-PC1_GI-PC3_water-PC4_water-PC5_water-PC6_water-PC3_GI-PC4_GI-PC5_GI-PC6_GI,data= pca_ac_sani_df)
summary(model2.3)

pred_ac_sani<-predict(model2.3,newdata = pca_ac_sani_df)
plot(pred_ac_sani,pca_ac_sani_df$`Population with access to improved sanitation`)

ggplot(pca_ac_sani_df,aes( x=`Population with access to improved sanitation`,y=pred_ac_sani, size= `Education Index`))+ 
  geom_point(color="blue")+
  # geom_smooth(method=lm,formula= y~x,color="black")+
  geom_abline(slope = 1,color="red",linetype=2)+
  # scale_x_continuous(limits = c(50, 100))+
  # scale_y_continuous(limits =  c(50, 100))+
  labs(x="Total Population with Access to Improved Sanitation",y= "Model Prediction (%)")+
  scale_size(range = c(5,10))+
  theme_bw()+
  theme(legend.position = "right")+
  theme(axis.title.x = element_text(size=30),
        axis.title.y = element_text(size = 30))+
  geom_text_repel(aes(`Population with access to improved sanitation`,pred_ac_sani, label = row.names(pca_ac_sani_df)),show.legend = FALSE)

#####dotsize version
ggplot(pca_ac_sani_df,aes( x=`Population with access to improved sanitation`,y=pred_ac_sani, size= `Education Index`))+ 
  geom_point(color="blue")+
  # geom_smooth(method=lm,formula= y~x,color="black")+
  geom_abline(slope = 1,color="red",linetype=2)+
  # scale_x_continuous(limits = c(50, 100))+
  # scale_y_continuous(limits =  c(50, 100))+
  labs(x="Total Population with Access to Improved Sanitation (%)",y= "Model Prediction (%)")+
  scale_size(range = c(5,10))+
  theme_bw()+
  theme(legend.position = "right")+
  theme(axis.title.x = element_text(size=30),
        axis.title.y = element_text(size = 30),
        axis.text.x = element_text(color="#993333", size=14),
        axis.text.y = element_text(color="#993333", size=14))+
  geom_text_repel(aes(`Population with access to improved sanitation`,pred_ac_sani, label = row.names(pca_ac_sani_df)),show.legend = FALSE)




##plot correlations
GGally::ggpairs(ac_dw_df[,1:6])
GGally::ggpairs(ac_dw_df[,10:15])



#svs to health
diarrhea_access <- data %>%
  # dplyr::rename(Parameter = Parameter.) %>% #change column name to exclude period
  #ac_dw,ppt_lt,tot_water_renew,var_interannual,var_sea,flood,drought,gdp,edu_ind,voi_ac,pol_sta,gov_ef,reg_qual,ru_law,con_corru,ru_percent
  dplyr::filter(Parameter. %in% c("Diarrhea as a cause of death for children under 5","Total population with access to safe drinking-water","Population with access to improved sanitation","Gross domestic product (GDP)","Education Index","Voice and Accountability","Political Stability and Absence of Violence/Terrorism","Government Effectiveness","Regulatory Quality","Rule of Law","Control of Corruption","Rural Population","Population")) %>%
  dplyr::select(-Burundi, -Comoros, -Eritrea, -Congo..Dem..Rep., -Equatorial.Guinea, -Seychelles, -Somalia, -South.Sudan, -Somalia,-China,-USA,-`Sri.Lanka`,-India,-Bangladesh)
row.names(diarrhea_access)<-diarrhea_access$Parameter.
diarrhea_access<-diarrhea_access[,-c(1,2,3,4)]


diarrhea_access<- diarrhea_access %>%
  dplyr::select(-Cabo.Verde,-Mauritius,-Sao.Tome.and.Principe) #emit countries not in Africa Continent

##. Create your df with predictors for access to sani alone


##. Explore missing datasets
summary(diarrhea_access) 

#make data frame numeric 
for (i in 1:ncol(diarrhea_access)){
  diarrhea_access[,i]=as.numeric(as.character(diarrhea_access[,i]))
}
diarrhea_access<-as.data.frame(t(diarrhea_access))

ru_percent<-diarrhea_access$`Rural Population`/diarrhea_access$Population*100

model3<-lm(`Diarrhea as a cause of death for children under 5`~.,data = diarrhea_access)
summary(model3)
###combine pc into the dataframe
pc_GI_scores<-pc_water_GI_scores %>%
  dplyr::select("PC1_GI","PC2_GI","PC3_GI","PC4_GI","PC5_GI","PC6_GI")
diarrhea_access_pc_GI<- diarrhea_access %>%
  dplyr::select("Diarrhea as a cause of death for children under 5","Total population with access to safe drinking-water","Population with access to improved sanitation","Gross domestic product (GDP)","Education Index","Rural Population","Population") %>%
  cbind.data.frame(pc_GI_scores) %>%
  cbind.data.frame(ru_percent)

model3.05<-lm(`Diarrhea as a cause of death for children under 5`~.,data = diarrhea_access_pc_GI)
summary(model3.05)
###model 3.05 as check for approapriate pc analysis , good!
##select parameters
model3.1<-regsubsets(`Diarrhea as a cause of death for children under 5`~.-PC3_GI-PC4_GI-PC5_GI-PC6_GI ,data = diarrhea_access_pc_GI,nv=8, method = "forward")
summary(model3.1)


model3.2<-lm(`Diarrhea as a cause of death for children under 5`~.-PC3_GI-PC4_GI-PC5_GI-PC6_GI-Population-ru_percent-`Population with access to improved sanitation`,data = diarrhea_access_pc_GI)
summary(model3.2)

pred_diar_acdw<-predict(model3.2,newdata = diarrhea_access_pc_GI)

ggplot(diarrhea_access_pc_GI, aes( x=`Diarrhea as a cause of death for children under 5`,y=pred_diar_acdw, size=`Total population with access to safe drinking-water`))+
  geom_point(color="orange")+
  # geom_smooth(method=lm,formula= y~x,color="black")+
  geom_abline(slope = 1,color="red",linetype=2)+
  scale_x_continuous(limits = c(7.5, 15))+
  scale_y_continuous(limits = c(7.5, 15))+
  labs(x="Diarrhea as a Cause of Death for Children Under 5 (%)",y= "Model Prediction (%)")+
  theme_bw()+
  theme(legend.position = "right")+
  theme(axis.title.x = element_text(size=30),
        axis.title.y = element_text(size = 30),
        axis.text.x = element_text(color="#993333", size=14),
        axis.text.y = element_text(color="#993333", size=14))+
  scale_size(range = c(3,6))+
  geom_text(aes(`Diarrhea as a cause of death for children under 5`, pred_diar_acdw, label = row.names(diarrhea_access_pc_GI)), show.legend = FALSE)

no_list<-rep(NA,49)
for (i in 1:nrow(svs_all_var)){
  ifelse(row.names(svs_all_var)[i] %in% row.names(ac_dw_df), print(" "), no_list[i]<-row.names(svs_all_var)[i])
}
no_list<-as.character(na.omit(no_list))
complete.cases(no_list)

####energy
ener_pp_df <- data %>%
  dplyr::filter(Parameter. %in% c("Energy supply per capita","Electricity consumption per capita","Electricity consumption","Total Primary Energy Supply (TPES)",
                                  "Causes of Death attributable to Air Pollt",
                                  "DALY attributable to Air Pollt",
                                  "Coal production","Crude oil production","Oil products production","Natural gas production","Nuclear production","Hydro production","Geothermo solar etc production","Biofuel waste production",
                                  "Net imports","energy export",
                                  "Fossil fuel reserves Total","Fossil fuel reserves Natural Gas share","Fossil fuel reserves Oil share",
                                  "Gross domestic product (GDP)","Education Index","Voice and Accountability","Political Stability and Absence of Violence/Terrorism","Government Effectiveness","Regulatory Quality","Rule of Law","Control of Corruption","Rural Population","Population")) %>%
  dplyr::select(-Burundi, -Comoros, -Eritrea, -Congo..Dem..Rep., -Equatorial.Guinea, -Seychelles, -Somalia, -South.Sudan, -Somalia,-China,-USA,-`Sri.Lanka`,-India,-Bangladesh)
row.names(ener_pp_df)<-ener_pp_df$Parameter.
ener_pp_df<-ener_pp_df[,-c(1,2,3,4)]

ener_pp_df<- ener_pp_df %>%
  dplyr::select(-Cabo.Verde,-Mauritius,-Sao.Tome.and.Principe) #emit countries not in Africa

#make dataframe numeric
for (i in 1:ncol(ener_pp_df)){
  ener_pp_df[,i]=as.numeric(as.character(ener_pp_df[,i]))
}
#transpose to make df in row of countries and col of parameters
ener_pp_df<-as.data.frame(t(ener_pp_df))
ener_pp_df<-na.omit(ener_pp_df)

#principal component analysis in governance variables
WBGI_pca_ener_pp<-ener_pp_df %>%
  dplyr::select("Voice and Accountability","Political Stability and Absence of Violence/Terrorism","Government Effectiveness","Regulatory Quality","Rule of Law","Control of Corruption")
pc_WBGI_ener<-prcomp(WBGI_pca_ener_pp)
summary(pc_WBGI_ener) ##two pc captured 92% of variance in governance

pc_ener_GI_scores<-as.data.frame(pc_WBGI_ener$x)
row.names(pc_ener_GI_scores)<-row.names(ener_pp_df)

colnames(pc_ener_GI_scores)<-c("PC1_GI","PC2_GI","PC3_GI","PC4_GI","PC5_GI","PC6_GI")

###select right "utility" proxy which is electricity consumption per capita since the supply per cap was calculated simply by sum up the production by sectors)
pca_ener_pp_df<- ener_pp_df %>%
  dplyr::select("Electricity consumption per capita",
                "Coal production","Crude oil production","Oil products production","Natural gas production",
                "Nuclear production","Hydro production","Geothermo solar etc production","Biofuel waste production",
                "Net imports","energy export",
                "Fossil fuel reserves Total","Fossil fuel reserves Natural Gas share","Fossil fuel reserves Oil share",
                "Gross domestic product (GDP)","Education Index","Rural Population","Population")%>%
  cbind.data.frame(pc_ener_GI_scores)
ru_percent_ener<-pca_ener_pp_df$`Rural Population`/pca_ener_pp_df$Population*100

####check independent variables colinearity of energy raw resources
ener_res_prod<-c("Coal production","Crude oil production","Oil products production","Natural gas production",
                  "Nuclear production","Hydro production","Geothermo solar etc production","Biofuel waste production","Net imports","energy export")
ener_rsv<-c("Fossil fuel reserves Total","Fossil fuel reserves Natural Gas share","Fossil fuel reserves Oil share")
ggpairs(pca_ener_pp_df[,c(ener_res_prod,ener_rsv)])
tot_fos<-pca_ener_pp_df[,c("Coal production","Crude oil production",
           # "Oil products production",
           "Net imports",
           "Natural gas production")]
ggpairs(tot_fos)
tot_non_fos<-pca_ener_pp_df[, c("Nuclear production",
                                "Hydro production",
                                "Geothermo solar etc production",
                                "Biofuel waste production")]
ggpairs(tot_non_fos)

sum_tot_fos<-tot_fos$`Coal production`+tot_fos$`Crude oil production`+tot_fos$`Natural gas production`
# +tot_fos$`Net imports`
sum_tot_fos<-as.data.frame(sum_tot_fos)

sum_tot_non_fos<-tot_non_fos$`Nuclear production`+tot_non_fos$`Hydro production`+ 
  tot_non_fos$`Geothermo solar etc production`+tot_non_fos$`Biofuel waste production`
sum_tot_non_fos<-as.data.frame(sum_tot_non_fos)
plot(sum_tot_fos$sum_tot_fos,sum_tot_non_fos$sum_tot_non_fos,xlab = "Total Fossil Fuel Domestic Production", ylab = "Total Non-Fossil Fuel Domestic Production")
#percentage of non-fossil fuel production in total primary energy supply
per_non_fos<-sum_tot_fos/ener_pp_df$`Total Primary Energy Supply (TPES)`*100

pca_ener_pp_df<-cbind.data.frame(pca_ener_pp_df,sum_tot_fos,sum_tot_non_fos)


############


pca_ener_pp_df<-cbind.data.frame(pca_ener_pp_df,ru_percent_ener)
#prove that the pca is conducted approapriately
# model1.1<-regsubsets(`Total population with access to safe drinking-water`~.-PC3_water-PC4_water-PC5_water-PC6_water-PC3_GI-PC4_GI-PC5_GI-PC6_GI,data= pca_ac_dw_df,nv=8,method = "forward")
# 
###modification 
# model4.1<-regsubsets(`Electricity consumption per capita`~.-PC3_GI-PC4_GI-PC5_GI-PC6_GI, data= pca_ener_pp_df,nv=18,method = "forward")
# summary(model4.1)




# pca_ener_pp_df<-pca_ener_pp_df%>%
#   dplyr::select("Electricity consumption per capita",
#                 "Coal production","Natural gas production","Net imports","Fossil fuel reserves Natural Gas share",
#                 "Geothermo solar etc production","Nuclear production","Hydro production",
#                 "PC1_GI","Gross domestic product (GDP)","ru_percent_ener")
# 

###modification
pca_ener_pp_df<-pca_ener_pp_df%>%
  dplyr::select("Electricity consumption per capita",
                "sum_tot_fos","Net imports","sum_tot_non_fos",
                "PC1_GI","PC2_GI","Gross domestic product (GDP)","ru_percent_ener","Education Index")

model4.1<-regsubsets(`Electricity consumption per capita`~., data= pca_ener_pp_df,nv=18,method = "exhaustive")
summary(model4.1)
model4.2<-lm(`Electricity consumption per capita`~.-`Education Index`,data= pca_ener_pp_df)
summary(model4.2)
model4.3<-lm(`Electricity consumption per capita`~.-`Education Index`-ru_percent_ener,data= pca_ener_pp_df)
summary(model4.3)
model4.4<-lm(`Electricity consumption per capita`~.-`Education Index`-sum_tot_fos-`Gross domestic product (GDP)`,data= pca_ener_pp_df)
summary(model4.4)
#model4.3 has very similar statistics results compare to the 4.4, choose model 4.3 instead due to better individual predictor significance.
pred_elec_cons_pp<-predict(model4.3,newdata = pca_ener_pp_df)
plot(pred_elec_cons_pp,pca_ener_pp_df$`Electricity consumption per capita`)

ggplot(pca_ener_pp_df,aes( x=`Electricity consumption per capita`,y=pred_elec_cons_pp,size=`Net imports`))+ 
  geom_point(color="brown")+
  # geom_smooth(method=lm,formula= y~x,color="black")+
  geom_abline(slope = 1,color="red",linetype=2)+
  # scale_x_continuous(limits = c(50, 100))+
  # scale_y_continuous(limits =  c(50, 100))+
  labs(x="Electricity Consumption Per Capita (kwh/person)",y= "Model Prediction (kwh/person)") + 
  theme_bw()+
  theme(legend.position = "right")+
  theme(axis.title.x = element_text(size=30),
        axis.title.y = element_text(size = 30),
        axis.text.x = element_text(color="#993333", size=14),
        axis.text.y = element_text(color="#993333", size=14))+
  scale_size(range = c(3,10))+
  geom_text_repel(aes(`Electricity consumption per capita`,pred_elec_cons_pp, label = row.names(pca_ener_pp_df)),show.legend = FALSE)
### PC1 Governance in electricity consump is ranging from 0.34 to 0.53, all positive. so pc1 larger means better overall governance.


####remove outliers for South Africa
pca_ener_pp_df<-pca_ener_pp_df[-which(row.names(pca_ener_pp_df) == "South.Africa"),]
###redo pca for pi without S.A.
# ener_pp_df_no_outlier<-ener_pp_df[-which(row.names(ener_pp_df) == "South.Africa"),]
WBGI_pca_ener_pp_N_Otlr<-ener_pp_df[-which(row.names(ener_pp_df) == "South.Africa"),] %>%
  dplyr::select("Voice and Accountability","Political Stability and Absence of Violence/Terrorism","Government Effectiveness","Regulatory Quality","Rule of Law","Control of Corruption")
pc_WBGI_ener_N_Otlr<-prcomp(WBGI_pca_ener_pp_N_Otlr)
summary(pc_WBGI_ener_N_Otlr) ##two pc captured 92% of variance in governance (not much changed from dataset contains South AFrica)

pc_ener_GI_scores_N_Otlr<-as.data.frame(pc_WBGI_ener_N_Otlr$x)
row.names(pc_ener_GI_scores_N_Otlr)<-row.names(ener_pp_df[-which(row.names(ener_pp_df) == "South.Africa"),])

colnames(pc_ener_GI_scores_N_Otlr)<-c("PC1_GI","PC2_GI","PC3_GI","PC4_GI","PC5_GI","PC6_GI")


pca_ener_pp_df<-pca_ener_pp_df%>%
  dplyr::select("Electricity consumption per capita",
                "sum_tot_fos","Net imports","sum_tot_non_fos",
                "Gross domestic product (GDP)","ru_percent_ener","Education Index") %>%
  cbind(pc_ener_GI_scores_N_Otlr[,c("PC1_GI","PC2_GI")])


model4.5 <-regsubsets(`Electricity consumption per capita`~., data= pca_ener_pp_df,nv=18,method = "exhaustive")
summary(model4.5)
model4.6<-lm(`Electricity consumption per capita`~.-sum_tot_non_fos,data= pca_ener_pp_df)
summary(model4.6)
model4.7<-lm(`Electricity consumption per capita`~.-`Gross domestic product (GDP)`-sum_tot_non_fos 
             - ru_percent_ener
             ,data= pca_ener_pp_df)
summary(model4.7)

pred_elec_cons_pp<-predict(model4.7,newdata = pca_ener_pp_df)
plot(pred_elec_cons_pp,pca_ener_pp_df$`Electricity consumption per capita`)

ggplot(pca_ener_pp_df,aes( x=`Electricity consumption per capita`,y=pred_elec_cons_pp,size=`Net imports`))+ 
  geom_point(color="brown")+
  # geom_smooth(method=lm,formula= y~x,color="black")+
  geom_abline(slope = 1,color="red",linetype=2)+
  # scale_x_continuous(limits = c(50, 100))+
  # scale_y_continuous(limits =  c(50, 100))+
  labs(x="Electricity Consumption Per Capita (kwh/person)",y= "Model Prediction (kwh/person)") + 
  theme_bw()+
  theme(legend.position = "right")+
  theme(axis.title.x = element_text(size=30),
        axis.title.y = element_text(size = 30),
        axis.text.x = element_text(color="#993333", size=14),
        axis.text.y = element_text(color="#993333", size=14))+
  scale_size(range = c(3,10))+
  geom_text_repel(aes(`Electricity consumption per capita`,pred_elec_cons_pp, label = row.names(pca_ener_pp_df)),show.legend = FALSE)



###############

pca_ener_health_df<- ener_pp_df %>%
  dplyr::select("Causes of Death attributable to Air Pollt",
                "DALY attributable to Air Pollt",
                "Electricity consumption",
                "Total Primary Energy Supply (TPES)",
                "Energy supply per capita",
                "Electricity consumption per capita",
                "Coal production","Crude oil production","Oil products production","Natural gas production","Nuclear production","Hydro production","Geothermo solar etc production","Biofuel waste production",
                "Net imports","energy export",
                "Fossil fuel reserves Total","Fossil fuel reserves Natural Gas share","Fossil fuel reserves Oil share",
               "Gross domestic product (GDP)","Education Index","Rural Population","Population")%>%
  cbind.data.frame(pc_ener_GI_scores)
# pca_ener_supl_df$`Total Primary Energy Supply (TPES)`<-pca_ener_supl_df$`Total Primary Energy Supply (TPES)`/pca_ener_supl_df$Population

df5.15<-pca_ener_health_df %>%
  dplyr::select(
    # "Causes of Death attributable to Air Pollt",
                "DALY attributable to Air Pollt",
                "Electricity consumption",
                "Total Primary Energy Supply (TPES)",
                # "Energy supply per capita",
                # "Electricity consumption per capita",
                # "Coal production","Crude oil production","Oil products production","Natural gas production","Nuclear production","Hydro production","Geothermo solar etc production","Biofuel waste production",
                # "Net imports","energy export",
                # "Fossil fuel reserves Total","Fossil fuel reserves Natural Gas share","Fossil fuel reserves Oil share",
                # "Gross domestic product (GDP)",
                # "Education Index",
                "Rural Population",
                # "Population",
                "PC1_GI","PC2_GI","PC3_GI","PC4_GI","PC5_GI","PC6_GI")
model5.1<-regsubsets(`Causes of Death attributable to Air Pollt`~.-PC3_GI-PC4_GI-PC5_GI-PC6_GI,data=pca_ener_health_df,nv=18,method="forward")
summary(model5.1)

model5.15<-lm(`DALY attributable to Air Pollt`~.-PC2_GI-PC3_GI-PC4_GI-PC5_GI-PC6_GI,data=df5.15,nv=18,method="forward")
summary(model5.15)

pred_ener_health_AP<-predict(model5.15, newdata = df5.15)

ggplot(df5.15,aes( x=`DALY attributable to Air Pollt`,y=pred_ener_health_AP,size=`Total Primary Energy Supply (TPES)`))+ 
  geom_point(color="Orange")+
  # geom_smooth(method=lm,formula= y~x,color="black")+
  geom_abline(slope = 1,color="red",linetype=2)+
  # scale_x_continuous(limits = c(50, 100))+
  # scale_y_continuous(limits =  c(50, 100))+
  labs(x="DALY: Deaths Due to Air Pollution (person)",y= "Model Prediction (person)") + 
  theme_bw()+
  theme(legend.position = "right")+
  theme(axis.title.x = element_text(size=30),
        axis.title.y = element_text(size = 30),
        axis.text.x = element_text(color="#993333", size=14),
        axis.text.y = element_text(color="#993333", size=14))+
  scale_size(range = c(3,10))+
  geom_text_repel(aes(`DALY attributable to Air Pollt`,pred_ener_health_AP, label = row.names(df5.15)),show.legend = FALSE)



pca_ener_health_df<- pca_ener_health_df %>%
  dplyr::select("Causes of Death attributable to Air Pollt",
                "Electricity consumption",
                "Energy supply per capita",
                "Electricity consumption per capita",
                "Crude oil production","Geothermo solar etc production",
                "Net imports","energy export",
                "Fossil fuel reserves Total","Fossil fuel reserves Oil share",
                "Gross domestic product (GDP)","Education Index","Rural Population","PC1_GI")

model5.2<-lm(`Causes of Death attributable to Air Pollt`~.,data=pca_ener_health_df)
summary(model5.2)

model5.3<-lm(`Causes of Death attributable to Air Pollt`~.-`Gross domestic product (GDP)`-`Education Index`-`Fossil fuel reserves Total`,data=pca_ener_health_df)
summary(model5.3)

####There is less relationships between the energy svs proxies and health, however, there are strong connections between sustainable energy, oil reliance to air pollution health problems.


plot(pca_ener_health_df$`Fossil fuel reserves Oil share`,pca_ener_health_df$`Causes of Death attributable to Air Pollt`)
####for country has higher percentaage of oil in their reserve has higher death rate due to air pollution



######Food Section

food_defi_df<- data %>%
  dplyr::filter(Parameter. %in% c(
    "% of total country area cultivated (%)",
    # "Agricultural Area",
    "Arable land",
    "Permanent crops",
    
    "Average dietary energy supply adequacy",
    "Average value of food production",
    "Share of dietary energy supply derived from cereals, roots, and tubers",
    "Average protein supply",
    "Average supply of protein of animal origin",
    
    "Rail lines density",
    
    "Prevalence of undernourishment",
    "Depth of the food deficit",
    "Percentage of children under 5 years of age who are stunted",
    "Percentage of children under 5 years of age affected by wasting",
    "Percentage of children under 5 years of age who are underweight",
    "Percentage of children under 5 years of age who are overweight",
    
    "Per capita food production variability",
    "Per capita food supply variability",
    "Value of food imports over total merchandise exports",
    "Percent of arable land equipped for irrigation",
    "Cereal import dependency ratio",
    "Land degradation (% territory)",
    "Gross domestic product (GDP)","Education Index","Voice and Accountability","Political Stability and Absence of Violence/Terrorism","Government Effectiveness","Regulatory Quality","Rule of Law","Control of Corruption","Rural Population","Population"
    # 
    # "Prevalence of severe food insecurity in the total population",
    # "Number of severely food insecure people"
    # 
  ))  %>%
  dplyr::select(-Burundi, -Comoros, -Eritrea, -Congo..Dem..Rep., -Equatorial.Guinea, -Seychelles, -Somalia, -South.Sudan, -Somalia,
                -China,-USA,-`Sri.Lanka`,-India,-Bangladesh)
row.names(food_defi_df)<-food_defi_df$Parameter.
food_defi_df<-food_defi_df[,-c(1,2,3,4)]

food_defi_df<- food_defi_df %>%
  dplyr::select(-Cabo.Verde,-Mauritius,-Sao.Tome.and.Principe) #emit countries not in Africa

#make dataframe numeric
for (i in 1:ncol(food_defi_df)){
  food_defi_df[,i]=as.numeric(as.character(food_defi_df[,i]))
}
#transpose to make df in row of countries and col of parameters
food_defi_df<-as.data.frame(t(food_defi_df))

food_defi_df_rail<-na.omit(food_defi_df)

###rail line density missing too much values so just remove for now.
food_defi_df<-food_defi_df%>%
  dplyr::select(-`Rail lines density`)
food_defi_df<-na.omit(food_defi_df)




#principal component analysis in governance variables
WBGI_pca_food_defi<-food_defi_df %>%
  dplyr::select("Voice and Accountability","Political Stability and Absence of Violence/Terrorism","Government Effectiveness","Regulatory Quality","Rule of Law","Control of Corruption")
pc_WBGI_food<-prcomp(WBGI_pca_food_defi)
summary(pc_WBGI_food) ###two pc captured 84% of variance for the food sector

pc_food_GI_scores<-as.data.frame(pc_WBGI_food$x)
row.names(pc_food_GI_scores)<-row.names(food_defi_df)

colnames(pc_food_GI_scores)<-c("PC1_GI","PC2_GI","PC3_GI","PC4_GI","PC5_GI","PC6_GI")

food_pca_health_proxy<-food_defi_df %>%
  dplyr::select(
    # "Prevalence of undernourishment",
    "Percentage of children under 5 years of age who are stunted",
    "Percentage of children under 5 years of age affected by wasting",
    "Percentage of children under 5 years of age who are underweight",
    "Percentage of children under 5 years of age who are overweight"
  )
pc_food_heal<-prcomp(food_pca_health_proxy)
summary(pc_food_heal)
pc_food_heal
pc_U5nourish_score<-pc_food_heal$x
colnames(pc_U5nourish_score)<-c("PC1_U5nourish","PC2_U5nourish","PC3_U5nourish","PC4_U5nourish")
ggpairs(food_defi_df[,20:23])
##we se that The U5 predictors are highly correlated. so we use 1 pc to capture 69 percent of the total variance as U5 proxy


pca_food_defi_df<-food_defi_df %>%
  dplyr::select(
    "Depth of the food deficit",
    
    "% of total country area cultivated (%)",
    # "Agricultural Area",
    "Arable land",
    "Permanent crops",
    
    # "Average dietary energy supply adequacy",
    # "Average value of food production",
    # "Share of dietary energy supply derived from cereals, roots, and tubers",
    # "Average protein supply",
    # "Average supply of protein of animal origin",
    # 
    "Per capita food production variability",
    "Per capita food supply variability",
    "Value of food imports over total merchandise exports",
    "Percent of arable land equipped for irrigation",
    "Cereal import dependency ratio",
    "Land degradation (% territory)",
    "Gross domestic product (GDP)","Education Index"
    # "Prevalence of severe food insecurity in the total population",
    # "Number of severely food insecure people"
    # 
    ) %>%
  cbind.data.frame(pc_food_GI_scores)

model6.1<-regsubsets(`Depth of the food deficit`~.-PC3_GI-PC4_GI-PC5_GI-PC6_GI, data = pca_food_defi_df, method="forward")
summary(model6.1)



model6.2<-lm(`Depth of the food deficit`~.-PC3_GI-PC4_GI-PC5_GI-PC6_GI, data = pca_food_defi_df)
summary(model6.2)

df6.3<-pca_food_defi_df %>%
  dplyr::select(
    "Depth of the food deficit",
    
    "% of total country area cultivated (%)",
    "Arable land",
    # "Permanent crops",
    
    # "Average dietary energy supply adequacy",
    # "Average value of food production",
    # "Share of dietary energy supply derived from cereals, roots, and tubers",
    # "Average protein supply",
    # "Average supply of protein of animal origin",
    # 
    "Per capita food production variability",
    # "Per capita food supply variability",
    # "Value of food imports over total merchandise exports",
    "Percent of arable land equipped for irrigation",
    "Cereal import dependency ratio",
    "Land degradation (% territory)",
    "Gross domestic product (GDP)",
    # "Education Index"
  
    "PC1_GI","PC2_GI"
    # ,"PC3_GI","PC4_GI","PC5_GI","PC6_GI"
    # "Prevalence of severe food insecurity in the total population",
    # "Number of severely food insecure people"
    # 
  )

model6.3<-lm(`Depth of the food deficit`~.-`Arable land`-PC2_GI-`Percent of arable land equipped for irrigation`-PC1_GI, data=df6.3)
summary(model6.3)

####food subcategory data manipulation
food_resources_idpt_var<-c(#land reources availability
                  "% of total country area cultivated (%)",
                  "Arable land",
                  "Permanent crops",
                  "Percent of arable land equipped for irrigation",
                  "Land degradation (% territory)",
                  
                 
                  
                  # #dependent variables
                  # "Depth of the food deficit",
                  #access
                  # "Rail lines density",
                  "Cereal import dependency ratio",
                  "Value of food imports over total merchandise exports",
                  "Average value of food production",
                  
                  # #health
                  # "Percentage of children under 5 years of age who are stunted",
                  # "Percentage of children under 5 years of age affected by wasting",
                  # "Percentage of children under 5 years of age who are underweight",
                  # "Percentage of children under 5 years of age who are overweight",
                  
                  #variability
                  "Per capita food production variability",
                  "Per capita food supply variability")

nutri_bal<-c("Share of dietary energy supply derived from cereals, roots, and tubers",
                 "Average protein supply",
                 "Average supply of protein of animal origin")

food_util<-c("Average dietary energy supply adequacy",
             "Prevalence of undernourishment",
             "Depth of the food deficit")

food_human_capacity<-c("Rail lines density", 
                    "Gross domestic product (GDP)",
                    "Education Index",
                    # "Voice and Accountability",
                    # "Political Stability and Absence of Violence/Terrorism",
                    # "Government Effectiveness",
                    # "Regulatory Quality",
                    # "Rule of Law",
                    # "Control of Corruption",
                    "Rural Population",
                    "Population")

ggpairs(food_defi_df_rail[,food_resources_idpt_var])

ggpairs(food_defi_df_rail[,nutri_bal])

ggpairs(food_defi_df_rail[,food_util])

ggpairs(food_defi_df_rail[,food_human_capacity])



###with rail line density
WBGI_pca_food_defi_rail<-food_defi_df_rail %>%
  dplyr::select("Voice and Accountability","Political Stability and Absence of Violence/Terrorism","Government Effectiveness","Regulatory Quality","Rule of Law","Control of Corruption")
pc_WBGI_food_rail<-prcomp(WBGI_pca_food_defi_rail)
summary(pc_WBGI_food_rail) ###two pc captured 84% of variance for the food sector

pc_food_GI_scores_rail<-as.data.frame(pc_WBGI_food_rail$x)
row.names(pc_food_GI_scores_rail)<-row.names(food_defi_df_rail)

colnames(pc_food_GI_scores_rail)<-c("PC1_GI","PC2_GI","PC3_GI","PC4_GI","PC5_GI","PC6_GI")

food_pca_health_proxy_rail<-food_defi_df_rail %>%
  dplyr::select(
    # "Prevalence of undernourishment",
    "Percentage of children under 5 years of age who are stunted",
    "Percentage of children under 5 years of age affected by wasting",
    "Percentage of children under 5 years of age who are underweight",
    "Percentage of children under 5 years of age who are overweight"
  )
pc_food_heal_rail<-prcomp(food_pca_health_proxy_rail)
summary(pc_food_heal_rail)
pc_food_heal_rail
pc_U5nourish_score_rail<-pc_food_heal_rail$x
colnames(pc_U5nourish_score_rail)<-c("PC1_U5nourish","PC2_U5nourish","PC3_U5nourish","PC4_U5nourish")
ggpairs(food_defi_df_rail[,20:23])
##we se that The U5 predictors are highly correlated. so we use 1 pc to capture 69 percent of the total variance as U5 proxy


pca_food_defi_df_rail<-food_defi_df_rail %>%
  dplyr::select(
    "Depth of the food deficit",
    
    "% of total country area cultivated (%)",
    # "Agricultural Area",
    "Arable land",
    "Permanent crops",
    
    # "Average dietary energy supply adequacy",
    # "Average value of food production",
    # "Share of dietary energy supply derived from cereals, roots, and tubers",
    # "Average protein supply",
    # "Average supply of protein of animal origin",
    # 
    "Per capita food production variability",
    "Per capita food supply variability",
    "Value of food imports over total merchandise exports",
    "Percent of arable land equipped for irrigation",
    "Cereal import dependency ratio",
    "Land degradation (% territory)",
    "Gross domestic product (GDP)",
    "Rail lines density",
    "Education Index"
    # "Prevalence of severe food insecurity in the total population",
    # "Number of severely food insecure people"
    # 
  ) %>%
  cbind.data.frame(pc_food_GI_scores_rail)

model7.1<-regsubsets(`Depth of the food deficit`~.-PC3_GI-PC4_GI-PC5_GI-PC6_GI, data = pca_food_defi_df_rail, method="exhaustive",nv=9)
summary(model7.1)



model7.2<-lm(`Depth of the food deficit`~.-PC3_GI-PC4_GI-PC5_GI-PC6_GI, data = pca_food_defi_df_rail)
summary(model7.2)

df7.3<-pca_food_defi_df_rail %>%
  dplyr::select(
    "Depth of the food deficit",
    
    "% of total country area cultivated (%)",
    "Arable land",
    "Permanent crops",
    "Rail lines density",
    # "Average dietary energy supply adequacy",
    # "Average value of food production",
    # "Share of dietary energy supply derived from cereals, roots, and tubers",
    # "Average protein supply",
    # "Average supply of protein of animal origin",
    # 
    "Per capita food production variability",
    "Per capita food supply variability",
    "Value of food imports over total merchandise exports",
    "Percent of arable land equipped for irrigation",
    "Cereal import dependency ratio",
    "Land degradation (% territory)",
    "Gross domestic product (GDP)",
    "Education Index",
    
    "PC1_GI",
    "PC2_GI"
    # ,"PC3_GI","PC4_GI","PC5_GI","PC6_GI"
    # "Prevalence of severe food insecurity in the total population",
    # "Number of severely food insecure people"
    # 
  )

model7.3<-lm(`Depth of the food deficit`~.
             -PC1_GI
             -`Land degradation (% territory)`
             -`Percent of arable land equipped for irrigation`
             -`Per capita food supply variability`
             -`Per capita food production variability`
             -`% of total country area cultivated (%)`,data=df7.3)
summary(model7.3)

model7.4<-lm(`Depth of the food deficit`~.
             -PC1_GI
             -`Land degradation (% territory)`
             -`Per capita food supply variability`
             -`Per capita food production variability`
             -`Cereal import dependency ratio`,data=df7.3)
summary(model7.4)

pred_food_defi_rail<-predict(model7.4, newdata = df7.3)

ggplot(df7.3,aes( x=`Depth of the food deficit`,y=pred_food_defi_rail,size=`Rail lines density`))+ 
  geom_point(color="green")+
  # geom_smooth(method=lm,formula= y~x,color="black")+
  geom_abline(slope = 1,color="red",linetype=2)+
  # scale_x_continuous(limits = c(50, 100))+
  # scale_y_continuous(limits =  c(50, 100))+
  labs(x="Depth of the Food Deficit (kcal/capita/day, 3yr avg)",y= "Model Prediction (kcal/capita/day, 3yr avg)") + 
  theme_bw()+
  theme(legend.position = "right")+
  theme(axis.title.x = element_text(size=30),
        axis.title.y = element_text(size = 30),
        axis.text.x = element_text(color="#993333", size=14),
        axis.text.y = element_text(color="#993333", size=14))+
  scale_size(range = c(3,10))+
  geom_text_repel(aes(`Depth of the food deficit`,pred_food_defi_rail, label = row.names(df7.3)),show.legend = FALSE)

####ultilization to health

pca_food_health_df<-food_defi_df_rail %>%
  dplyr::select(
    "Depth of the food deficit",
    "Average dietary energy supply adequacy",
    "Average value of food production",
    "Share of dietary energy supply derived from cereals, roots, and tubers",
    "Average protein supply",
    "Average supply of protein of animal origin",

    "Per capita food production variability",
    "Per capita food supply variability",
    # "Value of food imports over total merchandise exports",
    # "Percent of arable land equipped for irrigation",
    # "Cereal import dependency ratio",
    # "Land degradation (% territory)",
    "Gross domestic product (GDP)","Education Index"
    # "Prevalence of severe food insecurity in the total population",
    # "Number of severely food insecure people"
    # 
  ) %>%
  cbind.data.frame(pc_food_GI_scores_rail)%>%
  cbind.data.frame(pc_U5nourish_score_rail)
pca_food_health_df<- pca_food_health_df %>%
  dplyr::select(-PC3_GI,-PC4_GI,-PC5_GI,-PC6_GI,-PC2_U5nourish,-PC3_U5nourish,-PC4_U5nourish)

model8.1<-regsubsets(PC1_U5nourish~., data = pca_food_health_df, method = "exhaustive", nv=7)
summary(model8.1)

model8.2<-lm(PC1_U5nourish~.
             -PC2_GI
             -PC1_GI
             -`Per capita food production variability`
             -`Average dietary energy supply adequacy`
             -`Average value of food production`, data = pca_food_health_df)
summary(model8.2)

pred_U5nourish<-predict(model8.2, newdata = pca_food_health_df)

ggplot(pca_food_health_df, aes( x=PC1_U5nourish,y=pred_U5nourish, size=`Depth of the food deficit`))+ 
  geom_point(color="orange")+
  # geom_smooth(method=lm,formula= y~x,color="black")+
  geom_abline(slope = 1,color="red",linetype=2)+
  # scale_x_continuous(limits = c(50, 100))+
  # scale_y_continuous(limits =  c(50, 100))+
  labs(x="   PC1 Scores of Children Under 5 have Food-related Health Problems",y= "Model Prediction") + 
  theme_bw()+
  theme(legend.position = "right")+
  theme(axis.title.x = element_text(size=21),
        axis.title.y = element_text(size = 30),
        axis.text.x = element_text(color="#993333", size=14),
        axis.text.y = element_text(color="#993333", size=14))+
  scale_size(range = c(3,10))+
  geom_text_repel(aes(PC1_U5nourish,pred_U5nourish, label = row.names(pca_food_health_df)),show.legend = FALSE)


plot(pca_food_health_df$`Education Index`,pca_food_health_df$PC1_U5nourish)


####countries jointly in enery and food sector
yes_list<-rep(NA,nrow(food_defi_df_rail))
no_list<-rep(NA,nrow(food_defi_df_rail))
for (i in 1:nrow(ener_pp_df)){
  ifelse(row.names(ener_pp_df)[i] %in% row.names(food_defi_df_rail), yes_list[i]<-row.names(food_defi_df_rail)[i], no_list[i]<-row.names(food_defi_df_rail)[i])
}
no_list_ener_food<-no_list[complete.cases(no_list)]
yes_list_ener_food<-yes_list[complete.cases(yes_list)]

###countries jointly in water and food sector
yes_list<-rep(NA,nrow(food_defi_df_rail))
no_list<-rep(NA,nrow(food_defi_df_rail))
for (i in 1:nrow(ac_dw_df)){
  ifelse(row.names(ac_dw_df)[i] %in% row.names(food_defi_df_rail), yes_list[i]<-row.names(ac_dw_df)[i], no_list[i]<-row.names(ac_dw_df)[i])
}
no_list_water_food<-no_list[complete.cases(no_list)]
yes_list_water_food<-yes_list[complete.cases(yes_list)]

###countries jointly in water and energy sector
yes_list<-rep(NA,nrow(ener_pp_df))
no_list<-rep(NA,nrow(ener_pp_df))
for (i in 1:nrow(ac_dw_df)){
  ifelse(row.names(ac_dw_df)[i] %in% row.names(ener_pp_df), yes_list[i]<-row.names(ac_dw_df)[i], no_list[i]<-row.names(ac_dw_df)[i])
}
no_list_water_energy<-no_list[complete.cases(no_list)]
yes_list_water_energy<-yes_list[complete.cases(yes_list)]

###countries jointly in food and energy sector
yes_list<-rep(NA,nrow(food_defi_df_rail))
no_list<-rep(NA,nrow(food_defi_df_rail))
for (i in 1:nrow(ener_pp_df)){
  ifelse(row.names(ener_pp_df)[i] %in% row.names(food_defi_df_rail), yes_list[i]<-row.names(ener_pp_df)[i], no_list[i]<-row.names(ener_pp_df)[i])
}
no_list_food_energy<-no_list[complete.cases(no_list)]
yes_list_food_energy<-yes_list[complete.cases(yes_list)]


####water_energy link
water_energy_df<-cbind.data.frame(ac_dw_df[yes_list_water_energy,], ener_pp_df[yes_list_water_energy,]) %>%
  cbind.data.frame(ac_sani_df[yes_list_water_energy,]$`Population with access to improved sanitation`)
water_energy_df<-water_energy_df[,-(17:26)]
colnames(water_energy_df)[37]<-c("Population with access to improved sanitation")


water_ener_av_pca<-water_energy_df %>%
  dplyr::select("Long-term annual precip depth",
                "Total renewable water sources",
                "Interannual variability",
                "Seasonal variability",
                "Flood occurrence",
                "Drought frequency")

pc_water_ener<-prcomp(water_ener_av_pca, scale= TRUE, center = TRUE)
summary(pc_water_ener)

pc_water_ener_av_scores<-pc_water_ener$x
colnames(pc_water_ener_av_scores)<-c("PC1_water","PC2_water","PC3_water","PC4_water","PC5_water","PC6_water")#two pc captured 70% of the total variance in water availability

ru_percent<-water_energy_df$`Rural Population`/water_energy_df$Population*100

elec_cons_water_sec<-water_energy_df %>%
  dplyr::select("Electricity consumption per capita",
                "Gross domestic product (GDP)",
                "Education Index") %>%
  cbind.data.frame(pc_ener_GI_scores)%>%
  cbind.data.frame(pc_water_ener_av_scores)%>%
  cbind.data.frame(ru_percent)
  
elec_cons_water_sec<-elec_cons_water_sec %>%
  dplyr::select(-PC3_GI,-PC4_GI,-PC5_GI,-PC6_GI,-PC3_water,-PC4_water,-PC5_water,-PC6_water)
model9.1<-regsubsets(`Electricity consumption per capita`~.,data = elec_cons_water_sec, method="exhaustive",nv=7)
summary(model9.1)

model9.2<-lm(`Electricity consumption per capita`~.-PC1_GI
             -PC2_GI-PC2_water,data = elec_cons_water_sec)
summary(model9.2)

pred_elec_cons_pp_water<-predict(model9.2, newdata =  elec_cons_water_sec)

ggplot(elec_cons_water_sec,aes( x=`Electricity consumption per capita`,y=pred_elec_cons_pp_water, size=ru_percent))+ 
  geom_point(color="brown")+
  # geom_smooth(method=lm,formula= y~x,color="black")+
  geom_abline(slope = 1,color="red",linetype=2)+
  # scale_x_continuous(limits = c(50, 100))+
  # scale_y_continuous(limits =  c(50, 100))+
  labs(x="Electricity Consumption Per Capita (kwh/person)",y= "Model Prediction (kwh/person)") + 
  theme_bw()+
  theme(legend.position = "right")+
  theme(axis.title.x = element_text(size=30),
        axis.title.y = element_text(size = 30),
        axis.text.x = element_text(color="#993333", size=14),
        axis.text.y = element_text(color="#993333", size=14))+
  scale_size(range = c(3,10))+
  geom_text_repel(aes(`Electricity consumption per capita`,pred_elec_cons_pp_water, label = row.names(elec_cons_water_sec)),show.legend = FALSE)






####water_food link
water_food_df<-cbind.data.frame(ac_dw_df[yes_list_water_food,], food_defi_df_rail[yes_list_water_food,]) %>%
  cbind.data.frame(ac_sani_df[yes_list_water_food,]$`Population with access to improved sanitation`)
water_food_df<-water_food_df[,-(8:17)]#remove redundant variables
colnames(water_food_df)[39]<-c("Population with access to improved sanitation")


water_food_av_pca<-water_food_df %>%
  dplyr::select("Long-term annual precip depth",
                "Total renewable water sources",
                "Interannual variability",
                "Seasonal variability",
                "Flood occurrence",
                "Drought frequency")

pc_water_food<-prcomp(water_food_av_pca, scale= TRUE, center = TRUE)
summary(pc_water_food)

pc_water_food_av_scores<-pc_water_food$x
colnames(pc_water_food_av_scores)<-c("PC1_water","PC2_water","PC3_water","PC4_water","PC5_water","PC6_water")#two pc captured 70% of the total variance in water availability

ru_percent<-water_food_df$`Rural Population`/water_food_df$Population*100

defi_food_water_sec<-water_food_df %>%
  dplyr::select("Depth of the food deficit",
                "Gross domestic product (GDP)",
                "Education Index") %>%
  cbind.data.frame(pc_food_GI_scores_rail)%>%
  cbind.data.frame(pc_water_food_av_scores)%>%
  cbind.data.frame(ru_percent)

defi_food_water_sec<-defi_food_water_sec %>%
  dplyr::select(-PC3_GI,-PC4_GI,-PC5_GI,-PC6_GI,-PC3_water,-PC4_water,-PC5_water,-PC6_water)
model10.1<-regsubsets(`Depth of the food deficit`~.,data = defi_food_water_sec, method="exhaustive",nv=7)
summary(model10.1)

model10.2<-lm(`Depth of the food deficit`~.
              -`Gross domestic product (GDP)`
             -PC2_water
             ,data = defi_food_water_sec)
summary(model10.2)

pred_food_defi_water<-predict(model10.2, newdata =  defi_food_water_sec)

ggplot(defi_food_water_sec,aes( x=`Depth of the food deficit`,y=pred_food_defi_water, size=ru_percent))+ 
  geom_point(color="green")+
  # geom_smooth(method=lm,formula= y~x,color="black")+
  geom_abline(slope = 1,color="red",linetype=2)+
  # scale_x_continuous(limits = c(50, 100))+
  # scale_y_continuous(limits =  c(50, 100))+
  labs(x="Depth of the Food Deficit (kcal/capita/day, 3yr avg)",y= "Model Prediction (kcal/capita/day)") + 
  theme_bw()+
  theme(legend.position = "right")+
  theme(axis.title.x = element_text(size=30),
        axis.title.y = element_text(size = 30),
        axis.text.x = element_text(color="#993333", size=14),
        axis.text.y = element_text(color="#993333", size=14))+
  scale_size(range = c(3,10))+
  geom_text_repel(aes(`Depth of the food deficit`,pred_food_defi_water, label = row.names(defi_food_water_sec)),show.legend = FALSE)





####food_energy link
food_energy_df<-cbind.data.frame(ener_pp_df[yes_list_food_energy,], food_defi_df_rail[yes_list_food_energy,])
food_energy_df<-food_energy_df[,-(1:9)]#remove redundant variables
food_energy_df<-food_energy_df[,-20]#remove redundant variables
colnames(food_energy_df)[50]<-c("Rural Population")

ru_percent<-food_energy_df$`Rural Population`/food_energy_df$Population*100

pc_WBGI_food_energy<-prcomp(food_energy_df%>%
                              dplyr::select(
                                "Voice and Accountability",
                                "Political Stability and Absence of Violence/Terrorism",
                                "Government Effectiveness",
                                "Regulatory Quality",
                                "Rule of Law",
                                "Control of Corruption"
                              )) 
summary(pc_WBGI_food_energy)###two pcs captured 94% of total variance.
pc_GI_scores_food_ener<-pc_WBGI_food_energy$x
colnames(pc_GI_scores_food_ener)<-c("PC1_GI","PC2_GI","PC3_GI","PC4_GI","PC5_GI","PC6_GI")
elec_cons_food_sec<-food_energy_df %>%
  dplyr::select("Electricity consumption per capita",
    "% of total country area cultivated (%)",
    "Arable land",
    "Permanent crops",
    "Rail lines density",
    "Per capita food production variability",
    "Per capita food supply variability",
    "Value of food imports over total merchandise exports",
    "Percent of arable land equipped for irrigation",
    "Cereal import dependency ratio",
    "Land degradation (% territory)",
    "Gross domestic product (GDP)",
    "Education Index") %>%
  cbind.data.frame(pc_GI_scores_food_ener)


elec_cons_food_sec<-elec_cons_food_sec %>%
  dplyr::select(-PC3_GI,-PC4_GI,-PC5_GI,-PC6_GI)

model11.1<-regsubsets(`Electricity consumption per capita`~.,data = elec_cons_food_sec, method="exhaustive",nv=5)
summary(model11.1)

model11.2<-lm(`Electricity consumption per capita`~
                `Rail lines density`+
                `Per capita food production variability`+
                `Education Index`+
                PC2_GI
              ,data = elec_cons_food_sec)
summary(model11.2)

model11.3<-lm(`Electricity consumption per capita`~
                `Rail lines density`+
                `Per capita food production variability`+
                `Education Index`+
                `% of total country area cultivated (%)`+
                `Percent of arable land equipped for irrigation`
              ,data = elec_cons_food_sec)
summary(model11.3)

pred_elec_cons_food<-predict(model11.3, newdata =  elec_cons_food_sec)

ggplot(elec_cons_food_sec,aes( x=`Electricity consumption per capita`,y=pred_elec_cons_food, size=`Education Index` ))+ 
  geom_point(color="brown")+
  # geom_smooth(method=lm,formula= y~x,color="black")+
  geom_abline(slope = 1,color="red",linetype=2)+
  # scale_x_continuous(limits = c(50, 100))+
  # scale_y_continuous(limits =  c(50, 100))+
  labs(x="Electricity Consumption Per Capita (kwh/person)",y= "Model Prediction (kwh/person)") + 
  theme_bw()+
  theme(legend.position = "right")+
  theme(axis.title.x = element_text(size=30),
        axis.title.y = element_text(size = 30),
        axis.text.x = element_text(color="#993333", size=14),
        axis.text.y = element_text(color="#993333", size=14))+
  scale_size(range = c(3,10))+
  geom_text_repel(aes(`Electricity consumption per capita`, pred_elec_cons_food, label = row.names(elec_cons_food_sec)),show.legend = FALSE)



########Energy-Water linkages


ru_percent<-water_energy_df$`Rural Population`/water_energy_df$Population*100

ac_dw_ener_sec<-water_energy_df %>%
  dplyr::select("Total population with access to safe drinking-water",
                # "Population with access to improved sanitation",
                # "DALY attributable to Air Pollt",
                "Coal production","Crude oil production","Oil products production","Natural gas production","Nuclear production","Hydro production","Geothermo solar etc production","Biofuel waste production",
                "Net imports","energy export",
                "Fossil fuel reserves Total","Fossil fuel reserves Natural Gas share","Fossil fuel reserves Oil share",
                "Gross domestic product (GDP)","Education Index","Rural Population","Population") %>%
  cbind.data.frame(pc_ener_GI_scores)%>%
  cbind.data.frame(ru_percent) %>%
  dplyr::select(-PC3_GI,-PC4_GI,-PC5_GI,-PC6_GI)

ac_sani_ener_sec<-water_energy_df %>%
  dplyr::select("Population with access to improved sanitation",
                # "DALY attributable to Air Pollt",
                "Coal production","Crude oil production","Oil products production","Natural gas production","Nuclear production","Hydro production","Geothermo solar etc production","Biofuel waste production",
                "Net imports","energy export",
                "Fossil fuel reserves Total","Fossil fuel reserves Natural Gas share","Fossil fuel reserves Oil share",
                "Gross domestic product (GDP)","Education Index","Rural Population","Population") %>%
  cbind.data.frame(pc_ener_GI_scores)%>%
  cbind.data.frame(ru_percent) %>%
  dplyr::select(-PC3_GI,-PC4_GI,-PC5_GI,-PC6_GI)


model12.1<-regsubsets(`Total population with access to safe drinking-water`~.,data = ac_dw_ener_sec, method="exhaustive",nv=6)
summary(model12.1)

model12.2<-lm(`Total population with access to safe drinking-water`~.
              -`Coal production`  
              -`Hydro production`  
              -`Crude oil production`
              -`energy export`
              -Population
              -`Rural Population`
              -`Oil products production`
              -PC2_GI   
              -`Education Index` 
              -`Geothermo solar etc production` 
              -`Net imports` 
              # -`Gross domestic product (GDP)`
              # -`Fossil fuel reserves Total`
              # -`Fossil fuel reserves Total`
              #  -`energy export`
              #  -`Biofuel waste production`
              # -`Geothermo solar etc production`
              # -`Hydro production`
              # -`Oil products production`
              ,data = ac_dw_ener_sec)
summary(model12.2)

pred_ac_dw_energy_sec<-predict(model12.2, newdata =  ac_dw_ener_sec)

ggplot(ac_dw_ener_sec, aes( x=`Total population with access to safe drinking-water`,y=pred_ac_dw_energy_sec, size=`Nuclear production`))+ 
  geom_point(color="blue")+
  # geom_smooth(method=lm,formula= y~x,color="black")+
  geom_abline(slope = 1,color="red",linetype=2)+
  # scale_x_continuous(limits = c(50, 100))+
  # scale_y_continuous(limits =  c(50, 100))+
  labs(x="Total Population with Access to Safe Drinking Water (%)",y= "Model Prediction (%)") + 
  theme_bw()+
  theme(legend.position = "right")+
  theme(axis.title.x = element_text(size=30),
        axis.title.y = element_text(size = 30),
        axis.text.x = element_text(color="#993333", size=14),
        axis.text.y = element_text(color="#993333", size=14))+
  scale_size(range = c(3,10))+
  geom_text_repel(aes(`Total population with access to safe drinking-water`, pred_ac_dw_energy_sec, label = row.names(ac_dw_ener_sec)),show.legend = FALSE)

plot(ac_dw_ener_sec$`Nuclear production`,ac_dw_ener_sec$`Total population with access to safe drinking-water`)

model13.1<-regsubsets(`Population with access to improved sanitation`~.,data = ac_sani_ener_sec, method="exhaustive",nv=14)
summary(model13.1)

model13.2<-lm(`Population with access to improved sanitation`~.
              -`Coal production`
              # -`Hydro production`  
              # -`Crude oil production`
              # -`energy export`
              -Population
              -`Rural Population`
              -`Oil products production`
              # -PC2_GI   
              # -`Education Index` 
              -`Geothermo solar etc production`
              -`Fossil fuel reserves Natural Gas share`
              # -`Fossil fuel reserves Oil share`
              -PC2_GI
              # -`Education Index`
              # -`Net imports` 
              # -`Gross domestic product (GDP)`
              # -`Fossil fuel reserves Total`
              
              #  -`energy export`
              #  -`Biofuel waste production`
              # -`Geothermo solar etc production`
              -`Hydro production`
              # -`Oil products production`
              ,data = ac_sani_ener_sec)
summary(model13.2)

pred_ac_sani_energy_sec<-predict(model13.2, newdata =  ac_sani_ener_sec)

ggplot(ac_sani_ener_sec, aes(x=`Population with access to improved sanitation`, 
                             y=pred_ac_sani_energy_sec, 
                             size=`Gross domestic product (GDP)`))+ 
  geom_point(color="blue")+
  # geom_smooth(method=lm,formula= y~x,color="black")+
  geom_abline(slope = 1, color="red", linetype=2)+
  scale_x_continuous(limits = c(0, 100))+
  scale_y_continuous(limits =  c(0, 100))+
  labs(x="Population with Access to Improved Sanitation (%)",y= "Model Prediction (%)") + 
  theme_bw()+
  theme(legend.position = "right")+
  theme(axis.title.x = element_text(size=30),
        axis.title.y = element_text(size = 30),
        axis.text.x = element_text(color="#993333", size=14),
        axis.text.y = element_text(color="#993333", size=14))+
  scale_size(range = c(3,10)) +
  geom_text_repel(aes(`Population with access to improved sanitation`, 
                      pred_ac_sani_energy_sec, 
                      label = row.names(ac_sani_ener_sec)),show.legend = FALSE)



####WaterSVStoFoodHealth
ru_percent<-water_food_df$`Rural Population`/water_food_df$Population*100

U5nourish_water_svs<- water_food_df %>%
  dplyr::select("Total population with access to safe drinking-water",
                "Population with access to improved sanitation",
                "Gross domestic product (GDP)",
                "Education Index",
                "Population",
                "Rural Population")%>%
  cbind.data.frame(pc_food_GI_scores_rail %>%
                     dplyr::select(PC1_GI,PC2_GI)) %>%
  cbind.data.frame(ru_percent) %>%
  cbind.data.frame(pca_food_health_df %>%
                     dplyr::select(PC1_U5nourish))


                   

model14.1<-regsubsets(PC1_U5nourish~.-Population-`Rural Population`, data = U5nourish_water_svs, method = "exhaustive", nv=9)
summary(model14.1)

model14.2<-lm(PC1_U5nourish~.-Population-`Rural Population`
              -PC2_GI
              -ru_percent
              , data = U5nourish_water_svs)
summary(model14.2)

###p-Value is not significant.

####FoodSVStoWaterHealth

ru_percent<-water_food_df$`Rural Population`/water_food_df$Population*100

diar_food_svs<- water_food_df %>%
  dplyr::select("Depth of the food deficit",
                "Average dietary energy supply adequacy",
                "Average value of food production",
                "Share of dietary energy supply derived from cereals, roots, and tubers",
                "Average protein supply",
                "Average supply of protein of animal origin",
                "Gross domestic product (GDP)",
                "Education Index",
                "Population",
                "Rural Population")%>%
  cbind.data.frame(pc_food_GI_scores_rail %>%
                     dplyr::select(PC1_GI,PC2_GI)) %>%
  cbind.data.frame(ru_percent) %>%
  cbind.data.frame(diarrhea_access[yes_list_water_food,] %>%
                     dplyr::select("Diarrhea as a cause of death for children under 5"))

model15.1<-regsubsets(`Diarrhea as a cause of death for children under 5`~.,data=diar_food_svs, method= "exhaustive",nv=14)
summary(model15.1)

model15.2<-lm(`Diarrhea as a cause of death for children under 5`~.
              -`Share of dietary energy supply derived from cereals, roots, and tubers`
              -`Average supply of protein of animal origin`
              -ru_percent
              ,data=diar_food_svs)
summary(model15.2)

pred_diar_food_svs<-predict(model15.2, newdata = diar_food_svs)

ggplot(diar_food_svs, aes(x=`Diarrhea as a cause of death for children under 5`, 
                             y=pred_diar_food_svs, 
                             size=PC1_GI))+ 
  geom_point(color="orange")+
  # geom_smooth(method=lm,formula= y~x,color="black")+
  geom_abline(slope = 1, color="red", linetype=2)+
  # scale_x_continuous(limits = c(0, 100))+
  # scale_y_continuous(limits =  c(0, 100))+
  labs(x= "Diarrhea as a Cause of Death for Children Under 5 (%)",y= "Model Prediction (%)") + 
  theme_bw()+
  theme(legend.position = "right")+
  theme(axis.title.x = element_text(size=30),
        axis.title.y = element_text(size = 30),
        axis.text.x = element_text(color="#993333", size=14),
        axis.text.y = element_text(color="#993333", size=14))+
  scale_size(range = c(2,10)) +
  geom_text_repel(aes(`Diarrhea as a cause of death for children under 5`, 
                      pred_diar_food_svs,
                      label = row.names(diar_food_svs)),show.legend = FALSE)

####EnergySVStoWaterHealth

ru_percent<-food_energy_df$`Rural Population`/food_energy_df$Population*100

diar_energy_svs<- food_energy_df %>%
  dplyr::select("Electricity consumption",
                "Total Primary Energy Supply (TPES)",
                "Energy supply per capita",
                "Electricity consumption per capita",
                "Gross domestic product (GDP)",
                "Education Index",
                "Population",
                "Rural Population")%>%
  cbind.data.frame(as.data.frame(pc_GI_scores_food_ener) %>%
                     dplyr::select(PC1_GI,PC2_GI)) %>%
  cbind.data.frame(ru_percent) %>%
  cbind.data.frame(diarrhea_access[yes_list_food_energy,] %>%
                     dplyr::select("Diarrhea as a cause of death for children under 5"))

model16.1<-regsubsets(`Diarrhea as a cause of death for children under 5`~.,diar_energy_svs, method = "exhaustive", nv=9)
summary(model16.1)

model16.2<-lm(`Diarrhea as a cause of death for children under 5`~.
              -PC2_GI
              -`Education Index`
              -`Electricity consumption per capita`
              -Population
              -ru_percent
              , data=diar_energy_svs)
summary(model16.2)

pred_diar_energy_svs<-predict(model16.2, newdata = diar_energy_svs)

ggplot(diar_energy_svs, aes(x=`Diarrhea as a cause of death for children under 5`, 
                          y=pred_diar_energy_svs, 
                          size=`Energy supply per capita`))+ 
  geom_point(color="orange")+
  # geom_smooth(method=lm,formula= y~x,color="black")+
  geom_abline(slope = 1, color="red", linetype=2)+
  # scale_x_continuous(limits = c(0, 100))+
  # scale_y_continuous(limits =  c(0, 100))+
  labs(x= "Diarrhea as a Cause of Death for Children Under 5 (%)",y= "Model Prediction (%)") + 
  theme_bw()+
  theme(legend.position = "right")+
  theme(axis.title.x = element_text(size=24),
        axis.title.y = element_text(size = 30),
        axis.text.x = element_text(color="#993333", size=14),
        axis.text.y = element_text(color="#993333", size=14))+
  scale_size(range = c(3,10)) +
  geom_text_repel(aes(`Diarrhea as a cause of death for children under 5`, 
                      pred_diar_energy_svs,
                      label = row.names(diar_energy_svs)),show.legend = FALSE)


####EnergySvstoUndernourishment
U5nourish_energy_svs<- food_energy_df %>%
  dplyr::select("Electricity consumption",
                "Total Primary Energy Supply (TPES)",
                "Energy supply per capita",
                "Electricity consumption per capita",
                "Gross domestic product (GDP)",
                "Education Index",
                "Population",
                "Rural Population")%>%
  cbind.data.frame(as.data.frame(pc_GI_scores_food_ener) %>%
                     dplyr::select(PC1_GI,PC2_GI)) %>%
  cbind.data.frame(ru_percent) %>%
  cbind.data.frame(pca_food_health_df[yes_list_food_energy,] %>%
                     dplyr::select(PC1_U5nourish))

model17.1<-regsubsets(PC1_U5nourish~., data =U5nourish_energy_svs, method = "exhaustive", nv=9)
summary(model17.1)

model17.2<-lm(PC1_U5nourish~.
              -`Electricity consumption`  
              -`Education Index`
              -Population
              -`Rural Population` 
              # -PC1_GI
              -`Gross domestic product (GDP)`, data=U5nourish_energy_svs)
summary(model17.2)

pred_U5nourish_energy_svs<-predict(model17.2, newdata = U5nourish_energy_svs)

ggplot(U5nourish_energy_svs, aes(x=PC1_U5nourish, 
                            y=pred_U5nourish_energy_svs, 
                            size=`Total Primary Energy Supply (TPES)`))+ 
  geom_point(color="orange")+
  # geom_smooth(method=lm,formula= y~x,color="black")+
  geom_abline(slope = 1, color="red", linetype=2)+
  # scale_x_continuous(limits = c(0, 100))+
  # scale_y_continuous(limits =  c(0, 100))+
  labs(x= "Diarrhea as a Cause of Death for Children Under 5 (%)",y= "Model Prediction (%)") + 
  theme_bw()+
  theme(legend.position = "right")+
  theme(axis.title.x = element_text(size=24),
        axis.title.y = element_text(size = 30),
        axis.text.x = element_text(color="#993333", size=14),
        axis.text.y = element_text(color="#993333", size=14))+
  scale_size(range = c(3,10)) +
  geom_text_repel(aes(`Diarrhea as a cause of death for children under 5`, 
                      pred_diar_energy_svs,
                      label = row.names(diar_energy_svs)),show.legend = FALSE)

  geom_point(color="orange")+
  # geom_smooth(method=lm,formula= y~x,color="black")+
  geom_abline(slope = 1, color="red", linetype=2)+
  # scale_x_continuous(limits = c(0, 100))+
  # scale_y_continuous(limits =  c(0, 100))+
  labs(x="   PC1 Scores of Children Under 5 have Food-related Health Problems",y= "Model Prediction") + 
  theme_bw()+
  theme(legend.position = "right")+
  theme(axis.title.x = element_text(size=21),
        axis.title.y = element_text(size = 30),
        axis.text.x = element_text(color="#993333", size=14),
        axis.text.y = element_text(color="#993333", size=14))+
  scale_size(range = c(3,10)) +
  geom_text_repel(aes(PC1_U5nourish, 
                      pred_U5nourish_energy_svs,
                      label = row.names(U5nourish_energy_svs)),show.legend = FALSE)
