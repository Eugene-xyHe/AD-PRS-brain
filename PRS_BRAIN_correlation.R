library(data.table)
library(plyr)

imaging_area<-as.data.frame(fread("UKB_aparc_Area.txt"))
names(imaging_area)[69]<-"eid"
imaging_thickness<-as.data.frame(fread("UKB_aparc_Thickness.txt"))
names(imaging_thickness)[69]<-"eid"
imaging_volumn<-as.data.frame(fread("UKB_aparc_Volume.txt"))
names(imaging_volumn)[69]<-"eid"
imaging_DTI<-as.data.frame(fread("UKB_DTIdata.csv"))
dti_25488<-as.data.frame(fread("UKB_cortex_25488.txt"))
dti_25488<-dti_25488[,c(1,5)]
imaging_DTI<-merge(dti_25488,imaging_DTI,by="eid")
###sex——age
ad<-as.data.frame(fread("dementia_survival_data.csv"))
ad$Sex <- as.factor(ad$Sex)
ad<-ad[,c(1,10)]
imaging_age<-as.data.frame(fread("imaging_access_age.csv"))
imaging_age<-imaging_age[,-1]
###pca10
pca40<-as.data.frame(fread("PCA40.csv",sep = " ",header = F,fill = T))
pca40<-pca40[,c(1:11)]
names(pca40)[names(pca40) == 'V1'] <- 'eid'
###exclude who withdraw
paichu<-as.data.frame(fread("w19542_20220222.csv"))
ad$index<-ad$eid %in% paichu$V1
ad<-subset(ad,ad$index==FALSE)
ad<-ad[,-3]


ukbprs<-merge(ad,imaging_age,by="eid")
ukbprs<-na.omit(ukbprs)#40988有sex_age
ukbprs<-merge(ukbprs,pca40,by="eid")
ukbprs<-na.omit(ukbprs)#40048有pca

#include APOE####
prs<-read.csv('Kunkle_include_APOE_new.all_score',sep = " ",header = T)
#not notinclude APOE####
#prs<-read.csv('Kunkle_notinclude_APOE_new.all_score',sep = " ",header = T)
prs <- prs[,-1]
names(prs)[names(prs) == 'IID'] <- 'eid'
prs <- prs[,1:15]
colnames(prs)[2:15]<-c("5e_08",'5e_07',"1e_06","5e_06","1e_05","5e_05","0.0001","0.0005","0.001","0.005","0.01","0.05","0.1","0.2")
prs<-as.data.frame(prs[c("eid","5e_08","1e_06","5e_06","1e_05","5e_05","0.0001","0.001","0.01")])


ukbprs<-merge(ukbprs,prs,by="eid")#25417有prs

#exclude dementia at baseline
dementia<-read.csv("Dementia_survival_data.csv",sep = ",",header = T,stringsAsFactors = F)
dementia<-dementia[,c(1,3,9)]
dementia$dementia_date<-as.Date(dementia$dementia_date)
imaging_date<-as.data.frame(fread("imaging_accessdate.csv"))
imaging_date$x53_2_0<-as.Date(imaging_date$x53_2_0)
dementia<-merge(dementia,imaging_date,by="eid")
dementia$dif<-difftime(dementia$dementia_date,dementia$x53_2_0,units = "days")
paichu_dementia<-subset(dementia,dementia$dementia_status==1)
paichu_dementia<-subset(paichu_dementia,paichu_dementia$dif>0)
paichu_dementia2<-subset(dementia,dementia$dementia_status==0)
id<-c(paichu_dementia$eid,paichu_dementia2$eid)

apoe<-read.csv("dementia_survival_data.csv",sep = ",",header = T,stringsAsFactors = F)
apoe <- apoe[c("eid","APOE4")]
apoe$APOE4 <- as.factor(apoe$APOE4)
ukbprs<-ukbprs[ukbprs$eid %in% id,]#25401
ukbprs <- merge(ukbprs,apoe,by='eid')

#merge
subcortical<-as.data.frame(fread("UKB_Imagingdata.csv"))
subcortical<-subcortical[,c(1,141:154)]
ukbprs_sub<-merge(ukbprs,subcortical,by="eid")#23209

ukbprs_area<-merge(ukbprs,imaging_area,by="eid")#23366
ukbprs_thickness<-merge(ukbprs,imaging_thickness,by="eid")#23366
ukbprs_volume<-merge(ukbprs,imaging_volumn,by="eid")#23366
ukbprs_dti<-merge(ukbprs,imaging_DTI,by="eid")#21905


###global####
ukbprs_area['global'] <- ldply(apply(ukbprs_area[,23:90],1,mean))['V1']
ukbprs_thickness['global'] <- ldply(apply(ukbprs_thickness[,23:90],1,mean))['V1']
ukbprs_volume['global'] <- ldply(apply(ukbprs_volume[,23:90],1,mean))['V1']

########################data_volumn############################
colnames(ukbprs_volume)[23:90]<-substring(colnames(ukbprs_volume)[23:90],4)
ukbprs_volumn_left<-ukbprs_volume[,c(1:22,23:56,91)]
ukbprs_volumn_left$hemisphere<-"left"
ukbprs_volumn_right<-ukbprs_volume[,c(1:22,57:90,91)]
ukbprs_volumn_right$hemisphere<-"right"
ukbprs_volumn_merge<-rbind(ukbprs_volumn_left,ukbprs_volumn_right)
########################data_area##############################
colnames(ukbprs_area)[23:90]<-substring(colnames(ukbprs_area)[23:90],4)
ukbprs_area_left<-ukbprs_area[,c(1:22,23:56,91)]
ukbprs_area_left$hemisphere<-"left"
ukbprs_area_right<-ukbprs_area[,c(1:22,57:90,91)]
ukbprs_area_right$hemisphere<-"right"
ukbprs_area_merge<-rbind(ukbprs_area_left,ukbprs_area_right)
########################data_thickness##############################
colnames(ukbprs_thickness)[23:90]<-substring(colnames(ukbprs_thickness)[23:90],4)
ukbprs_thickness_left<-ukbprs_thickness[,c(1:22,23:56,91)]
ukbprs_thickness_left$hemisphere<-"left"
ukbprs_thickness_right<-ukbprs_thickness[,c(1:22,57:90,91)]
ukbprs_thickness_right$hemisphere<-"right"
ukbprs_thickness_merge<-rbind(ukbprs_thickness_left,ukbprs_thickness_right)
########################data_sub#############################
total_brain_volume <- as.data.frame(fread('whole_brain_volume.tsv.gz'))
total_brain_volume <- total_brain_volume[1:2]
names(total_brain_volume)[2] <- 'total_brain_volume'
ukbprs_sub <- merge(ukbprs_sub,total_brain_volume,by='eid')
ukbprs_subcortical_volumn_left<-ukbprs_sub[,c(1:22,35,33,25,31,29,27,23,37)]
ukbprs_subcortical_volumn_left$hemisphere<-"left"
ukbprs_subcortical_volumn_right<-ukbprs_sub[,c(1:22,36,34,26,32,30,28,24,37)]
ukbprs_subcortical_volumn_right$hemisphere<-"right"
colnames(ukbprs_subcortical_volumn_left)[23:29]<-c("accumbens","amygdala","caudate","hippocampus","pallidum","putamen","thalamus")
colnames(ukbprs_subcortical_volumn_right)[23:29]<-c("accumbens","amygdala","caudate","hippocampus","pallidum","putamen","thalamus")
ukbprs_subcortical_volumn_merge<-rbind(ukbprs_subcortical_volumn_left,ukbprs_subcortical_volumn_right)

########################data_WMtracts##############################
#FA
ukbprs_FA<-ukbprs_dti[,c(1:22,23:49)]
ukbprs_FA_bilateral_left<-ukbprs_FA[,c(1:22,23,25,37,27,29,33,35,40,42,44,46,48)]
ukbprs_FA_bilateral_left$hemisphere<-"left"
ukbprs_FA_bilateral_right<-ukbprs_FA[,c(1:22,24,26,38,28,30,34,36,41,43,45,47,49)]
ukbprs_FA_bilateral_right$hemisphere<-"right"
colnames(ukbprs_FA_bilateral_right)<-colnames(ukbprs_FA_bilateral_left)
ukbprs_FA_bilateral_merge<-rbind(ukbprs_FA_bilateral_left,ukbprs_FA_bilateral_right)
ukbprs_FA_single<-ukbprs_FA[,c(1:22,31,32,39)]

#MD
ukbprs_MD<-ukbprs_dti[,c(1:22,185:211)]
ukbprs_MD_bilateral_left<-ukbprs_MD[,c(1:22,23,25,37,27,29,33,35,40,42,44,46,48)]
ukbprs_MD_bilateral_left$hemisphere<-"left"
ukbprs_MD_bilateral_right<-ukbprs_MD[,c(1:22,24,26,38,28,30,34,36,41,43,45,47,49)]
ukbprs_MD_bilateral_right$hemisphere<-"right"
colnames(ukbprs_MD_bilateral_right)<-colnames(ukbprs_MD_bilateral_left)
ukbprs_MD_bilateral_merge<-rbind(ukbprs_MD_bilateral_left,ukbprs_MD_bilateral_right)
ukbprs_MD_single<-ukbprs_MD[,c(1:22,31,32,39)]

rm(list = ls()[grep('left|right|imaging_',ls())])

#library_packages############################
library(lme4)
library(lmerTest)
library(sjstats)
library(MuMIn)


###volumn####
result<-data.frame(PRS=character(0),Region=character(0),random_effect_p=numeric(0),BETA=numeric(0),SE=numeric(0),t=numeric(0),R2=numeric(0),P=numeric(0),LCI=numeric(0),UCI=numeric(0))
result$PRS<-as.character(result$PRS)
result$Region<-as.character(result$Region)

print('volumn')
for (n in 14:21) {
  print(n)
  for (i in 23:56){
    mean_imaging<-mean(na.omit(ukbprs_volumn_merge[,i]))
    sd_imaging<-sd(na.omit(ukbprs_volumn_merge[,i]))
    ukbprs_volumn_merge_non_outlier<-ukbprs_volumn_merge[ukbprs_volumn_merge[,i]>=(mean_imaging-3*sd_imaging) & ukbprs_volumn_merge[,i]<=(mean_imaging+3*sd_imaging),]
    
    fit<-lmer(scale(ukbprs_volumn_merge_non_outlier[,i]) ~ scale(ukbprs_volumn_merge_non_outlier[,n])+global+age_imag+I(age_imag^2)+Sex+V2+V3+V4+V5+V6+V7+V8+V9+V10+V11
              +(1|hemisphere),
              data = ukbprs_volumn_merge_non_outlier)
    fit1<-lmer(scale(ukbprs_volumn_merge_non_outlier[,i]) ~ global+age_imag+I(age_imag^2)+Sex+V2+V3+V4+V5+V6+V7+V8+V9+V10+V11
               +(1|hemisphere),
               data = ukbprs_volumn_merge_non_outlier)
    PRS<-colnames(ukbprs_volumn_merge_non_outlier)[n]
    Region<-colnames(ukbprs_volumn_merge_non_outlier)[i]
    random_effect_p<-ranova(fit)[2,6]
    BETA<-parameters::model_parameters(fit)[2,2]
    SE<-parameters::model_parameters(fit)[2,3]
    LCI<-parameters::model_parameters(fit)[2,5]
    UCI<-parameters::model_parameters(fit)[2,6]
    t<-parameters::model_parameters(fit)[2,7]
    p<-parameters::model_parameters(fit)[2,9]
    R2 <- abs(r.squaredGLMM(fit)[1,2]-r.squaredGLMM(fit1)[1,2])
    result <-rbind(result,data.frame(PRS,Region,random_effect_p,BETA,SE,t,R2,p,LCI,UCI))
  }
}

fwrite(result,"vol.csv",row.names = F,quote = F)

###area##########################
result<-data.frame(PRS=character(0),Region=character(0),random_effect_p=numeric(0),BETA=numeric(0),SE=numeric(0),t=numeric(0),R2=numeric(0),P=numeric(0),LCI=numeric(0),UCI=numeric(0))
result$PRS<-as.character(result$PRS)
result$Region<-as.character(result$Region)
print('area')
for (n in 14:21) {
  print(n)
  for (i in 23:56){
    mean_imaging<-mean(na.omit(ukbprs_area_merge[,i]))
    sd_imaging<-sd(na.omit(ukbprs_area_merge[,i]))
    ukbprs_area_merge_non_outlier<-ukbprs_area_merge[ukbprs_area_merge[,i]>=(mean_imaging-3*sd_imaging) & ukbprs_area_merge[,i]<=(mean_imaging+3*sd_imaging),]
    fit<-lmer(scale(ukbprs_area_merge_non_outlier[,i]) ~ scale(ukbprs_area_merge_non_outlier[,n])+global+age_imag+I(age_imag^2)+Sex+V2+V3+V4+V5+V6+V7+V8+V9+V10+V11
              +(1|hemisphere),data = ukbprs_area_merge_non_outlier)
    fit1<-lmer(scale(ukbprs_area_merge_non_outlier[,i]) ~ global+age_imag+I(age_imag^2)+Sex+V2+V3+V4+V5+V6+V7+V8+V9+V10+V11
               +(1|hemisphere),data = ukbprs_area_merge_non_outlier)
    
    PRS<-colnames(ukbprs_area_merge_non_outlier)[n]
    Region<-colnames(ukbprs_area_merge_non_outlier)[i]
    random_effect_p<-ranova(fit)[2,6]
    BETA<-parameters::model_parameters(fit)[2,2]
    SE<-parameters::model_parameters(fit)[2,3]
    LCI<-parameters::model_parameters(fit)[2,5]
    UCI<-parameters::model_parameters(fit)[2,6]
    t<-parameters::model_parameters(fit)[2,7]
    p<-parameters::model_parameters(fit)[2,9]
    R2 <- abs(r.squaredGLMM(fit)[1,2]-r.squaredGLMM(fit1)[1,2])
    result <-rbind(result,data.frame(PRS,Region,random_effect_p,BETA,SE,t,R2,p,LCI,UCI))
  }
}

fwrite(result,"area.csv",row.names = F,quote = F)

###thickness####
result<-data.frame(PRS=character(0),Region=character(0),random_effect_p=numeric(0),BETA=numeric(0),SE=numeric(0),t=numeric(0),R2=numeric(0),P=numeric(0),LCI=numeric(0),UCI=numeric(0))
result$PRS<-as.character(result$PRS)
result$Region<-as.character(result$Region)
print('thick')
#non_global
for (n in 14:21) {
  print(n)
  for (i in 23:56){
    mean_imaging<-mean(na.omit(ukbprs_thickness_merge[,i]))
    sd_imaging<-sd(na.omit(ukbprs_thickness_merge[,i]))
    ukbprs_thickness_merge_non_outlier<-ukbprs_thickness_merge[ukbprs_thickness_merge[,i]>=(mean_imaging-3*sd_imaging) & ukbprs_thickness_merge[,i]<=(mean_imaging+3*sd_imaging),]
    fit<-lmer(scale(ukbprs_thickness_merge_non_outlier[,i]) ~ scale(ukbprs_thickness_merge_non_outlier[,n])+global+age_imag+I(age_imag^2)+Sex+V2+V3+V4+V5+V6+V7+V8+V9+V10+V11
              +(1|hemisphere),data = ukbprs_thickness_merge_non_outlier)
    fit1<-lmer(scale(ukbprs_thickness_merge_non_outlier[,i]) ~ global+age_imag+I(age_imag^2)+Sex+V2+V3+V4+V5+V6+V7+V8+V9+V10+V11
               +(1|hemisphere),data = ukbprs_thickness_merge_non_outlier)
    
    PRS<-colnames(ukbprs_thickness_merge_non_outlier)[n]
    Region<-colnames(ukbprs_thickness_merge_non_outlier)[i]
    random_effect_p<-ranova(fit)[2,6]
    BETA<-parameters::model_parameters(fit)[2,2]
    SE<-parameters::model_parameters(fit)[2,3]
    LCI<-parameters::model_parameters(fit)[2,5]
    UCI<-parameters::model_parameters(fit)[2,6]
    t<-parameters::model_parameters(fit)[2,7]
    p<-parameters::model_parameters(fit)[2,9]
    R2 <- abs(r.squaredGLMM(fit)[1,2]-r.squaredGLMM(fit1)[1,2])
    result <-rbind(result,data.frame(PRS,Region,random_effect_p,BETA,SE,t,R2,p,LCI,UCI))
  }
}

fwrite(result,"thick.csv",row.names = F,quote = F)


###sub####
result<-data.frame(PRS=character(0),Region=character(0),random_effect_p=numeric(0),BETA=numeric(0),SE=numeric(0),t=numeric(0),R2=numeric(0),P=numeric(0),LCI=numeric(0),UCI=numeric(0))
result$PRS<-as.character(result$PRS)
result$Region<-as.character(result$Region)
print('sub')
n=14
i=23
for (n in 14:21) {
  print(n)
  for (i in 23:29){
    mean_imaging<-mean(na.omit(ukbprs_subcortical_volumn_merge[,i]))
    sd_imaging<-sd(na.omit(ukbprs_subcortical_volumn_merge[,i]))
    ukbprs_subcortical_volumn_merge_non_outlier<-ukbprs_subcortical_volumn_merge[ukbprs_subcortical_volumn_merge[,i]>=(mean_imaging-3*sd_imaging) & ukbprs_subcortical_volumn_merge[,i]<=(mean_imaging+3*sd_imaging),]
    fit<-lmer(scale(ukbprs_subcortical_volumn_merge_non_outlier[,i]) ~ scale(ukbprs_subcortical_volumn_merge_non_outlier[,n])+total_brain_volume+age_imag+I(age_imag^2)+Sex+V2+V3+V4+V5+V6+V7+V8+V9+V10+V11
              +(1|hemisphere),data = ukbprs_subcortical_volumn_merge_non_outlier)
    fit1<-lmer(scale(ukbprs_subcortical_volumn_merge_non_outlier[,i]) ~ total_brain_volume+age_imag+I(age_imag^2)+Sex+V2+V3+V4+V5+V6+V7+V8+V9+V10+V11
               +(1|hemisphere),data = ukbprs_subcortical_volumn_merge_non_outlier)
    PRS<-colnames(ukbprs_subcortical_volumn_merge_non_outlier)[n]
    Region<-colnames(ukbprs_subcortical_volumn_merge_non_outlier)[i]
    random_effect_p<-ranova(fit)[2,6]
    BETA<-parameters::model_parameters(fit)[2,2]
    SE<-parameters::model_parameters(fit)[2,3]
    LCI<-parameters::model_parameters(fit)[2,5]
    UCI<-parameters::model_parameters(fit)[2,6]
    t<-parameters::model_parameters(fit)[2,7]
    p<-parameters::model_parameters(fit)[2,9]
    R2 <- abs(r.squaredGLMM(fit)[1,2]-r.squaredGLMM(fit1)[1,2])
    result <-rbind(result,data.frame(PRS,Region,random_effect_p,BETA,SE,t,R2,p,LCI,UCI))
  }
}

fwrite(result,"sub.csv",row.names = F,quote = F)

###FA####
result<-data.frame(PRS=character(0),Region=character(0),random_effect_p=numeric(0),BETA=numeric(0),SE=numeric(0),t=numeric(0),R2=numeric(0),P=numeric(0),LCI=numeric(0),UCI=numeric(0))
result$PRS<-as.character(result$PRS)
result$Region<-as.character(result$Region)
print('fa bi')
for (n in 14:21) {
  print(n)
  for (i in 23:34){
    mean_imaging<-mean(na.omit(ukbprs_FA_bilateral_merge[,i]))
    sd_imaging<-sd(na.omit(ukbprs_FA_bilateral_merge[,i]))
    ukbprs_FA_bilateral_merge_non_outlier<-ukbprs_FA_bilateral_merge[ukbprs_FA_bilateral_merge[,i]>=(mean_imaging-3*sd_imaging) & ukbprs_FA_bilateral_merge[,i]<=(mean_imaging+3*sd_imaging),]
    fit<-lmer(scale(ukbprs_FA_bilateral_merge_non_outlier[,i]) ~ scale(ukbprs_FA_bilateral_merge_non_outlier[,n])+age_imag+I(age_imag^2)+Sex+V2+V3+V4+V5+V6+V7+V8+V9+V10+V11
              +(1|hemisphere),data = ukbprs_FA_bilateral_merge_non_outlier)
    fit1<-lmer(scale(ukbprs_FA_bilateral_merge_non_outlier[,i]) ~age_imag+I(age_imag^2)+Sex+V2+V3+V4+V5+V6+V7+V8+V9+V10+V11
               +(1|hemisphere),data = ukbprs_FA_bilateral_merge_non_outlier)
    PRS<-colnames(ukbprs_FA_bilateral_merge_non_outlier)[n]
    Region<-colnames(ukbprs_FA_bilateral_merge_non_outlier)[i]
    random_effect_p<-ranova(fit)[2,6]
    BETA<-parameters::model_parameters(fit)[2,2]
    SE<-parameters::model_parameters(fit)[2,3]
    LCI<-parameters::model_parameters(fit)[2,5]
    UCI<-parameters::model_parameters(fit)[2,6]
    t<-parameters::model_parameters(fit)[2,7]
    p<-parameters::model_parameters(fit)[2,9]
    R2 <- abs(r.squaredGLMM(fit)[1,2]-r.squaredGLMM(fit1)[1,2])
    result <-rbind(result,data.frame(PRS,Region,random_effect_p,BETA,SE,t,R2,p,LCI,UCI))
  }
}

fwrite(result,"fa.csv",row.names = F,quote = F)

###MD####
result<-data.frame(PRS=character(0),Region=character(0),random_effect_p=numeric(0),BETA=numeric(0),SE=numeric(0),t=numeric(0),R2=numeric(0),P=numeric(0),LCI=numeric(0),UCI=numeric(0))
result$PRS<-as.character(result$PRS)
result$Region<-as.character(result$Region)
print('md bi')
for (n in 14:21) {
  print(n)
  for (i in 23:34){
    mean_imaging<-mean(na.omit(ukbprs_MD_bilateral_merge[,i]))
    sd_imaging<-sd(na.omit(ukbprs_MD_bilateral_merge[,i]))
    ukbprs_MD_bilateral_merge_non_outlier<-ukbprs_MD_bilateral_merge[ukbprs_MD_bilateral_merge[,i]>=(mean_imaging-3*sd_imaging) & ukbprs_MD_bilateral_merge[,i]<=(mean_imaging+3*sd_imaging),]
    fit<-lmer(scale(ukbprs_MD_bilateral_merge_non_outlier[,i]) ~ scale(ukbprs_MD_bilateral_merge_non_outlier[,n])+age_imag+I(age_imag^2)+Sex+V2+V3+V4+V5+V6+V7+V8+V9+V10+V11
              +(1|hemisphere),data = ukbprs_MD_bilateral_merge_non_outlier)
    fit1<-lmer(scale(ukbprs_MD_bilateral_merge_non_outlier[,i]) ~ age_imag+I(age_imag^2)+Sex+V2+V3+V4+V5+V6+V7+V8+V9+V10+V11
               +(1|hemisphere),data = ukbprs_MD_bilateral_merge_non_outlier)
    PRS<-colnames(ukbprs_MD_bilateral_merge_non_outlier)[n]
    Region<-colnames(ukbprs_MD_bilateral_merge_non_outlier)[i]
    random_effect_p<-ranova(fit)[2,6]
    BETA<-parameters::model_parameters(fit)[2,2]
    SE<-parameters::model_parameters(fit)[2,3]
    LCI<-parameters::model_parameters(fit)[2,5]
    UCI<-parameters::model_parameters(fit)[2,6]
    t<-parameters::model_parameters(fit)[2,7]
    p<-parameters::model_parameters(fit)[2,9]
    R2 <- abs(r.squaredGLMM(fit)[1,2]-r.squaredGLMM(fit1)[1,2])
    result <-rbind(result,data.frame(PRS,Region,random_effect_p,BETA,SE,t,R2,p,LCI,UCI))
  }
}

fwrite(result,"md.csv",row.names = F,quote = F)

###FA unilateral####
result<-data.frame(PRS=character(0),Region=character(0),BETA=numeric(0),SE=numeric(0),t=numeric(0),R2=numeric(0),P=numeric(0))
result$PRS<-as.character(result$PRS)
result$Region<-as.character(result$Region)
print('fa uni')
for (n in 14:21) {
  print(n)
  for (i in 23:25){
    mean_imaging<-mean(na.omit(ukbprs_FA_single[,i]))
    sd_imaging<-sd(na.omit(ukbprs_FA_single[,i]))
    ukbprs_FA_single_non_outlier<-ukbprs_FA_single[ukbprs_FA_single[,i]>=(mean_imaging-3*sd_imaging) & ukbprs_FA_single[,i]<=(mean_imaging+3*sd_imaging),]
    fit<-lm(scale(ukbprs_FA_single_non_outlier[,i]) ~ scale(ukbprs_FA_single_non_outlier[,n])+age_imag+I(age_imag^2)+Sex+V2+V3+V4+V5+V6+V7+V8+V9+V10+V11,
            data = ukbprs_FA_single_non_outlier)
    fit1<-lm(scale(ukbprs_FA_single_non_outlier[,i]) ~ age_imag+I(age_imag^2)+Sex+V2+V3+V4+V5+V6+V7+V8+V9+V10+V11,
             data = ukbprs_FA_single_non_outlier)
    PRS<-colnames(ukbprs_FA_single_non_outlier)[n]
    Region<-colnames(ukbprs_FA_single_non_outlier)[i]
    BETA<-coef(summary(fit))[2,1]
    SE<-coef(summary(fit))[2,2]
    t<-coef(summary(fit))[2,3]
    p<-coef(summary(fit))[2,4]
    R2<-summary(fit)$r.squared-summary(fit1)$r.squared
    result <-rbind(result,data.frame(PRS,Region,random_effect_p,BETA,SE,t,R2,p,LCI,UCI))
  }
}

fwrite(result,"FA_unilateral.csv",row.names = F,quote = F)


###MD unilateral####
result<-data.frame(PRS=character(0),Region=character(0),BETA=numeric(0),SE=numeric(0),t=numeric(0),R2=numeric(0),P=numeric(0))
result$PRS<-as.character(result$PRS)
result$Region<-as.character(result$Region)
print('md uni')
for (n in 14:21) {
  print(n)
  for (i in 23:25){
    mean_imaging<-mean(na.omit(ukbprs_MD_single[,i]))
    sd_imaging<-sd(na.omit(ukbprs_MD_single[,i]))
    ukbprs_MD_single_non_outlier<-ukbprs_MD_single[ukbprs_MD_single[,i]>=(mean_imaging-3*sd_imaging) & ukbprs_MD_single[,i]<=(mean_imaging+3*sd_imaging),]
    fit<-lm(scale(ukbprs_MD_single_non_outlier[,i]) ~ scale(ukbprs_MD_single_non_outlier[,n])+age_imag+I(age_imag^2)+Sex+V2+V3+V4+V5+V6+V7+V8+V9+V10+V11,
            data = ukbprs_MD_single_non_outlier)
    fit1<-lm(scale(ukbprs_MD_single_non_outlier[,i]) ~ age_imag+I(age_imag^2)+Sex+V2+V3+V4+V5+V6+V7+V8+V9+V10+V11,
             data = ukbprs_MD_single_non_outlier)
    PRS<-colnames(ukbprs_MD_single_non_outlier)[n]
    Region<-colnames(ukbprs_MD_single_non_outlier)[i]
    BETA<-coef(summary(fit))[2,1]
    SE<-coef(summary(fit))[2,2]
    t<-coef(summary(fit))[2,3]
    p<-coef(summary(fit))[2,4]
    R2<-summary(fit)$r.squared-summary(fit1)$r.squared
    result <-rbind(result,data.frame(PRS,Region,random_effect_p,BETA,SE,t,R2,p,LCI,UCI))
  }
}
fwrite(result,"MD_unilateral.csv",row.names = F,quote = F)