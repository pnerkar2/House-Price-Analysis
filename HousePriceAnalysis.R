#Team - PAT our Backs!!
#Runtime - 3 Minutes with Lasso Testing, 1 Minute without Lasso (Only Linear)
library(tidyverse)
library(dplyr)
library(ggplot2)

hist <- read.csv("historic_property_data.csv")

colSums(is.na(hist))*100/nrow(hist)

#Removing Columns with more than 50 percent NA values

histclean1 = subset(hist,select= -c(char_renovation,meta_cdu,char_apts,char_porch,char_attic_fnsh,char_tp_dsgn))

#Removing columns where IsPredictor is false and 2 columns

histclean2 = subset(histclean1, select= -c(ind_arms_length,char_ot_impr,geo_property_zip,geo_black_perc,geo_other_perc,char_cnst_qlty,meta_certified_est_land,geo_property_city,geo_white_perc,geo_his_perc,geo_municipality,meta_class,meta_certified_est_bldg,geo_tract_pop,geo_asian_perc,geo_fips,char_site,meta_deed_type,meta_nbhd))

colSums(is.na(histclean2))

#Replacing NAs values with mode in columns where count of NAs are > 5000 

#Function to Replace values with Mode
calc_mode <- function(x){
  
  # List the distinct / unique values
  distinct_values <- unique(x)
  
  # Count the occurrence of each distinct value
  distinct_tabulate <- tabulate(match(x, distinct_values))
  
  # Return the value with the highest occurrence
  distinct_values[which.max(distinct_tabulate)]
}


#Updating all the columns removing NA

y <- histclean2
y$char_tp_plan[is.na(y$char_tp_plan)]<-calc_mode(y$char_tp_plan)
y$char_gar1_cnst[is.na(y$char_gar1_cnst)]<-calc_mode(y$char_gar1_cnst)
y$char_gar1_att[is.na(y$char_gar1_att)]<-calc_mode(y$char_gar1_att)
y$char_gar1_area[is.na(y$char_gar1_area)]<-calc_mode(y$char_gar1_area)

y$char_ext_wall[is.na(y$char_ext_wall)]<-calc_mode(y$char_ext_wall)
y$char_roof_cnst[is.na(y$char_roof_cnst)]<-calc_mode(y$char_roof_cnst)
y$char_bsmt[is.na(y$char_bsmt)]<-calc_mode(y$char_bsmt)
y$char_bsmt_fin[is.na(y$char_bsmt_fin)]<-calc_mode(y$char_bsmt_fin)
y$char_heat[is.na(y$char_heat)]<-calc_mode(y$char_heat)
y$char_oheat[is.na(y$char_oheat)]<-calc_mode(y$char_oheat)
y$char_air[is.na(y$char_air)]<-calc_mode(y$char_air)

y$char_frpl[is.na(y$char_frpl)]<-median(y$char_frpl, na.rm = TRUE)

y$char_attic_type[is.na(y$char_attic_type)]<-calc_mode(y$char_attic_type)
y$char_gar1_size[is.na(y$char_gar1_size)]<-calc_mode(y$char_gar1_size)
y$char_use[is.na(y$char_use)]<-calc_mode(y$char_use)
y$char_repair_cnd[is.na(y$char_repair_cnd)]<-calc_mode(y$char_repair_cnd)
y$char_type_resd[is.na(y$char_type_resd)]<-calc_mode(y$char_type_resd)

y$econ_midincome[is.na(y$econ_midincome)]<-median(y$econ_midincome, na.rm = TRUE)
y$geo_fs_flood_factor[is.na(y$geo_fs_flood_factor)]<-median(y$geo_fs_flood_factor, na.rm = TRUE)
y$geo_fs_flood_risk_direction[is.na(y$geo_fs_flood_risk_direction)]<-median(y$geo_fs_flood_risk_direction, na.rm = TRUE)

y$geo_floodplain[is.na(y$geo_floodplain)]<-0
y$geo_ohare_noise[is.na(y$geo_ohare_noise)]<-0
y$geo_withinmr100[is.na(y$geo_withinmr100)]<-0
y$geo_withinmr101300[is.na(y$geo_withinmr101300)]<-0
y$ind_garage[is.na(y$ind_garage)]<- 0

y$geo_school_elem_district[is.na(y$geo_school_elem_district)]<- "GOUDY"
y$geo_school_hs_district[is.na(y$geo_school_hs_district)]<- "GOUDY"
str(y)
histclean3 <- y 

#Check if the columns has no nulls
colSums(is.na(histclean3))
nrow(histclean3)

#Box Plot to check the outliers - Commented to run faster
library(ggplot2)
#ggplot(stack(histclean3), aes(x = ind, y = values)) +
#  geom_boxplot()+
#  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

#We found 2 Columns with outliers - Sale_Price, Char Building SquareFeet
x<-histclean3
outliers1 <- boxplot(histclean3$sale_price,plot=FALSE)$out
outliers2 <- boxplot(x$econ_midincome,plot=FALSE)$out

x<- x[-which(x$sale_price %in% outliers1),]
x<- x[-which(x$econ_midincome %in% outliers2),]

#Checking if this fixed after dropping Sales_Price, econ_midincome because of axis shrinkage
x1 <- subset(x,select= -c(sale_price,econ_midincome ))
#ggplot(stack(x1), aes(x = ind, y = values)) +
#  geom_boxplot()+
#  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

outliers3 <- boxplot(x$char_hd_sf,plot=FALSE)$out
outliers4 <- boxplot(x$char_bldg_sf,plot=FALSE)$out

x<- x[-which(x$char_hd_sf %in% outliers3),]
x<- x[-which(x$char_bldg_sf %in% outliers4),]

df_new1 <- x
#Split data into train and test
set.seed(42)
library(rsample)
df_trn_tst_splity = initial_split(df_new1, prop = 0.70)
df_trn_y = training(df_trn_tst_splity)
df_tst_y = testing(df_trn_tst_splity)

#Build model
lm_full_y = lm(sale_price ~ char_rooms+char_frpl+char_bldg_sf+econ_tax_rate+char_age+char_fbath+econ_midincome+
                 as.factor(geo_floodplain)+as.factor(geo_withinmr100)+as.factor(geo_ohare_noise)+as.factor(char_heat)+
                 as.factor(char_gar1_att)+as.factor(char_bsmt)+as.factor(char_attic_type)+as.factor(char_tp_plan)+
                 char_beds+char_hbath+char_hd_sf+geo_fs_flood_factor+geo_fs_flood_risk_direction+as.factor(geo_withinmr101300)+
                 as.factor(ind_garage)+as.factor(char_ext_wall)+as.factor(char_type_resd)+as.factor(char_roof_cnst)+
                 as.factor(char_oheat)+as.factor(char_gar1_size)+as.factor(char_gar1_area)+
                 as.factor(char_bsmt_fin) +as.factor(char_air) + as.factor(char_use) + as.factor(ind_large_home) + as.factor(geo_school_elem_district)+as.factor(geo_school_hs_district)+
                 as.factor(meta_town_code),
               df_trn_y)


#Find mse
mse1 = mean((df_tst_y$sale_price - predict.lm(lm_full_y, df_tst_y)) ^ 2)

#Calculating through forward/backward and stepwise is taking a lot of time hence dropping from final run
#Forward Selection
#lm.null <- lm(sale_price~1, data = df_trn_y)
#lm.step.forward <- step(lm.null, scope=list(lower=lm.null, upper=lm_full_y), direction = "forward")


#lm.step.pred.forward <- predict(lm.step.forward, df_tst_y)
#head(lm.step.pred.forward)

#mean((df_tst_y$sale_price-lm.step.pred.forward)^2)

#lm.step.backward <- step(lm_full_y, direction = "backward")
#summary(lm.step.backward) 

#lm.step.pred.backward <- predict(lm.step.backward, df_tst_y)
#head(lm.step.pred.backward)

#mean((df_tst_y$sale_price-lm.step.pred.backward)^2)


#lm.step.both <- step(lm_full_y, direction = "both")

#lm.step.pred.both <- predict(lm.step.both, df_tst_y)
#head(lm.step.pred.both)

#mse2 = mean((df_tst_y$sale_price-lm.step.pred.both)^2)

#Lasso is also Giving MSE higher than Linear
library(glmnet)
x <- model.matrix(sale_price~.,df_new1)[,-1]
yy <- df_new1$sale_price
train.index <- sample(c(1:dim(x)[1]), dim(x)[1]*0.7)
test.index <- (-train.index)
yy.test <- yy[test.index]

fit<- glmnet(x[train.index,],yy[train.index],alpha=1)
dim(coef(fit))
lambda.small <- fit$lambda[74]

cv.fit <- cv.glmnet(x[train.index,],yy[train.index],alpha=1, type.measure="mse")
lambda.best <- cv.fit$lambda.min
coef.lambda.best <- predict(cv.fit,s=lambda.best,type="coefficients")[1:33,]
pred.lambda.best <- predict(cv.fit,s=lambda.best,newx=x[test.index,])
mse3 = mean((yy.test-pred.lambda.best)^2)
plot(cv.fit)
coef(cv.fit, cv.fit$lambda.min)


#LM Training on the full data set
lm_full_hist = lm(sale_price ~ char_rooms+char_frpl+char_bldg_sf+econ_tax_rate+char_age+char_fbath+econ_midincome+
                 as.factor(geo_floodplain)+as.factor(geo_withinmr100)+as.factor(geo_ohare_noise)+as.factor(char_heat)+
                 as.factor(char_gar1_att)+as.factor(char_bsmt)+as.factor(char_attic_type)+as.factor(char_tp_plan)+
                 char_beds+char_hbath+char_hd_sf+geo_fs_flood_factor+geo_fs_flood_risk_direction+as.factor(geo_withinmr101300)+
                 as.factor(ind_garage)+as.factor(char_ext_wall)+as.factor(char_type_resd)+as.factor(char_roof_cnst)+
                 as.factor(char_oheat)+as.factor(char_gar1_size)+as.factor(char_gar1_area)+
                 as.factor(char_bsmt_fin) +as.factor(char_air) + as.factor(char_use) + as.factor(ind_large_home) + as.factor(geo_school_elem_district)+as.factor(geo_school_hs_district)+
                 as.factor(meta_town_code),
               histclean3)
summary(lm_full_hist)

#Loading Prediction Data
pred <- read.csv("predict_property_data.csv")

colSums(is.na(pred))*100/nrow(pred)

#Removing Columns from referring above as History
predclean1 = subset(pred,select= -c(char_renovation,meta_cdu,char_apts,char_porch,char_attic_fnsh,char_tp_dsgn, ind_arms_length,char_ot_impr,geo_property_zip,geo_black_perc,geo_other_perc,char_cnst_qlty,meta_certified_est_land,geo_property_city,geo_white_perc,geo_his_perc,geo_municipality,meta_class,meta_certified_est_bldg,geo_tract_pop,geo_asian_perc,geo_fips,char_site,meta_deed_type,meta_nbhd))

#Checking Null counts
colSums(is.na(predclean1))

#Replacing Null values
predclean1$ind_garage[is.na(predclean1$ind_garage)]<- 0

predclean1$char_ext_wall[is.na(predclean1$char_ext_wall)]<-calc_mode(predclean1$char_ext_wall)
predclean1$char_roof_cnst[is.na(predclean1$char_roof_cnst)]<-calc_mode(predclean1$char_roof_cnst)
predclean1$char_bsmt[is.na(predclean1$char_bsmt)]<-calc_mode(predclean1$char_bsmt)
predclean1$char_bsmt_fin[is.na(predclean1$char_bsmt_fin)]<-calc_mode(predclean1$char_bsmt_fin)
predclean1$char_heat[is.na(predclean1$char_heat)]<-calc_mode(predclean1$char_heat)
predclean1$char_oheat[is.na(predclean1$char_oheat)]<-calc_mode(predclean1$char_oheat)
predclean1$char_air[is.na(predclean1$char_air)]<-calc_mode(predclean1$char_air)
predclean1$char_attic_type[is.na(predclean1$char_attic_type)]<-calc_mode(predclean1$char_attic_type)
predclean1$char_tp_plan[is.na(predclean1$char_tp_plan)]<-calc_mode(predclean1$char_tp_plan)
predclean1$char_gar1_size[is.na(predclean1$char_gar1_size)]<-calc_mode(predclean1$char_gar1_size)
predclean1$char_gar1_cnst[is.na(predclean1$char_gar1_cnst)]<-calc_mode(predclean1$char_gar1_cnst)
predclean1$char_gar1_att[is.na(predclean1$char_gar1_att)]<-calc_mode(predclean1$char_gar1_att)
predclean1$char_gar1_area[is.na(predclean1$char_gar1_area)]<-calc_mode(predclean1$char_gar1_area)
predclean1$char_repair_cnd[is.na(predclean1$char_repair_cnd)]<-calc_mode(predclean1$char_repair_cnd)
predclean1$char_use[is.na(predclean1$char_use)]<-calc_mode(predclean1$char_use)
predclean1$char_type_resd[is.na(predclean1$char_type_resd)]<-calc_mode(predclean1$char_type_resd)

predclean1$char_frpl[is.na(predclean1$char_frpl)]<-median(predclean1$char_frpl, na.rm = TRUE)

#Checking Nulls
colSums(is.na(predclean1))

nrow(predclean1)

#Checking all column types from Pred
str(predclean1)

#Checking all column types from Hist
str(histclean3)

#Prediction
prediction=predict.lm(lm_full_hist, predclean1)
#Converting list to Dataframe

summary(pred_df)
summary(histclean3$sale_price)
pred_df = data.frame(prediction)
#Adding header column
colnames(pred_df) <- ('assessed_value')
#Replacing negative Sales Prediction to 0
pred_df[pred_df < 0] <- 0 
#Replacing null values to 0
pred_df$assessed_value[is.na(pred_df$assessed_value)]<-0
#check head
head(pred_df)
#Write to a csv file
write.csv(pred_df,"assessed_value.csv", row.names = TRUE)
