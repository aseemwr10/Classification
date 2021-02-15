library(data.table)
library(readxl)
library(stringr)
library(dplyr)
library(gdata)
library(tidyr)
library(caTools)
library(rpart)
library(randomForest)

options(scipen = 999)
train <- fread("train.csv")
test <- fread("test.csv")

##### PART 1 - DATA EXPLORATION AND CLEANING #####

na_count <-sapply(train, function(y) sum(length(which(is.na(y)))))
na_count <- as.data.frame(na_count)

blank_count <-sapply(train, function(y) sum(length(which(y==""))))
blank_count <- as.data.frame(blank_count)

train <- as.data.frame(train)

unique_values <- sapply(as.numeric(1:40), function(x){
  if (class(train[,x]) == "character"){
    return(length(unique(train[,x])))}
  else {return(NA)}
})
unique_values <- as.data.frame(unique_values)
unique_values <- cbind(names(train)[1:40],unique_values)
names(unique_values)[1] <- 'Column_Name'
unique_values$Column_Name <- as.character(unique_values$Column_Name)

numeric_variables <- unique_values$Column_Name[which(is.na(unique_values$unique_values))]

zero_count <- sapply(numeric_variables, function(y) sum(length(which(train[,y]==0))))
zero_count <- as.data.frame(zero_count)

neg_count <- sapply(numeric_variables, function(y) sum(length(which(train[,y]<0))))
neg_count <- as.data.frame(neg_count)

#Step 1 : Imputing Subvillage Mean to 0 Lat/Lon

train$latitude <- gsub(-0.00000002,0,train$latitude)
train$latitude <- as.numeric(train$latitude)

mean_latlong <- train %>% subset(train$longitude!=0) %>% 
  group_by(district_code,region_code,ward,lga,subvillage) %>% 
  summarise(lat = mean(latitude),long = mean(longitude)) %>% ungroup()

train <- left_join(train,mean_latlong)
train$longitude <- if_else(train$longitude==0,train$long,train$longitude)
train$latitude <- if_else(train$latitude==0,train$lat,train$latitude)
train[,c(42,43)] <- NULL
train$latitude[is.na(train$latitude)] <- 0
train$longitude[is.na(train$longitude)] <- 0

mean_latlong <- train %>% subset(train$longitude!=0) %>% 
  group_by(district_code,region_code,ward,lga) %>% 
  summarise(lat = mean(latitude),long = mean(longitude)) %>% ungroup()

train <- left_join(train,mean_latlong)
train$longitude <- if_else(train$longitude==0,train$long,train$longitude)
train$latitude <- if_else(train$latitude==0,train$lat,train$latitude)
train[,c(42,43)] <- NULL
train$latitude[is.na(train$latitude)] <- 0
train$longitude[is.na(train$longitude)] <- 0

mean_latlong <- train %>% subset(train$longitude!=0) %>% 
  group_by(district_code,region_code) %>% 
  summarise(lat = mean(latitude),long = mean(longitude)) %>% ungroup()

train <- left_join(train,mean_latlong)
train$longitude <- if_else(train$longitude==0,train$long,train$longitude)
train$latitude <- if_else(train$latitude==0,train$lat,train$latitude)
train[,c(42,43)] <- NULL
train$latitude[is.na(train$latitude)] <- 0
train$longitude[is.na(train$longitude)] <- 0

mean_latlong <- train %>% subset(train$longitude!=0) %>% 
  group_by(district_code) %>% 
  summarise(lat = mean(latitude),long = mean(longitude)) %>% ungroup()

train <- left_join(train,mean_latlong)
train$longitude <- if_else(train$longitude==0,train$long,train$longitude)
train$latitude <- if_else(train$latitude==0,train$lat,train$latitude)
train[,c(42,43)] <- NULL
train$latitude[is.na(train$latitude)] <- 0
train$longitude[is.na(train$longitude)] <- 0


keep(train,test,sure = T)

#Step 2 : Imputiong Population with median

mean_pop <- train %>% subset(train$population!=0) %>% 
  group_by(district_code,region_code,ward,lga,subvillage) %>% 
  summarise(pop = mean(population)) %>% ungroup()
mean_pop$pop <- as.integer(mean_pop$pop)

train <- left_join(train,mean_pop)
train$population <- if_else(train$population==0,train$pop,train$population)

train[,c(42)] <- NULL
train$population[which(is.na(train$population))] <- 0

train$population <- if_else(train$population==0,median(train$population[which(train$population != 0)]),train$population)

rm(mean_pop)

#Step 3 : Imputing gps_height with mean at Subvillage,Ward,Lga,region,district

train$gps_height[which(train$gps_height < 0)] <- 0

#sub-village
mean_ht <- train %>% subset(train$gps_height!=0) %>% 
  group_by(district_code,region_code,ward,lga,subvillage) %>% 
  summarise(hgt = mean(gps_height)) %>% ungroup()
mean_ht$hgt <- as.integer(mean_ht$hgt)

train <- left_join(train,mean_ht)
train$gps_height <- if_else(train$gps_height==0,train$hgt,train$gps_height)

train[,c(42)] <- NULL
train$gps_height[which(is.na(train$gps_height))] <- 0
train$gps_height <- as.integer(train$gps_height)
length(which(train$gps_height==0))

#ward
mean_ht <- train %>% subset(train$gps_height!=0) %>% 
  group_by(district_code,region_code,ward,lga) %>% 
  summarise(hgt = mean(gps_height)) %>% ungroup()
mean_ht$hgt <- as.integer(mean_ht$hgt)

train <- left_join(train,mean_ht)
train$gps_height <- if_else(train$gps_height==0,train$hgt,train$gps_height)

train[,c(42)] <- NULL
train$gps_height[which(is.na(train$gps_height))] <- 0
train$gps_height <- as.integer(train$gps_height)
length(which(train$gps_height==0))

#lga
mean_ht <- train %>% subset(train$gps_height!=0) %>% 
  group_by(district_code,region_code,lga) %>% 
  summarise(hgt = mean(gps_height)) %>% ungroup()
mean_ht$hgt <- as.integer(mean_ht$hgt)

train <- left_join(train,mean_ht)
train$gps_height <- if_else(train$gps_height==0,train$hgt,train$gps_height)

train[,c(42)] <- NULL
train$gps_height[which(is.na(train$gps_height))] <- 0
train$gps_height <- as.integer(train$gps_height)
length(which(train$gps_height==0))

#region
mean_ht <- train %>% subset(train$gps_height!=0) %>% 
  group_by(district_code,region_code) %>% 
  summarise(hgt = mean(gps_height)) %>% ungroup()
mean_ht$hgt <- as.integer(mean_ht$hgt)

train <- left_join(train,mean_ht)
train$gps_height <- if_else(train$gps_height==0,train$hgt,train$gps_height)

train[,c(42)] <- NULL
train$gps_height[which(is.na(train$gps_height))] <- 0
train$gps_height <- as.integer(train$gps_height)
length(which(train$gps_height==0))

#district
mean_ht <- train %>% subset(train$gps_height!=0) %>% 
  group_by(district_code) %>% 
  summarise(hgt = mean(gps_height)) %>% ungroup()
mean_ht$hgt <- as.integer(mean_ht$hgt)

train <- left_join(train,mean_ht)
train$gps_height <- if_else(train$gps_height==0,train$hgt,train$gps_height)

train[,c(42)] <- NULL
train$gps_height[which(is.na(train$gps_height))] <- 0
train$gps_height <- as.integer(train$gps_height)
length(which(train$gps_height==0))

train$gps_height[which(train$ward == "Masanga")] <- 1173
train$gps_height[which(train$ward == "Matomondo")] <- 1091
train$gps_height[which(train$ward == "Gode Gode")] <- 837
train$gps_height[which(train$ward == "Kiegeani")] <- 53

rm(mean_ht)

#Step 4 : Imputing Construction Year with Median 

train$construction_year[which(train$construction_year==0)] <- median(train$construction_year[which(train$construction_year!=0)])

#Step 5 : Imputing Permit and Public Meeting

train$permit[which(is.na(train$permit))] <- TRUE
train$public_meeting[which(is.na(train$public_meeting))] <- TRUE


#Step 6 : Imputing Scheme Management

train$scheme_management <- if_else(train$scheme_management=="",train$management,train$scheme_management)

#Step 7 : Creating Groups for Funder and Installer
train$funder <- tolower(train$funder)
train$funder_grp <- 'Other'
train$funder_grp[which(train$funder %like% "african|gov|government|council|ministry|government|goverm|agency|district water depar|department|tanzania|Government")] <- "Government"
train$funder_grp[which(train$funder %like% "unicef|world|european|netherlands|italian|Germany|japan|german| korea| niger|frankfurt| british| netherlands| embassy| u.s.a| european union |holland| international| africa| finland| unesco| irish| Greec| swisland| imf| china|swedish")] <- "International"
train$funder_grp[which(train$funder %like% "aid|Aid")] <- "Aid"
train$funder_grp[which(train$funder %like% "sister|church|catholic|muslim|missionary")] <- "Religious"
train$funder_grp[which(train$funder %like% "priva|private|private company|private individual")] <- "Private"
train$funder_grp[which(train$funder %like% "district|rural|village|municipal|local|community")] <- "Local"
train$funder_grp[which(train$funder %like% "0|1|no|not known")] <- "Unknown"
train$funder_grp[which(nchar(train$funder)==1)] <- "Unknown"
train$funder_grp[which((train$funder)=="")] <- "Unknown"

train$installer <- tolower(train$installer)
train$installer_grp <- 'Other'
train$installer_grp[which(train$installer %like% "african|gov|government|council|ministry|government|goverm|agency|district water depar|department|tanzania|Government")] <- "Government"
train$installer_grp[which(train$installer %like% "unicef|world|european|netherlands|italian|Germany|japan|german| korea| niger|frankfurt| british| netherlands| embassy| u.s.a| european union |holland| international| africa| finland| unesco| irish| Greec| swisland| imf| china|swedish")] <- "International"
train$installer_grp[which(train$installer %like% "aid|Aid")] <- "Aid"
train$installer_grp[which(train$installer %like% "sister|church|catholic|muslim|missionary")] <- "Religious"
train$installer_grp[which(train$installer %like% "priva|private|private company|private individual")] <- "Private"
train$installer_grp[which(train$installer %like% "district|rural|village|municipal|local|community")] <- "Local"
train$installer_grp[which(train$installer %like% "0|1|no|not known")] <- "Unknown"
train$installer_grp[which(nchar(train$installer)==1)] <- "Unknown"
train$installer_grp[which((train$installer)=="")] <- "Unknown"

##### PART 2 - MODELLING #####
model_variables <- c("amount_tsh",    
                     "gps_height","longitude","latitude",      
                     "population",           
                     "public_meeting","scheme_management",    
                     "permit","construction_year",    
                     "extraction_type","extraction_type_group","extraction_type_class",
                     "management","management_group","payment",              
                     "payment_type","water_quality","quality_group",        
                     "quantity","quantity_group","source",               
                     "source_type","source_class","waterpoint_type",      
                     "waterpoint_type_group","funder_grp",           
                     "installer_grp","defective")


train_subset <- train[,model_variables]
train_subset$scheme_management <- tolower(train_subset$scheme_management)
train_subset <- as.data.frame(unclass(train_subset))
train_subset$defective <- as.character(train_subset$defective)
train_subset$defective <- if_else(train_subset$defective=='no',FALSE,TRUE)


model_1 <- glm(defective ~.,data = train_subset)

library(MASS)
step <- stepAIC(model_1, direction="both")

step


classifier = randomForest(x = train_subset[,c("amount_tsh","gps_height","longitude","latitude", 
                                              "population","public_meeting","scheme_management","permit", 
                                              "construction_year","extraction_type","management","payment", 
                                              "water_quality","quantity","source","waterpoint_type","funder_grp", 
                                              "installer_grp")],
                          y = train_subset$defective,
                          ntree = 5)


##### PART 3 - APPLYING MODEL #####

test$latitude <- gsub(-0.00000002,0,test$latitude)
test$latitude <- as.numeric(test$latitude)

mean_latlong <- test %>% subset(test$longitude!=0) %>% 
  group_by(district_code,region_code,ward,lga,subvillage) %>% 
  summarise(lat = mean(latitude),long = mean(longitude)) %>% ungroup()

test <- left_join(test,mean_latlong)
test$longitude <- if_else(test$longitude==0,test$long,test$longitude)
test$latitude <- if_else(test$latitude==0,test$lat,test$latitude)
test[,c(41,42)] <- NULL
test$latitude[is.na(test$latitude)] <- 0
test$longitude[is.na(test$longitude)] <- 0

mean_latlong <- test %>% subset(test$longitude!=0) %>% 
  group_by(district_code,region_code,ward,lga) %>% 
  summarise(lat = mean(latitude),long = mean(longitude)) %>% ungroup()

test <- left_join(test,mean_latlong)
test$longitude <- if_else(test$longitude==0,test$long,test$longitude)
test$latitude <- if_else(test$latitude==0,test$lat,test$latitude)
test[,c(41,42)] <- NULL
test$latitude[is.na(test$latitude)] <- 0
test$longitude[is.na(test$longitude)] <- 0

mean_latlong <- test %>% subset(test$longitude!=0) %>% 
  group_by(district_code,region_code) %>% 
  summarise(lat = mean(latitude),long = mean(longitude)) %>% ungroup()

test <- left_join(test,mean_latlong)
test$longitude <- if_else(test$longitude==0,test$long,test$longitude)
test$latitude <- if_else(test$latitude==0,test$lat,test$latitude)
test[,c(41,42)] <- NULL
test$latitude[is.na(test$latitude)] <- 0
test$longitude[is.na(test$longitude)] <- 0

mean_latlong <- test %>% subset(test$longitude!=0) %>% 
  group_by(district_code) %>% 
  summarise(lat = mean(latitude),long = mean(longitude)) %>% ungroup()

test <- left_join(test,mean_latlong)
test$longitude <- if_else(test$longitude==0,test$long,test$longitude)
test$latitude <- if_else(test$latitude==0,test$lat,test$latitude)
test[,c(41,42)] <- NULL
test$latitude[is.na(test$latitude)] <- 0
test$longitude[is.na(test$longitude)] <- 0

rm(mean_latlong)
#Step 2 : Imputiong Population with median

mean_pop <- test %>% subset(test$population!=0) %>% 
  group_by(district_code,region_code,ward,lga,subvillage) %>% 
  summarise(pop = mean(population)) %>% ungroup()
mean_pop$pop <- as.integer(mean_pop$pop)

test <- left_join(test,mean_pop)
test$population <- if_else(test$population==0,test$pop,test$population)

test[,c(41)] <- NULL
test$population[which(is.na(test$population))] <- 0

test$population <- if_else(test$population==0,median(test$population[which(test$population != 0)]),test$population)

rm(mean_pop)

#Step 3 : Imputing gps_height with mean at Subvillage,Ward,Lga,region,district

test$gps_height[which(test$gps_height < 0)] <- 0

#sub-village
mean_ht <- test %>% subset(test$gps_height!=0) %>% 
  group_by(district_code,region_code,ward,lga,subvillage) %>% 
  summarise(hgt = mean(gps_height)) %>% ungroup()
mean_ht$hgt <- as.integer(mean_ht$hgt)

test <- left_join(test,mean_ht)
test$gps_height <- if_else(test$gps_height==0,test$hgt,test$gps_height)

test[,c(41)] <- NULL
test$gps_height[which(is.na(test$gps_height))] <- 0
test$gps_height <- as.integer(test$gps_height)
length(which(test$gps_height==0))

#ward
mean_ht <- test %>% subset(test$gps_height!=0) %>% 
  group_by(district_code,region_code,ward,lga) %>% 
  summarise(hgt = mean(gps_height)) %>% ungroup()
mean_ht$hgt <- as.integer(mean_ht$hgt)

test <- left_join(test,mean_ht)
test$gps_height <- if_else(test$gps_height==0,test$hgt,test$gps_height)

test[,c(41)] <- NULL
test$gps_height[which(is.na(test$gps_height))] <- 0
test$gps_height <- as.integer(test$gps_height)
length(which(test$gps_height==0))

#lga
mean_ht <- test %>% subset(test$gps_height!=0) %>% 
  group_by(district_code,region_code,lga) %>% 
  summarise(hgt = mean(gps_height)) %>% ungroup()
mean_ht$hgt <- as.integer(mean_ht$hgt)

test <- left_join(test,mean_ht)
test$gps_height <- if_else(test$gps_height==0,test$hgt,test$gps_height)

test[,c(41)] <- NULL
test$gps_height[which(is.na(test$gps_height))] <- 0
test$gps_height <- as.integer(test$gps_height)
length(which(test$gps_height==0))

#region
mean_ht <- test %>% subset(test$gps_height!=0) %>% 
  group_by(district_code,region_code) %>% 
  summarise(hgt = mean(gps_height)) %>% ungroup()
mean_ht$hgt <- as.integer(mean_ht$hgt)

test <- left_join(test,mean_ht)
test$gps_height <- if_else(test$gps_height==0,test$hgt,test$gps_height)

test[,c(41)] <- NULL
test$gps_height[which(is.na(test$gps_height))] <- 0
test$gps_height <- as.integer(test$gps_height)
length(which(test$gps_height==0))

#district
mean_ht <- test %>% subset(test$gps_height!=0) %>% 
  group_by(district_code) %>% 
  summarise(hgt = mean(gps_height)) %>% ungroup()
mean_ht$hgt <- as.integer(mean_ht$hgt)

test <- left_join(test,mean_ht)
test$gps_height <- if_else(test$gps_height==0,test$hgt,test$gps_height)

test[,c(41)] <- NULL
test$gps_height[which(is.na(test$gps_height))] <- 0
test$gps_height <- as.integer(test$gps_height)
length(which(test$gps_height==0))

test$gps_height[which(test$ward == "Masanga")] <- 1173
test$gps_height[which(test$ward == "Matomondo")] <- 1091
test$gps_height[which(test$ward == "Gode Gode")] <- 837
test$gps_height[which(test$ward == "Kiegeani")] <- 53
test$gps_height[which(test$ward == "Kilindoni")] <- 53

rm(mean_ht)

#Step 4 : Imputing Construction Year with Median 

test$construction_year[which(test$construction_year==0)] <- median(test$construction_year[which(test$construction_year!=0)])

#Step 5 : Imputing Permit and Public Meeting

test$permit[which(is.na(test$permit))] <- TRUE
test$public_meeting[which(is.na(test$public_meeting))] <- TRUE


#Step 6 : Imputing Scheme Management

test$scheme_management <- if_else(test$scheme_management=="",test$management,test$scheme_management)

#Step 7 : Creating Groups for Funder and Installer
test$funder <- tolower(test$funder)
test$funder_grp <- 'Other'
test$funder_grp[which(test$funder %like% "african|gov|government|council|ministry|government|goverm|agency|district water depar|department|tanzania|Government")] <- "Government"
test$funder_grp[which(test$funder %like% "unicef|world|european|netherlands|italian|Germany|japan|german| korea| niger|frankfurt| british| netherlands| embassy| u.s.a| european union |holland| international| africa| finland| unesco| irish| Greec| swisland| imf| china|swedish")] <- "International"
test$funder_grp[which(test$funder %like% "aid|Aid")] <- "Aid"
test$funder_grp[which(test$funder %like% "sister|church|catholic|muslim|missionary")] <- "Religious"
test$funder_grp[which(test$funder %like% "priva|private|private company|private individual")] <- "Private"
test$funder_grp[which(test$funder %like% "district|rural|village|municipal|local|community")] <- "Local"
test$funder_grp[which(test$funder %like% "0|1|no|not known")] <- "Unknown"
test$funder_grp[which(nchar(test$funder)==1)] <- "Unknown"
test$funder_grp[which((test$funder)=="")] <- "Unknown"

test$installer <- tolower(test$installer)
test$installer_grp <- 'Other'
test$installer_grp[which(test$installer %like% "african|gov|government|council|ministry|government|goverm|agency|district water depar|department|tanzania|Government")] <- "Government"
test$installer_grp[which(test$installer %like% "unicef|world|european|netherlands|italian|Germany|japan|german| korea| niger|frankfurt| british| netherlands| embassy| u.s.a| european union |holland| international| africa| finland| unesco| irish| Greec| swisland| imf| china|swedish")] <- "International"
test$installer_grp[which(test$installer %like% "aid|Aid")] <- "Aid"
test$installer_grp[which(test$installer %like% "sister|church|catholic|muslim|missionary")] <- "Religious"
test$installer_grp[which(test$installer %like% "priva|private|private company|private individual")] <- "Private"
test$installer_grp[which(test$installer %like% "district|rural|village|municipal|local|community")] <- "Local"
test$installer_grp[which(test$installer %like% "0|1|no|not known")] <- "Unknown"
test$installer_grp[which(nchar(test$installer)==1)] <- "Unknown"
test$installer_grp[which((test$installer)=="")] <- "Unknown"

model_variables <- c("amount_tsh",    
                     "gps_height","longitude","latitude",      
                     "population",           
                     "public_meeting","scheme_management",    
                     "permit","construction_year",    
                     "extraction_type","extraction_type_group","extraction_type_class",
                     "management","management_group","payment",              
                     "payment_type","water_quality","quality_group",        
                     "quantity","quantity_group","source",               
                     "source_type","source_class","waterpoint_type",      
                     "waterpoint_type_group","funder_grp",           
                     "installer_grp")


test_subset <- test[,model_variables]
test_subset$scheme_management <- tolower(test_subset$scheme_management)
test_subset <- as.data.frame(unclass(test_subset))

test_subset <- rbind(train_subset[1,-28],test_subset)
test_subset <- test_subset[-1,]

y_pred = predict(classifier, newdata = test_subset)

y_pred <- if_else(y_pred<0.5,'No','Yes')

output <- cbind(test[,1],y_pred)

names(output) <- c('id','defective')
output <-as.data.frame(output)

fwrite(output,"Output.csv",row.names = F)
