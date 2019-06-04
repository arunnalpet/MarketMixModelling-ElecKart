###  This is the code to perform Market Mix Modelling for given sales data.    
###  Data Sources available:                                                   
###    - Item wise sales data.                                                 
###    - Media Investment Spends.                                              
###    - Special Sale Calander                                                 
###    - Monthly NPS score.                                                    
###   Note: Please reset the working directory according to your environment   


# Importing necessary packages
library(lubridate)
library(dplyr)
library(MASS)
library(car)

# set directory
setwd('C:/Project/IIITB/7. Capstone Project/dataset')


###########################################################################
###########################################################################
###                                                                     ###
###                              LOAD DATA                              ###
###                                                                     ###
###########################################################################
###########################################################################

# Sales Data
# TODO: stringsAsFactors??
sales <- read.csv('ConsumerElectronics.csv')

# For convinience and since data was small, xls was manually converted to csv
# in R readable format.

# Media Investment Data
media <- read.csv('MediaInvestment.csv')
media[is.na(media)] <- 0


# Special Sale Calendar.
splSale <- read.csv('SpecialSale.csv')
splSale$Date<- mdy(splSale$Date, tz = "UTC")

# NPS Score
npsscore <- read.csv('MonthlyNPSscore.csv')
npsscore$Date<- mdy(npsscore$Date, tz = "UTC")

############################################################################
############################################################################
###                                                                      ###
###                           DATA EXPLORATION                           ###
###                                                                      ###
############################################################################
############################################################################


summary(sales)
str(sales)

# No. of records present.
nrow(sales)
# 16,48,824

# cust_id, pincode, OrderID and OrderItemID are represented in exponential format.
# Convert it to integers.
options(scipen=999)

# Check the data now
# View(sales)

# Rename FSN, order payment type column name
colnames(sales)[1] <- "fsn_id"
colnames(sales)[11] <- "payment_type"

# Analyze each column
# FSN_ID:
# Unique records of each SKU
length(unique(sales$fsn_id))
# 21,219 different products are available in ElecKart store.

# Order Date:
# This column contains the date format. We will not be dealing with time series forecasting.
# Hence will not include this model directly for modelling.
# But later, will need to derive other factors from this column.

# Order_id
# Unique records of order_id
length(unique(sales$order_id))
# 15,01,177

# order_item_id
# Unique records of order_item_id
length(unique(sales$order_item_id))
# 1480765

# GMV: Gross Merchandise Value or Revenue
# Observing the data for higher numbers of units, 
# We are assuming GMV mentioned here is for the total number of units specified against it.
summary(sales$gmv)
# There are Records having 0 GMV. Should be incorrect, hence delete it.
nrow(sales[which(sales$gmv == 0),])
# 1349 rows doesnt have GMV

# Units
sort(unique(sales$units))

# Order Payment Type:
unique(sales$payment_type)
# We have two types
# Prepaid, Cash on Delivery.

# SLA
sort(unique(sales$sla))
# Observe records which have SLA more than 1000 days.
sales[which(sales$sla>200),]
# 4 items have SLA more than 200, which seems faulty data.

sales[which(sales$sla>65),]
# 8 records have more than 65 days of SLA.
# We should reset these to 65 days.

# We can now see that customer ID and zip code is in negative. Lets explore it.
nrow(sales[which(sales$cust_id < 0),])
# 8,23,414 have negative cutomer id.

length(unique(sales[which(sales$cust_id < 0),"cust_id"]))
# 6,01,237 are unique customers which have negative value.
# This looks random
# Hence Leaving it as is.
# This is unique ID, hence leaving it as is, not considering for our analysis.

# Zip code is also negative.
# Not considering Zipcode for our analysis.

# Product MRP
summary(sales$product_mrp)
# Product ranges from 0 to 2,99,999
# Explore free products.
nrow(sales[which(sales$product_mrp == 0),])
# 5308 products have zero MRP but positive GMV. 
# Need to clean up/lookup MRP for that.
length(unique(sales[which(sales$product_mrp == 0), "fsn_id"]))
# 249 out of 21,219 products do not have MRP

# Product Procurement SLA
summary(sales$product_procurement_sla)
sort(unique(sales$product_procurement_sla))
nrow(sales[which(sales$product_procurement_sla>999),])
# 4745 rows
length(unique(sales[which(sales$product_procurement_sla>999),"fsn_id"]))
# 59 SKUs have product procurement slas greater than 999.

nrow(sales[which(sales$product_procurement_sla == -1),])
# 75986 records have negative procurement sla.
length(unique(sales[which(sales$product_procurement_sla == -1),"fsn_id"]))
# 540 SKUs have product procurement sla of -1


# Analyze Month
summary(sales$Month)
sort(unique(sales$Month))
# Month data is clean

# Analyze Year
unique(sales$Year)
# Data is clean.

# deliverycdays and deliverybdays
# TODO: Assume -1 is zero delay.
unique(sales$deliverybdays)
unique(sales$deliverycdays)

nrow(sales[which(sales$deliverybdays == "\\N"),])
# 1312972
nrow(sales[which(sales$deliverycdays == "\\N"),])
# 1312971
# Almost 80% of the data is filled with this value. Which has no interpretation.
# Hence ignoring these two columns for modelling.

# Look for NAs
sum(is.na(sales))
# 14,712

# Check which columns have NA values.
sapply(sales, function(x) length(which(is.na(x))))
# cust_id, pin-code ang gmv each of them have 4094 null values.



###########################################################################
###########################################################################
###                                                                     ###
###                               CLEANUP                               ###
###                                                                     ###
###########################################################################
###########################################################################


# Delete rows having 0 for these attributes.
# gmv
# product_mrp
# product_procurement_sla
sales <- sales[-which(sales$gmv == 0),]
sales <- sales[-which(sales$product_mrp == 0),]
sales <- sales[-which(sales$product_procurement_sla == 0),]

##----------------------------------------------------------------

# Delete rows having -1 for these attributes.
# product_procurement_sla
sales <- sales[-which(sales$product_procurement_sla == -1),]

##----------------------------------------------------------------

# Cap these attributes to trim the outliers.
# sla
# product_procurement_sla

# cap sla greater than 100 to 65
sales$sla[which(sales$sla > 100)] <- 65

# cap product_procurement_sla greater than 100 to 15
sales$product_procurement_sla[which(sales$product_procurement_sla > 100)] <- 15

##----------------------------------------------------------------

# Delete Rows having NA for the following attributes.
# gmv
# cust_id
sales <- na.omit(sales)


##----------------------------------------------------------------

# Convert to date format.
head(sales$order_date)
sales$order_date <- ymd_hms(sales$order_date)

##----------------------------------------------------------------

# Clean up data that does not lies between July 2015 to June 2016
nrow(sales[which(sales$order_date < "2015-07-01"),])
# 6
nrow(sales[which(sales$order_date > "2016-06-30"),])
# 4830
sales <- sales[-which(sales$order_date < "2015-07-01"),]
sales <- sales[-which(sales$order_date > "2016-06-30"),]

##----------------------------------------------------------------

# Filter data to include only three analytic sub category as mentioned in problem statement.
# CameraAccessory
# AudioAccessory
# HomeAudio

sales <- sales[which(sales$product_analytic_sub_category %in% c("CameraAccessory", "AudioAccessory", "HomeAudio")),]


############################################################################
############################################################################
###                                                                      ###
###                    DERIVE OTHER FACTORS FROM DATA                    ###
###                                                                      ###
############################################################################
############################################################################

# Derive week number.

# Since we are considering 1 years data, we will have 53 weeks.
# Consider 1-July-2015 as week1
# Week number is calculated as  (sales$order_date - 30-June-2015)/7. Modulo of this will be the week number.
start_date_ref <- as.POSIXct(ymd("2015-06-30", tz = "UTC"))
# as.numeric(sales[1,"order_date"] - as.POSIXct(ymd("2015-07-01", tz = "UTC")))
sales$week_num <- ceiling(as.numeric(sales$order_date - start_date_ref)/7)


##----------------------------------------------------------------

# dicount offered. ((MRP-(GMV/Units))/MRP)*100

sales$discount <- ((sales$product_mrp-(sales$gmv/sales$units))/sales$product_mrp)*100
sales$discount <- round(sales$discount,2)
sort(unique(ceiling(sales$discount)))
# Discount ranges from -195% to 99%
# negative value here indicates that the product was upsold. (as compared to mrp)

##----------------------------------------------------------------

# Unit price

sales$unitPrice <- sales$gmv/sales$units
summary(sales$unitPrice)


###########################################################################
###########################################################################
###                                                                     ###
###                              GENERATE                               ###
###                    WEEKLY AGGREGATED INFORMATION                    ###
###                                                                     ###
###########################################################################
###########################################################################


##################################################################
##                           Generate                           ##
##                     Weekly consumer data                     ##
##################################################################


# Aggregate by product_analytic_sub_category and week.
# sum(gmv)
# sum(units)
# Payment Type: Categorical value, convert to dummy.
# mean(sla)
# product mrp??? mean/sum?
# mean(product_procurement_sla)
# week number
# mean(discount)
# mean(unitPrices) mean/sum?

# Payment Type: Converting to binary format
sales$payment_cod <- ifelse(sales$payment_type=="COD",1,0)
sales$payment_prepaid <- ifelse(sales$payment_type=="Prepaid",1,0)

sales_per_week <- group_by(sales, week_num, product_analytic_sub_category)

sales_per_week <- summarise(sales_per_week, total_gmv = sum(gmv), total_units=sum(units),
          no_of_orders = n(),        # Total number of orders placed during that week
          total_payment_cod=sum(payment_cod), total_payment_prepaid = sum(payment_prepaid),
          avg_sla=mean(sla), avg_product_mrp=mean(product_mrp),
          avg_product_procurement_sla=mean(product_procurement_sla),
          avg_discount=mean(discount),
          avg_unitPrice = mean(unitPrice))

sales_per_week <- as.data.frame(sales_per_week)


##################################################################
##                           Generate                           ##
##                 Weekly Media Investment Data                 ##
##################################################################


# Create date to week mapping data frame
daytoweek<-data.frame(day = (seq(ymd('2015-07-01'),ymd('2016-06-30'), by = 'days')), daynum=seq(1,366))
daytoweek$week_num <- ceiling(daytoweek$daynum/7)
daytoweek$day <- ymd(daytoweek$day)
daytoweek$Month <- month(as.POSIXlt(daytoweek$day))

##----------------------------------------------------------------

# Calculate daily wise advertisement spend.


media_weekly <- daytoweek

temp <- merge(x=media_weekly, y=media, by = "Month")

for (media_type in c("Total.Investment","TV","Digital","Sponsorship",
                     "Content.Marketing","Online.marketing","Affiliates","SEM","Radio","Other"))
  temp[,media_type] <- temp[,media_type]/(as.numeric(days_in_month(temp$Month)))

media_weekly <- temp

##----------------------------------------------------------------

# Aggregate daily wise spend to week wise.
media_weekly <- as.data.frame(temp[,c(4,6:15)] %>% group_by(week_num) %>% summarise_all(list(sum)))


#################################################################
##                          Calculate                          ##
##                     Weekly Special Sale                     ##
#################################################################

# Check how many special sale are there in each week.
splSale$spl_days <- 1
splSale <- splSale[,-2]
splSale$week_num <- ceiling(as.numeric(splSale$Date - start_date_ref)/7)

splSale <- as.data.frame(splSale[,c(2,3)] %>% group_by(week_num) %>% summarise(spl_days=sum(spl_days)))

# splSale_weekly <- merge(x = daytoweek, y=splSale, on.x = "day", on.y = "Date")

temp <- data.frame(week_num=seq(1,53))
splSale_weekly <- merge(x=temp, y=splSale, on=week_num, all.x = TRUE)
splSale_weekly[is.na(splSale_weekly)] <- 0


#################################################################
##                          Generate                           ##
##                       Weekly NPS data                       ##
#################################################################

# npsscore$week_num <- ceiling(as.numeric(npsscore$Date - start_date_ref)/7)
npsscore$Month <- month(as.POSIXlt(npsscore$Date))

nps_weekly <- daytoweek

temp <- merge(x=nps_weekly, y=npsscore[,c(2,3)], by = "Month")

temp[,"NPS"] <- temp[,"NPS"]/(as.numeric(days_in_month(temp$Month)))

nps_weekly <- temp

##----------------------------------------------------------------

# Aggregate daily wise spend to week wise.
nps_weekly <- as.data.frame(temp[,c(4,5)] %>% group_by(week_num) %>% summarise(NPS=sum(NPS)))




############################################################################
############################################################################
###                                                                      ###
###                          MERGE ALL WEEKLY:                           ###
###              SALES, SPL HOLIDAYS, MEDIA INVESTMENT, NPS              ###
###                                                                      ###
############################################################################
############################################################################

sales_all <- merge(x=sales_per_week, y=media_weekly, on=week_num)
sales_all <- merge(x=sales_all, y=splSale_weekly, on=week_num)
sales_all <- merge(x=sales_all, y=nps_weekly, on=week_num)

# TODO: Remove redundant/unwanted columns not needed for model building.
# Total.Investments
# Week Number
# Analytics sub category.


###########################################################################
###########################################################################
###                                                                     ###
###                            MODEL BUILDING                           ###
###                          LINEAR REGRESSION                          ###
###                                                                     ###
###########################################################################
###########################################################################



# Performing Model building, to identify the relationship of KPIs with the gmv
# Since we are only trying to predict the top KPIs, we dont need to split data to test and train.

##----------------------------------------------------------------


#################################################################
##            Model Building for Camera Accessories            ##
#################################################################

sales_cam_acc <- sales_all[which(sales_all$product_analytic_sub_category == "CameraAccessory"),]

# Consider only derived KPIs and marketing levers, including spl days, nps and media investment.
temp <- sales_cam_acc
sales_cam_acc <- sales_cam_acc[,-c(1,2,4:10,12,13)]


# Scale all data (had no effect)
temp <- data.frame(lapply(sales_cam_acc[,c(2:13)],function(x) scale(x)))
sales_cam_acc <- cbind(sales_cam_acc[,1], temp)
colnames(sales_cam_acc)[1] <- "total_gmv"

model_1 <- lm(total_gmv~., data = sales_cam_acc)
summary(model_1)
# Adjusted R-squared: 0.9758

step<-stepAIC(model_1,direction = "both")

model_2 <- lm(formula = total_gmv ~ TV + Digital + Sponsorship + Content.Marketing + 
                Affiliates + SEM + Radio + spl_days, data = sales_cam_acc)

summary(model_2)
# Adjusted R-squared: 0.9776
# Adjusted R-squared: 0.5562 NEW

sort(vif(model_2))


# VIF of digital marketing is too high = 115. Remove it.

model_3 <- lm(formula = total_gmv ~ TV + Sponsorship + Content.Marketing + 
                Affiliates + SEM + Radio + spl_days, data = sales_cam_acc)

summary(model_3)
# Adjusted R-squared: 0.964
# Adjusted R-squared: 0.501

sort(vif(model_3))


# VIF of content marketing is too high = 48. Remove it.

model_4 <- lm(formula = total_gmv ~ TV + Sponsorship + 
                Affiliates + SEM + Radio + spl_days, data = sales_cam_acc)

summary(model_4)
# Adjusted R-squared: 0.502

sort(vif(model_4))

# P-value of Radio is too high = 0.3. Remove it.
model_5 <- lm(formula = total_gmv ~ TV + Sponsorship + 
                Affiliates + SEM + spl_days, data = sales_cam_acc)

summary(model_5)
# Adjusted R-squared: 0.5037

sort(vif(model_5))


# P-Value of spl days is too high = 0.37. Remove it.
model_6 <- lm(formula = total_gmv ~ TV + Sponsorship + 
                Affiliates + SEM, data = sales_cam_acc)
summary(model_6)
# Adjusted R-squared: 0.5059

sort(vif(model_6))

# Test
predict_1 <- predict(model_6, sales_cam_acc[,-which(colnames(sales_cam_acc) == "total_gmv")])

cor(predict_1,sales_cam_acc$total_gmv)
# 0.7380205
cor(predict_1,sales_cam_acc$total_gmv)^2
# 0.5446743

# Adjusted R2 of model6 = 0.5059
# R2 from prediction = 0.5446743
# Difference is 0.0387743 which is low. That means, model is able to explain the data well.

# Plot actual and predicted
plot(sales_cam_acc$total_gmv, main='Camera Accessory', xlab = 'week', ylab = 'PredictGMV')
lines(sales_cam_acc$total_gmv)
lines(predict_1, col='red', lwd=2)


##################################################################
##                          Conclusion:                         ##
##                      Camera Accessories                      ##
##################################################################

# All are significant so far.
# TV, Sponsorship, Affiliates and SEM are key factors influencing the revenue.



##################################################################
##             Model Building for Audio Accessories             ##
##################################################################

sales_aud_acc <- sales_all[which(sales_all$product_analytic_sub_category == "AudioAccessory"),]

# Consider only derived KPIs and marketing levers, including spl days, nps and media investment.
temp <- sales_aud_acc
sales_aud_acc <- sales_aud_acc[,-c(1,2,4:10,12,13)]


# Scale all data (had no effect)
temp <- data.frame(lapply(sales_aud_acc[,c(2:13)],function(x) scale(x)))
sales_aud_acc <- cbind(sales_aud_acc[,1], temp)
colnames(sales_aud_acc)[1] <- "total_gmv"

model_1 <- lm(total_gmv~., data = sales_aud_acc)
summary(model_1)
# Adjusted R-squared: 0.3004

step<-stepAIC(model_1,direction = "both")
step

model_2 <- lm(formula = total_gmv ~ Digital + Sponsorship + Content.Marketing + 
                Affiliates + SEM + Radio + Other, data = sales_aud_acc)

summary(model_2)
# Adjusted R-squared: 0.3737

sort(vif(model_2))

# Content.Marketing has high vif 46, and low p-value=0.133. Remove it.
model_3 <- lm(formula = total_gmv ~ Digital + Sponsorship +
                Affiliates + SEM + Radio + Other, data = sales_aud_acc)

summary(model_3)
# Adjusted R-squared: 0.3542

sort(vif(model_3))


# Digital has high VIF=204 and very high p-value: 0.055. Remove it.
model_4 <- lm(formula = total_gmv ~ Sponsorship +
                Affiliates + SEM + Radio + Other, data = sales_aud_acc)

summary(model_4)
# Adjusted R-squared: 0.3117

sort(vif(model_4))

# Other has high VIF: Remove it
# Digital has high VIF=204 and very high p-value: 0.055. Remove it.
model_5 <- lm(formula = total_gmv ~ Sponsorship +
                Affiliates + SEM + Radio, data = sales_aud_acc)

summary(model_5)
# Adjusted R-squared: 0.2354

sort(vif(model_5))


# Radio has least significance: 0.588. Remove it.
model_6 <- lm(formula = total_gmv ~ Sponsorship +
                Affiliates + SEM, data = sales_aud_acc)

summary(model_6)
# Adjusted R-squared: 0.2471

sort(vif(model_6))


# Test
predict_1 <- predict(model_6, sales_aud_acc[,-which(colnames(sales_aud_acc) == "total_gmv")])


cor(predict_1,sales_aud_acc$total_gmv)
# 0.5414558
cor(predict_1,sales_aud_acc$total_gmv)^2
# 0.2931744

# Adjusted R2 of model6 = 0.2471
# R2 from prediction = 0.2931744
# Difference is 0.046 which is low. That means, model is able to explain the data well.

# Plot actual and predicted
plot(sales_aud_acc$total_gmv, main='Audio Accessory', xlab = 'week', ylab = 'PredictGMV')
lines(sales_aud_acc$total_gmv)
lines(predict_1, col='red', lwd=2)

##################################################################
##                          Conclusion:                         ##
##                       Audio Accessories                      ##
##################################################################

# All are significant so far.
# Sponsorship, Affiliates and SEM are key factors influencing the revenue.


##################################################################
##                 Model Building for Home Audio                ##
##################################################################

sales_hom_aud <- sales_all[which(sales_all$product_analytic_sub_category == "HomeAudio"),]

# Consider only derived KPIs and marketing levers, including spl days, nps and media investment.
temp <- sales_hom_aud
sales_hom_aud <- sales_hom_aud[,-c(1,2,4:10,12,13)]


# Scale all data (had no effect)
temp <- data.frame(lapply(sales_hom_aud[,c(2:13)],function(x) scale(x)))
sales_hom_aud <- cbind(sales_hom_aud[,1], temp)
colnames(sales_hom_aud)[1] <- "total_gmv"

model_1 <- lm(total_gmv~., data = sales_hom_aud)
summary(model_1)
# Adjusted R-squared: 0.4798

step<-stepAIC(model_1,direction = "both")
step

model_2 <- lm(formula = total_gmv ~ avg_discount + Digital + Sponsorship + 
                Online.marketing + Affiliates + SEM + Radio + Other + spl_days, 
              data = sales_hom_aud)
summary(model_2)
# Adjustd R-squared: 0.5124

sort(vif(model_2))

# Online marketing has high VIF. Eliminating it.
model_3 <- lm(formula = total_gmv ~ avg_discount + Digital + Sponsorship + 
                Affiliates + SEM + Radio + Other + spl_days, 
              data = sales_hom_aud)
summary(model_3)
# Adjustd R-squared: 0.4868

sort(vif(model_3))


# SEM has highest VIF. Remove it.
model_4 <- lm(formula = total_gmv ~ avg_discount + Digital + Sponsorship + 
                Affiliates + Radio + Other + spl_days, 
              data = sales_hom_aud)
summary(model_4)
# Adjustd R-squared: 0.4352

sort(vif(model_4))


# Radio has high VIF. Remove it.
model_5 <- lm(formula = total_gmv ~ avg_discount + Digital + Sponsorship + 
                Affiliates + Other + spl_days, 
              data = sales_hom_aud)
summary(model_5)
# Adjustd R-squared: 0.4476

sort(vif(model_5))


# Digital has least significance. Remove it.
model_6 <- lm(formula = total_gmv ~ avg_discount + Sponsorship + 
                Affiliates + Other + spl_days, 
              data = sales_hom_aud)
summary(model_6)
# Adjustd R-squared: 0.4564

sort(vif(model_6))


# Other category has low significance. Remove it.
model_7 <- lm(formula = total_gmv ~ avg_discount + Sponsorship + 
                Affiliates + spl_days, 
              data = sales_hom_aud)
summary(model_7)
# Adjustd R-squared: 0.4648

sort(vif(model_7))


# Affiliates has low significance. Remove it.
model_8 <- lm(formula = total_gmv ~ avg_discount + Sponsorship + 
                spl_days, 
              data = sales_hom_aud)
summary(model_8)
# Adjustd R-squared: 0.4534

sort(vif(model_8))

# Test
predict_1 <- predict(model_8, sales_hom_aud[,-which(colnames(sales_hom_aud) == "total_gmv")])


cor(predict_1,sales_hom_aud$total_gmv)
# 0.6977267
cor(predict_1,sales_hom_aud$total_gmv)^2
# 0.4868225

# Adjusted R2 of model6 = 0.4534
# R2 from prediction = 0.4868225
# Difference is 0.033 which is low. That means, model is able to explain the data well.

##################################################################
##                          Conclusion:                         ##
##                       Audio Accessories                      ##
##################################################################

# All are significant so far.
# AverageDiscount, Sponsorship and spl_days are key factors influencing the revenue.


# CHARTS
# On MRP, GMV
# Charts on product procurement SLA.
# Charts based on Order Payment Type
# Make box plots, check the range
# check colored bar charts.

# Check co-relation
# Co-relate discounts with sale days.

# Advanced
# Feature Selection: Check the variable importance.


# Create different datasets for different sub-categories.


###########################################################################
###########################################################################
###                                                                     ###
###                      EXPLORATORY DATA ANALYSIS                      ###
###                                                                     ###
###########################################################################
###########################################################################

# Plot Univariate and bivariate analysis plots, wrt dependent variable (revenue)