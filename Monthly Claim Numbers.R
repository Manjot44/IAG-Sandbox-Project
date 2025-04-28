#####################################################################           
# IMPORTING PACKAGES                                                #
#####################################################################

# Installing packages
list.of.packages <- c('dplyr', 'ggplot2', 'lubridate', 'MASS', 'boot')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# Note, assumes all relevant files are in the same folder as the R script
rm(list = ls())
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

library('lubridate')
library('dplyr')
library('ggplot2')
library('MASS')
library('boot')


#####################################################################
# IMPORTING DATA                                                    #
#####################################################################

data <- read.csv('ACTL31425110AssignmentData2022.csv')
data$accident_month = ymd(data$accident_month)
data$year_month = floor_date(data$accident_month, "month")

imports <- read.csv('Import data.csv')
fuel <- read.csv('Mogas_95.csv')
wages <- read.csv('Annual wage growth - 1998 to 2022.csv')


#####################################################################
# SETTING UP DATAFRAMES (INTERNAL PREDICTORS)                       #
#####################################################################

# getting data by month
bymonth <- data %>%
  group_by(year_month) %>%
  summarise(total = n(),
            total_sum_insured = sum(sum_insured),
            avg_sum_insured = total_sum_insured/total,
            avg_yomanuf = sum(year_of_manufacture)/total,
            avg_tenure = sum(policy_tenure)/total,
            monthly_exposure = sum(exposure),
            NSW = sum(risk_state_name == "NSW"),
            VIC = sum(risk_state_name == "VIC"),
            QLD = sum(risk_state_name == "QLD"),
            ACT = sum(risk_state_name == "ACT"),
            TAS = sum(risk_state_name == "TAS"),
            NT = sum(risk_state_name == "NT"),
            SA = sum(risk_state_name == "SA"),
            WA = sum(risk_state_name == "WA"),
            class1 = sum(vehicle_class == "Class 1"),
            class2 = sum(vehicle_class == "Class 2"),
            class3 = sum(vehicle_class == "Class 3"),
            class4 = sum(vehicle_class == "Class 4"),
            class5 = sum(vehicle_class == "Class 5"),
            class6 = sum(vehicle_class == "Class 6"),
            class7 = sum(vehicle_class == "Class 7"),
            class8 = sum(vehicle_class == "Class 8"),
            class9 = sum(vehicle_class == "Class 9"),
            class10 = sum(vehicle_class == "Class 10"),
            class11 = sum(vehicle_class == "Class 11"),
            class12 = sum(vehicle_class == "Class 12"),
            class13 = sum(vehicle_class == "Class 13"),
            class14 = sum(vehicle_class == "Class 14"),
            class15 = sum(vehicle_class == "Class 15"))


# getting the number of claims per month
data_claimsonly <- na.omit(data) %>%
  filter(total_claims_cost != 0)
num_claims <- data_claimsonly %>%
  group_by(year_month) %>%
  summarise(total = n(),
            claims_cost = sum(total_claims_cost),
            avg_claims_cost = claims_cost/total,
            total_sum_insured = sum(sum_insured),
            avg_sum_insured = total_sum_insured/total,
            avg_yomanuf = sum(year_of_manufacture)/total,
            avg_tenure = sum(policy_tenure)/total,
            monthly_exposure = sum(exposure),
            NSW = sum(risk_state_name == "NSW"),
            VIC = sum(risk_state_name == "VIC"),
            QLD = sum(risk_state_name == "QLD"),
            ACT = sum(risk_state_name == "ACT"),
            TAS = sum(risk_state_name == "TAS"),
            NT = sum(risk_state_name == "NT"),
            SA = sum(risk_state_name == "SA"),
            WA = sum(risk_state_name == "WA"),
            class1 = sum(vehicle_class == "Class 1"),
            class2 = sum(vehicle_class == "Class 2"),
            class3 = sum(vehicle_class == "Class 3"),
            class4 = sum(vehicle_class == "Class 4"),
            class5 = sum(vehicle_class == "Class 5"),
            class6 = sum(vehicle_class == "Class 6"),
            class7 = sum(vehicle_class == "Class 7"),
            class8 = sum(vehicle_class == "Class 8"),
            class9 = sum(vehicle_class == "Class 9"),
            class10 = sum(vehicle_class == "Class 10"),
            class11 = sum(vehicle_class == "Class 11"),
            class12 = sum(vehicle_class == "Class 12"),
            class13 = sum(vehicle_class == "Class 13"),
            class14 = sum(vehicle_class == "Class 14"),
            class15 = sum(vehicle_class == "Class 15"))

# calculating frequency of claims and adding to dataframe
freq_bymonth <- (num_claims$total)/(bymonth$monthly_exposure)
bymonth <- cbind(bymonth, freq_bymonth)
bymonth['num_claims'] <- num_claims$total


#####################################################################
# SETTING UP DATAFRAMES (EXTERNAL PREDICTORS)                       #
#####################################################################

bymonth['goods_import'] = imports$goods[7:66]
bymonth['fuel'] = fuel$Mogas_95[2:61]

# ADDING INTERNATIONAL BORDER CLOSURE = 1
bymonth['border_restr'] = 0
for(i in 1:60) {
  if (bymonth$year_month[i] >= as.Date('2020-03-01')) {
    bymonth$border_restr[i] = 1
  }
}

#ADDING MONTHS
bymonth["Jan"] = 0
bymonth$Jan[seq(7, 55, by = 12)] = 1
bymonth["Feb"] = 0
bymonth$Feb[seq(8, 56, by = 12)] = 1
bymonth["Mar"] = 0
bymonth$Mar[seq(9, 57, by = 12)] = 1
bymonth["Apr"] = 0
bymonth$Apr[seq(10, 58, by = 12)] = 1
bymonth["May"] = 0
bymonth$May[seq(11, 59, by = 12)] = 1
bymonth["Jun"] = 0
bymonth$Jun[seq(12, 60, by = 12)] = 1
bymonth["Jul"] = 0
bymonth$Jul[seq(1, 49, by = 12)] = 1
bymonth["Aug"] = 0
bymonth$Aug[seq(2, 50, by = 12)] = 1
bymonth["Sep"] = 0
bymonth$Sep[seq(3, 51, by = 12)] = 1
bymonth["Oct"] = 0
bymonth$Oct[seq(4, 52, by = 12)] = 1
bymonth["Nov"] = 0
bymonth$Nov[seq(5, 53, by = 12)] = 1
bymonth["Dec"] = 0
bymonth$Dec[seq(6, 54, by = 12)] = 1

# LAGGING PREDICTORS
lagged_bymonth <- bymonth[-c(1, 31, 32)] #need to change this
lagged_bymonth <- lagged_bymonth[1:59, ]

lagged_bymonth['year_month'] = bymonth$year_month[2:60]
lagged_bymonth['freq_bymonth'] = freq_bymonth[2:60]
lagged_bymonth['num_claims'] = num_claims$total[2:60]
lagged_bymonth['wages'] = wages$Seasonally.adjusted[1:59]



#####################################################################
# MODELLING                                                         #
#####################################################################

freq_glm <- glm(
  num_claims ~ goods_import + 
    Feb + Apr + Aug + Oct +
    class4 + class5 + class8 +
    class12 + class14,
  data = lagged_bymonth, family = poisson, offset = log(monthly_exposure)
)
summary(freq_glm)

freq_glm1 <- glm.nb(
  num_claims ~ offset(log(monthly_exposure)) + goods_import + 
    Feb + Apr + Aug + Oct +
    class4 + class5 + class8 +
    class12 + class14,
  data = lagged_bymonth,
)
summary(freq_glm1)

freq_glm2 <- glm(
  num_claims ~ goods_import + 
    Feb + Apr + Aug + Oct +
    class4 + class5 + class8 +
    class12 + class14,
  data = lagged_bymonth, family = quasipoisson, offset = log(monthly_exposure)
)
summary(freq_glm2)

sqrt(cv.glm(lagged_bymonth, freq_glm)$delta[1])
sqrt(cv.glm(lagged_bymonth, freq_glm1)$delta[1])
sqrt(cv.glm(lagged_bymonth, freq_glm2)$delta[1])

cv.glm(lagged_bymonth, freq_glm)$delta[1]
cv.glm(lagged_bymonth, freq_glm1)$delta[1]
cv.glm(lagged_bymonth, freq_glm2)$delta[1]

mean(num_claims$total)
sd(num_claims$total)


#####################################################################
# PREDICTING FUTURE CLAIMS                                          #
#####################################################################

# comparing the model to the training data
predicted_num <- matrix(0, nrow = 59, ncol = 1)
for(i in 1:59) {
  tmp <- coef(freq_glm) %*% c(1, lagged_bymonth$goods_import[i],
                                           lagged_bymonth$Feb[i], lagged_bymonth$Apr[i],
                                           lagged_bymonth$Aug[i], lagged_bymonth$Oct[i],
                                           lagged_bymonth$class4[i], lagged_bymonth$class5[i],
                                           lagged_bymonth$class8[i], lagged_bymonth$class12[i],
                                           lagged_bymonth$class14[i])
  tmp = tmp + log(lagged_bymonth$monthly_exposure[i])
  tmp = exp(tmp)
  predicted_num[i, 1] = tmp 
}
predicted_num = cbind(num_claims$total[2:60], predicted_num)

# exporting the claim numbers data so total claims cost can be compared to the training data
write.csv(predicted_num, "Claim Numbers.csv")

# predicting the claims for the next month
tmp = coef(freq_glm) %*% c(1, bymonth$goods_import[60],
                   bymonth$Feb[60], bymonth$Apr[60],
                   bymonth$Aug[60], bymonth$Oct[60],
                   bymonth$class4[60], bymonth$class5[60],
                   bymonth$class8[60], bymonth$class12[60],
                   bymonth$class14[60])
tmp = tmp + log(bymonth$monthly_exposure[60])
tmp = exp(tmp)

# cloning data for next year of predictions
cloning <- data %>%
  filter(year_month == as.Date('2021-06-01'))
cloning$year_month <- as.Date('2021-06-01')
future_dates <- cloning
cloning$year_month <- as.Date('2021-07-01')
future_dates <- rbind(future_dates, cloning)
cloning$year_month <- as.Date('2021-08-01')
future_dates <- rbind(future_dates, cloning)
cloning$year_month <- as.Date('2021-09-01')
future_dates <- rbind(future_dates, cloning)
cloning$year_month <- as.Date('2021-10-01')
future_dates <- rbind(future_dates, cloning)
cloning$year_month <- as.Date('2021-11-01')
future_dates <- rbind(future_dates, cloning)
cloning$year_month <- as.Date('2021-12-01')
future_dates <- rbind(future_dates, cloning)
cloning$year_month <- as.Date('2022-01-01')
future_dates <- rbind(future_dates, cloning)
cloning$year_month <- as.Date('2022-02-01')
future_dates <- rbind(future_dates, cloning)
cloning$year_month <- as.Date('2022-03-01')
future_dates <- rbind(future_dates, cloning)
cloning$year_month <- as.Date('2022-04-01')
future_dates <- rbind(future_dates, cloning)
cloning$year_month <- as.Date('2022-05-01')
future_dates <- rbind(future_dates, cloning)

# exporting cloned data so it can be used by claim severity model
write.csv(future_dates, "cloned data.csv")

# setting up dataframe for next year of predictions
future_bymonth <- future_dates %>%
  group_by(year_month) %>%
  summarise(monthly_exposure = sum(exposure),
            class4 = sum(vehicle_class == "Class 4"),
            class5 = sum(vehicle_class == "Class 5"),
            class8 = sum(vehicle_class == "Class 8"),
            class12 = sum(vehicle_class == "Class 12"),
            class14 = sum(vehicle_class == "Class 14"))

future_bymonth["Feb"] = 0
future_bymonth$Feb[9] = 1
future_bymonth["Apr"] = 0
future_bymonth$Apr[11] = 1
future_bymonth["Aug"] = 0
future_bymonth$Aug[3] = 1
future_bymonth["Oct"] = 0
future_bymonth$Oct[5] = 1

future_bymonth['goods_import'] = imports$goods[66:77]


# predicting the number of claims for the next year
year_pred_num <- matrix(0, nrow = 12, ncol = 1)
for(i in 1:12) {
  tmp <- coef(freq_glm) %*% c(1, future_bymonth$goods_import[i],
                              future_bymonth$Feb[i], future_bymonth$Apr[i],
                              future_bymonth$Aug[i], future_bymonth$Oct[i],
                              future_bymonth$class4[i], future_bymonth$class5[i],
                              future_bymonth$class8[i], future_bymonth$class12[i],
                              future_bymonth$class14[i])
  tmp = tmp + log(future_bymonth$monthly_exposure[i])
  tmp = exp(tmp)
  year_pred_num[i, 1] = tmp 
}

# exporting predictions so claims inflation number can be calculated
write.csv(year_pred_num, "Yearly Prediction.csv")

# creating combined number of claims dataframe for plot
all_months <- matrix(as.factor(c(bymonth$year_month, future_bymonth$year_month[2:12])), ncol = 1)
all_months <- matrix(c(all_months, "2022-06-01"))
all_num <- matrix(c(num_claims$total, year_pred_num), ncol = 1)
all_num <- cbind(all_months, all_num)
all_num <- data.frame(all_num)
all_num$X1 = ymd(all_num$X1)
all_num$X2 = as.numeric(all_num$X2)

# creating vector for grouping
value <- c()
for (i in 1:60) {
  value <- cbind(value, "True Value")
}
for (i in 1:12) {
  value<- cbind(value, "Prediction")
}
all_num['Value'] <- t(value)

#plot of the claim numbers prediction
ggplot(all_num, aes(X1, X2)) +
  geom_point(aes(colour = factor(Value[,1]))) +
  labs(colour = "Value",
       title = "Actual and Predicted Claim Numbers") + 
  xlab("Month") +
  ylab("Claim Numbers")

# calculating total claims cost for 2021 financial year
# and finding the claims inflation (given from claim severity file)
total_cost_2021 <- sum(num_claims$claims_cost[49:60])
15396885/total_cost_2021

