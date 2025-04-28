#####################################################################           
# IMPORTING PACKAGES                                                #
#####################################################################

# Installing packages
list.of.packages <- c('dplyr', 'ggplot2', 'lubridate', 'car', 'rgl')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# Note, assumes all relevant files are in the same folder as the R script
rm(list = ls())
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

library('dplyr')
library('ggplot2')
library('lubridate')
library("car")
library("rgl")


#####################################################################
# IMPORTING DATA                                                    #
#####################################################################

data <- read.csv("ACTL31425110AssignmentData2022.csv")
parts_inflation <- read.csv("ProducerIndex.csv")
parts_inflation1 <- read.csv("ProducerIndex1.csv")
unemployment_rate <- read.csv("Unemployment rate.csv")



#####################################################################           
# EDA PROCESS                                                       #
#####################################################################

summary(data)

data$accident_month = ymd(data$accident_month)
data$year_month = floor_date(data$accident_month, "month")

#distribution of vehicle class
class_factor <- data$vehicle_class
barplot(prop.table(table(class_factor)), las = 2)

#sum insured by month
data$accident_month <- as.factor(data$accident_month)
bymonth <- data %>%
  group_by(year_month) %>%
  summarise(total_number = n(),
            total_sum_insured = sum(sum_insured),
            avg_sum_insured = total_sum_insured/total_number,
            avg_yomanuf = sum(year_of_manufacture)/total_number,
            avg_tenure = sum(policy_tenure)/total_number,
            monthly_exposure = sum(exposure))

# getting the number of claims per month
data_claimsonly <- na.omit(data)
num_claims <- data_claimsonly %>%
  group_by(year_month) %>%
  summarise(total = n(),
            claims_cost = sum(total_claims_cost),
            avg_claims_cost = claims_cost/total)


freq_bymonth <- (num_claims$total)/(bymonth$monthly_exposure)
bymonth <- cbind(bymonth, freq_bymonth)

#plots
ggplot(bymonth, aes(year_month, freq_bymonth)) +
  geom_point() +
  labs(x = "Accident Month", y = "Claim Frequency") +
  ggtitle("Claim Frequency for each Accident Month") +
  geom_smooth(method = 'lm', 
              formula = y ~ x, 
              se = F, 
              color = "green3")


ggplot(bymonth, aes(year_month, avg_sum_insured)) +
  geom_point() +
  labs(x = "Accident Month", y = "Avg. Sum Insured") +
  ggtitle("Avg. Sum Insured for each Accident Month") +
  geom_smooth(method = 'lm', 
             formula = y ~ x, 
             se = F, 
             color = "green3") 

ggplot(bymonth, aes(year_month, total_sum_insured)) +
  geom_point() +
  labs(x = "Accident Month", y = "Total Sum Insured") +
  ggtitle("Total Sum Insured for each Accident Month") +
  geom_smooth(method = 'lm', 
              formula = y ~ x, 
              se = F, 
              color = "green3")

ggplot(num_claims, aes(year_month, avg_claims_cost)) +
  geom_point()
ggplot(num_claims, aes(year_month, claims_cost)) +
  geom_point()


# checking the proportion of zero valued claims
num_zeroes <- data_claimsonly %>%
  group_by(year_month) %>%
  filter(total_claims_cost == 0) %>%
  summarise(total = n())

prop_zeroes <- data.frame(num_zeroes$total/num_claims$total)
prop_zeroes["year_month"] = num_claims$year_month

prop_zeroes$num_zeroes.total.num_claims.total

ggplot(prop_zeroes, aes(year_month, num_zeroes.total.num_claims.total)) +
  geom_point() +
  labs(title = "Proportion of Zero Valued Claims") +
  xlab("Month") + 
  ylab("Proportion")

# checking the effect of outliers in the data
hist(data$year_of_manufacture)
hist(data$policy_tenure)

no_outliers <- data %>%
  filter(policy_tenure <= 32) %>%
  filter(year_of_manufacture >= 1960) %>%
  group_by(year_month) %>%
  summarise(total = n(),
            avg_yomanuf1 = sum(year_of_manufacture)/total,
            avg_tenure1 = sum(policy_tenure)/total)

compare_outliers <- cbind(bymonth[, c(1, 5, 6)], no_outliers[, c(3, 4)])

# plots comparing data with and without outliers
colours <- c("With Outliers" = "blue", "Without Outliers" = "red")

ggplot(compare_outliers, aes(year_month, avg_yomanuf, colour = "With Outliers")) +
  geom_point() +
  geom_point(aes(year_month, avg_yomanuf1, colour = "Without Outliers")) +
  labs(x = "Month",
       y = "Avg Year of Manufacture",
       title = "Avg Year of Manufacture By Month",
       colour = "Legend") +
  scale_colour_manual(values = colours)

ggplot(compare_outliers, aes(year_month, avg_tenure, colour = "With Outliers")) +
  geom_point() +
  geom_point(aes(year_month, avg_tenure1, colour = "Without Outliers")) +
  labs(x = "Month",
       y = "Avg Policy Tenure",
       title = "Avg Policy Tenure By Month",
       colour = "Legend") +
  scale_colour_manual(values = colours)


#####################################################################      
# MODELLING EXTERNAL FACTORS                                        #
#####################################################################

trim_parts_inf <- parts_inflation[-c(1:175, 236:246), ]
colnames(trim_parts_inf) <- c("Date", "PartsIndex") 

num_claims <- cbind(num_claims, trim_parts_inf$PartsIndex)
num_claims <- cbind(num_claims, bymonth$monthly_exposure)
names(num_claims)[names(num_claims) == 'trim_parts_inf$PartsIndex'] <- 'PartsIndex'
names(num_claims)[names(num_claims) == 'bymonth$monthly_exposure'] <- 'monthly_exposure'

trim_parts_inf1 <- parts_inflation[c(152:211), ]
colnames(trim_parts_inf1) <- c("Date", "PartsIndex1")

num_claims <- cbind(num_claims, trim_parts_inf1$PartsIndex1)
names(num_claims)[names(num_claims) == 'trim_parts_inf1$PartsIndex1'] <- 'PartsIndex1'


# total claims cost over time (what we're trying to model)
ggplot(num_claims, aes(year_month, claims_cost)) +
  geom_point()

# exposure over time --> COVID trend (there is variability due the the different
# #of days in each month)
ggplot(bymonth, aes(year_month, monthly_exposure)) +
  geom_point()

#frequency of claims over time
ggplot(bymonth, aes(year_month, freq_bymonth)) +
  geom_point()

#relationship between number of claims and monthly exposure
ggplot(num_claims, aes(monthly_exposure, total)) +
  geom_point() +
  geom_smooth(method = 'lm', 
              formula = y ~ x, 
              se = F, 
              color = "green3")

# NUMBER OF CLAIMS: linear and poisson glm model comparison
freq_model <- lm(
  total ~ monthly_exposure,
  data = num_claims
)
summary(freq_model)

freq_glm <- glm(
  total ~ monthly_exposure,
  data = num_claims, family = poisson
)
summary(freq_glm)

linear_model = predict(freq_model, interval = "confidence")
lin_df <- cbind(num_claims, linear_model)

ggplot(lin_df, aes(monthly_exposure, total)) +
  geom_point() +
  geom_ribbon(aes(ymin = lwr, ymax = upr, colour = NULL), fill = "green", alpha = 0.15) +
  geom_line(aes(y = fit), colour = 'green', size = 1) +
  ggtitle("Linear Model of Number of Claims vs Exposure") +
  xlab("Monthly Exposure") +
  ylab("Number of Claims")


ilink <- family(freq_glm)$linkinv
poi_df <- bind_cols(num_claims, setNames(as_tibble(predict(freq_glm, num_claims, se.fit = TRUE)[1:2]),
                                   c('fit_link','se_link')))
poi_df <- mutate(poi_df,
                fit_resp  = ilink(fit_link),
                upr = ilink(fit_link + (2 * se_link)),
                lwr = ilink(fit_link - (2 * se_link)))


ggplot(poi_df, aes(monthly_exposure, total)) +
  geom_point() +
  geom_ribbon(aes(ymin = lwr, ymax = upr, colour = NULL), fill = "green", alpha = 0.15) +
  geom_line(aes(y = fit_resp), colour = 'green', size = 1) +
  ggtitle("GLM of Number of Claims vs Exposure") +
  xlab("Monthly Exposure") +
  ylab("Number of Claims")


# NUMBER OF CLAIMS: poisson glm with no intercept variable (as when exposure = 0, #claims = 0)
freq_glm1 <- glm(
  total ~ monthly_exposure - 1,
  data = num_claims, family = poisson
)
summary(freq_glm1)

ilink <- family(freq_glm1)$linkinv
poi1_df <- bind_cols(num_claims, setNames(as_tibble(predict(freq_glm1, num_claims, se.fit = TRUE)[1:2]),
                                         c('fit_link','se_link')))
poi1_df <- mutate(poi1_df,
                 fit_resp  = ilink(fit_link),
                 upr = ilink(fit_link + (2 * se_link)),
                 lwr = ilink(fit_link - (2 * se_link)))


ggplot(poi1_df, aes(monthly_exposure, total)) +
  geom_point() +
  geom_ribbon(aes(ymin = lwr, ymax = upr, colour = NULL), fill = "green", alpha = 0.15) +
  geom_line(aes(y = fit_resp), colour = 'green', size = 1)+
  ggtitle("Modified GLM of Number of Claims vs Exposure") +
  xlab("Monthly Exposure") +
  ylab("Number of Claims")



# parts index over time
ggplot(num_claims, aes(year_month, PartsIndex)) +
  geom_point()


# average claims cost over time
ggplot(num_claims, aes(year_month, avg_claims_cost)) +
  geom_point()

#relationships between average claim cost and parts indexes
ggplot(num_claims, aes(PartsIndex, avg_claims_cost)) +
  geom_point() +
  geom_smooth(method = 'lm', 
              formula = y ~ x, 
              se = F, 
              color = "green3")
ggplot(num_claims, aes(PartsIndex1, avg_claims_cost)) +
  geom_point() +
  geom_smooth(method = 'lm', 
              formula = y ~ x, 
              se = F, 
              color = "green3")

# AVERAGE CLAIM COST:
cost_glm <- glm(
  avg_claims_cost ~ PartsIndex,
  data = num_claims, family = Gamma
)
summary(cost_glm)

cost_glm1 <- glm(
  avg_claims_cost ~ PartsIndex1,
  data = num_claims, family = 
)
summary(cost_glm1)


ilink <- family(cost_glm)$linkinv
costpoi_df <- bind_cols(num_claims, setNames(as_tibble(predict(cost_glm, num_claims, se.fit = TRUE)[1:2]),
                                         c('fit_link','se_link')))
costpoi_df <- mutate(costpoi_df,
                 fit_resp  = ilink(fit_link),
                 upr = ilink(fit_link + (2 * se_link)),
                 lwr = ilink(fit_link - (2 * se_link)))


ggplot(costpoi_df, aes(PartsIndex, avg_claims_cost)) +
  geom_point() +
  geom_ribbon(aes(ymin = lwr, ymax = upr, colour = NULL), fill = "green", alpha = 0.15) +
  geom_line(aes(y = fit_resp), colour = 'green', size = 1)

ilink <- family(cost_glm1)$linkinv
costpoi1_df <- bind_cols(num_claims, setNames(as_tibble(predict(cost_glm1, num_claims, se.fit = TRUE)[1:2]),
                                             c('fit_link','se_link')))
costpoi1_df <- mutate(costpoi1_df,
                     fit_resp  = ilink(fit_link),
                     upr = ilink(fit_link + (2 * se_link)),
                     lwr = ilink(fit_link - (2 * se_link)))


ggplot(costpoi1_df, aes(PartsIndex1, avg_claims_cost)) +
  geom_point() +
  geom_ribbon(aes(ymin = lwr, ymax = upr, colour = NULL), fill = "green", alpha = 0.15) +
  geom_line(aes(y = fit_resp), colour = 'green', size = 1) + 
  ggtitle("GLM of Avg. Claims Cost vs Vehicle Parts Manufacuring Index") +
  xlab("Vehicle Parts Manufacturing Index") +
  ylab("Avg. Claims Cost")


scatter3d(num_claims$PartsIndex1,  num_claims$claims_cost, num_claims$monthly_exposure)



#####################################################################      
# MILESTONE 3 MODELLING                                             #
#####################################################################

num_claims = cbind(num_claims, na.omit(unemployment_rate$People....))
names(num_claims)[names(num_claims) == 'na.omit(unemployment_rate$People....)'] <- 'unemployment_rate'

ggplot(num_claims, aes(year_month, total)) +
  geom_line(colour = 'red') +
  geom_line(aes(year_month, monthly_exposure/12.5), colour = 'blue') +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA))


lagged_claims = data.frame(num_claims$year_month[1:59], num_claims$total[1:59], num_claims$monthly_exposure[2:60])

ggplot(lagged_claims, aes(num_claims.year_month.1.59., num_claims.total.1.59.)) +
  geom_line(colour = 'red') +
  geom_line(aes(num_claims.year_month.1.59., num_claims.monthly_exposure.2.60./12.5), colour = 'blue') +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA))

freq_glm_modified <- glm(
  num_claims.total.1.59. ~ num_claims.monthly_exposure.2.60.,
  data = lagged_claims, family = poisson
)


lagged_claims1 = data.frame(num_claims$year_month[1:58], num_claims$total[1:58], num_claims$monthly_exposure[3:60])

ggplot(lagged_claims1, aes(num_claims.year_month.1.58., num_claims.total.1.58.)) +
  geom_line(colour = 'red', size = 1) +
  geom_line(aes(num_claims.year_month.1.58., num_claims.monthly_exposure.3.60./12.5), colour = 'blue', size = 1) +
  ggtitle("2-Month Lag Exposure vs Claim Freuquency") +
  xlab("Months") +
  ylab("Claim Frequency") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA))

freq_glm_modified1 <- glm(
  num_claims.total.1.58. ~ num_claims.monthly_exposure.3.60.,
  data = lagged_claims1, family = poisson
)




lagged_claims2 = data.frame(num_claims$year_month[1:57], num_claims$total[1:57], num_claims$monthly_exposure[4:60])

ggplot(lagged_claims2, aes(num_claims.year_month.1.57., num_claims.total.1.57.)) +
  geom_line(colour = 'red') +
  geom_line(aes(num_claims.year_month.1.57., num_claims.monthly_exposure.4.60./12.5), colour = 'blue') +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA))

freq_glm_modified2 <- glm(
  num_claims.total.1.57. ~ num_claims.monthly_exposure.4.60.,
  data = lagged_claims2, family = poisson
)



lagged_claims3 = data.frame(num_claims$year_month[1:56], num_claims$total[1:56], num_claims$monthly_exposure[5:60])

ggplot(lagged_claims3, aes(num_claims.year_month.1.56., num_claims.total.1.56.)) +
  geom_line(colour = 'red') +
  geom_line(aes(num_claims.year_month.1.56., num_claims.monthly_exposure.5.60./12.5), colour = 'blue') +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA))

freq_glm_modified3 <- glm(
  num_claims.total.1.56. ~ num_claims.monthly_exposure.5.60.,
  data = lagged_claims3, family = poisson
)

summary(freq_glm_modified)
summary(freq_glm_modified1)
summary(freq_glm_modified2)
summary(freq_glm_modified3)
summary(freq_glm)
summary(freq_glm1)


