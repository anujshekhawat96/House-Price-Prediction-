library(tidyverse)
library(readxl)
library(lubridate)
library(mice)
library(dplyr)
library(estimatr)
library(caret)
library(car)

# Data Input
House_Price <- read_excel(file.choose())
summary(House_Price)

#Data Clean, removing out of scope outliers
House_Price_Clean <- filter(House_Price,House_Price$price!=7062500& House_Price$price!=4668000&House_Price$price!=12899000&House_Price$price!=26590000)

#run linear model
Reg_1 <- lm(price~ bedrooms + sqft_living + sqft_lot + waterfront + view + condition + sqft_above + yr_built, House_Price_Clean)
summary(Reg_1)
plot(Reg_1)
plot(density(resid(Reg_1)))

#change view to binary
House_Price_Clean$view_dummy <- ifelse(House_Price_Clean$view >0, 1, 0)

#possible solution: year built to be re-interpreted, new column might required -> new R squared
#sqrt living and sqft_above only choose one, cor() to justify
House_Price_Clean$house_age = 2014 - House_Price_Clean$yr_built
Reg_2 <- lm(price~ bedrooms + sqft_living + sqft_lot + waterfront + view_dummy + condition + house_age, House_Price_Clean)
summary(Reg_2)
plot(Reg_2)
plot(density(resid(Reg_2)))
#residual distributed normally

linearHypothesis(Reg_2, c("house_age = 0"))

#produces heteroskedasticity-consistent inferences and tests in the same way a regular regression does. It will also use the corrected covariance matrix for any test statements used on the model.
Reg_3 <- lm_robust(price ~ bedrooms + sqft_living + sqft_lot + waterfront + view_dummy + condition + house_age, House_Price_Clean, se_type="HC2")
summary(Reg_3)

#test if bedrooms and sqft_living jointly belong to the model
linearHypothesis(Reg_3, c("bedrooms", "sqft_living"))


# add dummy variable based on 1957, before 1957 and after 1957
House_Price_Clean$yr_dummy <- ifelse(House_Price_Clean$yr_built < 1974 ,0 , 1)

#produce sqft_living
House_Price_Clean$sqft_living_yr_built <- House_Price_Clean$yr_dummy * House_Price_Clean$sqft_living

#chow test on year built to check if there is any structural change before and after 1957
#c0 = year_dummy, c1 = sqft_living_yr_built
Reg_4 <- lm(price~ bedrooms + sqft_living + sqft_lot + waterfront + view_dummy + condition + house_age + yr_dummy + sqft_living_yr_built,House_Price_Clean)
summary(Reg_4)
linearHypothesis(Reg_4, c("yr_dummy=0", "sqft_living_yr_built=0"))
#p-value is significant meaning at least one of yr_dummy and sqft_living_yr_built is siginficant
# there is a structural change before and after 1957

linearHypothesis(Reg_4, c("yr_dummy=0"))
linearHypothesis(Reg_4, c("sqft_living_yr_built=0"))
#both are significant, together they are significant

Reg_7 <- lm(price~ bedrooms + sqft_living + sqft_lot + waterfront + view_dummy + condition + house_age + sqft_living_yr_built,House_Price_Clean)
summary(Reg_7)


#chow test on waterfront
House_Price_Clean$sqft_living_waterfront = House_Price_Clean$waterfront * House_Price_Clean$sqft_living
#chow test on year built to check if there is difference between houses are located waterfront and are not
#c0 = waterfront, c1 = sqft_living_waterfront
Reg_5 <- lm(price~ bedrooms + sqft_living + sqft_lot + waterfront + view_dummy + condition + house_age + yr_dummy + sqft_living_yr_built ,House_Price_Clean)
summary(Reg_5)
linearHypothesis(Reg_5, c("waterfront=0"))

Reg_6 <- lm(price~ bedrooms + sqft_living + sqft_lot + waterfront + view_dummy + condition + house_age + yr_dummy + sqft_living_yr_built + sqft_living_waterfront,House_Price_Clean)
summary(Reg_6)
linearHypothesis(Reg_6, c("sqft_living_waterfront=0"))
#dummy variable waterfront is significant but combined with sqft_living is not significant

#therefore, old houses and new houses have different slope for sqft_living
#price and sqft_living relationship change over time

#if divide the dataset into waterfront and non-waterfront groups
#price and sqft_lviing relationship does not change between groups

#testing for yr_renovated
House_Price_Clean$reno_dummy <- ifelse(House_Price_Clean$yr_renovated > 0, 1, 0)
House_Price_Clean$reno_sqft_living <- ifelse(House_Price_Clean$yr_renovated > 0, House_Price_Clean$sqft_living, 0)
Reg_8 <- lm(price~ bedrooms + sqft_living + sqft_lot + waterfront + view_dummy + condition + house_age + sqft_living_yr_built + reno_dummy, House_Price_Clean)
summary(Reg_8)

#chow test for renovation dummy
linearHypothesis(Reg_8, c("reno_dummy = 0"))


Reg_9 <- lm(price~ bedrooms + sqft_living + sqft_lot + waterfront + view_dummy + condition + house_age + sqft_living_yr_built + reno_sqft_living, House_Price_Clean)
summary(Reg_9)

#chow test for renovation_sqft_living
linearHypothesis(Reg_9, c("reno_sqft_living = 0"))

# Perform Fragile Test on bedroom
FT_B1 <- lm(price~ bedrooms + sqft_lot + waterfront + view + condition + house_age,House_Price_Clean)
summary (FT_B1)

FT_B2 <- lm(price~ bedrooms + sqft_lot + waterfront + view, House_Price_Clean)
summary (FT_B2)

FT_B3 <- lm(price~ bedrooms + sqft_lot ,House_Price_Clean)
summary (FT_B3)
# Conclusion: bedroom is an important variable to the model. Its T-test shows that the p-value remains at 2e-16 
# in all three Fragile tests. 


# Perform Fragile Test on sqft_living
FT_SL1 <- lm(price~ sqft_living + sqft_lot + waterfront + view + condition + house_age, House_Price_Clean)
summary (FT_SL1)

FT_SL2 <- lm(price~ sqft_living + sqft_lot + waterfront + view, House_Price_Clean)
summary (FT_SL2)

FT_SL3 <- lm(price~ sqft_living + sqft_lot, House_Price_Clean)
summary (FT_SL3)
# Conclusion: sft_living is an important variable to the model. Its T-test shows that the p-value remains at 2e-16 
# in all three Fragile tests. 



#final model
Reg_vf <- lm(price~ bedrooms + sqft_living + sqft_lot + waterfront + view_dummy + condition + house_age + sqft_living_yr_built, House_Price_Clean)
summary(Reg_vf)
plot(Reg_vf)
plot(density(resid(Reg_vf)))


#produces heteroskedasticity-consistent inferences and tests in the same way a regular regression does. It will also use the corrected covariance matrix for any test statements used on the model.
Reg_vf_test <- lm_robust(price ~ bedrooms + sqft_living + sqft_lot + waterfront + view_dummy + condition + house_age + yr_dummy + sqft_living_yr_built, House_Price_Clean, se_type="HC2")
summary(Reg_vf_test)

ggplot(data = House_Price_Clean, aes(x = yr_built, y = price)) + geom_point()

ggplot(data = House_Price_Clean, aes(x = yr_built, y = sqft_living)) + geom_point()




