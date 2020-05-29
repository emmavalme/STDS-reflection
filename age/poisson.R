library(readxl)
library(broom)
library(sjPlot)
library(ggplot2)

#load dataset
reg_data <- read_excel("reg_data.xlsx")
View(reg_data)

# offset is set as follows
model <- glm(deaths ~ country + age_range, offset = log(n), family = poisson(link = "log"), data = reg_data)

# Result
summary(model)

# exp=T to give the exponent-result, 
#because Poissonregression uses logaritm as linkfunktion
tidy(model, exp=T) 

#prediction
exp(predict(model, newdata = data.frame(country = "Sweden", age_range = "80+", n = 1)))

#----------

# The new dataset involves country och age.range
data1 <- reg_data

# copy file
data2 <- data1

# We put the amount of persos to 1, to predict number of cases per person in each age group
data1$n <- 1
reg_data$pred_cases_per_1 <- exp(predict(model, data1))

# Nr of cases per 1000 pesons for each age group.
data2$n <- 1000
reg_data$pred_cases_per_1000 <- exp(predict(model, data2))

# Nu prediktar vi antal fall i den faktiska populationen
reg_data$pred_cases <- exp(predict(model))

reg_data %>%
ggplot(aes(age_range, pred_cases_per_1000, group=country, color=country)) +
  ggtitle("Predicted deaths per 1000 for each age group") +
  labs(x = "Age groups", y = "pred. deaths per 1000", colour = "Country") +
  geom_line()+
  geom_point()

