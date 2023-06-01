heartfailure <- read.csv("HeartFailure.csv", na ="")
str(heartfailure)

heartfailure$anaemia1 <- as.numeric(heartfailure$anaemia)
heartfailure$diabetes1 <- as.numeric(heartfailure$diabetes)
heartfailure$high_blood_pressure1 <- as.numeric(heartfailure$high_blood_pressure)
heartfailure$sex1 <- as.numeric(heartfailure$sex)
heartfailure$smoking1 <- as.numeric(heartfailure$smoking)
heartfailure$DEATH_EVENT1 <- as.numeric(heartfailure$DEATH_EVENT)
str(heartfailure)




# heartfailure$DEATH_EVENT <- heartfailure$DEATH_EVENT1

install.packages("psych")
library(psych)
pairs.panels(heartfailure,
            smooth = FALSE,
            scale = FALSE,
            ellipses = FALSE,
            lm = FALSE,
            jiggle = FALSE,
            stars = TRUE,
            ci = TRUE,
            density = TRUE,
            method = "spearman",
            pch = 21,
            cor = TRUE, factor = 2)

cor(heartfailure$DEATH_EVENT1, heartfailure$age)
cor(heartfailure$DEATH_EVENT1, heartfailure$anaemia1)
cor(heartfailure$DEATH_EVENT1, heartfailure$creatinine_phosphokinase)
cor(heartfailure$DEATH_EVENT1, heartfailure$diabetes1)
cor(heartfailure$DEATH_EVENT1, heartfailure$ejection_fraction)
cor(heartfailure$DEATH_EVENT1, heartfailure$high_blood_pressure1)
cor(heartfailure$DEATH_EVENT1, heartfailure$platelets)
cor(heartfailure$DEATH_EVENT1, heartfailure$serum_creatinine)
cor(heartfailure$DEATH_EVENT1, heartfailure$serum_sodium)
cor(heartfailure$DEATH_EVENT1, heartfailure$sex1)
cor(heartfailure$DEATH_EVENT1, heartfailure$smoking1)
cor(heartfailure$DEATH_EVENT1, heartfailure$time)

# Examine linearity in more detail
scatter.smooth(x = heartfailure$age,
               y = heartfailure$DEATH_EVENT,
               xlab = "Population (,000)",
               ylab = "Murder %", main = "Correlation of murder ~ population")


# Check for outliers
opar <- par(no.readonly = TRUE)
par(mfrow = c(4, 2)) # divide graph area in 3 rows by 2 columns
attach(heartfailure)
boxplot(DEATH_EVENT1,
        main = "death",
        sub = paste("Outlier rows: ",
                    boxplot.stats(DEATH_EVENT1)$out)) # box plot for 'Death'
boxplot(age,
        main = "Age of patients",
        sub = paste("Outlier rows: ",
                    boxplot.stats(age)$out))
boxplot(anaemia1,
        main = "Box plot for patients with Anaemia",
        sub = paste("Outlier rows: ",
                    boxplot.stats(anaemia1)$out))
boxplot(creatinine_phosphokinase,
        main = "Box plot for creatinine phosphokinase")
boxplot(ejection_fraction,
        main = "Box plot for ejection fraction",
        sub = paste("Outlier rows: ",
                    boxplot.stats(ejection_fraction)$out))
boxplot(platelets,
        main = "Box plot for number of platelets in patients")

boxplot(serum_creatinine,
        main = "Box plot for serum creatinine")

boxplot(serum_sodium,
        main = "Box plot for serum soidum")

boxplot(time,
        main = "Box plot for time")

detach(states)
par(opar)

set.seed(1)
# formula is dependent ~ independent, independent
attach(heartfailure)
model <- lm(formula = DEATH_EVENT ~ 
              age + high_blood_pressure1,
              data = heartfailure)
model

# Performing shapiro-wilks test for all the variables to check which of  them is normally distributed
shapiro.test(heartfailure$age)
shapiro.test(heartfailure$anaemia)
shapiro.test(heartfailure$creatinine_phosphokinase)
shapiro.test(heartfailure$diabetes)
shapiro.test(heartfailure$ejection_fraction)
shapiro.test(heartfailure$high_blood_pressure)
shapiro.test(heartfailure$sex1)
shapiro.test(heartfailure$platelets)
shapiro.test(heartfailure$serum_creatinine)
shapiro.test(heartfailure$smoking1)
shapiro.test(heartfailure$time)
shapiro.test(heartfailure$DEATH_EVENT1)


install.packages("e1071")
library(e1071)
opar <- par(no.readonly = TRUE)
# Show 4 rows x 2 cols of charts
par(mfrow = c(4,2))
plot(density(heartfailure$DEATH_EVENT1),
     main = "Density plot for Death",
     sub = paste("skewness: ", round(e1071::skewness(heartfailure$DEATH_EVENT1), 2)))

#Fill the area insode ghd plot with a color
polygon(density(heartfailure$DEATH_EVENT1), col = "red")


plot(density(heartfailure$age),
     main = "Density plot for age",
     sub = paste("skewness: ", round(e1071::skewness(heartfailure$age), 2)))

#Fill the area insode ghd plot with a color
polygon(density(heartfailure$age), col = "red")

plot(density(heartfailure$anaemia1),
     main = "Density plot for anaemia",
     sub = paste("skewness: ", round(e1071::skewness(heartfailure$anaemia1), 2)))

#Fill the area insode ghd plot with a color
polygon(density(heartfailure$anaemia1), col = "red")

plot(density(heartfailure$creatinine_phosphokinase),
     main = "Density plot for creatinine_phosphokinase",
     sub = paste("skewness: ", round(e1071::skewness(heartfailure$creatinine_phosphokinase), 2)))

#Fill the area insode ghd plot with a color
polygon(density(heartfailure$creatinine_phosphokinase), col = "red")

plot(density(heartfailure$diabetes1),
     main = "Density plot for diabetes",
     sub = paste("skewness: ", round(e1071::skewness(heartfailure$diabetes1), 2)))

#Fill the area insode ghd plot with a color
polygon(density(heartfailure$diabetes1), col = "red")

plot(density(heartfailure$ejection_fraction),
     main = "Density plot for ejection fraction",
     sub = paste("skewness: ", round(e1071::skewness(heartfailure$ejection_fraction), 2)))

#Fill the area insode ghd plot with a color
polygon(density(heartfailure$ejection_fraction), col = "red")


plot(density(heartfailure$high_blood_pressure1),
     main = "Density plot for high blood pressure",
     sub = paste("skewness: ", round(e1071::skewness(heartfailure$high_blood_pressure1), 2)))

#Fill the area insode ghd plot with a color
polygon(density(heartfailure$high_blood_pressure1), col = "red")

plot(density(heartfailure$platelets),
     main = "Density plot for platelets",
     sub = paste("skewness: ", round(e1071::skewness(heartfailure$platelets), 2)))

#Fill the area insode ghd plot with a color
polygon(density(heartfailure$platelets), col = "red")

plot(density(heartfailure$serum_creatinine),
     main = "Density plot for serum creatinine",
     sub = paste("skewness: ", round(e1071::skewness(heartfailure$serum_creatinine), 2)))

#Fill the area insode ghd plot with a color
polygon(density(heartfailure$serum_creatinine), col = "red")

plot(density(heartfailure$serum_sodium),
     main = "Density plot for serum sodium",
     sub = paste("skewness: ", round(e1071::skewness(heartfailure$serum_sodium), 2)))

#Fill the area insode ghd plot with a color
polygon(density(heartfailure$serum_sodium), col = "red")

plot(density(heartfailure$sex1),
     main = "Density plot for sex",
     sub = paste("skewness: ", round(e1071::skewness(heartfailure$sex1), 2)))

#Fill the area insode ghd plot with a color
polygon(density(heartfailure$sex1), col = "red")


plot(density(heartfailure$smoking1),
     main = "Density plot for smoking",
     sub = paste("skewness: ", round(e1071::skewness(heartfailure$smoking1), 2)))

#Fill the area insode ghd plot with a color
polygon(density(heartfailure$smoking1), col = "red")

plot(density(heartfailure$time),
     main = "Density plot for time",
     sub = paste("skewness: ", round(e1071::skewness(heartfailure$time), 2)))

#Fill the area insode ghd plot with a color
polygon(density(heartfailure$time), col = "red")



# Removing all the outliers in the dataset for all variables
# Use boxplot.stats() function to generate relevant outliers
outlier_values <- boxplot.stats(heartfailure$creatinine_phosphokinase)$out # outlier values.
paste("outliers: ", paste(outlier_values, collapse=", "))

heartfailure <- subset(heartfailure, heartfailure$creatinine_phosphokinase != 7861 & 
                               heartfailure$creatinine_phosphokinase != 2656 & 
                               heartfailure$creatinine_phosphokinase != 1380 & 
                               heartfailure$creatinine_phosphokinase != 3964 & 
                               heartfailure$creatinine_phosphokinase != 7702 & 
                               heartfailure$creatinine_phosphokinase != 5882 & 
                               heartfailure$creatinine_phosphokinase != 5209 & 
                               heartfailure$creatinine_phosphokinase != 1876 & 
                               heartfailure$creatinine_phosphokinase != 1808 & 
                               heartfailure$creatinine_phosphokinase != 4540 & 
                               heartfailure$creatinine_phosphokinase != 1548 & 
                               heartfailure$creatinine_phosphokinase != 1610 & 
                               heartfailure$creatinine_phosphokinase != 2261 & 
                               heartfailure$creatinine_phosphokinase != 1846 & 
                               heartfailure$creatinine_phosphokinase != 2334 & 
                               heartfailure$creatinine_phosphokinase != 2442 & 
                               heartfailure$creatinine_phosphokinase != 3966 & 
                               heartfailure$creatinine_phosphokinase != 1419 & 
                               heartfailure$creatinine_phosphokinase != 1896 & 
                               heartfailure$creatinine_phosphokinase != 1767 & 
                               heartfailure$creatinine_phosphokinase != 2281 & 
                               heartfailure$creatinine_phosphokinase != 2794 & 
                               heartfailure$creatinine_phosphokinase != 2017 & 
                               heartfailure$creatinine_phosphokinase != 2522 & 
                               heartfailure$creatinine_phosphokinase != 2695 &
                               heartfailure$creatinine_phosphokinase != 1688 & 
                               heartfailure$creatinine_phosphokinase != 1820 & 
                               heartfailure$creatinine_phosphokinase != 2060 & 
                               heartfailure$creatinine_phosphokinase != 2413)

#Removing the outliers in ejection fraction
outlier_values <- boxplot.stats(heartfailure$ejection_fraction)$out # outlier values.
paste("outliers: ", paste(outlier_values, collapse=", "))

heartfailure <- subset(heartfailure, heartfailure$ejection_fraction != 80 & 
                               heartfailure$ejection_fraction != 70)

#Removing the outliers in serum creatinine
outlier_values <- boxplot.stats(heartfailure$serum_creatinine)$out # outlier values.
paste("outliers: ", paste(outlier_values, collapse=", "))

heartfailure <- subset(heartfailure, heartfailure$serum_creatinine != 2.7 &
                               heartfailure, heartfailure$serum_creatinine != 9.4 &
                               heartfailure, heartfailure$serum_creatinine != 4 &
                               heartfailure, heartfailure$serum_creatinine != 5.8 &
                               heartfailure, heartfailure$serum_creatinine != 3.5 &
                               heartfailure, heartfailure$serum_creatinine != 3 &
                               heartfailure, heartfailure$serum_creatinine != 4.4 &
                               heartfailure, heartfailure$serum_creatinine != 2.7 &
                               heartfailure, heartfailure$serum_creatinine != 2.9 &
                               heartfailure, heartfailure$serum_creatinine != 2.5 &
                               heartfailure, heartfailure$serum_creatinine != 3.2 &
                               heartfailure, heartfailure$serum_creatinine != 3.7 &
                               heartfailure, heartfailure$serum_creatinine != 3.4 &
                               heartfailure, heartfailure$serum_creatinine != 2.5)

#Removing the outliers in platelets
outlier_values <- boxplot.stats(heartfailure$platelets)$out # outlier values.
paste("outliers: ", paste(outlier_values, collapse=", "))

heartfailure <- subset(heartfailure, heartfailure$platelets != 454000 &
                               heartfailure$platelets != 47000 &
                               heartfailure$platelets != 451000 &
                               heartfailure$platelets != 461000 &
                               heartfailure$platelets != 497000 &
                               heartfailure$platelets != 621000 &
                               heartfailure$platelets != 850000 &
                               heartfailure$platelets != 448000 &
                               heartfailure$platelets != 481000 &
                               heartfailure$platelets != 504000 &
                               heartfailure$platelets != 62000 &
                               heartfailure$platelets != 533000 &
                               heartfailure$platelets != 25100 &
                               heartfailure$platelets != 451000)

#Removing the outliers in serum sodium
outlier_values <- boxplot.stats(heartfailure$serum_sodium)$out # outlier values.
paste("outliers: ", paste(outlier_values, collapse=", "))

heartfailure <- subset(heartfailure, heartfailure$serum_sodium != 121 &
                               heartfailure$serum_sodium != 124 &
                               heartfailure$serum_sodium != 113)
                       

#Transforming the data
install.packages("recipes")
library(recipes)
recipe_obj <- recipe(DEATH_EVENT1 ~ anaemia1, data = heartfailure) %>%
        step_YeoJohnson(all_predictors())
prepped_recipe <- prep(recipe_obj)

transformed_data <- bake(prepped_recipe, new_data = heartfailure)
transformed_deathevent <- transformed_data$DEATH_EVENT1
shapiro.test(transformed_deathevent)
hist(transformed_deathevent)

# Creating training and testing data
# make this example reproducible
attach(heartfailure)
set.seed(1)
no_rows_data <- nrow(heartfailure)
sample <- sample(1:no_rows_data, size = round(0.7 * no_rows_data), replace = FALSE)
# 70% training, 30% testing
training_data <- heartfailure[sample, ]
testing_data <- heartfailure[-sample, ]

first_model <- glm(DEATH_EVENT1 ~ age + anaemia1 + creatinine_phosphokinase + ejection_fraction + diabetes1 + 
                           sex1 + platelets + serum_creatinine + serum_sodium + smoking1 + time + high_blood_pressure1,
                                family="binomial", 
                                data=training_data)

Second_model <- glm(DEATH_EVENT1 ~ age + ejection_fraction + 
                          + serum_sodium +time,
                   family="binomial", 
                   data=training_data)



#Measuring the AIC for the two models built
# Remeber the smallest AIC is the best model to use
stepAIC(first_model, direction="backward")


# measruing the AIC foe the second model
stepAIC(Second_model, direction="backward")

install.packages("leaps")
library(leaps)

MLR_subset_selection <-regsubsets(DEATH_EVENT1 ~ age + anaemia1 + creatinine_phosphokinase + ejection_fraction + diabetes1 + 
                                          sex1 + platelets + serum_creatinine + serum_sodium + smoking1 + time + high_blood_pressure1, data=training_data, nbest=6)
plot(MLR_subset_selection, scale="adjr2")


studentized_fit <- rstudent(first_model)
hist(studentized_fit, 
     breaks=10, 
     freq=FALSE, 
     xlab="Studentized Residual", 
     main="Distribution of Errors")



summary(first_model)


confint(first_model)


predicted_death <- predict(third_model, training_data)


actual_prediction <- data.frame(cbind(actuals = testing_data$DEATH_EVENT1,
                                     predicted = predicted_death))


head(actual_prediction)

cor_accuracy <- cor(actual_prediction)
cor_accuracy

third_model <- glm(DEATH_EVENT1 ~ age + diabetes1 + high_blood_pressure1,
                   family="binomial", 
                   data=training_data)

stepAIC(third_model, direction="backward")



#Will the patient die if diabetes =1, age = 80, smoking = 1, high blood pressure = 1

question <- data.frame(diabetes1 = c(1),
                       age = c(80),
                       smoking1 = c(1),
                       high_blood_pressure1 = c(1)
                       )


Death_prediction <- predict(third_model, question)
print(Death_prediction) 





