heartfailure <- read.csv("HeartFailure.csv", na ="")
str(heartfailure)

heartfailure$anaemia1 <- factor(heartfailure$anaemia)
heartfailure$diabetes1 <- factor(heartfailure$diabetes)
heartfailure$high_blood_pressure1 <- factor(heartfailure$high_blood_pressure)
heartfailure$sex1 <- factor(heartfailure$sex)
heartfailure$smoking1 <- factor(heartfailure$smoking)
heartfailure$DEATH_EVENT1 <- factor(heartfailure$DEATH_EVENT)

str(heartfailure)




heartfailure$DEATH_EVENT <- heartfailure$DEATH_EVENT1

pairs.panel(heartfailure,
            smooth = FALSE,
            density = TRUE,
            method = "spearman",
            pch = 21,
            cor = TRUE, factor = TRUE)
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
shapiro.test(heartfailure$age)
shapiro.test(heartfailure$age)
shapiro.test(heartfailure$age)
shapiro.test(heartfailure$age)































# Practical 9

# Q1
states <- as.data.frame(state.x77)
str(states)

# Renaming Life Exp and HS Grad variables as 
# these will cause possible issues when referring to
# them since they contain a space.
colnames(states)[colnames(states) == "Life Exp"] <- "Life_Exp"
colnames(states)[colnames(states) == "HS Grad"] <- "HS_Grad"

# Q3a
# Examine initial linearity between variables in the dataset
library(psych)
pairs.panels(states,
             smooth = FALSE,      # If TRUE, draws loess smooths
             scale = FALSE,      # If TRUE, scales the correlation text font
             density = TRUE,     # If TRUE, adds density plots and histograms
             ellipses = FALSE,    # If TRUE, draws ellipses
             method = "spearman",# Correlation method (also "pearson" or "kendall")
             pch = 21,           # pch symbol
             lm = FALSE,         # If TRUE, plots linear fit rather than the LOESS (smoothed) fit
             cor = TRUE,         # If TRUE, reports correlations
             jiggle = FALSE,     # If TRUE, data points are jittered
             factor = 2,         # Jittering factor
             hist.col = 4,       # Histograms color
             stars = TRUE,       # If TRUE, adds significance level with stars
             ci = TRUE)          # If TRUE, adds confidence intervals

# Examine linearity in more detail
scatter.smooth(x = states$Population,
               y = states$Murder,
               xlab = "Population (,000)",
               ylab = "Murder %", main = "Correlation of murder ~ population")

# chart appears to have medium correlation
# low correlation (-0.2 < x < 0.2) suggests that 
# much of variation of the response
# variable (Y) is unexplained by the predictor (X)
# in which case, we should probably look for better
# explanatory variables
cor(states$Murder, states$Population)

scatter.smooth(x = states$Illiteracy,
               y = states$Murder,
               main = "Correlation of Murder ~ Illiteracy",
               xlab = "Illiteracy %",
               ylab = "Murder %")

# Examining correlation between murder and illiteracy
cor(states$Murder, states$Illiteracy)

# This is a better correlation value between both variables.
# Lets examine murder and frost variables for correlation.
scatter.smooth(x = states$Frost,
               y = states$Murder,
               main = "Correlation of Murder ~ Frost",
               xlab = "Frost",
               ylab = "Murder %")
cor(states$Murder, states$Frost)

# Examining the other variables
paste("Correlation for Murder and Frost: ", cor(states$Murder, states$Frost))
paste("Correlation for Murder and Illiteracy: ", cor(states$Murder, states$Illiteracy))
paste("Correlation for Murder and Population: ", cor(states$Murder, states$Population))
paste("Correlation for Murder and HS Grad: ", cor(states$Murder, states$HS_Grad))
paste("Correlation for Murder and Income: ", cor(states$Murder, states$Income))
paste("Correlation for Murder and Life Exp: ", cor(states$Murder, states$Life_Exp))
paste("Correlation for Murder and Area: ", cor(states$Murder, states$Area))

# It appears that the variable Area has a vary low correlation with Murder. 
# Therefore I am going to remove it from the dataset. 
# Alternatively we can choose to exclude these independent variables when
# we are constructing the MLR model.

# Q3b
# Check for outliers
opar <- par(no.readonly = TRUE)
par(mfrow = c(4, 2)) # divide graph area in 3 rows by 2 columns
attach(states)
boxplot(Murder,
        main = "Murder",
        sub = paste("Outlier rows: ",
                    boxplot.stats(Murder)$out)) # box plot for 'murder'
boxplot(Population,
        main = "Population",
        sub = paste("Outlier rows: ",
                    boxplot.stats(Population)$out)) # box plot for 'Population'
boxplot(states$HS_Grad,
        main = "Graduation",
        sub = paste("Outlier rows: ",
                    boxplot.stats(states$HS_Grad)$out)) # box plot for 'HS Grad'
boxplot(Illiteracy,
        main = "Illiteracy",
        sub = paste("Outlier rows: ",
                    boxplot.stats(Illiteracy)$out)) # box plot for 'HS Grad'
boxplot(Income,
        main = "Income",
        sub = paste("Outlier rows: ",
                    boxplot.stats(Income)$out)) # box plot for 'HS Grad'
boxplot(Frost,
        main = "Frost",
        sub = paste("Outlier rows: ",
                    boxplot.stats(Frost)$out)) # box plot for 'HS Grad'
boxplot(states$Life_Exp,
        main = "Life Exp",
        sub = paste("Outlier rows: ",
                    boxplot.stats(states$Life_Exp)$out)) # box plot for 'HS␣Grad'
detach(states)
par(opar)

# Both the population and Income variables contain outliers.
# Use boxplot.stats() function to generate relevant outliers
outlier_values <- boxplot.stats(states$Population)$out # outlier values.
paste("Population outliers: ", paste(outlier_values, collapse=", "))

# Use boxplot.stats() function to generate relevant outliers
outlier_values <- boxplot.stats(states$Income)$out # outlier values.
paste("Income outliers: ", paste(outlier_values, collapse=", "))

states <- subset(states, states$Population != 21198 & 
                   states$Population != 11197 &
                   states$Population != 18076 &
                   states$Population != 11860 &
                   states$Population != 12237)

states <- subset(states, states$Income != 6315)

install.packages("e1071")
library(e1071)
opar <- par(no.readonly = TRUE)
# Show 4 rows x 2 cols of charts
par(mfrow = c(4,2))
plot(density(states$Population),
     main = "Density plot for population",
     sub = paste("skewness: ", round(e1071::skewness(states$Population), 2)))

#Fill the area insode ghd plot with a color
polygon(density(states$Population), col = "red")

# Skewness of < 1 or > 1 is highly skewed
# -1 to -0.5 and 0.5 to 1 moderately skewed
# -0.5 t0 0.5 = approx symetrical

shapiro.test(states$Population)
# option 1 drop the model and dont use in the model
# option 2 use the variable and ignore MLR assumptions
# Option 3 transform the variable to make it more normally distributed


#Transforming the data
install.packages("MASS")
library(MASS)
box_cox_transform <- boxcox(states$Murder ~ states$Population)
lambda <- box_cox_transform$x[which.max(box_cox_transform$y)]
lambda
normalized_population <- ((states$Population^lambda-1)/ lambda)
hist(normalized_population)
shapiro.test(normalized_population)
cor(states$Murder, states$Population)
cor(states$Murder, normalized_population)



plot(density(states$Population),
     main = "Density plot for population",
     sub = paste("skewness: ", round(e1071::skewness(states$Income), 2)))

shapiro.test(states$Income)
shapiro.test(states$Illiteracy)
shapiro.test(states$`Life Exp`)
shapiro.test(states$Murder)
shapiro.test(states$`HS Grad`)
shapiro.test(states$Frost)
shapiro.test(states$Area)

box_cox_transform <- boxcox(states$Murder ~ states$Illiteracy)
lambda <- box_cox_transform$x[which.max(box_cox_transform$y)]
lambda
normalized_illiteracy <- ((states$Illiteracy^lambda-1)/ lambda)
normalized_illiteracy
hist(normalized_illiteracy)
shapiro.test(normalized_illiteracy)
