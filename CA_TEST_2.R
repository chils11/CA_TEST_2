heartfailure <- read.csv("HeartFailure.csv", na ="")
str(heartfailure)

heartfailure$anaemia1 <- factor(heartfailure$anaemia)
heartfailure$diabetes1 <- factor(heartfailure$diabetes)
heartfailure$high_blood_pressure1 <- factor(heartfailure$high_blood_pressure)
heartfailure$sex1 <- factor(heartfailure$sex)
heartfailure$smoking1 <- factor(heartfailure$smoking)
heartfailure$DEATH_EVENT1 <- factor(heartfailure$DEATH_EVENT)






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

