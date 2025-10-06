library(tidyverse)
library(broom)
library(rstatix)
library(car)
library(lindia)
attach(data)
library(readxl)

data <- Book1
str(data)


names <- c(1,2,8)
names1 <- c("Phases","plot number","slope")
data[,names] <- lapply(data[,names], factor)
data$slope <- relevel(data$slope, ref = '23')
data$Phases <- relevel(data$Phases, ref = 'before felling')
str(data)
Log_runoff <- log(data$runoff+1)
 data1 <- mutate(data, Log_runoff)
colnames(data1) # "Plot No"  "Phase"  "Rainfall"  "Runoff"   "LT"    "GCVP"  "Basel Area"  "Slope"     "Runoff_sqrt"  "Log_Runoff+1"

##################### MODEL 1  #########################################
model_1 <- lm(Log_runoff ~ rainfall + Phases + `plot number` + LT + GCP + BA  , data = data1)
Anova(model_1)
model_1.1 <- lm(Log_runoff ~ rainfall + Phases + `plot number` + LT + GCP, data = data1)
Anova(model_1.1)
model_1.2 <- lm(Log_runoff ~ rainfall + Phases + `plot number` + GCP, data = data)
Anova(model_1.2)
# Inspect the model diagnostic metrics
model.metrics1 <- augment(model_1) 
model.metrics1.2 <- augment(model_1.2) 

# Assess normality of residuals using shapiro wilk test
shapiro_test(model.metrics1$.resid)
shapiro_test(model.metrics1.2$.resid)

# Homogeneity of variances
ncvTest(model_1)
ncvTest(model_1.2)

# Multicollinearity
vif(model_1)
vif(model_1.2)

# Influential Outliers
gg_cooksd(model_1)
gg_cooksd(model_1.2)
par(mfrow = c(1,1))
plot(model_1.2, 5) # 38, 191, 226
par(mfrow = c(1,1))
plot(model_1.2, 3) # 181, 213, 226

# removing influential outliers
df <- data1 %>% mutate(id = row_number())
View(df)
dim(df)

df <- df[-c(38, 181, 191, 213, 226),] 
dim(df)

df <- df[,-11]
dim(df)

##################### MODEL 2  #########################################
#Refitting the model
model_2 <- lm(Log_runoff ~ rainfall + Phases + `plot number` + GCP, data = df)
Anova(model_2)
summary(model_2)

# Inspect the model diagnostic metrics
model.metrics2 <- augment(model_2) 

# Assess normality of residuals using shapiro wilk test
shapiro_test(model.metrics2$.resid)

# Homogeneity of variances
ncvTest(model_2)

# Multicollinearity
vif(model_2)

# Influential Outliers
gg_cooksd(model_2) # 28
par(mfrow = c (2, 2))
plot(model_2) # 31, 101, 145, 177, 212

# removing influential outliers
df2 <- df[-c(28, 31, 101, 145, 177, 212),]
dim(df2)

##################### MODEL 3  #########################################
#Refitting the model
model_3 <- lm(Log_runoff ~ rainfall + Phases + `plot number`+ GCP, data = df2)
Anova(model_3)
summary(model_3)
# Inspect the model diagnostic metrics
model.metrics3 <- augment(model_3) 
# Assess normality of residuals using shapiro wilk test
shapiro_test(model.metrics3$.resid)
# Homogeneity of variances
ncvTest(model_3)
# Multicollinearity
vif(model_3)

# Influential Outliers
gg_cooksd(model_3)
par(mfrow = c (2, 2))
plot(model_3)
par(mfrow = c (1, 1))
plot(model_3, 4)

# removing influential outliers
df3 <- df2[-c(4, 7, 18, 60, 62, 75, 102, 112, 151, 180, 196, 207),]
dim(df3)

##################### MODEL 4  #########################################
#Refitting the model
model_4 <- lm(Log_runoff ~ rainfall + Phases + `plot number` + GCP, data = df3)
# Inspect the model diagnostic metrics
model.metrics4 <- augment(model_4) 

# Assess normality of residuals using shapiro wilk test
shapiro_test(model.metrics4$.resid)

# Homogeneity of variances
ncvTest(model_4)

# Multicollinearity
vif(model_4)

# Influential Outliers
gg_cooksd(model_4)
par(mfrow = c (2, 2))
plot(model_4)

# removing influential outliers
df4 <- df3[-c(46, 66),]
dim(df4)

##################### MODEL 5  #########################################
#Refitting the model
model_5 <- lm(Log_runoff ~ rainfall + Phases + `plot number` + GCP, data = df4)
# Inspect the model diagnostic metrics
model.metrics5 <- augment(model_5) 

# Assess normality of residuals using shapiro wilk test
shapiro_test(model.metrics5$.resid)

# Homogeneity of variances
ncvTest(model_5)

# Multicollinearity
vif(model_5)

# Influential Outliers
gg_cooksd(model_5)
par(mfrow = c (2, 2))
plot(model_5)

# removing influential outliers
df5 <- df4[-c(167),]
dim(df5)

##################### MODEL 6  #########################################
#Refitting the model
model_6 <- lm(Log_runoff ~ rainfall + Phases + `plot number` + GCP, data = df5)
# Inspect the model diagnostic metrics
model.metrics6 <- augment(model_6) 

# Assess normality of residuals using shapiro wilk test
shapiro_test(model.metrics6$.resid)

# Homogeneity of variances
ncvTest(model_6)

# Multicollinearity
vif(model_6)

# Influential Outliers
gg_cooksd(model_6)
par(mfrow = c (2, 2))
plot(model_6)
par(mfrow = c (1, 1))
plot(model_6, 5)

# removing influential outliers
df6 <- df5[-c(3, 6, 65, 77, 79, 100, 119, 130, 141, 162),]
dim(df6)

##################### MODEL 7  #########################################
#Refitting the model
model_7 <- lm(Log_runoff ~ rainfall + Phases + `plot number` + GCP + `plot number`* Phases , data = df6)
summary(model_7)
Anova(model_7)

df6 <- df6 %>% rename(Plot = `plot number`)
pwc <- df6 %>% 
  group_by(Phases) %>%
  emmeans_test(
    Log_runoff ~ plot_number, covariate = c("rainfall", "GCP"),
    p.adjust.method = "bonferroni"
  )

pwc

pwc1 <- df6 %>% 
  group_by(plot_number) %>%
  emmeans_test(
    Log_runoff ~ Phases, covariate = c("rainfall", "GCP"),
    p.adjust.method = "bonferroni"
  )

pwc1

emmeans_Phases <- emmeans(model_7, ~ Phases)
summary(emmeans_Phases)

pairwise_comparisons_Phases <- contrast(emmeans_Phases, method = "pairwise")
summary(pairwise_comparisons_Phases)

library(ggpubr)
library(emmeans)


# homogenity of regression slopes 
unite(df6 , col = "group" ,  Phases , `plot number`)
?anova_test
anova_test(unite(df6 , col = "group" ,  Phases , `plot number`), Log_runoff ~ rainfall*group+group*GCP+GCP*rainfall*group , type = 3)

#linearity assumption
ggplot(df6, aes(x = rainfall , y = Log_runoff)) + stat_smooth(method = "loess")+ geom_point()+ facet_grid(Phases~`plot number`)
ggplot(df6, aes(x = GCP , y = Log_runoff)) + stat_smooth(method = "loess")+ geom_point()+ facet_grid(Phases~`plot number`)


# Inspect the model diagnostic metrics
model.metrics7 <- augment(model_7) 

par(mfrow = c (2, 2))
plot(model_7) 

# Assess normality of residuals using shapiro wilk test
shapiro_test(model.metrics7$.resid)

# Homogeneity of variances
levene_test(.resid ~ Phases*`plot number`, data = model.metrics7)

# Multicollinearity
vif(model_7)

# Influential Outliers
gg_cooksd(model_7)

par(mfrow = c (1, 1))
plot(model_7, 1)
par(mfrow = c (1, 1))
plot(model_7, 2)
plot(model_7, 4)

# removing influential outliers
df7 <- df6[-c(6, 169),]
dim(df7)

##################### MODEL 8  #########################################
#Refitting the model
model_8 <- lm(`Log_Runoff+1` ~ Rainfall + Phase + `Plot No` + GCVP, data = df7)
# Inspect the model diagnostic metrics
model.metrics8 <- augment(model_8) 

par(mfrow = c (2, 2))
plot(model_8)

# Assess normality of residuals using shapiro wilk test
shapiro_test(model.metrics8$.resid)

# Homogeneity of variances
levene_test(.resid ~ Phase*`Plot No`, data = model.metrics8)

# Multicollinearity
vif(model_8)

# Influential Outliers
gg_cooksd(model_8)


summary(model_1)$adj.r.squared # 0.7143161
summary(model_1.2)$adj.r.squared # 0.7057403

summary(model_2)$adj.r.squared # 0.7391091
summary(model_3)$adj.r.squared # 0.7686993
summary(model_4)$adj.r.squared # 0.7926547
summary(model_5)$adj.r.squared # 0.8245509
summary(model_6)$adj.r.squared # 0.8352214
summary(model_7)$adj.r.squared # 0.8488462
summary(model_8)$adj.r.squared # 0.8300179

glance(model_1)$AIC # 453.2867
glance(model_1.2)$AIC # 458.1029


glance(model_2)$AIC # 421.9126
glance(model_3)$AIC # 386.2219
glance(model_4)$AIC # 337.8657
glance(model_5)$AIC # 284.2443
glance(model_6)$AIC # 261.4466
glance(model_7)$AIC # 215.7471
glance(model_8)$AIC # 276.2843
 

glance(model_1.1)$BIC # 487.5802
glance(model_2)$BIC # 449.17
glance(model_3)$BIC # 413.2611
glance(model_4)$BIC # 364.4887
glance(model_5)$BIC # 310.4283
glance(model_6)$BIC # 287.4228
glance(model_7)$BIC # 241.2907
glance(model_8)$BIC # 309.1163

####################################################################
##################### MODEL A  #########################################
#Refitting the model
model_A <- lm(Log_runoff ~ rainfall + Phases + slope + GCP + LT + BA, data = data)
summary(model_A)
anova(model_A)

# Inspect the model diagnostic metrics
model.metricsA <- augment(model_A) 

par(mfrow = c (2, 2))
plot(model_A) 

# Assess normality of residuals using shapiro wilk test
shapiro_test(model.metricsA$.resid)

# Homogeneity of variances
levene_test(.resid ~ Phases*`plot number`, data = model.metricsA)

# Multicollinearity
vif(model_A)

# Influential Outliers
gg_cooksd(model_A)


#Refitting the model
model_B <- lm(`Log_Runoff+1` ~ Rainfall + Phase + Slope + GCVP + `Basel Area`, data = data)
summary(model_B)
# Inspect the model diagnostic metrics
model.metricsB <- augment(model_B) 

par(mfrow = c (2, 2))
plot(model_B) 

# Assess normality of residuals using shapiro wilk test
shapiro_test(model.metricsB$.resid)

# Homogeneity of variances
levene_test(.resid ~ Phase*`Plot No`, data = model.metricsB)

# Multicollinearity
vif(model_B)
dim(data)
dim(df)
dim(df2)

####################################################################
##################### MODEL c  #########################################

#Refitting the model
model_C <- lm(Log_runoff ~ rainfall + Phases + slope + GCP + LT, data = data)
summary(model_C)
anova(model_AC)

# Inspect the model diagnostic metrics
model.metricsC <- augment(model_C) 

par(mfrow = c (2, 2))
plot(model_Z) 

# Assess normality of residuals using shapiro wilk test
shapiro_test(model.metricsC$.resid)

# Homogeneity of variances
levene_test(.resid ~ Phases*`plot number`, data = model.metricsC)

# Multicollinearity
vif(model_C)

# Influential Outliers
gg_cooksd(model_C)


