library(readr)
library(tidyverse)
library(ggthemes)
lab7 = read_csv("C:/Users/gabfr/OneDrive/Área de Trabalho/Trab_Regress/labs-regressao/laboratorio_7/input/lab7.csv")
lab7[-1] |> distinct()

summary(lab7)

#univariados
plot1 = lab7$age |> hist(
  main = "Dados sobre pacientes fumantes do sudeste dos Estados Unidos",
  xlab = "Idade dos pacientes",
  ylab = "Frequência", col = "chocolate",
  border = "brown")

plot2 = lab7$bmi |> hist(
  main = "Dados sobre pacientes fumantes do sudeste dos Estados Unidos",
  xlab = "IMC dos pacientes",
  ylab = "Frequência", col = "chocolate",
  border = "brown")

plot3 = lab7$charges |> hist(
  main = "Dados sobre pacientes fumantes do sudeste dos Estados Unidos",
  xlab = "Preço do seguro de saúde dos pacientes (dollar)",
  ylab = "Frequência", col = "chocolate",
  border = "brown")

#bivariados
disp1 = lab7 |>
  ggplot(aes(x = age, y = charges)) + 
  geom_point() +
  labs(subtitle = "Preço do plano de saúde Versus Idade",
       x = "Idade", 
       y = "Preço em Dollar") +
  theme_economist()
disp1
ggsave("Figura 1", plot = disp1, path = "./plots", device = "png")

disp2 = lab7 |>
  ggplot(aes(x = bmi, y = charges)) + 
  geom_point() +
  labs(subtitle = "Preço do plano de saúde Versus IMC",
       x = "IMC", 
       y = "Preço em Dollar") +
  theme_economist()
disp2
ggsave("Figura 2", plot = disp2, path = "./plots", device = "png")

disp3 = lab7 |>
  ggplot(aes(x = age, y = bmi)) + 
  geom_point() +
  labs(subtitle = "IMC Versus Idade",
       x = "Idade", 
       y = "IMC") +
  theme_economist()
disp3
ggsave("Figura 3", plot = disp3, path = "./plots", device = "png")

#Com evidencia de correlação 95%
cor.test(lab7$age, lab7$charges)

#Com evidencia de correlação a 95%
cor.test(lab7$bmi, lab7$charges)

#Sem evidencia de correlação a 95%
cor.test(lab7$age, lab7$bmi)

##Curiosidade:
install.packages("GGally")
library(GGally)
ggpairs(lab7)
#SpeedRun de descritiva

# Ajustando modelo
fit1 = lm(charges ~ age + bmi, lab7)
summary(fit1)
anova(fit1)
par(mfrow = c(2,2))
plot4=plot(fit1)
ggsave("Figura 4", plot = plot4, path = "./plots", device = "png")
shapiro.test(fit1$residuals)
lmtest::gqtest(fit1)

library(car)
qqPlot(fit1, main="QQ Plot") #qq plot for studentized resid 
# distribution of studentized residuals
library(MASS)
sresid <- studres(fit1) 
hist(sresid, freq=FALSE, 
     main="Distribution of Studentized Residuals")
xm<-seq(min(sresid),max(sresid),length=40) 
ym<-dnorm(xm) 
lines(xm, ym)

# Influential Observations
# Cook's D plot
# identify D values > 4/(n-k-1) 
cutoff <- 4/((nrow(lab7)-length(fit1$coefficients)-2)) 
plot(fit1, which=4, cook.levels=cutoff)
abline(h = 4/91, lty = 2)

#DfFits
dffits <- as.data.frame(dffits(fit1))
thresh <- 2*sqrt((nrow(lab7)/length(fit1$coefficients)-1))
plot(dffits(fit1), type = 'h')
abline(h = thresh, lty = 2)
abline(h = -thresh, lty = 2)

#DfBetas
dfbetas <- as.data.frame(dfbetas(fit1))
thresh2 <- 2/sqrt( nrow(lab7))
par(mfrow=c(2,1))
plot(dfbetas$age, type='h')
abline(h = thresh2, lty = 2)
abline(h = -thresh2, lty = 2)
plot(dfbetas$bmi, type='h')
abline(h = thresh2, lty = 2)
abline(h = -thresh2, lty = 2)

dfbetas$abs1 = abs(dfbetas$age)  
dfbetas$abs2 = abs(dfbetas$bmi)  
# 71 e 90 (talvez 85)

#COVratio
plot(covratio(fit1))

### Repetindo tudo tirando os pontos

fit2 = lm(charges ~ age + bmi, lab7[-c(90,71),])
summary(fit2)
anova(fit2)
par(mfrow = c(2,2))
plotn=plot(fit2)
shapiro.test(fit2$residuals)
lmtest::gqtest(fit2)
qqPlot(fit2, main="QQ Plot") #qq plot for studentized resid 
# distribution of studentized residuals
sresid2 <- studres(fit2) 
hist(sresid2, freq=FALSE, 
     main="Distribution of Studentized Residuals")
xm2<-seq(min(sresid2),max(sresid2),length=40) 
ym2<-dnorm(xm2) 
lines(xm2, ym2)

# Influential Observations
# Cook's D plot
# identify D values > 4/(n-k-1) 
cutoff2 <- 4/((nrow(lab7[-c(90,71),])-length(fit2$coefficients)-2)) 
plot(fit2, which=4, cook.levels=cutoff2)
abline(h = 4/91, lty = 2)

#DfFits
dffits2 <- as.data.frame(dffits(fit2))
thresh3 <- 2*sqrt((nrow(lab7[-c(90,71),])/length(fit2$coefficients)-1))
plot(dffits(fit2), type = 'h')
abline(h = thresh3, lty = 2)
abline(h = -thresh3, lty = 2)

#DfBetas
dfbetas2 <- as.data.frame(dfbetas(fit2))
thresh4 <- 2/sqrt( nrow(lab7[-c(90,71),]))
par(mfrow=c(2,1))
plot(dfbetas2$age, type='h')
abline(h = thresh4, lty = 2)
abline(h = -thresh4, lty = 2)
plot(dfbetas2$bmi, type='h')
abline(h = thresh4, lty = 2)
abline(h = -thresh4, lty = 2)

dfbetas2$abs1 = abs(dfbetas2$age)  
dfbetas2$abs2 = abs(dfbetas2$bmi)  
# 71 e 90 (talvez 85)

#COVratio
plot(covratio(fit2))


