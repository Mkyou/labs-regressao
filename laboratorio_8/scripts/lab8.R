library(readr)
library(tidyverse)
library(ggthemes)
lab8 = read_csv("C:/Users/gabfr/OneDrive/Área de Trabalho/Trab_Regress2/labs-regressao/laboratorio_8/input/lab8.csv")
lab8[-1] |> distinct()

summary(lab8)

library(GGally)
ggpairs(lab8)


disp1 = lab8 |>
  ggplot(aes(x = tempo, y = preco)) + 
  geom_point() +
  labs(subtitle = "Preço do imóvel por tempo de construção",
       x = "Tempo", 
       y = "Preço") +
  theme_economist()
disp1
ggsave("Figura 1", plot = disp1, path = "./plots", device = "png")

#Item 2,3,4:
#Modelos:

fit1 = lm(preco ~ tempo, lab8)
fit2 = lm(preco ~ tempo + I(tempo^2), lab8)
fit3 = lm(preco ~ tempo + I(tempo^2) + I(tempo^3), lab8)
fit2o = lm(preco ~ poly(tempo, degree=2, raw=F), lab8)
fit3o = lm(preco ~ poly(tempo, degree=3, raw=F), lab8)

summary(fit1)
summary(fit2)
summary(fit3)
summary(fit2o)
summary(fit3o)

anova(fit1, fit2)
anova(fit1, fit3)
anova(fit2, fit3)

# item 5: 45 anos
54.34 -2.052253*45 + 0.046474*(45^2)

#item 6:
fit4 = lm(preco ~ tempo + I(tempo^2) + metro + loja, lab8)
summary(fit4)

#Item 7,8:
#AIC
step(fit4, direction = "backward")

step(lm(preco ~ 1, lab8), direction = "forward", scope = ~ tempo + I(tempo^2) + metro + loja)

step(fit4, direction = "both")

#BIC
step(fit4, direction = "backward", k =log(360))

step(lm(preco ~ 1, lab8), direction = "forward", scope = ~ tempo + I(tempo^2) + metro + loja, k =log(360) )

step(fit4, direction = "both", k =log(360) )

fit5 =  lm( preco ~ ., lab8)

install.packages("olsrr")
library(olsrr)
ols_mallows_cp(fit1, fit5)
ols_mallows_cp(fit2, fit5)
ols_mallows_cp(fit3, fit5)
ols_mallows_cp(fit4, fit5)


library(MASS)
summary(fit4)
anova(fit4)
par(mfrow = c(2,2))
plotn=plot(fit4)
shapiro.test(fit4$residuals)
lmtest::gqtest(fit4)
qqPlot(fit4, main="QQ Plot") #qq plot for studentized resid 
# distribution of studentized residuals
sresid2 <- studres(fit4) 
hist(sresid2, freq=FALSE, 
     main="Distribution of Studentized Residuals")
xm2<-seq(min(sresid2),max(sresid2),length=40) 
ym2<-dnorm(xm2) 
lines(xm2, ym2)

# Influential Observations
# Cook's D plot
# identify D values > 4/(n-k-1) 
cutoff2 <- 4/((nrow(lab8)-length(fit4$coefficients)-2)) 
plot(fit4, which=4, cook.levels=cutoff2)
abline(h = 4/360, lty = 2)

#DfFits
dffits2 <- as.data.frame(dffits(fit4))
thresh3 <- 2*sqrt((nrow(lab8)/length(fit4$coefficients)-1))
plot(dffits(fit4), type = 'h')
abline(h = thresh3, lty = 2)
abline(h = -thresh3, lty = 2)

#DfBetas
dfbetas2 <- as.data.frame(dfbetas(fit4))
thresh4 <- 2/sqrt( nrow(lab8))
par(mfrow=c(2,1))
plot(dfbetas2$tempo, type='h')
abline(h = thresh4, lty = 2)
abline(h = -thresh4, lty = 2)
plot(dfbetas2$`I(tempo^2)` , type='h')
abline(h = thresh4, lty = 2)
abline(h = -thresh4, lty = 2)
plot(dfbetas2$metro , type='h')
abline(h = thresh4, lty = 2)
abline(h = -thresh4, lty = 2)
plot(dfbetas2$loja , type='h')
abline(h = thresh4, lty = 2)
abline(h = -thresh4, lty = 2)


dfbetas2$abs1 = abs(dfbetas2$tempo )  
dfbetas2$abs2 = abs(dfbetas2$`I(tempo^2)`)  
dfbetas2$abs3 = abs(dfbetas2$metro)  
dfbetas2$abs4 = abs(dfbetas2$loja)  

#COVratio
plot(covratio(fit4))
