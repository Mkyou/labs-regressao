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
