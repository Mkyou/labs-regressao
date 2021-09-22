#importando bibliotecas
library(tidyverse)
library(ggthemes)
library(lmtest)

#lendo e visualizando o banco
df = read_csv("./input/lab4.csv")

df |> head()
df |> tail()
df |> glimpse()

#mudando nomes de colunas

df = df |> rename(id = ...1, gasto_ped = gasto.PeD, gasto_adm = gasto.adm,
             gasto_mkt = gasto.mkt)

df |> head()

#Verificando se existem valores duplicados
df[-1] |> distinct()

#Medidas resumo e histogramas
df |> summary()

plot1 = df$gasto_ped |> hist(
  main = "Gastos com Pesquisa e Desenvolvimento",
  xlab = "Gastos em Unidades Monetárias",
  ylab = "Frequência", col = "chocolate",
  border = "brown")
        

plot2 = df$gasto_adm |> hist(
  main = "Gastos com Despesas Administrativas",
  xlab = "Gastos em Unidades Monetárias",
  ylab = "Frequência", col = "chocolate",
  border = "brown")

plot3 = df$gasto_mkt |> hist(
  main = "Gastos com Marketing",
  xlab = "Gastos em Unidades Monetárias",
  ylab = "Frequência", col = "chocolate",
  border = "brown")

plot4 = df$lucro |> hist(
  main = "Lucro",
  xlab = "Lucro em Unidades Monetárias",
  ylab = "Frequência", col = "chocolate",
  border = "brown")


#diagramas de dispersão

disp1 = df |>
  ggplot(aes(x = gasto_ped, y = lucro)) + 
  geom_point() +
  labs(subtitle = "Lucro Versus Gastos com Pesquisa e Desenvolvimento",
       x = "Gastos em Unidades Monetárias", 
       y = "Lucro em Unidades Monetárias") +
  theme_economist_white()
disp1
ggsave("Figura 1", plot = disp1, path = "./plots", device = "png")

disp2 = df|>
  ggplot(aes(x = gasto_adm, y = lucro)) + 
  geom_point() + 
  labs(subtitle = "Lucro Versus Gastos com Despesas Administrativa",
       x = "Gastos em Unidades Monetárias", 
       y = "Lucro em Unidades Monetárias") +
  theme_economist_white()
disp2
ggsave("Figura 2", plot = disp2, path = "./plots", device = "png")

disp3 = df|>
  ggplot(aes(x = gasto_mkt, y = lucro)) + 
  geom_point() + 
  labs(subtitle = "Lucro Versus Gastos com Marketing",
       x = "Gastos em Unidades Monetárias", 
       y = "Lucro em Unidades Monetárias") +
  theme_economist_white()
disp3
ggsave("Figura 3", plot = disp3, path = "./plots", device = "png")

# Verificando correlação

#correlação de 97% com evidência de relação a 95%
cor.test(df$gasto_ped, df$lucro)

#correlação de 20%, sem evidência de relação entre as variáveis a 95%
cor.test(df$gasto_adm, df$lucro)

#correlação de 75%, com evidência de relação a 95%
cor.test(df$gasto_mkt, df$lucro)

# Ajustando modelos

fit1 = lm(lucro ~ gasto_ped, df[-50,])
summary(fit1)
anova(fit1)
par(mfrow = c(2,2))
plot(fit1)
shapiro.test(fit1$residuals)
lmtest::gqtest(fit1)

#teste para b0 e b1


fit2 = lm(lucro ~ gasto_adm, df[-50,])
summary(fit2)
anova(fit2)
par(mfrow = c(2,2))
plot(fit2)
shapiro.test(fit2$residuals)
lmtest::gqtest(fit2)



fit3 = lm(lucro ~ gasto_mkt, df[-47,])
summary(fit3)
anova(fit3)
par(mfrow = c(2,2))
plot(fit3)
shapiro.test(fit3$residuals)
lmtest::gqtest(fit3)



