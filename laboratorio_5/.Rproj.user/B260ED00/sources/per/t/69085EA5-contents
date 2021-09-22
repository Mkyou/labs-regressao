#importando bibliotecas
library(tidyverse)
library(ggthemes)
library(lmtest)
library(MASS)
library(car)
library(gridExtra)

#lendo e visualizando o banco
df = trees[-1]

df |> head()
df |> glimpse()

#mudando nomes de colunas

df = df |> rename(altura = Height, volume = Volume)

df |> head()

#Verificando se existem valores duplicados
df |> distinct()

#Medidas resumo e histogramas
df |> summary()

plot1 = df$altura |> hist(
  main = "Distribuição da Altura das Cerejeiras",
  xlab = "Altura das Cerejeiras (em pés)",
  ylab = "Frequência", col = "chocolate",
  border = "brown")
        

plot2 = df$volume |> hist(
  main = "Distribuição do Volume das Cerejeiras (em pés cúbicos)",
  xlab = "Volume das Cerejeiras",
  ylab = "Frequência", col = "chocolate",
  border = "brown")

#diagramas de dispersão

disp1 = df |>
  ggplot(aes(x = altura, y = volume)) + 
  geom_point(aes(x=altura, y=volume), colour = 'blue') +
  geom_smooth(method="lm", se = F) +
  labs(subtitle = "Volume Vs. Altura das Cereijas",
       x = "Altura em pés", 
       y = "Volume em pés cúbicos") +
  theme_economist()
disp1
ggsave("Figura 1", plot = disp1, path = "./plots", device = "png")

# Verificando correlação

#correlação de 60% com evidência de relação a 95%
cor.test(df$altura, df$volume)

# Ajustando modelos

fit1 = lm(volume ~ altura, df)
summary(fit1)
anova(fit1)

jack = function(modelo){
  fit_ajustados = c(modelo$fitted.values)
  fit_studres = c(studres(modelo))
  fit_jack = as_tibble(data.frame(fit_ajustados, fit_studres))
  
  return(list(fit_ajustados, fit_studres))
}

#Transformações
df['raiz_y'] = sqrt(df$volume)
df['log_y'] = log(df$volume)
df['y_quadrado'] = (df$volume)^2
head(df)

fit2 = lm(raiz_y ~ altura, df)
summary(fit2)
anova(fit2)

fit3 = lm(log_y ~ altura, df)
summary(fit3)
anova(fit3)

fit4 = lm(y_quadrado ~ altura, df)
summary(fit4)
anova(fit4)

df1 = as_data_frame(df)
bc = boxcox(lm(df1$volume ~ df1$altura))
lambda = bc$x[which.max(bc$y)]

df['y_boxcox'] = ((df$volume^lambda)-1)/lambda

head(df)

fit5 = lm(y_boxcox ~ altura, df)
summary(fit5)
anova(fit5)

par(mfrow = c(2,3))

qqPlot(fit1)
qqPlot(fit2)
qqPlot(fit3)
qqPlot(fit4)
qqPlot(fit5)

j1 = jack(fit1)
j1plot = scatterplot(x = j1[[1]], y = j1[[2]],
            xlab = "Valores Ajustados", 
            ylab = "Resíduos",
            smooth = T, regLine = F, boxplots = F)

j2 = jack(fit2)
j2plot = scatterplot(x = j2[[1]], y = j2[[2]],
            xlab = "Valores Ajustados", 
            ylab = "Resíduos",
            smooth = T, regLine = F, boxplots = F)

j3 = jack(fit3)
j3plot = scatterplot(x = j3[[1]], y = j3[[2]],
            xlab = "Valores Ajustados", 
            ylab = "Resíduos",
            smooth = T, regLine = F, boxplots = F)

j4 = jack(fit4)
j4plot = scatterplot(x = j4[[1]], y = j4[[2]],
            xlab = "Valores Ajustados", 
            ylab = "Resíduos",
            smooth = T, regLine = F, boxplots = F)

j5 = jack(fit5)
j5plot = scatterplot(x = j5[[1]], y = j5[[2]],
            xlab = "Valores Ajustados", 
            ylab = "Resíduos",
            smooth = T, regLine = F, boxplots = F)


#fit1 não rejeita h0 normal, rejeita homocedasticidade
shapiro.test(fit1$residuals)
lmtest::gqtest(fit1)

#fit2 rejeita h0 normal, rejeita homocedasticidade
shapiro.test(fit2$residuals)
lmtest::gqtest(fit2)

#fit3 rejeita h0 normal, não rejeita homocedasticidade
shapiro.test(fit3$residuals)
lmtest::gqtest(fit3)

#fit4 rejeita h0 normal, rejeita homocedasticidade
shapiro.test(fit4$residuals)
lmtest::gqtest(fit4)

#fit1 rejeita h0, não rejeita homocedasticidade
shapiro.test(fit5$residuals)
lmtest::gqtest(fit5)

