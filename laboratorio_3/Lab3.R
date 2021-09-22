library(tidyverse)
library(ggthemes)
library(ggpubr)

df = read_csv("lab3.csv")

#Sem dados repetidos
df[-1] |> distinct()

#Sem NA
summary(df)

colnames(df) = c("id", "idade_casa", "distancia_metro", 
                 "lojas_conveniencia", "preco_unidade_area")

hist(df$idade_casa)
hist(df$lojas_conveniencia)
hist(df$distancia_metro)
hist(df$preco_unidade_area)

#Removendo valor discrepante de preços
df1= df[-271,]

plot(y = filter(df1, df1$distancia_metro<1000)$preco_unidade_area, x= filter(df1, df1$distancia_metro<1000)$distancia_metro)

df1$dist2 = ifelse(df1$distancia_metro>1000,0,1)

graf_disp = ggplot(df1, aes(x=distancia_metro, y=preco_unidade_area, color=factor(dist2), shape=factor(dist2))) +
  geom_point() +
  geom_smooth(method="lm", se = F) +
  labs(subtitle = "Preço da unidade da área por distância até o metrô",
       y = "Preço por unidade da área(Dolares)", x = "Distância até o metrô(Jardas)",
       caption = "Fonte: web scrapping") +
  theme_economist()
graf_disp

#Criar subconjunto:
df2 = (filter(df1, df1$distancia_metro<=1000))
df3 = (filter(df1, df1$distancia_metro>1000))

#Plotando os diagramas de disperssão e os boxplots 
graf_disp2 = ggplot(df2, aes(x=idade_casa, y=preco_unidade_area)) +
  geom_point() +
  geom_smooth(method="lm", se = F) +
  labs(subtitle = "Preço da unidade da área por idade da casa, para distâncias menores ou iguais a 1000 jardas do metrô",
       y = "Preço por unidade da área(Dolares)", x = "Idade da casa(Anos)",
       caption = "Fonte: web scrapping") +
  theme_economist()
graf_disp2

graf_disp3 = ggplot(df3, aes(x=idade_casa, y=preco_unidade_area)) +
  geom_point() +
  geom_smooth(method="lm", se = F) +
  labs(subtitle = "Preço da unidade da área por idade da casa, para distâncias maiores que 1000 jardas do metrô",
       y = "Preço por unidade da área(Dolares)", x = "Idade da casa(Anos)",
       caption = "Fonte: web scrapping") +
  theme_economist()
graf_disp3

ggarrange(graf_disp2, graf_disp3, ncol = 2, labels = c("", ""), nrow = 1)

boxplot1=ggplot(df2, aes(x=factor(lojas_conveniencia), y=preco_unidade_area, color=factor(lojas_conveniencia))) +
  geom_boxplot() +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  theme_economist() +
  labs(title="Boxplot com observações individuais",subtitle = "Número de conveniências por preço da unidade de área, para distâncias menores ou iguais a 1000 jardas do metrô",
       y = "Preço por unidade da área(Dolares)", x = "Número de conveniências",
       caption = "Fonte: web scrapping") 
boxplot1

boxplot2=ggplot(df3, aes(x=factor(lojas_conveniencia), y=preco_unidade_area, color=factor(lojas_conveniencia))) +
  geom_boxplot() +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  theme_economist() +
  labs(title="Boxplot com observações individuais",subtitle = "Número de conveniências por preço da unidade de área, para distâncias maiores que 1000 jardas do metrô",
       y = "Preço por unidade da área(Dolares)", x = "Número de conveniências",
       caption = "Fonte: web scrapping") 
boxplot2

ggarrange(boxplot1, boxplot2, ncol = 2, labels = c("", ""), nrow = 1)

#Checando as correlações
cor.test(x = df2$idade_casa, y = df2$preco_unidade_area)

cor.test(x = df2$lojas_conveniencia, y = df2$preco_unidade_area)

cor.test(x = df3$idade_casa, y = df3$preco_unidade_area)

cor.test(x = df3$lojas_conveniencia, y = df3$preco_unidade_area)

cor.test(x = df2$distancia_metro, y = df2$preco_unidade_area)

cor.test(x = df3$distancia_metro, y = df3$preco_unidade_area)

#Pelas correlações usaremos a distancia do metro como variavel resposta

#Modelos simples:
fit1= lm(df2$preco_unidade_area ~ df2$idade_casa)
fit2= lm(df2$preco_unidade_area ~ df2$distancia_metro)
fit3= lm(df3$preco_unidade_area ~ df3$idade_casa)
fit4= lm(df3$preco_unidade_area ~ df3$distancia_metro)
summary(fit1)
summary(fit2)
summary(fit3)
summary(fit4)

#Estimar as variancias modelo 2 e 4, respectivamente
var_param = function(x_v, qmsres){
  b1 = sqrt(qmsres)/(sqrt(sum((x_v-mean(x_v))^2)))
  b0 = (sqrt(qmsres)*sqrt(sum(x_v^2)))/
    sqrt(length(x_v)*sum((x_v-mean(x_v))^2))
  return (list(b0, b1))
}

anova(fit2)
qmsres1 = 80.7

anova(fit4)
qmsres2 = 41.49

var_param(df2$distancia_metro, qmsres1)
var_param(df3$distancia_metro, qmsres2)

#Comentar sobre os pressuspostos: relação linear entre x e y, x fixados, média do erro = 0, homocedasticidade, erros com distribuição normal; ao longo dos textos

