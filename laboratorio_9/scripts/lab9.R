library(tidyverse)

raw_df = read_csv('./input/lab09.csv')
raw_df |> glimpse()

df = raw_df |> 
  filter(age >= 20)
df |> head()


df$`Estado Nutricional` = 
  ifelse(df$bmi<= 18.5 & df$age<60 | df$bmi<22 & df$age>59, 
         "Magreza", ifelse(df$bmi>18.5 & df$bmi<25 & df$age<60 
                           |df$bmi>=22 & df$bmi<=27 & df$age>59,
         "Eutrofia", "Sobrepeso/Obesidade"))

df$sex = as.factor(df$sex)
df$smoker = as.factor(df$smoker)
df$region = as.factor(df$region)
df$children = as.factor(df$children)
df$`Estado Nutricional` = as.factor(df$`Estado Nutricional`)

df |> summary()


plot1 = df |> ggplot(aes(x = smoker, y = charges, fill = smoker)) +
  geom_boxplot() +
  labs(title = "Preço do seguro por hábito de fumo",
       x = "Fumante", y = "Preço do seguro ($)") +
  scale_fill_discrete("Fumante") +
  theme_minimal()
  
plot1

plot2 = df |> ggplot(aes(x = `Estado Nutricional`, y = charges, 
                         fill = `Estado Nutricional`)) + 
  geom_boxplot() + 
  labs(
    title = "Preço do seguro por estado nutricional (IMC)",
    y = "Preço do seguro ($)") + 
  scale_fill_discrete("Estado Nutricional") + 
  theme_minimal()

plot2

plot3 = df |> ggplot(aes(x = sex, y = charges, fill = sex)) + 
  geom_boxplot() + 
  labs(title = "Preço do seguro por sexo", x = "Sexo", 
       y = "Preço do seguro ($)") + 
  scale_fill_discrete("Sexo") +
  theme_minimal()

plot3

plot4 = df |> ggplot(aes(x = children, y = charges, fill = children)) + 
  geom_boxplot() + 
  labs(title = "Preço do seguro por número de filhos", 
       x = "Número de filhos", y = "Preço do seguro($)") + 
  scale_fill_discrete("N° Filhos") + 
  theme_minimal()

plot4

plot5 = df |> ggplot(aes(x = region, y = charges, fill = region)) + 
  geom_boxplot() + 
  labs(title = "Preço do seguro por região", 
       x = "Região", y = "Preço do seguro($)") + 
  scale_fill_discrete("Região") + 
  theme_minimal()

plot5

plot6 = df |> ggplot(aes(x = `Estado Nutricional`, y = charges, 
                         fill = `smoker`)) + 
  geom_boxplot() + 
  labs(
    title = "Preço do seguro por estado nutricional (IMC) e hábito de fumo",
    y = "Preço do seguro ($)") + 
  scale_fill_discrete("Fumante") + 
  theme_minimal()

plot6

plot7 = df |> ggplot(aes(x = region, y = charges, 
                         fill = `smoker`)) + 
  geom_boxplot() + 
  labs(
    title = "Preço do seguro por região e hábito de fumo",
    y = "Preço do seguro ($)") + 
  scale_fill_discrete("Fumante") + 
  theme_minimal()

plot7

plot8 = df |> ggplot(aes(x = region, y = charges, 
                         fill = `Estado Nutricional`)) + 
  geom_boxplot() + 
  labs(
    title = "Preço do seguro por região e estado nutricional",
    y = "Preço do seguro ($)") + 
  scale_fill_discrete("Estado Nutricional") + 
  theme_minimal()

plot8

plot9 = df |> ggplot(aes(x = age, y = charges, fill = sex)) +
  geom_point(aes(x = age, y = charges, color = sex, pch=smoker)) + 
  labs(title = "Preço do seguro por idade e sexo", x = "Idade", 
       y = "Preço do seguro ($)") + 
  scale_fill_discrete("Sexo") + 
  theme_minimal()

plot9

plt1 = multiplot(plot1, plot2, plot3, plot4, cols = 2)
plt2 = multiplot(plot5, plot6, plot7, plot8, cols = 2)


aux = df[,-c(2,4,5,6,8)]

my_fn <- function(data, mapping, ...){
  p <- ggplot(data = data, mapping = mapping) +
    geom_point() +
    geom_smooth(method=lm, aes=(accuracy = 0.01),
                fill="green", color="blue", ...)
  p}

GGally::ggpairs(aux,
                upper = list(continuous = "cor"),
                lower = list(continuous = my_fn),
                axisLabels="none")


fit = lm(charges ~ smoker + age + sex + children + region + 
              `Estado Nutricional`, data = df)
fit |> summary()

fit1 = lm(charges ~ smoker + age + sex + age*sex + children + region +
            `Estado Nutricional`, data = df)
fit1 |> summary()

res_an = residual_graph_analyses(fit)
res_an
model_hyphotesys_analyses(fit)
