######Ler comentários em Notas de Edições.txt##########
library(tidyverse)


#Tratamento dos dados

#caminho dos arquivos
filepath = "./inputs/imports-85.csv"

#nomes das colunas
df_names = c("symboling", "normalized_losses", "make", "fuel_type",
             "aspiration", "num_doors", "body_style", "drive_wheels",
             "engine_location", "wheel_base", "length", "width", "height", 
             "curb_weight", "engine_type", "num_cylinders", "engine_size", 
             "fuel_system", "bore", "stroke", "compression_ratio", 
             "horsepower", "peak_rpm", "city_mpg", "highway_mpg", "price")

#leitura de dados
raw_df = read_csv(filepath, col_names = F)

#colocando nomes das colunas na tibble
new_raw_df = rbind(df_names, raw_df)
colnames(new_raw_df) = as.character(new_raw_df[1,])
new_raw_df = new_raw_df[-1,]

new_raw_df |> glimpse()

#todas as colunas são vetores de caracteres
#além disso, parece que a coluna 2 possui muitos missings
sum(new_raw_df[, 2] == "?")

#41 missings em 205 linhas é muito missing, vamos remover a coluna
new_raw_df = new_raw_df[, -2]
glimpse(new_raw_df)

#vamos alterar os tipos dos dados agora.
new_raw_df = new_raw_df |> mutate(across(symboling, as.double))
new_raw_df = new_raw_df |> mutate(across(make, as.factor))
new_raw_df = new_raw_df |> mutate(across(fuel_type, as.factor))
new_raw_df = new_raw_df |> mutate(across(aspiration, as.factor))
new_raw_df = new_raw_df |> mutate(across(num_doors, as.factor))
new_raw_df = new_raw_df |> mutate(across(body_style, as.factor))
new_raw_df = new_raw_df |> mutate(across(drive_wheels, as.factor))
new_raw_df = new_raw_df |> mutate(across(engine_location, as.factor))
new_raw_df = new_raw_df |> mutate(across(wheel_base, as.double))
new_raw_df = new_raw_df |> mutate(across(length, as.double))
new_raw_df = new_raw_df |> mutate(across(width, as.double))
new_raw_df = new_raw_df |> mutate(across(height, as.double))
new_raw_df = new_raw_df |> mutate(across(curb_weight, as.double))
new_raw_df = new_raw_df |> mutate(across(engine_type, as.factor))
new_raw_df = new_raw_df |> mutate(across(num_cylinders, as.factor))
new_raw_df = new_raw_df |> mutate(across(engine_size, as.double))
new_raw_df = new_raw_df |> mutate(across(fuel_system, as.factor))
new_raw_df = new_raw_df |> mutate(across(bore, as.double))
new_raw_df = new_raw_df |> mutate(across(stroke, as.double))
new_raw_df = new_raw_df |> mutate(across(compression_ratio, as.double))
new_raw_df = new_raw_df |> mutate(across(horsepower, as.double))
new_raw_df = new_raw_df |> mutate(across(peak_rpm, as.double))
new_raw_df = new_raw_df |> mutate(across(city_mpg, as.double))
new_raw_df = new_raw_df |> mutate(across(highway_mpg, as.double))
new_raw_df = new_raw_df |> mutate(across(price, as.double))

#Vamos verificar novamente
glimpse(new_raw_df)
tail(new_raw_df)

#Parece tudo ok, vejamos um sumário dos dados
new_raw_df |> summary()

#na variável num_doors existem duas observações marcadas assim: "?"
#Assim como os outros NA, devemos retirá-las. 
new_raw_df = filter(new_raw_df, new_raw_df$num_doors != "?")

#temos o banco de dados tratado
df = new_raw_df |> drop_na()

df |> glimpse()

#todas as observações são diferentes umas das outras
df |> distinct()


  
#Categorizando compression_ratio (compress>15 = 1; compress<15 = 0)
df$compression_ratio = as_factor(ifelse(df$compression_ratio>15, 1, 0))

#Verificando quantidade de valores maiores que 15
df$compression_ratio


df$num_cylinders = ifelse(df$num_cylinders == "eight", 8,
                          ifelse(df$num_cylinders == "five", 5,
                                 ifelse(df$num_cylinders == "four", 4,
                                        ifelse(df$num_cylinders == "six", 6,
                                               ifelse(df$num_cylinders == "three", 3, 12)))))

df$num_cylinders = as.numeric(df$num_cylinders)

df$num_cylinders = as_factor(ifelse(df$num_cylinders >= 8, 2,
                                    ifelse(df$num_cylinders <= 4, 0, 1)))



df$make = as.character(df$make)

df$make = ifelse(df$make == "mercedes-benz", "mercedes", df$make)
df$make = as_factor(df$make)
df$make

df = df[-67,]

