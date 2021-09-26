# sumário da nova base de dados
df |> summary()

#aparentemente o número de portas ainda tem "?" nos níveis, mesmo sem
#nenhuma observação com esse valor
df$num_doors

#removendo esse level do fator
df$num_doors = df$num_doors |> droplevels()
df |> summary()

#a vari?vel make tem quase 50% das observa??es iguais a "other"
#na vari?vel fuel_system ? s? uma

#Varivel respostal = price
plot(df$wheel_base, df$price) #
plot(df$length, df$price)
plot(df$width, df$price)
plot(df$height, df$price) #
plot(df$curb_weight, df$price)
plot(df$engine_size, df$price)
plot(df$bore, df$price) #
plot(df$stroke, df$price) #
plot(df$compression_ratio, df$price) ##
plot(df$horsepower, df$price)
plot(df$peak_rpm, df$price) #
plot(df$city_mpg, df$price)
plot(df$highway_mpg, df$price)

#As variaveis Wheel_base, Height, bore, stroke, compression_ratio, peak_rpm, aparentemente,... 
#não parecem ter uma relação linear com a variavel resposta

plot(df$make, df$price)
plot(df$fuel_type, df$price)
plot(df$aspiration, df$price)
plot(df$num_doors, df$price) #
plot(df$body_style, df$price)
plot(df$drive_wheels, df$price)
plot(df$engine_location, df$price)
plot(df$engine_type, df$price)
plot(df$num_cylinders, df$price)
plot(df$fuel_system, df$price)

#A unica varivel que parece n?o ter muita rela??o com o pre?o ? o num_doors
#Os boxplots apresentam possiveis valores descrepantes acima de 30000 $

# Modelos de regress?o simples:
fit1 = lm(price ~ length, df)
fit2 = lm(price ~ width, df)
fit3 = lm(price ~ curb_weight, df)
fit4 = lm(price ~ engine_size, df)
fit5 = lm(price ~ horsepower, df)
fit6 = lm(price ~ city_mpg, df)
fit7 = lm(price ~ highway_mpg, df)
summary(fit1)
summary(fit2) #
summary(fit3) ##
summary(fit4) ##
summary(fit5) #
summary(fit6)
summary(fit7) #
par(mfrow=c(2,2), mar=c(5.1,4.1,4.1,2.1))
plot(fit1)
plot(fit2)
plot(fit3)
plot(fit4)
plot(fit5)
plot(fit6)
plot(fit7)
#Todos os residuos n?o apresentam homocedasticidade nem normalidade

