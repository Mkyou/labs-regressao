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
plot(fit4) #n normal
plot(fit5)
plot(fit6)
plot(fit7)
#Todos os residuos n?o apresentam homocedasticidade nem normalidade