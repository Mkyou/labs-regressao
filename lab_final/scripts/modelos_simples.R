fit1 = lm(price ~ poly(length, degree = 2, raw = F), df)
fit2 = lm(price ~ poly(width, degree = 2, raw = F), df)
fit3 = lm(price ~ poly(curb_weight, degree = 2, raw = F), df)
fit4 = lm(price ~ poly(engine_size, degree = 1, raw = F), df)
fit5 = lm(price ~ poly(horsepower, degree = 1, raw = F), df)
fit6 = lm(price ~ poly(city_mpg, degree = 4, raw = F), df)
fit7 = lm(price ~ poly(highway_mpg, degree = 3, raw = F), df)
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