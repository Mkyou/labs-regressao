library(tidyverse)

#rodar trat_dados primeiro

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
plot(fit4) #n normal
plot(fit5)
plot(fit6)
plot(fit7)
#Todos os residuos n?o apresentam homocedasticidade nem normalidade

step(lm(price ~ ., df), direction = "backward")

df$num_cylinders

fitbetter = lm(formula = price ~ symboling + make + aspiration + body_style + 
                 wheel_base + length + width + height + curb_weight + engine_type + 
                 engine_size + fuel_system + bore + stroke + 
                 compression_ratio + peak_rpm + highway_mpg, data = df)
summary(fitbetter)
plot(fitbetter)

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

residual_graph_analyses = function(fit){
  library(docstring)
  library(qqplotr)
  library(ggfortify)
  library(olsrr)
  library(car)
  #' Dado um modelo de regressão linear, plota vários gráficos
  #' referentes aos resíduos.
  #'
  #' Atenção: é preciso atribuir a função a algum objeto 
  #' para visualizar os gráficos do DFBETAS
  #' Atenção 2: Plote: ols_plot_resid_lev(fit) fora da função
  #' 
  #' @param fit, o modelo a que se pretende realizar a análise
  
  #Resíduos studentizados
  res_stu = rstudent(fit)
  #Resíduos padronizados
  res_pad = rstandard(fit)
  #Transformando em tibble
  residual_data = as_tibble(cbind(res_stu, res_pad, fit$fitted.values))
  
  #valores ajustados vs resíduos
  plt1 <- ggplot(residual_data, aes(x=fit$fitted.values, y=res_pad)) + 
    geom_point(color="#6B3A2C") +
    geom_smooth(method=loess , color="red", fill="#69b3a2", se=TRUE) +
    geom_hline(yintercept=0, linetype='dashed', color='red') + 
    labs(title = "Resíduos Vs. Ajustados", x = "Valores Ajustados",
         y = "Resíduos Padronizados") + 
    theme_minimal()
  
  #qqplot com bandas sensitivas a caudas (TS) 
  #e inversão do teste de kolmogorov (ks)
  
  plt2 <- ggplot(residual_data, mapping = aes(sample = res_pad)) +
    geom_qq_band(bandType = "ks", mapping = aes(fill = "KS-Test"), alpha = 0.5) +
    geom_qq_band(bandType = "pointwise", mapping = aes(fill = "Normal"), alpha = 0.5) +
    geom_qq_band(bandType = "ts", mapping = aes(fill = "Tail-Sensitive"), alpha = 0.5) +
    stat_qq_line() +
    stat_qq_point() +
    labs(title = "Gráfico Quantil Quantil", x = "Quantis Teóricos", 
         y = "Resíduos Padronizados") +
    scale_fill_discrete("Bandtype") + 
    theme_minimal()
  
  plt3 <- ggplot(residual_data, aes(x = fit$fitted.values, 
                                    y = sqrt(abs(res_pad)))) +
    geom_point(color="#6B3A2C") +
    geom_smooth(method=loess , color="red", fill="#69b3a2", se=TRUE) +
    labs(title = "Escala - Locação", x = "Valores Ajustados",
         y = "Raíz dos Resíduos Padronizados") + 
    theme_minimal()
  
  plt4 <- autoplot(fit, which = 5) + 
    geom_hline(yintercept=0, linetype='dashed', color='red') + 
    theme_minimal() 
  
  plt_a <- multiplot(plt1, plt2, plt3, plt4, cols = 2)
  plt_b <- multiplot(ols_plot_cooksd_chart(fit) + theme_minimal(), 
                     ols_plot_dffits(fit) + theme_minimal(), 
                     cols = 2)
  
  ols_plot_resid_lev(fit)
  crPlots(fit)
  avPlots(fit)
  
  
  
  plt_c <- ols_plot_dfbetas(fit)
  return(plt_c)
  
}

model_hyphotesys_analyses = function(fit){
  library(lmtest)
  library(docstring)
  library(car)
  #' Dado um modelo de regressão linear, realiza testes de hipóteses
  #' verificando algumas das suposições.
  #'
  #' Atenção: não realiza teste de linearidade 
  #' 
  #' @param fit, o modelo a que se pretende realizar a análise
  
  #shapiro
  normal = shapiro.test(rstandard(fit))
  
  #goldfeld-quandt
  var = gqtest(fit)
  #breush pagan
  var2 = bptest(fit, studentize = F)
  
  #durbin watson
  indep = dwtest(fit)
  
  #multicolinearidade
  vifs = list(`VIFs` = vif(fit), `Média dos Vifs` = mean(vif(fit)), 
              `Tolerância` = 1/vif(fit))
  
  
  
  return(list(normal, var, var2, indep, vifs))
}

aux = residual_graph_analyses(fitbetter)
aux

aux2 = model_hyphotesys_analyses(fitbetter)
aux2[1]
aux2[2]
aux2[3]
aux2[5]
