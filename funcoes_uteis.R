library(tidyverse)

set.seed(50)
x = as_tibble(cbind(rnorm(50, 0, 1), rnorm(50, 20, 2)))
y = rnorm(50, 2, 3)

fit = lm(y ~ x$V1 + x$V2)
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

#exemplo
aux = residual_graph_analyses(fit)
aux

#exemplo2
aux2 = model_hyphotesys_analyses(fit)
aux2[1]
aux2[2]
aux2[3]
aux2[5]



#y = c(42.1,38.0,31.3,43.9,38.8,39.8,38.3,30.0,49.7,39.5,40.1,38.2,28.6,
#      49.6,39.1,40.5,38.1,29.1,48.8,39.1,41.8,38.9,29.6,46.0,39.1)
#x = c("M1","M1","M1","M1","M1","M2","M2","M2","M2","M2","M3","M3",
#      "M3","M3","M3","M4","M4","M4","M4","M4","M5","M5","M5","M5","M5")
#x = as.factor(x)


#anova(lm(y ~ x))

#summary(lm(y ~ x))
