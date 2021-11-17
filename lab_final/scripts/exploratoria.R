######Ler comentários em Notas de Edições.txt##########
library(DescTools)
library(tidyverse)

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
  var2 = bptest(fit, studentize = T)
  
  #durbin watson
  indep = dwtest(fit)
  
  #multicolinearidade
  vifs = list(`VIFs` = vif(fit), `Média dos Vifs` = mean(vif(fit)), 
              `Tolerância` = 1/vif(fit))
  
  
  
  return(list(normal, var, var2, indep, vifs))
}


# sumário da nova base de dados
df |> summary()

#aparentemente o número de portas ainda tem "?" nos níveis, mesmo sem
#nenhuma observação com esse valor
df$num_doors

#removendo esse level do fator
df$num_doors = df$num_doors |> droplevels()
df |> summary()

#Varivel respostal = price

#----------------------------Exploratória------------------------------#

my_fn <- function(data, mapping, ...){
  p <- ggplot(data = data, mapping = mapping) +
    geom_point() +
    geom_smooth(method=lm, aes=(accuracy = 0.01),
                fill="green", color="blue", ...)
  p}

GGally::ggpairs(df[,-c(1,2,3,4,5,6,7,8,14,15,17,20)],
                upper = list(continuous = "cor"),
                lower = list(continuous = my_fn),
                axisLabels="none")

plot1 = df |> ggplot(aes(x = make, y = price, fill = make)) +
  geom_boxplot() +
  labs(title = "Preço do carro por fabricante",
       x = "Fabricante", y = "Preço do Carro ($)") +
    scale_fill_discrete("Fabricante") +
    theme_minimal()

#plot1 #Visualização ruim.


plot2 = df |> ggplot(aes(x = fuel_type, y = price, fill = fuel_type)) +
  geom_boxplot() +
  labs(title = "Preço do carro por tipo de combustível",
       x = "Tipo de Combustível", y = "Preço do Carro ($)") +
  scale_fill_discrete("Tipo de Combustível") +
  theme_minimal()

plot2 #Possível relação; não vai usar (multicolinearidade)


plot3 = df |> ggplot(aes(x = aspiration, y = price, fill = aspiration)) +
  geom_boxplot() +
  labs(title = "Preço do carro por Aspiration",
       x = "Aspiration", y = "Preço do Carro ($)") +
  scale_fill_discrete("Aspiration") +
  theme_minimal()

plot3 #vamo usar


plot4 = df |> ggplot(aes(x = num_doors, y = price, fill = num_doors)) +
  geom_boxplot() +
  labs(title = "Preço do carro por Número de Portas",
       x = "Número de Portas", y = "Preço do Carro ($)") +
  scale_fill_discrete("Número de Portas") +
  theme_minimal()

plot4 #Não vamo usar


plot5 = df |> ggplot(aes(x = body_style, y = price, fill = body_style)) +
  geom_boxplot() +
  labs(title = "Preço do carro por Body Style",
       x = "Body Style", y = "Preço do Carro ($)") +
  scale_fill_discrete("Body Style") +
  theme_minimal()

plot5 #vamo usar


plot6 = df |> ggplot(aes(x = drive_wheels, y = price, 
                         fill = drive_wheels)) +
  geom_boxplot() +
  labs(title = "Preço do carro por Rodas Motrizes",
       x = "Rodas Motrizes", y = "Preço do Carro ($)") +
  scale_fill_discrete("Rodas Motrizes") +
  theme_minimal()

plot6 #vamo usar


plot7 = df |> ggplot(aes(x = engine_location, 
                         y = price, fill = engine_location)) +
  geom_boxplot() +
  labs(title = "Preço do carro por Localização do Motor",
       x = "Localização do Motor", y = "Preço do Carro ($)") +
  scale_fill_discrete("Localização do Motor") +
  theme_minimal()

plot7 #vamo usar


plot8 = df |> ggplot(aes(x = engine_type, y = price, fill = engine_type)) +
  geom_boxplot() +
  labs(title = "Preço do carro por Tipo de Motor",
       x = "Tipo de Motor", y = "Preço do Carro ($)") +
  scale_fill_discrete("Tipo de Motor") +
  theme_minimal()

plot8 #vamo usar


plot9 = df |> ggplot(aes(x = num_cylinders, y = price, 
                          fill = num_cylinders)) +
  geom_boxplot() +
  labs(title = "Preço do carro por número de cilíndros",
       x = "Número de cilíndros", y = "Preço do Carro ($)") +
  scale_fill_discrete("Número de cilíndros") +
  theme_minimal()

plot9 #vamo usar


plot10 = df |> ggplot(aes(x = fuel_system, y = price, 
                         fill = fuel_system)) +
  geom_boxplot() +
  labs(title = "Preço do carro por Sistema de Combustível",
       x = "Sistema de Combustível", y = "Preço do Carro ($)") +
  scale_fill_discrete("Sistema de Combustível") +
  theme_minimal()

plot10 #não vamo usar (multicolinearidade)

plot11 = df |> ggplot(aes(x = compression_ratio, y = price, 
                          fill = compression_ratio)) +
  geom_boxplot() +
  labs(title = "Preço do carro por taxa de compressão",
       x = "Taxa de compressão", y = "Preço do Carro ($)") +
  scale_fill_discrete("Taxa de compressão") +
  theme_minimal()

#plot11 vamo usar

plt1 = multiplot(plot2, plot3, plot4, plot5, cols = 2)
plt2 = multiplot(plot6, plot7, plot8, plot9, cols = 2)
plt3 = multiplot(plot10, plot11, cols=2) 
plot1
#Os boxplots apresentam possiveis valores descrepantes acima de 30000 $


#relação entre variáveis categóricas
corrplot::corrplot(DescTools::PairApply(df[,c(1,2,3,4,5,6,7,8,14,15,17,20)], 
                                        DescTools::CramerV),
                   method = "number", diag = F)


#Correlação entre fuel system, compression ratio e fuel type é 1.
cramerV(df$fuel_system, df$compression_ratio)

