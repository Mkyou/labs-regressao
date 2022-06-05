res_vs_fitted = function(model, res_type = "deviance"){
  plot(y = residuals.glm(model, type = res_type), x = model$fitted.values,
       ylab = paste("Resíduos", res_type), xlab = "Valores Ajustados")
  abline(h = 2, lwd = 1, lty = 2)
  abline(h = -2, lwd = 1, lty = 2)
}
res_vs_fitted(stepwise)

local_influence = function(model, identify = NULL){
  package = "glmtoolbox"
  if(!require(package, character.only = T)){
    install.packages(package, dependencies = T)
  }
  glmtoolbox::localInfluence(model, plot.it = T, identity = identify)
}
local_influence(fit3)

res_vs_index = function(model, res_type = "deviance"){
  plot(residuals.glm(model, type = res_type),
       ylab = paste("Resíduos", res_type),
       xlab = "Índice")
  abline(h = c(-2, 2), lwd = 1, lty = 3)
}
res_vs_index(stepwise)

res_envelope = function(model, res_type = "quantile", rep = 100,
                    identify = NULL){
  package = "glmtoolbox"
  if(!require(package, character.only = T)){
    install.packages(package, dependencies = T)
  }
  glmtoolbox::envelope(model, type = res_type, rep = rep)
}
res_envelope(fit3)

hl_test = function(model){
  package = "glmtoolbox"
  if(!require(package, character.only = T)){
    install.packages(package, dependencies = T)
  }
  glmtoolbox::hltest(model)
}
hl_test(fit3)


#res_influence = function(model){
  package = "boot"
  if(!require(package, character.only = T)){
    install.packages(package, dependencies = T)
    require(boot)
  }
  glm_diag = function (glmfit, glmdiag = glm.diag(glmfit)) {
    if (is.null(glmdiag)) 
      glmdiag <- glm.diag(glmfit)
    if (is.null(subset)) 
      subset <- seq_along(glmdiag$h)
    else if (is.logical(subset)) 
      subset <- seq_along(subset)[subset]
    else if (is.numeric(subset) && all(subset < 0)) 
      subset <- (1L:(length(subset) + length(glmdiag$h)))[subset]
    else if (is.character(subset)) {
      if (is.null(labels)) 
        labels <- subset
      subset <- seq_along(subset)
    }
    hh <- glmdiag$h/(1 - glmdiag$h)
    plot(hh, glmdiag$cook, xlab = "h/(1-h)", ylab = "Cook statistic")
    rx <- range(hh)
    ry <- range(glmdiag$cook)
    rank.fit <- glmfit$rank
    nobs <- rank.fit + glmfit$df.residual
    cooky <- 8/(nobs - 2 * rank.fit)
    hy <- (2 * rank.fit)/(nobs - 2 * rank.fit)
    if ((cooky >= ry[1L]) && (cooky <= ry[2L])) 
      abline(h = cooky, lty = 2)
    if ((hy >= rx[1L]) && (hy <= rx[2L])) 
      abline(v = hy, lty = 2)
  }
  glm_diag(model)
}
#res_influence(stepwise)




