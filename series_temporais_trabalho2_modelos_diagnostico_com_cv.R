
library(forecast)   # Arima(), fitted(), accuracy()
library(MASS)       # boxcox()
library(jsonlite)   # fromJSON()
library(Metrics)    # rmse(), mae(), mape()

# 1) Baixar e preparar os dados
# -------------------------------------------------------------
url   <- "https://api.bcb.gov.br/dados/serie/bcdata.sgs.1403/dados?formato=json"
dados <- fromJSON(url)

dados$data  <- as.Date(dados$data, format = "%d/%m/%Y")
dados$valor <- as.numeric(dados$valor)

lambda_bc <- boxcox(valor ~ 1, data = dados, plotit = FALSE)$x       |>
  (\(x) x[which.max(boxcox(valor ~ 1, data = dados, plotit = FALSE)$y)])()

dados$valor_bc <- (dados$valor ^ lambda_bc - 1) / lambda_bc

# Série mensal, remover tendência e sazonalidade --------------
serie_bc         <- ts(dados$valor_bc, frequency = 12)
decomp           <- decompose(serie_bc, type = "additive")
serie_estacionar <- na.omit(serie_bc - decomp$trend - decomp$seasonal)

# 2) Função para extrair métricas
# -------------------------------------------------------------
avaliar <- function(modelo, y, nome){
  f <- fitted(modelo)
  data.frame(
    Modelo = nome,
    AIC  = AIC(modelo),
    BIC  = BIC(modelo),
    RMSE = rmse(y, f),
    MAE  = mae (y, f),
    MAPE = mape(y, f)
  )
}

# 3) Estimar modelos
# -------------------------------------------------------------
resultados <- list()

## AR ----------------------------------------------------------------
for(p in 1:3){
  mod <- Arima(serie_estacionar, order = c(p, 0, 0))
  resultados[[paste0("AR(", p, ")")]] <- avaliar(mod, serie_estacionar,
                                                 paste0("AR(", p, ")"))
}

## MA ----------------------------------------------------------------
for(q in 1:3){
  mod <- Arima(serie_estacionar, order = c(0, 0, q))
  resultados[[paste0("MA(", q, ")")]] <- avaliar(mod, serie_estacionar,
                                                 paste0("MA(", q, ")"))
}

## ARMA p = q = 1–3 --------------------------------------------------
for(pq in 1:3){
  mod <- Arima(serie_estacionar, order = c(pq, 0, pq))
  resultados[[paste0("ARMA(", pq, ",", pq, ")")]] <-
    avaliar(mod, serie_estacionar, paste0("ARMA(", pq, ",", pq, ")"))
}

## ARMA(2,3) extra ---------------------------------------------------
mod23 <- Arima(serie_estacionar, order = c(2, 0, 3))
resultados[["ARMA(2,3)"]] <- avaliar(mod23, serie_estacionar, "ARMA(2,3)")

# -------------------------------------------------------------
# 4) Tabela final ordenada por AIC
# -------------------------------------------------------------
tabela <- do.call(rbind, resultados)
tabela <- tabela[order(tabela$AIC), ]
print(tabela, row.names = FALSE, digits = 4)

##################### Diagnostico de Resíduos #############
# -------------------------------------------------------------
# 0) Pacotes
# -------------------------------------------------------------
library(forecast)   # Arima(), residuals(), Acf(), Pacf()
library(MASS)       # boxcox()
library(jsonlite)   # fromJSON()
library(Metrics)    # rmse(), mae(), mape()
library(lmtest)     # Box.test(), bptest()
library(tseries)    # jarque.bera.test(), ArchTest()

# -------------------------------------------------------------
# 1) Baixar e preparar os dados  (mesmo pipeline anterior)
# -------------------------------------------------------------
url   <- "https://api.bcb.gov.br/dados/serie/bcdata.sgs.1403/dados?formato=json"
dados <- fromJSON(url)

dados$data  <- as.Date(dados$data, format = "%d/%m/%Y")
dados$valor <- as.numeric(dados$valor)

lambda_bc <- {
  bc <- boxcox(valor ~ 1, data = dados, plotit = FALSE)
  bc$x[which.max(bc$y)]
}
dados$valor_bc <- (dados$valor ^ lambda_bc - 1) / lambda_bc

serie_bc         <- ts(dados$valor_bc, frequency = 12)
decomp           <- decompose(serie_bc, type = "additive")
serie_estacionar <- na.omit(serie_bc - decomp$trend - decomp$seasonal)

# -------------------------------------------------------------
# 2) Avaliar todos os modelos (AR 1–3, MA 1–3, ARMA 1–3/1–3, 2/3)
#    — se você já executou o script anterior, pode pular
# -------------------------------------------------------------
model_store <- list()   # guarda objetos  Arima()
metrics     <- list()   # guarda métricas

avaliar <- function(mod, y, nome) {
  f <- fitted(mod)
  data.frame(
    Modelo = nome,
    AIC  = AIC(mod),
    BIC  = BIC(mod),
    RMSE = rmse(y, f),
    MAE  = mae (y, f),
    MAPE = mape(y, f)
  )
}

## AR ----------------------------------------------------------
for (p in 1:3) {
  mod <- Arima(serie_estacionar, order = c(p,0,0))
  nm  <- paste0("AR(",p,")")
  model_store[[nm]] <- mod
  metrics[[nm]]     <- avaliar(mod, serie_estacionar, nm)
}

## MA ----------------------------------------------------------
for (q in 1:3) {
  mod <- Arima(serie_estacionar, order = c(0,0,q))
  nm  <- paste0("MA(",q,")")
  model_store[[nm]] <- mod
  metrics[[nm]]     <- avaliar(mod, serie_estacionar, nm)
}

## ARMA p=q -----------------------------------------------------
for (pq in 1:3) {
  mod <- Arima(serie_estacionar, order = c(pq,0,pq))
  nm  <- paste0("ARMA(",pq,",",pq,")")
  model_store[[nm]] <- mod
  metrics[[nm]]     <- avaliar(mod, serie_estacionar, nm)
}

## ARMA(2,3) extra ---------------------------------------------
mod23 <- Arima(serie_estacionar, order = c(2,0,3))
model_store[["ARMA(2,3)"]] <- mod23
metrics[["ARMA(2,3)"]]     <- avaliar(mod23, serie_estacionar, "ARMA(2,3)")

# Tabela para decidir melhor modelo ----------------------------
tabela <- do.call(rbind, metrics)
tabela <- tabela[order(tabela$AIC), ]

print(tabela, row.names = FALSE, digits = 4)

# -------------------------------------------------------------
# 3) Escolher melhor modelo  (menor AIC)
# -------------------------------------------------------------
best_name  <- tabela$Modelo[1]
best_model <- model_store[[best_name]]

cat("\n>>> Melhor modelo pelo AIC:", best_name, "\n")




############## PARTE ADICIONAL ##############################

library(forecast)

# AR(3)
modelo_ar <- Arima(serie_estacionar, order = c(3, 0, 0), include.mean = TRUE)

# MA(3)
modelo_ma <- Arima(serie_estacionar, order = c(0, 0, 3), include.mean = TRUE)

# ARMA(3,3)
modelo_arma <- Arima(serie_estacionar, order = c(3, 0, 3), include.mean = TRUE)

# Sumários
summary(modelo_ar)
summary(modelo_ma)
summary(modelo_arma)

# Previsão futura
# Função para prever e plotar na escala original
prever_plotar <- function(modelo, nome_modelo, h = 12) {
  prev <- forecast(modelo, h = h)
  
  # Recupera tendência e sazonalidade
  tend_futuro <- rep(tail(na.omit(decomp$trend), 1), h)
  saz_futuro  <- rep(tail(decomp$seasonal, 12), length.out = h)
  
  # Volta para escala transformada
  previsao_boxcox <- prev$mean + tend_futuro + saz_futuro
  
  # Inversa de Box-Cox
  inv_boxcox <- function(y, lambda) {
    if (lambda == 0) return(exp(y))
    return((y * lambda + 1)^(1 / lambda))
  }
  previsao_final <- inv_boxcox(previsao_boxcox, lambda_bc)
  
  # Construir série para plot
  serie_original <- ts(dados$valor, frequency = 12, start = start(serie_estacionar))
  inicio_previsao <- end(serie_original) + c(0, 1)
  previsao_ts <- ts(previsao_final, start = inicio_previsao, frequency = 12)
  
  # Plot
  ts.plot(serie_original, previsao_ts, col = c("black", "blue"), lty = c(1, 2),
          main = paste("Previsão com", nome_modelo, "na Escala Original"),
          ylab = "Valor")
  legend("topleft", legend = c("Série Original", "Previsão"),
         col = c("black", "blue"), lty = c(1, 2))
}

# Aplicar para os três modelos
prever_plotar(model_store[["AR(3)"]],     "AR(3)")
prever_plotar(model_store[["MA(3)"]],     "MA(3)")
prever_plotar(model_store[["ARMA(3,3)"]], "ARMA(3,3)")





# Função de CV para modelos ARIMA
cv_ts <- function(serie, order, h = 1, initial = 60) {
  n <- length(serie)
  erros <- c()
  
  for (i in seq(initial, n - h)) {
    treino <- window(serie, end = c(1, i))
    teste  <- window(serie, start = c(1, i + 1), end = c(1, i + h))
    
    mod <- tryCatch(
      Arima(treino, order = order),
      error = function(e) NULL
    )
    
    if (!is.null(mod)) {
      prev <- forecast(mod, h = h)$mean
      erro <- teste - prev
      erros <- c(erros, erro)
    }
  }
  
  return(sqrt(mean(erros^2, na.rm = TRUE)))
}

# Aplicar CV para os modelos selecionados
rmse_ar3   <- cv_ts(serie_estacionar, order = c(3, 0, 0))
rmse_ma3   <- cv_ts(serie_estacionar, order = c(0, 0, 3))
rmse_arma  <- cv_ts(serie_estacionar, order = c(3, 0, 3))

# Exibir resultados
cv_resultados <- data.frame(
  Modelo = c("AR(3)", "MA(3)", "ARMA(3,3)"),
  RMSE_CV = c(rmse_ar3, rmse_ma3, rmse_arma)
)

print(cv_resultados)


#################################################################


######### Diagnóstico de Resíduos comentado -- abaixo 

# 

################################################################
#########################DIAGNÓSTICO############################

# -------------------------------------------------------------
# 4) Diagnóstico dos resíduos do melhor modelo
# -------------------------------------------------------------
resid <- residuals(best_model)

## 4.1 Série de resíduos ---------------------------------------
plot(resid, type = "l", main = paste("Resíduos -", best_name),
     ylab = "Resíduo", xlab = "Tempo")
abline(h = 0, col = "red", lty = 2)

## 4.2 Histograma ---------------------------------------------
hist(resid, breaks = 30, freq = FALSE,
     main = paste("Histograma dos resíduos -", best_name),
     xlab = "Resíduo")
lines(density(resid), lwd = 2)

## 4.3 QQ-plot -------------------------------------------------## QQ-plot dos resíduos  (use qqnorm + qqline)
qqnorm(resid,
       main = paste("QQ-plot dos resíduos ‒", best_name),
       pch  = 1,           # bolinhas vazadas
       cex  = 0.8)         # tamanho um pouco menor

qqline(resid,
       col = "red",        # cor chamativa
       lwd = 2,            # linha mais grossa
       lty = 1)            # linha contínua


## 4.4 ACF -----------------------------------------------------
acf(resid, main = paste("ACF dos resíduos -", best_name))

## 4.5 PACF ----------------------------------------------------
pacf(resid, main = paste("PACF dos resíduos -", best_name))

# -------------------------------------------------------------
# 5) Testes estatísticos
# -------------------------------------------------------------
cat("Teste Ljung-Box (lag = 20)")
print(Box.test(resid, lag = 20, type = "Ljung-Box"))

cat("Teste Jarque-Bera (normalidade)")
print(jarque.bera.test(resid))

cat("Teste Shapiro-Wilk (normalidade)")
print(shapiro.test(resid))

# cat("Teste ARCH (Engle) lags = 12")
# print(ArchTest(resid, lags = 12))

cat("McLeod-Li (Box-Pierce nos resíduos, lag = 20) ")
print(Box.test(resid^2, lag = 20, type = "Ljung-Box"))
