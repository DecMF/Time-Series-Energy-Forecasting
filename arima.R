##############################################
# 0) Pacotes ---------------------------------------------------------------
##############################################
library(forecast)   # auto.arima(), Arima(), forecast(), tsCV()
library(jsonlite)   # fromJSON()
library(Metrics)    # rmse(), mae(), mape()
library(lmtest)     # Box.test()
library(tseries)    # jarque.bera.test()
library(MASS)       # boxcox()

set.seed(175072)    # reprodutibilidade

##############################################
# 1) Baixar e preparar os dados --------------------------------------------
##############################################
url   <- "https://api.bcb.gov.br/dados/serie/bcdata.sgs.1403/dados?formato=json"
dados <- fromJSON(url)

dados$data  <- as.Date(dados$data, format = "%d/%m/%Y")
dados$valor <- as.numeric(dados$valor)

# Box-Cox (estima λ ótimo; retorna λ ≈ 0 se log for melhor)
lambda_bc <- {
  bc <- boxcox(valor ~ 1, data = dados, plotit = FALSE)
  bc$x[which.max(bc$y)]
}

# Aplicar transformação de Box-Cox
if (abs(lambda_bc) < 1e-6) {
  dados$valor_bc <- log(dados$valor)
} else {
  dados$valor_bc <- (dados$valor^lambda_bc - 1) / lambda_bc
}

serie_bc <- ts(dados$valor_bc, frequency = 12,
               start = c(lubridate::year(dados$data[1]),
                         lubridate::month(dados$data[1])))

##############################################
# 2) Escolher o melhor ARIMA via auto.arima() --------------------------------
##############################################
best_arima <- auto.arima(
  serie_bc,
  seasonal      = FALSE,   # aqui queremos apenas ARIMA (sem componente saz.)
  stepwise      = FALSE,   # busca exaustiva → resultado mais confiável
  approximation = FALSE
)

cat("\n>>> Melhor ARIMA encontrado (AICc):",
    sprintf("ARIMA(%d,%d,%d)", best_arima$arma[1],
            best_arima$arma[6],
            best_arima$arma[2]), "\n")
print(summary(best_arima))

##############################################
# 3) Validação cruzada (rolling-origin) -------------------------------------
##############################################
# Função de previsão 1-passo-à-frente usando a ordem encontrada
order_best <- c(best_arima$arma[1], best_arima$arma[6], best_arima$arma[2])

fc_fun <- function(y, h) {
  # Ajusta ARIMA com mesma ordem nos dados de treino
  mod <- Arima(y, order = order_best, include.mean = best_arima$include.mean)
  forecast(mod, h = h)$mean
}

# ... seu best_arima, order_best e fc_fun definidos como antes ...

# rodar tsCV com initial = 60 (5 anos de treino antes de começar a computar erros)
cv_errors <- tsCV(serie_bc, fc_fun, h = 1, initial = 60)

# agora sim vai ter valores não-NA para calcular RMSE
rmse_cv <- sqrt(mean(cv_errors^2, na.rm = TRUE))

cat("\n>>> RMSE fora-da-amostra (tsCV, h=1, initial=60):",
    round(rmse_cv,4), "\n")


##############################################
# 4) Métricas dentro da amostra ------------------------------------------------
##############################################
fitted_vals <- fitted(best_arima)

metrics_in <- data.frame(
  Modelo = sprintf("ARIMA(%d,%d,%d)", order_best[1], order_best[2], order_best[3]),
  AIC    = AIC(best_arima),
  BIC    = BIC(best_arima),
  RMSE   = rmse(serie_bc, fitted_vals),
  MAE    = mae (serie_bc, fitted_vals),
  MAPE   = mape(serie_bc, fitted_vals),
  RMSE_CV = rmse_cv
)

print(metrics_in, row.names = FALSE, digits = 4)

##############################################
# 5) Diagnóstico dos resíduos --------------------------------------------------
##############################################
resid <- residuals(best_arima)

par(mfrow = c(3,2))  # layout 3x2

# 5.1 Série dos resíduos
plot(resid, type = "l",
     main = "Resíduos do ARIMA", ylab = "Resíduo", xlab = "Tempo")
abline(h = 0, col = "red", lty = 2)

# 5.2 Histograma + densidade
hist(resid, breaks = 30, freq = FALSE,
     main = "Histograma dos resíduos", xlab = "Resíduo")
lines(density(resid), lwd = 2)

# 5.3 QQ-plot
qqnorm(resid, main = "QQ-plot dos resíduos")
qqline(resid, col = "red", lwd = 2)

# 5.4 ACF
acf(resid, main = "ACF dos resíduos")

# 5.5 PACF
pacf(resid, main = "PACF dos resíduos")

# 5.6 Resíduos ao quadrado (heterocedasticidade visual)
plot(resid^2, type="l", main="Resíduos² — inspeção vis. de ARCH")

par(mfrow = c(1,1))  # volta layout

##############################################
# 6) Testes formais -----------------------------------------------------------
##############################################
cat("\n--- Testes formais de resíduos ---\n")

cat("\nLjung-Box (lag = 20):\n")
print(Box.test(resid, lag = 20, type = "Ljung-Box"))

cat("\nMcLeod-Li (lag = 20, resíduos²):\n")
print(Box.test(resid^2, lag = 20, type = "Ljung-Box"))

cat("\nJarque-Bera (normalidade):\n")
print(jarque.bera.test(resid))

cat("\nShapiro-Wilk (normalidade):\n")
print(shapiro.test(resid))
