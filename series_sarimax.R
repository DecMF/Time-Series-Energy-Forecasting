# 0) Pacotes -------------------------------------------------------------
library(jsonlite)
library(forecast)
library(tseries)
library(lubridate)
library(ggplot2)

# 1) Dados
url        <- "https://api.bcb.gov.br/dados/serie/bcdata.sgs.1403/dados?formato=json"
dados      <- fromJSON(url)
dados$data <- as.Date(dados$data, "%d/%m/%Y")
dados$valor <- as.numeric(dados$valor)

# 2) Box–Cox 
bc     <- boxcox(valor ~ 1, data = dados, plotit = FALSE)
lambda <- bc$x[which.max(bc$y)]
dados$valor_bc <- BoxCox(dados$valor, lambda)

# 3) Criar ts
start_year  <- year(dados$data[1])
start_month <- month(dados$data[1])
serie_bc    <- ts(dados$valor_bc,
                  start     = c(start_year, start_month),
                  frequency = 12)

# 4) Dummy para quebra em Jul/2001 
break_year  <- 2001
break_month <- 7
time_index  <- time(serie_bc)
dummy       <- ifelse(time_index >= (break_year + (break_month-1)/12), 1, 0)

library(forecast)
library(dplyr)

# ── 1) Definir os ranges das ordens p,d,q,P,D,Q
p_range <- 2:3
d_range <- 0:1
q_range <- 2:3

P_range <- 2:3
D_range <- 0:1
Q_range <- 2:3

# ── 2) Combinações das ordens
grid <- expand.grid(p = p_range,
                    d = d_range,
                    q = q_range,
                    P = P_range,
                    D = D_range,
                    Q = Q_range) %>%
  mutate(AIC  = NA_real_,
         RMSE = NA_real_,
         MAE  = NA_real_)



########## CÓDIGO COMENTADO ABAIXO PORQUE DEMORA PARA CARAMBA-
######### Foi feito inicialmente com uma grande número de combinaçoes da ordem p,d,q...
######## Para selecionar os melhores modelos de acordo com suas ordens p,d,q....
#### Demora cerca de 10 minutos

# ── 3) Loop de ajuste SARIMAX + métricas 
# for(i in seq_len(nrow(grid))) {
#   par  <- grid[i, ]
#   fit  <- tryCatch(
#     Arima(serie_bc,
#           order      = c(par$p, par$d, par$q),
#           seasonal   = list(order = c(par$P, par$D, par$Q),
#                             period = 12),
#           xreg       = dummy,
#           include.constant = TRUE),
#     error = function(e) NULL
#   )
#   if(!is.null(fit)) {
#     res           <- residuals(fit)
#     grid$AIC[i]   <- AIC(fit)
#     grid$RMSE[i]  <- sqrt(mean(res^2))
#     grid$MAE[i]   <- mean(abs(res))
#   }
# }

# ── 4) Selecionar melhores
# results <- grid %>% filter(!is.na(AIC))
# 
# cat("Top 5 por AIC:\n")
# print(results %>% arrange(AIC)  %>% slice(1:5))
# 
# cat("\nTop 5 por RMSE:\n")
# print(results %>% arrange(RMSE) %>% slice(1:5))
# 
# cat("\nTop 5 por MAE:\n")
# print(results %>% arrange(MAE)  %>% slice(1:5))
# 


################## A partir das permutações dos Modelos sele
library(forecast)
library(ggplot2)


mod_aic  <- Arima(serie_bc,
                  order      = c(3,0,3),
                  seasonal   = list(order = c(3,1,3), period = 12),
                  xreg       = dummy,
                  include.constant = TRUE)

mod_rmse <- Arima(serie_bc,
                  order      = c(3,0,3),
                  seasonal   = list(order = c(2,1,3), period = 12),
                  xreg       = dummy,
                  include.constant = TRUE)

mod_mae  <- Arima(serie_bc,
                  order      = c(3,0,3),
                  seasonal   = list(order = c(2,1,3), period = 12),
                  xreg       = dummy,
                  include.constant = TRUE)


### Resíduos
plot_sarimax <- function(fit, title) {
  cat("\n===========\n", title, "\n===========\n")
  print(summary(fit))
  checkresiduals(fit)                 # resíduos, ACF/PACF, Ljung–Box, histograma
  
  # previsão 12 meses, já invertendo Box–Cox 
  h  <- 12
  fc <- forecast(fit,
                 xreg    = rep(1, h),
                 h       = h,
                 lambda  = lambda_bc,
                 biasadj = TRUE)
  
  # série histórica na escala original
  serie_orig <- InvBoxCox(serie_bc, lambda_bc)
  
  
  start_fc <- tsp(serie_orig)[2] + 1/12
  fc_ts    <- ts(fc$mean, start = start_fc, frequency = 12)
  
  autoplot(serie_orig, series = "Histórico") +
    autolayer(fc_ts,      series = "Previsão") +
    ggtitle(title) +
    xlab("Tempo") + ylab("Valor") +
    scale_colour_manual(values = c("Histórico"="black", "Previsão"="blue")) +
    theme_minimal()
}


#Rodar para os três melhores modelos segundo os critérios
p1 <- plot_sarimax(mod_aic,
                   "Previsão 12m — melhor por AIC\nSARIMAX(3,0,3)(3,1,3)[12]")

p2 <- plot_sarimax(mod_rmse,
                   "Previsão 12m — melhor por RMSE\nSARIMAX(3,0,3)(2,1,3)[12]")

p3 <- plot_sarimax(mod_mae,
                   "Previsão 12m — melhor por MAE\nSARIMAX(3,0,3)(2,1,3)[12]")

#Plotagem
print(p1); print(p2); print(p3)


### Validação cruzada demora bastante também == Não saia rodando discaradamente 

######### Aplicar validação cruzadaaa #########################

# Função para CV com SARIMAX
cv_sarimax <- function(ts_data, order, seasonal_order, xreg_full, h = 1, initial = 100) {
  n      <- length(ts_data)
  erros  <- c()
  
  for (i in seq(initial, n - h)) {
    # --- extração por índice em vez de window() ---
    y_train   <- ts_data[1:i]
    y_test    <- ts_data[(i + 1):(i + h)]
    xreg_train <- matrix(xreg_full[1:i], ncol = 1)
    xreg_test  <- matrix(xreg_full[(i + 1):(i + h)], ncol = 1)
    
    fit <- tryCatch(
      Arima(
        y_train,
        order            = order,
        seasonal         = list(order = seasonal_order, period = frequency(ts_data)),
        xreg             = xreg_train,
        include.constant = TRUE
      ),
      error = function(e) NULL
    )
    
    if (!is.null(fit)) {
      pred   <- forecast(fit, xreg = xreg_test, h = h)$mean
      erros  <- c(erros, as.numeric(y_test - pred))
    }
  }
  sqrt(mean(erros^2, na.rm = TRUE))
}



# Ordem dos modelos
order_fixed <- c(3, 0, 3)

# Modelos selecionados
rmse_cv_aic <- cv_sarimax(
  ts_data       = serie_bc,
  order         = order_fixed,
  seasonal_order= c(3, 1, 3),
  xreg_full     = dummy
)

rmse_cv_rmse <- cv_sarimax(
  ts_data       = serie_bc,
  order         = order_fixed,
  seasonal_order= c(2, 1, 3),
  xreg_full     = dummy
)

rmse_cv_mae <- cv_sarimax(
  ts_data       = serie_bc,
  order         = order_fixed,
  seasonal_order= c(2, 1, 3),
  xreg_full     = dummy
)

# Resultados comparativos
cv_sarimax_result <- data.frame(
  Modelo       = c("AIC", "RMSE", "MAE"),
  SARIMA       = c("SARIMAX(3,0,3)(3,1,3)[12]", 
                   "SARIMAX(3,0,3)(2,1,3)[12]",
                   "SARIMAX(3,0,3)(2,1,3)[12]"),
  RMSE_CV_1_ahead = c(rmse_cv_aic, rmse_cv_rmse, rmse_cv_mae)
)

print(cv_sarimax_result, row.names = FALSE)


rmse_cv_aic