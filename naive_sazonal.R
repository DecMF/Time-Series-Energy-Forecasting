## Naive Sazonal


library(httr)
library(jsonlite)
library(forecast)
library(ggplot2)
library(tseries)



url <- "https://api.bcb.gov.br/dados/serie/bcdata.sgs.1403/dados?formato=json"
resp <- GET(url)
if (status_code(resp) != 200) stop("Erro API: ", status_code(resp))

df <- fromJSON(content(resp, "text", encoding="UTF-8"))
df$data  <- as.Date(df$data, "%d/%m/%Y")
df$valor <- as.numeric(df$valor)

# Ordenar e transformar em série mensal
df <- df[order(df$data), ]
ts_data <- ts(df$valor,
              start=c(as.numeric(format(min(df$data),"%Y")),
                      as.numeric(format(min(df$data),"%m"))),
              frequency=12)





h <- 12                      # horizonte de teste = 12 meses
n <- length(ts_data)
train <- window(ts_data, end = c(time(ts_data)[n-h]))
test  <- window(ts_data, start = c(time(ts_data)[n-h+1]))





# Previsão no conjunto de treino
naive_fc <- snaive(train, h = h)

# Série de previsão
fc_values <- naive_fc$mean


# Métrica de avaliação
mae <- mean(abs(test - fc_values))
cat(sprintf("MAE (h=12): %.4f\n", mae))

# Plot histórico + previsão
autoplot(window(ts_data, end=time(test)[1]-1/12)) +
  autolayer(test,  series="Observado (test)",   color="black") +
  autolayer(fc_values, series="Previsão Naive Sazonal", linetype="dashed") +
  labs(title="Naive Sazonal – Consumo de Eletricidade (Brasil Residencial)",
       x="Ano", y="GWh") +
  theme_minimal()



res_test <- test - fc_values

# Série temporal dos resíduos
autoplot(res_test) +
  ggtitle("Resíduos da Previsão Naive Sazonal") +
  theme_minimal()

# Histograma dos resíduos
ggplot(data.frame(res=res_test), aes(x=res)) +
  geom_histogram(aes(y=..density..), bins=15, fill="steelblue", alpha=0.7) +
  geom_density(linetype="dashed") +
  labs(title="Distribuição dos Resíduos") +
  theme_minimal()

# FAC e FACP
ggAcf(res_test, lag.max=24) + ggtitle("FAC dos Resíduos")
ggPacf(res_test, lag.max=24) + ggtitle("FACP dos Resíduos")



# # Função de previsão para tsCV
# naive_fun <- function(y, h) {
#   forecast::snaive(y, h = h)$mean
# }
library(forecast)

# Função de previsão (passamos directa)
ff      <- snaive
h1      <- 1
h12     <- 12

# 1) Erros one-step-ahead
cv_err1 <- tsCV(ts_data, forecastfunction = ff, h = h1)

# Métricas
mse1   <- mean(cv_err1^2,   na.rm=TRUE)
rmse1  <- sqrt(mse1)
mae1   <- mean(abs(cv_err1),na.rm=TRUE)

cat(sprintf("One-step CV:\n  MAE  = %.4f\n  MSE  = %.4f\n  RMSE = %.4f\n\n",
            mae1, mse1, rmse1))

# 2) Erros com horizonte sazonal (h = 12)
cv_err12 <- tsCV(ts_data, forecastfunction = ff, h = h12)

mse12   <- mean(cv_err12^2,   na.rm=TRUE)
rmse12  <- sqrt(mse12)
mae12   <- mean(abs(cv_err12),na.rm=TRUE)

cat(sprintf("Horizon = 12 CV:\n  MAE  = %.4f\n  MSE  = %.4f\n  RMSE = %.4f\n",
            mae12, mse12, rmse12))