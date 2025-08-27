### Suavização exponencial
# Instale estes pacotes se ainda não tiver
# install.packages(c("httr", "jsonlite", "forecast", "ggplot2", "tseries"))

library(httr)
library(jsonlite)
library(forecast)
library(ggplot2)
library(tseries)


# 2.1. Baixar JSON da API
url <- "https://api.bcb.gov.br/dados/serie/bcdata.sgs.1403/dados?formato=json"
resp <- GET(url)

if (status_code(resp) != 200) {
  stop("Erro ao acessar a API: ", status_code(resp))
}

# 2.2. Converter em data.frame
df <- fromJSON(content(resp, "text", encoding="UTF-8"))
df$data  <- as.Date(df$data, format="%d/%m/%Y")
df$valor <- as.numeric(df$valor)

# 2.3. Ordenar e converter em série temporal mensal
df <- df[order(df$data), ]
serie_ts <- ts(df$valor, start=c(as.numeric(format(min(df$data), "%Y")),
                                 as.numeric(format(min(df$data), "%m"))),
               frequency=12)



# 3.1. Ajuste (tendência e sazonalidade aditivas)
hw_model <- HoltWinters(serie_ts,
                        seasonal="add",
                        beta=TRUE,   # calcula componente de tendência
                        gamma=TRUE)  # calcula componente sazonal

# 3.2. Parâmetros ajustados
cat("Alpha (nível):",  hw_model$alpha, "\n")
cat("Beta (tendência):", hw_model$beta, "\n")
cat("Gamma (sazonalidade):", hw_model$gamma, "\n")



# 4.1. Previsão para os próximos 12 meses
h <- 12
hw_fc <- forecast(hw_model, h=h)

# 4.2. Gráfico histórico + previsão
autoplot(hw_fc) +
  autolayer(serie_ts, series="Histórico") +
  labs(title="Previsão Holt–Winters (12 meses)",
       x="Ano", y="Consumo (GWh)") +
  theme_minimal()



# 6.1. Definir função de previsão para tsCV
hw_forecast <- function(y, h) {
  forecast(HoltWinters(y, seasonal="add"), h=h)
}

# 6.2. Cálculo dos erros de previsão one-step-ahead
#    Aqui usamos h=1 para erros de um passo à frente
cv_errors <- tsCV(serie_ts, hw_forecast, h=1)

# 6.3. Métricas de erro
mae_cv  <- mean(abs(cv_errors), na.rm=TRUE)
mse_cv  <- mean(cv_errors^2, na.rm=TRUE)
cat(sprintf("MAE CV one-step: %.4f\nMSE CV one-step: %.4f\n", mae_cv, mse_cv))

# 6.4. Plot dos erros de CV
autoplot(cv_errors) +
  labs(title="Erros de One-Step-Ahead via tsCV",
       x="Ano", y="Erro") +
  theme_minimal()




