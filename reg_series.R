## Regressão

# 
# # 0) Instalar e carregar pacotes -------------------------------------------
# install.packages(c(
#   "httr","jsonlite","forecast","Metrics",
#   "ggplot2","zoo","nortest"
# ), repos="https://cloud.r-project.org")
library(httr);    library(jsonlite)
library(forecast);library(Metrics)
library(ggplot2); library(zoo)
library(nortest)  # para teste de normalidade
library(lubridate)

# 1) Coleta e preparação dos dados -----------------------------------------
url  <- "https://api.bcb.gov.br/dados/serie/bcdata.sgs.1403/dados?formato=json"
resp <- GET(url)
# stop_if_not(status_code(resp)==200, "Erro ao baixar dados")

df <- fromJSON(content(resp, "text", encoding="UTF-8"))
df$data  <- as.Date(df$data, "%d/%m/%Y")
df$valor <- as.numeric(df$valor)
df <- df[order(df$data), ]

# 2) Feature engineering ---------------------------------------------------
df$mes              <- as.integer(format(df$data, "%m"))
df$ano              <- as.integer(format(df$data, "%Y"))
df$ordem_temporal   <- seq_len(nrow(df))

num_lags <- 3
for(i in 1:num_lags) {
  df[[paste0("lag_",i)]] <- dplyr::lag(df$valor, i)
}
df <- na.omit(df)

# 3) Split treino / teste (80% / 20%) --------------------------------------
n_total <- nrow(df)
n_train <- floor(0.8 * n_total)
train_df <- df[1:n_train, ]
test_df  <- df[(n_train+1):n_total, ]

# 4) Ajuste do modelo Linear -----------------------------------------------
fórmula <- as.formula(paste(
  "valor ~ mes + ano + ordem_temporal +",
  paste(paste0("lag_",1:num_lags), collapse=" + ")
))
model <- lm(fórmula, data = train_df)

# 5) Previsões e métricas --------------------------------------------------
train_df$pred <- predict(model, newdata = train_df)
test_df$pred  <- predict(model, newdata = test_df)

# Métricas Treino
mse_train  <- mse(train_df$valor, train_df$pred)
rmse_train <- rmse(train_df$valor, train_df$pred)

# Métricas Teste
mse_test   <- mse(test_df$valor,  test_df$pred)
rmse_test  <- rmse(test_df$valor,  test_df$pred)
mae_test   <- mae(test_df$valor,  test_df$pred)

cat(sprintf(
  "TREINO  → MSE = %.4f, RMSE = %.4f\n", mse_train, rmse_train
))
cat(sprintf(
  "TESTE   → MSE = %.4f, RMSE = %.4f, MAE = %.4f\n\n",
  mse_test, rmse_test, mae_test
))

# 6) Gráfico histórico vs. predito -----------------------------------------
ggplot() +
  geom_line(aes(data, valor),    data = train_df, color = "black") +
  geom_line(aes(data, pred),     data = train_df, color = "blue", linetype="dashed") +
  geom_line(aes(data, valor),    data = test_df,  color = "black") +
  geom_line(aes(data, pred),     data = test_df,  color = "red",  linetype="dashed") +
  labs(
    title = "Regressão Linear com Lags (p=3)",
    x = "Data", y = "Valor"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=45, hjust=1))

# 7) Previsão iterativa para os próximos 12 meses ---------------------------
future_steps <- 12
last_date    <- max(df$data)

# Data frame vazio para previsões
future_df <- data.frame(
  data            = seq(
    from = as.Date(last_date %m+% months(1)),
    by   = "1 month",
    length.out = future_steps
  ),
  mes             = NA_integer_,
  ano             = NA_integer_,
  ordem_temporal  = NA_integer_
)

# preencher month/year/ordem
future_df$mes            <- as.integer(format(future_df$data, "%m"))
future_df$ano            <- as.integer(format(future_df$data, "%Y"))
future_df$ordem_temporal <- seq_len(nrow(df) + future_steps)[(nrow(df)+1):(nrow(df)+future_steps)]

# inicializar lags e previsões
for(i in 1:num_lags) future_df[[paste0("lag_",i)]] <- NA_real_
future_preds <- numeric(future_steps)
hist_vals    <- df$valor[(n_total-num_lags+1):n_total]

for(i in seq_len(future_steps)) {
  # preencher lags
  for(l in 1:num_lags) {
    if (i <= l) {
      future_df[i, paste0("lag_",l)] <- hist_vals[num_lags - l + i]
    } else {
      future_df[i, paste0("lag_",l)] <- future_preds[i-l]
    }
  }
  # predizer
  future_preds[i] <- predict(model, newdata = future_df[i, ])
}

future_df$valor_previsto <- future_preds

# exibir tabela
print(future_df[, c("data","valor_previsto")], row.names=FALSE)

# gráfico com histórico + previsão
ggplot() +
  geom_line(aes(data, valor), data = df,  color="black") +
  geom_line(aes(data, valor_previsto),
            data = future_df,
            color="red", linetype="dashed") +
  labs(
    title = "Previsão Futura (12 meses)",
    x = "Data", y = "Valor"
  ) +
  theme_minimal()

# 8) Análise de resíduos ----------------------------------------------------
residuos <- test_df$valor - test_df$pred

# 8.1 Distribuição + densidade
ggplot(data.frame(res=residuos), aes(x=res)) +
  geom_histogram(aes(y=..density..),
                 bins=15, fill="skyblue", color="black") +
  geom_density(linetype="dashed") +
  labs(
    title="Distribuição dos Resíduos",
    x="Resíduo", y="Densidade"
  ) +
  theme_minimal()

# 8.2 Q-Q plot
qqnorm(residuos); qqline(residuos, col="red", lwd=2)

# 8.3 FAC
acf(residuos, lag.max=20, main="FAC dos Resíduos")

# 8.4 Teste de normalidade (Anderson–Darling)
ad_test <- ad.test(residuos)
cat(sprintf(
  "\nAnderson–Darling normality: A = %.4f, p-value = %.4f → %s\n",
  ad_test$statistic, ad_test$p.value,
  if(ad_test$p.value>0.05) "normais" else "não-normais"
))

# 8.5 Teste de Ljung–Box (autocorrelação)
lb <- Box.test(residuos, lag=10, type="Ljung-Box")
cat(sprintf(
  "Ljung-Box: statistic = %.4f, p-value = %.4f → %s\n",
  lb$statistic, lb$p.value,
  if(lb$p.value>0.05) "independentes" else "autocorrelacionados"
))

# 9) Validação cruzada temporal (one-step) ---------------------------------
# usaremos só os lags, porque tsCV não dispõe das colunas mes/ano diretamente
ts_data <- ts(df$valor,
              start = c(as.numeric(format(min(df$data),"%Y")),
                        as.numeric(format(min(df$data),"%m"))),
              frequency = 12)
library(forecast)
library(forecast)

# número de lags que você usou no modelo
num_lags <- 3

fc_lm <- function(y, h) {
  # se não tiver dados suficientes, devolve h NAs
  if (length(y) <= num_lags) {
    return(structure(list(mean = rep(NA_real_, h)), class="forecast"))
  }
  
  # 1) monta a matriz y_t ~ lags
  M     <- embed(y, num_lags + 1)
  y_dep <- M[,1]
  Xlags <- as.data.frame(M[,-1,drop=FALSE])
  colnames(Xlags) <- paste0("lag_", 1:num_lags)
  
  # 2) ajusta o lm em cada janela
  fit_cv <- lm(y_dep ~ ., data = Xlags)
  
  # 3) prepara os lags para o ponto de previsão
  last_vals <- tail(y, num_lags)
  newdf     <- as.data.frame(t(last_vals))
  colnames(newdf) <- paste0("lag_", 1:num_lags)
  
  # 4) faz a predição de h passos — como é regressão pura, 
  #    ele só tem um valor, então repetimos para preencher h
  one_pred <- predict(fit_cv, newdata = newdf)
  preds    <- rep(one_pred, length.out = h)
  
  # 5) empacota num objeto com $mean para tsCV
  out <- list(mean = preds)
  class(out) <- "forecast"
  return(out)
}

# teste em one-step (h = 1)
cv_err1  <- tsCV(as.numeric(ts_data), fc_lm, h = 1)
mae1     <- mean(abs(cv_err1), na.rm=TRUE)
mse1     <- mean(cv_err1^2, na.rm=TRUE)
rmse1    <- sqrt(mse1)
cat(sprintf("One-step CV → MAE = %.4f, MSE = %.4f, RMSE = %.4f\n",
            mae1, mse1, rmse1))

# teste em horizon = 12 (h = 12)
cv_err12 <- tsCV(as.numeric(ts_data), fc_lm, h = 12)
mae12    <- mean(abs(cv_err12), na.rm=TRUE)
mse12    <- mean(cv_err12^2, na.rm=TRUE)
rmse12   <- sqrt(mse12)
cat(sprintf("Horizon-12 CV → MAE = %.4f, MSE = %.4f, RMSE = %.4f\n",
            mae12, mse12, rmse12))
