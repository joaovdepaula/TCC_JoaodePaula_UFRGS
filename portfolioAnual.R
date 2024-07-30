### Chamando as bibliotecas
library(fPortfolio)
library(timeSeries)
library(xts)
library(quantmod)
library(ggplot2)
library(tidyr)
library(lubridate)
library(readxl)
library(writexl)

### Lendo arquivo com cotas dos fundos
cotas_fundos <- read_xlsx("cotas_fundos_final.xlsx")


### Transformando $Date no formato de data e deixando como index
cotas_fundos$data <- as.Date(cotas_fundos$data, format = "%Y-%m-%d")
cotas_datas <- cotas_fundos$data
cotas_fundos <- cotas_fundos[, -c(1)]
rownames(cotas_fundos) <- cotas_datas


### Gerando df de log returns
cotas_returns <- log(tail(cotas_fundos, -1) / head(cotas_fundos, -1))
cotas_return_matrix <- as.timeSeries(cotas_returns)

### Gerando os pesos dos fundos por ano
pesos_por_ano_lista = list()
for (ano in 2014:2023) {
  cotas_ate_x <- data.frame(cotas_fundos)
  cotas_ate_x$data <- cotas_datas
  cotas_ate_x$data <- as.Date(cotas_ate_x$data, format = "%Y-%m-%d")
  data_final <- sprintf("%d%s", ano, "-12-31")
  data_final <- as.Date(data_final, format = "%Y-%m-%d")
  cotas_ate_x <- subset(cotas_ate_x, data <= data_final)
  cotas_ate_x <- subset(cotas_ate_x, select = -data)
  returns_ate_x <- log(tail(cotas_ate_x, -1) / head(cotas_ate_x, -1))
  return_matrix_ate_x <- as.timeSeries(returns_ate_x)

    
  efficiente_portfolio = tangencyPortfolio(
    return_matrix_ate_x, spec = portfolioSpec(), constraints = "LongOnly"
    )
  weights = getWeights(efficiente_portfolio)
  df_temp <- data.frame(Value = weights)
  df_temp <- t(df_temp)
  rownames(df_temp)[1] <- ano
    
  pesos_por_ano_lista <- c(pesos_por_ano_lista, list(df_temp))
}

### Gerando a df dos pesos por ano
add_missing_columns <- function(df) {
  df <- as.data.frame(df)
  missing_columns <- setdiff(names(cotas_fundos), colnames(df))
  if (length(missing_columns) > 0) {
    for (col in missing_columns) {
      df[[col]] <- 0
    }
  }
  return(df)
}

### Organizando os dados da df
pesos_por_ano_lista <- lapply(pesos_por_ano_lista, add_missing_columns)
pesos_por_ano <- do.call(rbind, pesos_por_ano_lista)
colunas_nao_zero <- colnames(pesos_por_ano)[colSums(pesos_por_ano) != 0]
pesos_por_ano <- pesos_por_ano[colunas_nao_zero]
pesos_por_ano <- cbind(anos = rownames(pesos_por_ano), pesos_por_ano)

### Salvando a df dos pesos por ano
write_xlsx(pesos_por_ano, "pesos_por_ano.xlsx")



### grafico pesos por ano
pesos_por_ano_nome_curto = pesos_por_ano
colnames(pesos_por_ano_nome_curto) <- substring(colnames(pesos_por_ano_nome_curto), 1, 15)
pesos_por_ano_long <- pivot_longer(pesos_por_ano_nome_curto, cols = -anos, names_to = "Variable", values_to = "Value")

ggplot(pesos_por_ano_long, aes(x = anos, y = Value, color = Variable, group = Variable)) +
  geom_line(size=1.5) +
  labs(title = "Rebalanceamentos",
       x = "Ano",
       y = "Peso",
       color = "Variable") +
  theme_minimal()



### Printando cotas por ano
fundos_usados <- colnames(pesos_por_ano)
fundos_usados <- gsub("\\.\\.\\.", " - ", fundos_usados)
fundos_usados <- gsub("\\.", " ", fundos_usados)
fundos_usados <- fundos_usados[-1]

cotas_fundos_usados <- cotas_fundos
cotas_fundos_usados <- cotas_fundos_usados[, fundos_usados]
cotas_fundos_usados_normalizado <- sweep(cotas_fundos_usados, 2, unlist(cotas_fundos_usados[1, ]), `/`)
cotas_fundos_usados_normalizado$Date <- cotas_datas

cotas_fundos_usados_normalizado_nome_curto = cotas_fundos_usados_normalizado
colnames(cotas_fundos_usados_normalizado_nome_curto) <- substring(colnames(cotas_fundos_usados_normalizado_nome_curto), 1, 15)
cotas_fundos_usados_normalizado_long <- pivot_longer(cotas_fundos_usados_normalizado_nome_curto, cols = -Date, names_to = "Variable", values_to = "Value")
ggplot(cotas_fundos_usados_normalizado_long, aes(x = Date, y = Value, color = Variable, group = Variable)) +
  geom_line(size=1.5) +
  labs(title = "Retornos",
       x = "Ano",
       y = "Percentual",
       color = "Variable") +
  theme_minimal()



### Cotizando o portfolio
pesos_por_ano <- subset(pesos_por_ano, select = -anos)
colnames(pesos_por_ano) <- gsub("\\.\\.\\.", " - ", colnames(pesos_por_ano))
colnames(pesos_por_ano) <- gsub("\\.", " ", colnames(pesos_por_ano))

PL_INICIAL = 1000000
df_temp <- data.frame(
  PL = PL_INICIAL,
  row.names = as.Date(rownames(cotas_fundos)[1])
)

pl_portfolio = list(df_temp)

ano_anterior = 2014
quant_cotas = 0

for (row in rownames(cotas_fundos)) {
  if (ano_anterior == 2013){
    ano_anterior=2014
  }
  ano = year(row)
  if (ano > ano_anterior){
    pl_passado = pl_portfolio[[length(pl_portfolio)]]$PL
    alocacao_perc <- pesos_por_ano[as.character(ano-1), ]
    alocacao_perc <- alocacao_perc[, alocacao_perc[1,] > 0, drop=FALSE]
    alocacao_valor = alocacao_perc * pl_passado

    preco_cotas = cotas_fundos[row, colnames(alocacao_perc)]
    quant_cotas = alocacao_valor/preco_cotas
  }
  if (any(quant_cotas != 0)){
    preco_cotas = cotas_fundos[row, colnames(quant_cotas)]
    pl = sum(rowSums(preco_cotas*quant_cotas))

    
    df_temp <- data.frame(
      PL = pl,
      row.names = as.Date(row)
    )
    pl_portfolio <- c(pl_portfolio, list(df_temp))
    
  }else {
    df_temp <- data.frame(
      PL = PL_INICIAL,
      row.names = as.Date(row)
    )
    pl_portfolio <- c(pl_portfolio, list(df_temp))
  }
  ano_anterior = ano
}

pl_portfolio <- do.call(rbind, pl_portfolio)

### Comparando o Portfolio com o IBOV
pl_portfolio_2 <- pl_portfolio
pl_portfolio_2$Date <- rownames(pl_portfolio_2)

BOVA11 <- getSymbols("BOVA11.SA", src = "yahoo")
BOVA11 <- BOVA11.SA
BOVA11 = BOVA11$BOVA11.SA.Adjusted
BOVA11 <- BOVA11["2013-12-30/"]
BOVA11 <- as.data.frame(BOVA11)
BOVA11_2 <- BOVA11
BOVA11_2$Date <- rownames(BOVA11_2)

portfolio_e_bova <- merge(pl_portfolio_2, BOVA11_2, by = "Date", all = TRUE)

rownames(portfolio_e_bova) <- portfolio_e_bova$Date
portfolio_e_bova <- subset(portfolio_e_bova, select = -Date)
portfolio_e_bova <- portfolio_e_bova[complete.cases(portfolio_e_bova),]

portfolio_e_bova = tail(portfolio_e_bova, nrow(portfolio_e_bova)-248)
portfolio_e_bova_normalizado <- sweep(portfolio_e_bova, 2, unlist(portfolio_e_bova[1, ]), `/`)

#portfolio_e_bova_normalizado$Date <- rownames(portfolio_e_bova_normalizado)
portfolio_e_bova_normalizado <- cbind(Date = rownames(portfolio_e_bova_normalizado), portfolio_e_bova_normalizado)
portfolio_e_bova_normalizado_long <- pivot_longer(portfolio_e_bova_normalizado, cols = -Date, names_to = "Variable", values_to = "Value")


### Gerando o grafico de comparação com o ibov
ggplot(portfolio_e_bova_normalizado_long, aes(x = Date, y = Value, color = Variable, group = Variable)) +
  geom_line(size=1.5) +
  labs(title = "Retornos",
       x = "Ano",
       y = "Percentual",
       color = "Variable") +
  theme_minimal()

### Salvando o resultado final
write_xlsx(portfolio_e_bova_normalizado, "portfolio_e_bova.xlsx")

