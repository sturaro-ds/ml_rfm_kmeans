setwd("C:\\STURARO\\2_CURSOS\\DSA\\BigDataRAzure\\Cap06")
getwd()

# imports
library(tidyverse)
library(dplyr)
library(ggplot2)
library(caret)
library(plotly)
library(readxl)
library(rfm)
library(stats)
library(factoextra)

# funcao para carregar os dados das sheets da planilha Excel
carrega_dados <- function(){
  setwd("C:\\STURARO\\2_CURSOS\\DSA\\BigDataRAzure\\Cap06")
  sheet1 <- read_excel("online_retail_II.xlsx", sheet = "Year 2009-2010")
  sheet2 <- read_excel("online_retail_II.xlsx", sheet = "Year 2010-2011")
  combina_dados <- rbind(sheet1, sheet2)
  return(combina_dados)
}

# executa a funcao para criar um df com as sheets da plan
dados <- carrega_dados()
dim(dados)
str(dados)
summary(dados)
View(dados)

# funcao para verificar valores vazios
verifica_missing <- function(x){
  return(colSums(is.na(x)))
}

# executa a funcao
verifica_missing(dados)

# decisao soobre os valores NA:
# na coluna descricao os valores vazios representam apenas 0.004% do total, logo, irei excluir da analise
# na coluna Customer ID os valores vazios poderiam receber a tecnica "imputamento"? nao, pois sao valores key primarios de cada cliente e nao faz sentido preencher, logo, decidimos excluir

# funcao para limpar e pré-processar os dados

preprocessa_dados <- function(x){
  # criando uma coluna chamada TotalPrice
  x$TotalPrice <- x$Quantity * x$Price
  
  # remove registros com valores ausentes
  x <- na.omit(x)
  
  # removemos as linhas da coluna Invoice que contem a letra C (que significa pedidos cancelados)
  x <- x[!grepl("C", x$Invoice), ]
  
  # retorna a nova dataset
  return(x)
}

dataset <- preprocessa_dados(dados)
dim(dataset)
str(dataset)
View(dataset)

# analise exploratória
# verificando a distribuicao da variável TotalPrice
ggplot(dataset,
       aes(x = TotalPrice)) +
  geom_density(fill="#69b3a2", color="#e9ecef", alpha=3.5) +
  scale_x_continuous(limits = c(0, 100)) +
  labs(title="Distribuicao da Variável TotalPrice")

# numero de clintes
length(dataset$`Customer ID`) # transacoes comerciais
length(unique(dataset$`Customer ID`)) # clientes unicos


# total monetario gasto por cliente
total_gasto <- dataset %>%
  group_by(dataset$`Customer ID`) %>%
  summarise(Sum = sum(dataset$TotalPrice))

View(total_gasto)

# criando uma data customizada (natal de 2011)
# usaremos para calcular a recencia
max(dataset$InvoiceDate)
date1 <- as.Date.character("25/12/2011", "%d/%m/%y")

# funcao para converter as datas do formato POISxt para Date
converte_data <- function(x)
{
  options(digits.secs=3)
  return(as.Date.POSIXct(x$InvoiceDate, "GMT"))
}
dataset$InvoiceDate <- converte_data(dataset)
View(dataset)


# funcao para calcular a Recencia, Frequencia e Valor Monetario
calcula_rfm <- function(x){
  z <- x %>% group_by(`Customer ID`) %>%
    summarise(Recency = as.numeric(date1 - max(InvoiceDate)),
              Frequency = n(),
              Monetary = sum(TotalPrice), 
              primeira_compra = min(InvoiceDate))
  # removendo transacoes com valores acima do 3. Quartil e abaixo do 1. Quartil (removendo outliers)
  Q1 <- quantile(z$Monetary, .25)
  Q3 <- quantile(z$Monetary, .75)
  IQR <- IQR(z$Monetary)
  z <- subset(z, z$Monetary >= (Q1 - 1.5*IQR) & z$Monetary <= (Q3 + 1.5*IQR))
  return(z)
}

# executa a funcao
valores_rfm <- calcula_rfm(dataset)
View(valores_rfm)

# MACHINE LEARNING - CLUSTERIZACAO K-MEANS

# set seed
set.seed(1029)

# funcao para a segmentacao de clientes com base nos valores RFM
segmenta_cliente <- function(rfm)
{
  # cria uma lista
  resultados <- list()
  
  # obtém os valores RFM
  dados_rfm <- select(rfm, c("Recency", "Frequency", "Monetary"))
  
  # cria o modelo
  modelo_kmeans <- kmeans(dados_rfm, centers = 5, iter.max = 50)
  
  # plot do modelo
  resultados$plot <- fviz_cluster(modelo_kmeans,
                                  data = dados_rfm,
                                  geom = c("point"),
                                  ellipse.type = "euclid")
  # organiza os dados
  dados_rfm$"Customer ID" <- rfm$"Customer ID"
  dados_rfm$clusters <- modelo_kmeans$cluster
  resultados$data <- dados_rfm
  
  return(resultados)
}

# executa a funcao
grafico <- segmenta_cliente(valores_rfm)[1]
grafico

tabela_rfm <- segmenta_cliente(valores_rfm)[2]
tabela_rfm <- as.data.frame(tabela_rfm)
class(tabela_rfm)
colnames(tabela_rfm) <- c("Recency", "Frequency", "Monetary", "Customer_ID", "Clusters")
View(tabela_rfm)