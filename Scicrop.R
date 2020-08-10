getwd()
setwd("Diretório")
list.files()

safra_2020 <- read.csv("Safra_2020.csv")
## Lendo o arquivo csv e armazenando-o na variável safra
safra <- read.csv("Safra_2018-2019.csv")
## Verificando os nomes dos preditores
names(safra)
str(safra)

## Irei utilizar o processo de 'One-hot' para realizar a modelagem.
## Primeiro irei renomear alguns valores dos preditores 'Categoria_
## Pesticida' e  'Temporada'para assim poder aplicar a função 'dummyVars'.
safra$Categoria_Pesticida[safra$Categoria_Pesticida==1] <- 'Nunca_usou'
safra$Categoria_Pesticida[safra$Categoria_Pesticida==2] <- 'Ja_usou'
safra$Categoria_Pesticida[safra$Categoria_Pesticida==3] <- 'Esta_usando'
safra$Temporada[safra$Temporada==1] <- 'Primeira_Temp'
safra$Temporada[safra$Temporada==2] <- 'Segunda_Temp'
safra$Temporada[safra$Temporada==3] <- 'Terceira_Temp'
safra$dano_na_plantacao[safra$dano_na_plantacao == 0] <- 'Sem_dano'
safra$dano_na_plantacao[safra$dano_na_plantacao == 1] <- 'Danos_outros'
safra$dano_na_plantacao[safra$dano_na_plantacao == 2] <- 'Danos_pesticidas'

## Ordenando os leveis da variável dependente e de variáveis
## independentes relevantes
safra$dano_na_plantacao <- factor(safra$dano_na_plantacao,
                                  levels = c('Sem_dano','Danos_outros','Danos_pesticidas'),
                                  ordered = T)
safra$Tipo_de_Cultivo <- factor(safra$Tipo_de_Cultivo, levels = c("0","1"), ordered = T)
safra$Tipo_de_Solo <- factor(safra$Tipo_de_Solo, levels = c("0","1"), ordered = T)

## Criando novas colunas binárias (Obs.: Repare que para evitar colinearidade
## perfeita não foi adicionado todos os fatores da variáveis, sendo omitido a
## o preditor base)
require(tidyr)
require(dplyr)
safra <- safra %>% mutate(value = 1)  %>% spread(Categoria_Pesticida, value,  fill = 0 )
safra <- safra %>% mutate(value = 1)  %>% spread(Temporada, value,  fill = 0 )
## Omitindo preditores de variáveis dammies
safra <- safra %>% select(X:Terceira_Temp, -c(Terceira_Temp, Esta_usando))


## Dividindo a base de dados entre conjuntos de treino e de teste
## Amostra aleatória 
Tam_amostra = 0.60*nrow(safra)
set.seed(100)
index = sample(seq_len(nrow(safra)), size = Tam_amostra)
#Creating training and test set 
safra_train = safra[index,]
safra_test = safra[-index,]

## Como a nossa variável dependente é categórica ordenada,
## devemos utilizar o modelo de regressão logístico. Para isso
## vamos carregar algumas bibliotecas
require(foreign)
require(ggplot2)
require(MASS)
require(Hmisc)
require(reshape2)

## Nós vamos utilizar a função polr (proportional odds logistic regression)
## para realizar nossas estimativas.
## Realizando nosso modelo Logit e guardando em 'logit.fit'. Colocamos 'Hess = T'
## para retornar uma matriz hessiana que será essencial para utilizar a funcao 'summary'
## ou então calcular a matriz de variancia e covariancia do nosso modelo
logit.fit <- polr(dano_na_plantacao ~ Estimativa_de_Insetos + Ja_usou + Tipo_de_Solo + 
                        Doses_Semana,
                  data = safra_train, Hess = T)
summary(logit.fit)

## O output acima nos fornece os coeficientes e os interceptos, em nosso caso dois,
## da nossa regressão, porém não nos fornece o p-valor. Para conseguirmos o p-valor
## vamos realizar algumas operações primeiro

## Guardando a tabela (Obs.: Coeficientes interpretados como chances proporcionais)
(ctable <- coef(summary(logit.fit)))
## Calculando o p-valor e guardando em 'p_valor'
p_valor <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
## Combinando as tabelas
(ctable <- cbind(ctable, "p value" = p_valor))

## Calculando a tabela de confusão e erro de classificação incorreta.
predict_dano = predict(logit.fit, newdata = safra_test)
table(atual = safra_test$dano_na_plantacao, predito = predict_dano)
mean(as.character(safra_test$dano_na_plantacao) != as.character(predictdano))

## Realizando o plote de alguns efeitos 
library("effects")
Effect(focal.predictors = "Estimativa_de_Insetos", logit.fit)
plot(Effect(focal.predictors = "Ja_usou", logit.fit))
plot(Effect(focal.predictors = c("Estimativa_de_Insetos", "Ja_usou"), logit.fit))







