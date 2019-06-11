knitr::opts_chunk$set(echo = TRUE)

library(readr)
library(magrittr)
library(dplyr)
library(stringr)
library(tidytext)
library(stringr)

avaliacoes_df <- read_csv("workspace/metodologia/reclamacoes-do-gf/data/3-avaliacao-humana/reclamacoes-avaliadas-20190515.csv")
avaliacoes <- avaliacoes_df %>% 
  select(orgao = `orgao`,
         data = `data`, 
         texto = `texto`, 
         insatisfacao = `insatisfacao`, 
         avaliadores = `avaliadores`, 
         range = `range.avaliacoes`)

count_h = str_count(avaliacoes$texto, "\\b[A-Z]{2,}\\b")
count_n = sapply(strsplit(avaliacoes$texto, " "), length)
count_prop = count_h/count_n

avaliacoes_maisculas = c()
avaliacoes_minusculas = c()

for (i in 1:length(count_prop)) {
  if (count_prop[i] > 0.1)
    avaliacoes_maisculas[i] <- avaliacoes$insatisfacao[i]
  else
    avaliacoes_minusculas[i] <- avaliacoes$insatisfacao[i]
}
boxplot(avaliacoes_maisculas, avaliacoes_minusculas, ylab="Insatisfação", names = c("Alta prop. de palavras maiusculas", "Baixa prop. de palavras maiusculas"))

ordered <- avaliacoes[order(str_count(avaliacoes$texto)),]
plot(str_count(ordered$texto), ordered$insatisfacao)

reg2 <- lm(count_h ~ avaliacoes$insatisfacao)
anova(reg2)
summary(reg2)

reg <- lm(str_count(ordered$texto) ~ ordered$insatisfacao)
anova(reg)
summary(reg)



