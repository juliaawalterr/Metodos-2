library(tidyverse)
library(dplyr)
library(knitr)
amostra <- read.csv("amostra_221008991.csv")

tabela_regiao <- amostra %>%
  count(REGIAO) %>%
  rename(Frequência = n) %>%
  mutate(Percentual = Frequência / sum(Frequência) * 100)

tabela_dependencia_adm <- amostra %>%
  count(DEPENDENCIA_ADM) %>%
  rename(Frequência = n) %>%
  mutate(Percentual = Frequência / sum(Frequência) * 100)

tabela_raca_cor <- amostra %>%
  count(RACA_COR) %>%
  rename(Frequência = n) %>%
  mutate(Percentual = Frequência / sum(Frequência) * 100)

tabela_computador <- amostra %>%
  count(COMPUTADOR) %>%
  rename(Frequência = n) %>%
  mutate(Percentual = Frequência / sum(Frequência) * 100)

tabela_afazeres_dom <- amostra %>%
  count(AFAZERES_DOM) %>%
  rename(Frequência = n) %>%
  mutate(Percentual = Frequência / sum(Frequência) * 100)

kable(tabela_regiao, caption = "Distribuição por Região")
kable(tabela_dependencia_adm, caption = "Distribuição por Dependência Administrativa")
kable(tabela_raca_cor, caption = "Distribuição por Raça/Cor")
kable(tabela_computador, caption = "Possui Computador")
kable(tabela_afazeres_dom, caption = "Responsável por Afazeres Domésticos")


library(ggplot2)
ggplot(amostra, aes(x = REGIAO)) + geom_bar() + ggtitle("Distribuição por Região")
ggplot(amostra, aes(x = DEPENDENCIA_ADM)) + geom_bar() + ggtitle("Distribuição por Dependência Administrativa")
ggplot(amostra, aes(x = RACA_COR)) + geom_bar() + ggtitle("Distribuição por Raça/Cor")
ggplot(amostra, aes(x = COMPUTADOR)) + geom_bar() + ggtitle("Possui Computador")
ggplot(amostra, aes(x = AFAZERES_DOM)) + 
  geom_bar() + 
  ggtitle("Responsável por Afazeres Domésticos") +
  theme(axis.text.x = element_text(size = 6))




nota_LP_classes <- cut(amostra$NOTA_LP, breaks = seq(min(amostra$NOTA_LP, na.rm = TRUE), max(amostra$NOTA_LP, na.rm = TRUE), by = 50), include.lowest = TRUE)
nota_MT_classes <- cut(amostra$NOTA_MT, breaks = seq(min(amostra$NOTA_MT, na.rm = TRUE), max(amostra$NOTA_MT, na.rm = TRUE), by = 50), include.lowest = TRUE)

tabela_freq_LP <- table(nota_LP_classes)
tabela_freq_MT <- table(nota_MT_classes)

print(tabela_freq_LP)
print(tabela_freq_MT)



ggplot(amostra, aes(x = NOTA_LP)) +
  geom_histogram(binwidth = 50, color = "black", fill = "blue") +
  labs(title = "Histograma das Notas de Língua Portuguesa", x = "Nota LP", y = "Frequência")

ggplot(amostra, aes(x = NOTA_MT)) +
  geom_histogram(binwidth = 50, color = "black", fill = "green") +
  labs(title = "Histograma das Notas de Matemática", x = "Nota MT", y = "Frequência")




library(moments)

estatisticas <- function(x) {
  list(
    Media = mean(x, na.rm = TRUE),
    Mediana = median(x, na.rm = TRUE),
    Moda = as.numeric(names(sort(table(x), decreasing = TRUE)[1])),
    Desvio_Padrao = sd(x, na.rm = TRUE),
    Variancia = var(x, na.rm = TRUE),
    Assimetria = skewness(x, na.rm = TRUE),
    Curtose = kurtosis(x, na.rm = TRUE)
  )
}

estatisticas_LP <- estatisticas(amostra$NOTA_LP)
estatisticas_MT <- estatisticas(amostra$NOTA_MT)

print(estatisticas_LP)
print(estatisticas_MT)


ggplot(amostra, aes(y = NOTA_LP)) +
  geom_boxplot(fill = "blue") +
  labs(title = "Box-plot das Notas de Língua Portuguesa", y = "Nota LP")

ggplot(amostra, aes(y = NOTA_MT)) +
  geom_boxplot(fill = "green") +
  labs(title = "Box-plot das Notas de Matemática", y = "Nota MT")




















