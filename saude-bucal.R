library(tidyverse)
library(stringr)
library(sqldf)
library(magrittr)


saude_bucal_escovacoes <- read.csv("vw_saude_bucal_escovacoes.csv", header = T, sep = ";", row.names = NULL)

saude_bucal_escovacoes <- saude_bucal_escovacoes %>%
  separate(data_registro, c('data', 'hora'), " ") 

saude_bucal_escovacoes$ibge <- as.factor(saude_bucal_escovacoes$ibge)

saude_bucal_escovacoes %<>%
  mutate(data = as.Date(data, format = "%Y-%m-%d"))

str(saude_bucal_escovacoes_faixa_etaria)


saude_bucal_escovacoes_faixa_etaria <- read.csv("vw_saude_bucal_escovacoes_faixa_etaria.csv", header = T, sep = ";", row.names = NULL)
saude_bucal_escovacoes_faixa_etaria

saude_bucal_escovacoes_faixa_etaria <- cbind(index = rownames(saude_bucal_escovacoes_faixa_etaria), saude_bucal_escovacoes_faixa_etaria)
rownames(saude_bucal_escovacoes_faixa_etaria) <- 1:nrow(saude_bucal_escovacoes_faixa_etaria)
saude_bucal_escovacoes_faixa_etaria$ibge <- as.factor(saude_bucal_escovacoes_faixa_etaria$ibge)



tempo <- read.csv("vw_tempo.csv", header = T, sep = ";", row.names = NULL)
tempo


## -----------------------------------------------------------------------------
# bloco escovações supervisionadas
esc_sup <- nrow(saude_bucal_escovacoes)
esc_sup


## -----------------------------------------------------------------------------
# média de participantes
media_part <- saude_bucal_escovacoes %>%
  summarise_at("numero_participantes", mean, na.rm = T)
media_part <- as.integer(media_part)
media_part


## -----------------------------------------------------------------------------
# Equipes realizando escovações
n_equipes <- saude_bucal_escovacoes %>%
  distinct(ine) %>%
  nrow()
n_equipes


## -----------------------------------------------------------------------------
# Escovações supervisionadas
  ## set the levels in order we want
saude_bucal_escovacoes <- within(saude_bucal_escovacoes, 
                   nome_unidade_saude <- factor(nome_unidade_saude, 
                                      levels=names(sort(table(nome_unidade_saude), 
                                                        decreasing=TRUE))))

  ## plot
ggplot(saude_bucal_escovacoes,aes(x=nome_unidade_saude)) +
  geom_bar(fill = "#00B200") +
  scale_y_log10() +
  geom_text(aes(label = ..count..), stat = "count", vjust = -0.3, colour = "black") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
  


## -----------------------------------------------------------------------------
# Distribuição por idade e sexo dos cidadãos participantes

ggplot(saude_bucal_escovacoes_faixa_etaria, aes(nome_faixa_etaria, fill = nome_sexo)) +
  geom_bar(data = subset(saude_bucal_escovacoes_faixa_etaria, nome_sexo == "Masculino")) +
  geom_bar(data = subset(saude_bucal_escovacoes_faixa_etaria, nome_sexo == "Feminino"),aes(y=..count..*(-1))) +
  scale_y_continuous() + 
  coord_flip()


str(saude_bucal_escovacoes_faixa_etaria)


## -----------------------------------------------------------------------------
# filtro IBGE
#
#

# lista de ibges
cidades <- levels(saude_bucal_escovacoes$ibge)
cidades[1]


tb_filtro_ibge <- saude_bucal_escovacoes %>%
  filter(ibge == cidades[2])


## plot1
ggplot(tb_filtro_ibge,aes(x=nome_unidade_saude)) +
  geom_bar(fill = "#00B200") +
  scale_y_log10() +
  geom_text(aes(label = ..count..), stat = "count", vjust = -0.3, colour = "black") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


## plot2
str(saude_bucal_escovacoes_faixa_etaria)

cidades <- levels(saude_bucal_escovacoes_faixa_etaria$ibge)
cidades[1]

tb2_filtro_ibge <- saude_bucal_escovacoes_faixa_etaria %>%
  filter(ibge == cidades[2])

ggplot(tb2_filtro_ibge, aes(nome_faixa_etaria, fill = nome_sexo)) +
  geom_bar(data = subset(saude_bucal_escovacoes_faixa_etaria, nome_sexo == "Masculino")) +
  geom_bar(data = subset(saude_bucal_escovacoes_faixa_etaria, nome_sexo == "Feminino"),aes(y=..count..*(-1))) +
  scale_y_continuous() + 
  coord_flip()


## -----------------------------------------------------------------------------
# filtro datas 
#
#

str(saude_bucal_escovacoes_faixa_etaria)

cities <- levels(saude_bucal_escovacoes$ibge)


tb_filtro_ibge <- saude_bucal_escovacoes %>%
  filter(ibge == cidades[2])



data_min <- min(saude_bucal_escovacoes$data)
data_max <- max(saude_bucal_escovacoes$data)

intervalo <- interval(data_min, data_max)
intervalo

date <- as.Date("2021-01-01")
date

data_min <- as.Date("2019-01-01")
data_max <- as.Date("2019-03-01")

date %within% intervalo

tb_filtro_ibge <- saude_bucal_escovacoes %>%
  filter((ibge == cidades[2]) & (data >= data_min) & (data <= data_max))
tb_filtro_ibge

# 
UBS <- unique(saude_bucal_escovacoes$nome_unidade_saude)
UBS


