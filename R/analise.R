
##

rm(list=ls())
load("data-raw/dados.RData")

library(tidyverse)

## remover NA intangivel e remover ano 2017

sele <- dados %>%
  filter(is.na(`1.02.04 Intangível`)) 

dados <- dados %>%
  filter(!(name.company %in% sele$name.company)) %>%
  filter(ano!=2017)

# Remove empresas sem alguma informaccao no periodo 10-16
sele <- dados %>%
  count(name.company) %>%
  filter(n<6)

dados <- dados %>%
  filter(!(name.company %in% sele$name.company)) 

# remove empresas sem setores

dados <- dados %>%
  filter(!is.na(setor)) 



## intangibilidade dos ativos e rentabilidade dos ativos

dados <- dados %>%
  mutate(intang_ativo = `1.02.04 Intangível`/`1 Ativo Total`) %>%
  mutate(rent_ativo =  `3.11 Lucro/Prejuízo do Período`/`1 Ativo Total`) %>%
  mutate(setor = as.factor(setor))

# remove empresas com intag_ativo >1
sele <- dados %>%
  filter(intang_ativo>1) 
dados <- dados %>%
  filter(!(name.company %in% sele$name.company)) 



ggplot(data = dados) + 
  geom_point(mapping = aes(x = intang_ativo, 
                           y = rent_ativo, 
                           color = setor)) +
  xlim(c(0, 1)) + ylim(c(-1, 1))

ggplot(data = dados) + 
  geom_point(mapping = aes(x = intang_ativo, 
                           y = rent_ativo)) +
  xlim(c(0, 1)) + ylim(c(-1, 1)) + 
  facet_wrap( ~ segmento)



ggplot(data = dados) +
  geom_histogram(mapping = aes(x = intang_ativo), binwidth = 0.2)

ggplot(data = dados, mapping = aes(x = intang_ativo, color=setor)) +
  geom_freqpoly(binwidth = 0.1)


ggplot(data = dados, mapping = aes(x = setor, y = intang_ativo)) +
  geom_boxplot() 

ggplot(data = dados, mapping = aes(x = segmento, y = intang_ativo)) +
  geom_boxplot() 

ggplot(data = dados) +
  geom_boxplot(mapping = aes(x = reorder(setor, intang_ativo, FUN = median), y = intang_ativo)) +
  coord_flip()

dados <-dados %>%
  mutate( segmento = as.factor(segmento))

ggplot(data = dados) +
  geom_boxplot(mapping = aes(x = reorder(segmento, intang_ativo, FUN = median), y = intang_ativo)) +
  coord_flip()

teste <- group_by(dados, segmento)

summarise(teste, count = n(),
          patentes = sum(patentes))

teste <- dados %>%
  count(setor) 

sum(teste[,2])
