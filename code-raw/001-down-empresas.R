##########################
### Download dados brutos empresas da BMF Bovespa B3
##################################

## Carrega pacote GetDFPData alterado para recuperar a DVA

#library(devtools)
#devtools::install_github('santoscsGetDFPData')
library(GetDFPData)
library(tidyverse)


# Empresas listadas
df.info <- gdfpd.get.info.companies(type.data = 'companies', cache.folder = tempdir())

# Exclui empresas do setor financeiro


sele <- df.info %>%
  filter(main.sector!="Financeiro e Outros") %>%
  filter(situation == "ATIVO") %>%
  select(name.company) %>%
  as.matrix()


sele <- sele[,1]
first.date <- '2010-01-01'
last.date  <- '2018-01-01'


df.reports <- gdfpd.GetDFPData(name.companies = sele,
                               first.date = first.date,
                               last.date = last.date,
                               do.cache = FALSE,
                               selected.data = "DFP",
                               cache.folder = "../../../Documents/cache-b3/",
                               folder.out = "../../../Documents/cache-b3/")

save(df.reports, file="data-raw/empresas-b3.RData")

df.info <-  df.info %>%
  filter(situation == "ATIVO") 
  
save(df.info, file="data-raw/info-empresas-b3.RData")

