

rm(list=ls())

library(tidyverse)
library(reshape)
library(lubridate)


load("data-raw/cedentes.RData")
load("data-raw/cessionaria.RData")
#load("data-raw/desenhos.RData")
load("data-raw/empresas-b3.RData")
load("data-raw/info-empresas-b3.RData")
#load("data-raw/marcas.RData")
load("data-raw/patentes.RData")
#load("data-raw/programas.RData")

##############
### Organiza e agrega dados da B3 BMF Bovespa
########################



## assets junta ativos

tab1 <- df.reports$fr.assets.consolidated
tab2 <- df.reports$fr.assets
aux <- NULL
for(i in 1:length(tab1)){
  if(dim(tab1[[i]])[1]==0){
    aux <- rbind(aux, tab2[[i]])
  }else{
    aux <- rbind(aux, tab1[[i]]) 
  }
}
ativo <- mutate(aux, acc.nd = paste(acc.number, acc.desc))
ativo <- cast(ativo, name.company + ref.date ~ acc.nd, value = "acc.value")

## liabilities junta passivo
tab1 <- df.reports$fr.liabilities.consolidated
tab2 <- df.reports$fr.liabilities
aux <- NULL
for(i in 1:length(tab1)){
  if(dim(tab1[[i]])[1]==0){
    aux <- rbind(aux, tab2[[i]])
  }else{
    aux <- rbind(aux, tab1[[i]]) 
  }
}
passivo <- mutate(aux, acc.nd = paste(acc.number, acc.desc))
passivo <- cast(passivo, name.company + ref.date ~ acc.nd, value = "acc.value")

## income junta resultado

tab1 <- df.reports$fr.income.consolidated
tab2 <- df.reports$fr.income
aux <- NULL
for(i in 1:length(tab1)){
  if(dim(tab1[[i]])[1]==0){
    aux <- rbind(aux, tab2[[i]])
  }else{
    aux <- rbind(aux, tab1[[i]]) 
  }
}
resultado <- aux %>% 
  filter(nchar(acc.number)<5) %>%
  mutate(acc.nd = paste(acc.number, acc.desc))
resultado <- cast(resultado, name.company + ref.date ~ acc.nd, value = "acc.value")


##  cashflow junta caixa
# tab <- df.reports$fr.cashflow.consolidated
# aux <- NULL
# for(i in 1:length(tab)){
#   aux <- rbind(aux, tab[[i]])
# }
# contas <- c("6.01" ,  "6.02" ,  "6.03" ,  "6.04" ,  "6.05" ,  "6.05.01" , "6.05.02")
# sele <- aux$acc.number %in% contas
# caixa <- mutate(aux[sele, ], acc.nd = paste(acc.number, acc.desc))
# caixa <- cast(caixa, name.company + ref.date ~ acc.nd, value = "acc.value")

# value junta valor agragado (dva)
tab1 <- df.reports$fr.value.consolidated
tab2 <- df.reports$fr.value
aux <- NULL
for(i in 1:length(tab1)){
  if(dim(tab1[[i]])[1]==0){
    aux <- rbind(aux, tab2[[i]])
  }else{
    aux <- rbind(aux, tab1[[i]]) 
  }
}
valor <- mutate(aux, acc.nd = paste(acc.number, acc.desc))
valor <- cast(valor, name.company + ref.date ~ acc.nd, value = "acc.value")


#  junta todos os dados em uma unica base
ativo <- as_tibble(ativo)
passivo <- as_tibble(passivo)
resultado <- as_tibble(resultado)
# caixa <- as_tibble(caixa)
valor <- as_tibble(valor)

df <- left_join(ativo, passivo)
df <- left_join(df, resultado)
# df <- left_join(df, caixa)
dados <- left_join(df, valor)

aux <- tibble(name.company=df.reports$company.name, cnpj=df.reports$cnpj)
dados <- left_join(dados, aux)

dados <- dados %>%
  mutate(ano = year(ymd(ref.date))) %>%
  mutate(constitu=NA) %>%
  mutate(setor=NA) %>%
  mutate(subsetor=NA) %>%
  mutate(estado=NA) %>%
  mutate(segmento=NA)


#df.info <- GetDFPData::gdfpd.get.info.companies(type.data = 'companies', cache.folder = tempdir())

df.info <-  df.info %>%
  filter(main.sector!="Financeiro e Outros") %>%
  filter(situation == "ATIVO") 


for(i in 1:length(df.info$name.company)){
  sele <- dados$name.company %in% df.info$name.company[i]
  dados[sele,"cnpj"] <- rep(gsub(" ", "0", as.character(format(df.info$cnpj[i], digits = 14, width = 14))), sum(sele))
  dados[sele,"constitu"] <- as.character(rep(df.info$date.constitution[i], sum(sele)))
  dados[sele,"setor"] <- rep(df.info$main.sector[i], sum(sele))
  dados[sele,"subsetor"] <- rep(df.info$sub.sector[i], sum(sele))
  dados[sele,"estado"] <- rep(df.info$estate[i], sum(sele))
  dados[sele,"segmento"] <- rep(df.info$segment[i], sum(sele))
}




dados[dados$cnpj=="92781335000102", "constitu"] <- rep("1996-08-11", 8)
dados[dados$cnpj=="85778074000106", "constitu"] <- rep("1996-01-31", 8)
dados[dados$cnpj=="08424178000171", "constitu"] <- rep("1969-12-15", 8)
dados[dados$cnpj=="21314559000166", "constitu"] <- rep("2014-10-30", 4)
dados[dados$cnpj=="84683408000103", "constitu"] <- rep("1966-08-08", 8)
dados[dados$cnpj=="33050071000158", "constitu"] <- rep("1966-08-01", 8)



dados[dados$cnpj=="60851615000153", "constitu"] <- rep("1971-02-15", 8)
dados[dados$cnpj=="17245234000100", "constitu"] <- rep("1966-01-07", 8)

dados[dados$cnpj=="97191902000194", "constitu"] <- rep("1966-08-20", 8)
dados[dados$cnpj=="61695227000193", "constitu"] <- rep("1971-09-08", 8)
dados[dados$cnpj=="83475913000191", "constitu"] <- rep("1977-10-17", 8)
dados[dados$cnpj=="88610191000154", "constitu"] <- rep("1917-09-05", 8)


dados[dados$cnpj=="61067161000197", "constitu"] <- rep("1966-07-08", 8)



################
### Organiza e agreagaos dados do INPI
####################

# tidy data


# patentes


sele <- lapply(patentes, function(x) is.matrix(x))
aux <- lapply(patentes[sele==TRUE], function(x){ year(dmy(x[,2]))})
aux <- lapply(aux, function(x) ungroup(filter(count(group_by(tibble(ano=x), ano)), ano >2009 & ano < 2018)))
aux <- lapply(aux, function(x) as.matrix(x))
cnpj <- names(patentes)
nomes <- cnpj[sele==TRUE]

aux2<-NULL
for(i in 1:length(aux)){
  aux2 <- rbind(aux2 ,cbind(aux[[i]], nomes[i]))
}

colnames(aux2) <- c("ano", "patentes", "cnpj")
aux2  <- as_tibble(aux2)
aux2 <- aux2 %>% 
  mutate(ano = as.numeric(ano)) %>%
  mutate(patentes = as.numeric(patentes))

dados <- left_join(dados, aux2)

# # marcas
# sele <- lapply(marcas, function(x)is.matrix(x))
# aux <- lapply(marcas[sele==TRUE], function(x){ year(dmy(x[,2]))})
# aux <- lapply(aux, function(x) ungroup(filter(count(group_by(tibble(ano=x), ano)), ano >2009 & ano < 2018)))
# aux <- lapply(aux, function(x) as.matrix(x))
# nomes <- df.reports$cnpj[sele==TRUE]
# 
# aux2<-NULL
# for(i in 1:length(aux)){
#   aux2 <- rbind(aux2 ,cbind(aux[[i]], nomes[i]))
# }
# 
# colnames(aux2) <- c("ano", "marcas", "cnpj")
# aux2  <- as_tibble(aux2)
# aux2 <- aux2 %>% 
#   mutate(ano = as.numeric(ano)) %>%
#   mutate(marcas = as.numeric(marcas))
# 
# dados <- left_join(dados, aux2)
# 
# ## desenhos
# 
# sele <- lapply(desenhos, function(x)is.matrix(x))
# aux <- lapply(desenhos[sele==TRUE], function(x){ year(dmy(x[,2]))})
# aux <- lapply(aux, function(x) ungroup(filter(count(group_by(tibble(ano=x), ano)), ano >2009 & ano < 2018)))
# aux <- lapply(aux, function(x) as.matrix(x))
# nomes <- df.reports$cnpj[sele==TRUE]
# 
# aux2<-NULL
# for(i in 1:length(aux)){
#   aux2 <- rbind(aux2 ,cbind(aux[[i]], nomes[i]))
# }
# 
# colnames(aux2) <- c("ano", "desenhos", "cnpj")
# aux2  <- as_tibble(aux2)
# aux2 <- aux2 %>% 
#   mutate(ano = as.numeric(ano)) %>%
#   mutate(desenhos = as.numeric(desenhos))
# 
# dados <- left_join(dados, aux2)
# 
# ## programas
# sele <- lapply(programas, function(x)is.matrix(x))
# aux <- lapply(programas[sele==TRUE], function(x){ year(dmy(x[,2]))})
# aux <- lapply(aux, function(x) ungroup(filter(count(group_by(tibble(ano=x), ano)), ano >2009 & ano < 2018)))
# aux <- lapply(aux, function(x) as.matrix(x))
# nomes <- df.reports$cnpj[sele==TRUE]
# 
# aux2<-NULL
# for(i in 1:length(aux)){
#   aux2 <- rbind(aux2 ,cbind(aux[[i]], nomes[i]))
# }
# 
# colnames(aux2) <- c("ano", "programas", "cnpj")
# aux2  <- as_tibble(aux2)
# aux2 <- aux2 %>% 
#   mutate(ano = as.numeric(ano)) %>%
#   mutate(programas = as.numeric(programas))
# 
# dados <- left_join(dados, aux2)

# cedentes
sele <- lapply(cedentes, function(x)is.matrix(x))
aux <- lapply(cedentes[sele==TRUE], function(x){ year(dmy(x[,2]))})
aux <- lapply(aux, function(x) ungroup(filter(count(group_by(tibble(ano=x), ano)), ano >2009 & ano < 2018)))
aux <- lapply(aux, function(x) as.matrix(x))
cnpj <- names(cedentes)
nomes <- cnpj[sele==TRUE]

aux2<-NULL
for(i in 1:length(aux)){
  aux2 <- rbind(aux2 ,cbind(aux[[i]], nomes[i]))
}

colnames(aux2) <- c("ano", "cedentes", "cnpj")
aux2  <- as_tibble(aux2)
aux2 <- aux2 %>% 
  mutate(ano = as.numeric(ano)) %>%
  mutate(cedentes = as.numeric(cedentes))

dados <- left_join(dados, aux2)

## cessionaria
sele <- lapply(cessionaria, function(x)is.matrix(x))
aux <- lapply(cessionaria[sele==TRUE], function(x){ year(dmy(x[,2]))})
aux <- lapply(aux, function(x) ungroup(filter(count(group_by(tibble(ano=x), ano)), ano >2009 & ano < 2018)))
aux <- lapply(aux, function(x) as.matrix(x))
cnpj <- names(cessionaria)
nomes <- cnpj[sele==TRUE]

aux2<-NULL
for(i in 1:length(aux)){
  aux2 <- rbind(aux2 ,cbind(aux[[i]], nomes[i]))
}

colnames(aux2) <- c("ano", "cessionaria", "cnpj")
aux2  <- as_tibble(aux2)
aux2 <- aux2 %>% 
  mutate(ano = as.numeric(ano)) %>%
  mutate(cessionaria = as.numeric(cessionaria))

dados <- left_join(dados, aux2)

# numero total  patentes

sele <- lapply(patentes, function(x) is.matrix(x))
aux <- lapply(patentes[sele==TRUE], function(x){ year(dmy(x[,2]))})
aux <- lapply(aux, function(x) ungroup(count(group_by(tibble(ano=x), ano))))
aux <- lapply(aux, function(x) as.matrix(x))
aux <- lapply(aux, function(x) {x[,2] <- cumsum(x[,2]) 
return(x) })
cnpj <- names(patentes)
nomes <- cnpj[sele==TRUE]

aux2<-NULL
for(i in 1:length(aux)){
  aux2 <- rbind(aux2 ,cbind(aux[[i]], nomes[i]))
}

colnames(aux2) <- c("ano", "num_patentes", "cnpj")
aux2  <- as_tibble(aux2)
aux2 <- aux2 %>% 
  mutate(ano = as.numeric(ano)) %>%
  mutate(num_patentes = as.numeric(num_patentes))

dados <- left_join(dados, aux2)



# ## numero total de marcas
# 
# sele <- lapply(marcas, function(x) is.matrix(x))
# aux <- lapply(marcas[sele==TRUE], function(x){ year(dmy(x[,2]))})
# aux <- lapply(aux, function(x) ungroup(count(group_by(tibble(ano=x), ano))))
# aux <- lapply(aux, function(x) as.matrix(x))
# aux <- lapply(aux, function(x) {x[,2] <- cumsum(x[,2]) 
# return(x) })
# 
# nomes <- df.reports$cnpj[sele==TRUE]
# 
# aux2<-NULL
# for(i in 1:length(aux)){
#   aux2 <- rbind(aux2 ,cbind(aux[[i]], nomes[i]))
# }
# 
# colnames(aux2) <- c("ano", "num_marcas", "cnpj")
# aux2  <- as_tibble(aux2)
# aux2 <- aux2 %>% 
#   mutate(ano = as.numeric(ano)) %>%
#   mutate(num_marcas = as.numeric(num_marcas))
# 
# dados <- left_join(dados, aux2)
# 
# ## numero total de desenhos
# 
# sele <- lapply(desenhos, function(x) is.matrix(x))
# aux <- lapply(desenhos[sele==TRUE], function(x){ year(dmy(x[,2]))})
# aux <- lapply(aux, function(x) ungroup(count(group_by(tibble(ano=x), ano))))
# aux <- lapply(aux, function(x) as.matrix(x))
# aux <- lapply(aux, function(x) {x[,2] <- cumsum(x[,2]) 
# return(x) })
# 
# nomes <- df.reports$cnpj[sele==TRUE]
# 
# aux2<-NULL
# for(i in 1:length(aux)){
#   aux2 <- rbind(aux2 ,cbind(aux[[i]], nomes[i]))
# }
# 
# colnames(aux2) <- c("ano", "num_desenhos", "cnpj")
# aux2  <- as_tibble(aux2)
# aux2 <- aux2 %>% 
#   mutate(ano = as.numeric(ano)) %>%
#   mutate(num_desenhos = as.numeric(num_desenhos))
# 
# dados <- left_join(dados, aux2)
# 
# ## numero total de programas
# 
# sele <- lapply(programas, function(x) is.matrix(x))
# aux <- lapply(programas[sele==TRUE], function(x){ year(dmy(x[,2]))})
# aux <- lapply(aux, function(x) ungroup(count(group_by(tibble(ano=x), ano))))
# aux <- lapply(aux, function(x) as.matrix(x))
# aux <- lapply(aux, function(x) {x[,2] <- cumsum(x[,2]) 
# return(x) })
# 
# nomes <- df.reports$cnpj[sele==TRUE]
# 
# aux2<-NULL
# for(i in 1:length(aux)){
#   aux2 <- rbind(aux2 ,cbind(aux[[i]], nomes[i]))
# }
# 
# colnames(aux2) <- c("ano", "num_programas", "cnpj")
# aux2  <- as_tibble(aux2)
# aux2 <- aux2 %>% 
#   mutate(ano = as.numeric(ano)) %>%
#   mutate(num_programas = as.numeric(num_programas))
# 
# dados <- left_join(dados, aux2)


## numero total de cedente

sele <- lapply(cedentes, function(x) is.matrix(x))
aux <- lapply(cedentes[sele==TRUE], function(x){ year(dmy(x[,2]))})
aux <- lapply(aux, function(x) ungroup(count(group_by(tibble(ano=x), ano))))
aux <- lapply(aux, function(x) as.matrix(x))
aux <- lapply(aux, function(x) {x[,2] <- cumsum(x[,2]) 
return(x) })

cnpj <- names(cedentes)
nomes <- cnpj[sele==TRUE]

dados <- dados %>%
  mutate(e_cedente = ifelse(cnpj %in% nomes, 1, 0))

aux2<-NULL
for(i in 1:length(aux)){
  aux2 <- rbind(aux2 ,cbind(aux[[i]], nomes[i]))
}

colnames(aux2) <- c("ano", "num_cedentes", "cnpj")
aux2  <- as_tibble(aux2)
aux2 <- aux2 %>% 
  mutate(ano = as.numeric(ano)) %>%
  mutate(num_cedentes = as.numeric(num_cedentes))

dados <- left_join(dados, aux2)

## numero total de cessionaria

sele <- lapply(cessionaria, function(x) is.matrix(x))
aux <- lapply(cessionaria[sele==TRUE], function(x){ year(dmy(x[,2]))})
aux <- lapply(aux, function(x) ungroup(count(group_by(tibble(ano=x), ano))))
aux <- lapply(aux, function(x) as.matrix(x))
aux <- lapply(aux, function(x) {x[,2] <- cumsum(x[,2]) 
return(x) })

cnpj <- names(cessionaria)
nomes <- cnpj[sele==TRUE]

dados <- dados %>%
  mutate(e_cessionaria = ifelse(cnpj %in% nomes, 1, 0))

aux2<-NULL
for(i in 1:length(aux)){
  aux2 <- rbind(aux2 ,cbind(aux[[i]], nomes[i]))
}

colnames(aux2) <- c("ano", "num_cessionaria", "cnpj")
aux2  <- as_tibble(aux2)
aux2 <- aux2 %>% 
  mutate(ano = as.numeric(ano)) %>%
  mutate(num_cessionaria = as.numeric(num_cessionaria))

dados <- left_join(dados, aux2)


dados[is.na(dados$patentes), "patentes"] <-0
# dados[is.na(dados$marcas), "marcas"] <-0
# dados[is.na(dados$desenhos), "desenhos"] <-0
# dados[is.na(dados$programas), "programas"] <-0
dados[is.na(dados$cedentes), "cedentes"] <-0
dados[is.na(dados$cessionaria), "cessionaria"] <-0

dados[is.na(dados$num_patentes), "num_patentes"] <-0
# dados[is.na(dados$num_marcas), "num_marcas"] <-0
# dados[is.na(dados$num_desenhos), "num_desenhos"] <-0
# dados[is.na(dados$num_programas), "num_programas"] <-0
dados[is.na(dados$num_cedentes), "num_cedentes"] <-0
dados[is.na(dados$num_cessionaria), "num_cessionaria"] <-0



################
### Organiza e agrega dados da CVM
######################

my.destfile <- 'data-raw/SPW_CIA_ABERTA.zip'

# read it
df.cvm <- read_delim(my.destfile,
                     delim = '\t',
                     locale = locale(encoding = 'Latin1'))

df.cvm$cnpj <- gsub(" ", "0", as.character(format(df.cvm$CNPJ, digits = 14)))
aux <- as_tibble(df.cvm[,c("cnpj", "CATEG_REG", "SETOR_ATIV", "UF", "DT_REG", "DT_CONST")])
aux$CATEG_REG[is.na(aux$CATEG_REG)] <- "novo mercado"

dados <- left_join(dados, aux)


### Salva os dados

write.csv2(dados, file = "data-raw/dados.csv")

save(dados, file = "data-raw/dados.RData")
