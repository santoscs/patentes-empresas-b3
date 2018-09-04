
rm(list=ls())

library(tidyverse)
library(reshape)
library(lubridate)
library(GetDFPData)


load("data-raw/cedentes.RData")
load("data-raw/cessionaria.RData")
load("data-raw/desenhos.RData")
load("data-raw/empresas_b3_10_17.RData")
load("data-raw/marcas.RData")
load("data-raw/patentes.RData")
load("data-raw/programas.RData")


## empresas  selecionadas

dados <- df.reports


## assets junta ativos


dados$fr.assets <- lapply(dados$fr.assets, function(x){ x <- mutate(x, acc.nd = paste(acc.number, acc.desc))})
tab <- lapply(dados$fr.assets, function(x) cast(x, name.company + ref.date ~ acc.nd, value = "acc.value"))

aux <- NULL
for(i in 1:length(tab)){
  aux <- rbind(aux, tab[[i]])
}

ativo <- aux

## liabilities junta passivo
dados$fr.liabilities <- lapply(dados$fr.liabilities, function(x){ x <- mutate(x, acc.nd = paste(acc.number, acc.desc))})
# exclui subcontas para compabilizacao
contas <- c("2" ,      "2.01" ,   "2.01.01" ,"2.01.02" ,"2.01.03" ,"2.01.04" ,"2.01.05" ,"2.01.06" ,"2.01.07" ,"2.02" ,  
            "2.02.01" ,"2.02.02" ,"2.02.03" ,"2.02.04" ,"2.02.05" ,"2.02.06" ,"2.03" ,   "2.03.01" ,"2.03.02" ,"2.03.03",
            "2.03.04" ,"2.03.05" ,"2.03.06" ,"2.03.07" ,"2.03.08" ,"2" ,      "2.01" ,   "2.01.01" ,"2.01.02" ,"2.01.03",
            "2.01.04" ,"2.01.05" ,"2.01.06" ,"2.01.07" ,"2.02" ,   "2.02.01" ,"2.02.02" ,"2.02.03" ,"2.02.04" ,"2.02.05",
            "2.02.06" ,"2.03" ,   "2.03.01" ,"2.03.02" ,"2.03.03" ,"2.03.04" ,"2.03.05" ,"2.03.06" ,"2.03.07" ,"2.03.08")
dados$fr.liabilities <- lapply(dados$fr.liabilities, function(x){ x[x$acc.number %in% contas, ] })
tab <- lapply(dados$fr.liabilities, function(x) cast(x, name.company + ref.date ~ acc.nd, value = "acc.value"))

aux <- NULL
for(i in 1:length(tab)){
  aux <- rbind(aux, tab[[i]])
}
passivo <- aux

## income junta resultado
dados$fr.income <- lapply(dados$fr.income, function(x){ x <- mutate(x, acc.nd = paste(acc.number, acc.desc))})
# exclui subcontas para compabilizacao
contas <- c("3.01", "3.02", "3.03", "3.04", "3.04.01", "3.04.02", "3.04.03"
            , "3.04.04", "3.04.05", "3.04.06", "3.05", "3.06", "3.06.01", "3.06.02"
            , "3.07", "3.08", "3.08.01", "3.08.02", "3.09", "3.10", "3.10.01"
            , "3.10.02", "3.11", "3.99", "3.99.01", "3.99.02")
dados$fr.income <- lapply(dados$fr.income, function(x){ x[x$acc.number %in% contas, ] })
tab <- lapply(dados$fr.income, function(x) cast(x, name.company + ref.date ~ acc.nd, value = "acc.value"))

aux <- NULL
for(i in 1:length(tab)){
  aux <- rbind(aux, tab[[i]])
}
resultado <- aux

##  cashflow junta caixa
dados$fr.cashflow <- lapply(dados$fr.cashflow, function(x){ x <- mutate(x, acc.nd = paste(acc.number, acc.desc))})
# exclui subcontas para compabilizacao
contas <- c("6.01" ,  "6.02" ,  "6.03" ,  "6.04" ,  "6.05" ,  "6.05.01" , "6.05.02")
dados$fr.cashflow <- lapply(dados$fr.cashflow, function(x){ x[x$acc.number %in% contas, ] })
tab <- lapply(dados$fr.cashflow, function(x) cast(x, name.company + ref.date ~ acc.nd, value = "acc.value"))

aux <- NULL
for(i in 1:length(tab)){
  aux <- rbind(aux, tab[[i]])
}

caixa <- aux


# mkt.avg.value junta valor de mercado

tab <- dados$history.mkt.value
aux <- NULL
for(i in 1:length(tab)){
  aux <- rbind(aux, tab[[i]])
}

valor <- aux 

valor <- as_tibble(valor)
valor <- valor %>%
  mutate(ref.date = year(ref.date))


# junta todos os dados em uma unica base

dados <- merge(ativo, passivo)
dados <- merge(dados, resultado)
dados <- merge(dados, caixa)

dados <- as_tibble(dados)
dados <- dados %>%
  mutate(ref.date = year(ref.date))

dados <- full_join(dados, valor)

aux <- tibble(name.company=df.reports$company.name, cnpj=df.reports$cnpj)

dados <- full_join(dados, aux)


dados <- dados %>%
  mutate(ano = ref.date) %>%
  mutate(constitu=NA) %>%
  mutate(setor=NA) %>%
  mutate(subsetor=NA) %>%
  mutate(estado=NA) %>%
  mutate(segmento=NA)


df.info <- gdfpd.get.info.companies(type.data = 'companies', cache.folder = tempdir())

for(i in 1:length(df.info$name.company)){
  sele <- dados$name.company %in% df.info$name.company[i]
  dados[sele,"constitu"] <- as.character(rep(df.info$date.constitution[i], sum(sele)))
  dados[sele,"setor"] <- rep(df.info$main.sector[i], sum(sele))
  dados[sele,"subsetor"] <- rep(df.info$main.sector[i], sum(sele))
  dados[sele,"estado"] <- rep(df.info$estate[i], sum(sele))
  dados[sele,"segmento"] <- rep(df.info$segment[i], sum(sele))
}

dados[dados$cnpj=="04032433000180", "constitu"] <- rep("2000-09-05", 7)
dados[dados$cnpj=="92781335000102", "constitu"] <- rep("1996-08-11", 7)
dados[dados$cnpj=="09288252000132", "constitu"] <- rep("2007-12-03", 5)
dados[dados$cnpj=="85778074000106", "constitu"] <- rep("1966-01-31", 7)
dados[dados$cnpj=="60651809000105", "constitu"] <- rep("1972-04-12", 7)
dados[dados$cnpj=="08424178000171", "constitu"] <- rep("1969-12-15",7)

dados <- dados %>%
  mutate(constitu=ymd(constitu)) %>%
  mutate(ano_constitu = year(constitu))

################
### Organiza os dados do INPI
##################

# tidy data

cnpj <- df.reports$cnpj
sele <- lapply(patentes, function(x)is.matrix(x))
tem_patente <- as.numeric(sele==TRUE)
sele <- lapply(marcas, function(x)is.matrix(x))
tem_marca <- as.numeric(sele==TRUE)
sele <- lapply(desenhos, function(x)is.matrix(x))
tem_desenho <- as.numeric(sele==TRUE)
sele <- lapply(programas, function(x)is.matrix(x))
tem_programa <- as.numeric(sele==TRUE)
sele <- lapply(cedentes, function(x)is.matrix(x))
e_cedente <- as.numeric(sele==TRUE)
sele <- lapply(cessionaria, function(x)is.matrix(x))
e_cessionaria <- as.numeric(sele==TRUE)

aux <- as_tibble(cbind(cnpj, tem_desenho, tem_marca, tem_patente, 
  tem_programa, e_cedente, e_cessionaria))

dados <- full_join(dados, aux)


# patentes

sele <- lapply(patentes, function(x) is.matrix(x))
aux <- lapply(patentes[sele==TRUE], function(x){ year(dmy(x[,2]))})
aux <- lapply(aux, function(x) ungroup(filter(count(group_by(tibble(ano=x), ano)), ano >2009 & ano < 2017)))
aux <- lapply(aux, function(x) as.matrix(x))
nomes <- df.reports$cnpj[sele==TRUE]

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

# marcas
sele <- lapply(marcas, function(x)is.matrix(x))
aux <- lapply(marcas[sele==TRUE], function(x){ year(dmy(x[,2]))})
aux <- lapply(aux, function(x) ungroup(filter(count(group_by(tibble(ano=x), ano)), ano >2009 & ano < 2018)))
aux <- lapply(aux, function(x) as.matrix(x))
nomes <- df.reports$cnpj[sele==TRUE]

aux2<-NULL
for(i in 1:length(aux)){
  aux2 <- rbind(aux2 ,cbind(aux[[i]], nomes[i]))
}

colnames(aux2) <- c("ano", "marcas", "cnpj")
aux2  <- as_tibble(aux2)
aux2 <- aux2 %>% 
  mutate(ano = as.numeric(ano)) %>%
  mutate(marcas = as.numeric(marcas))

dados <- left_join(dados, aux2)

## desenhos

sele <- lapply(desenhos, function(x)is.matrix(x))
aux <- lapply(desenhos[sele==TRUE], function(x){ year(dmy(x[,2]))})
aux <- lapply(aux, function(x) ungroup(filter(count(group_by(tibble(ano=x), ano)), ano >2009 & ano < 2018)))
aux <- lapply(aux, function(x) as.matrix(x))
nomes <- df.reports$cnpj[sele==TRUE]

aux2<-NULL
for(i in 1:length(aux)){
  aux2 <- rbind(aux2 ,cbind(aux[[i]], nomes[i]))
}

colnames(aux2) <- c("ano", "desenhos", "cnpj")
aux2  <- as_tibble(aux2)
aux2 <- aux2 %>% 
  mutate(ano = as.numeric(ano)) %>%
  mutate(desenhos = as.numeric(desenhos))

dados <- left_join(dados, aux2)

## programas
sele <- lapply(programas, function(x)is.matrix(x))
aux <- lapply(programas[sele==TRUE], function(x){ year(dmy(x[,2]))})
aux <- lapply(aux, function(x) ungroup(filter(count(group_by(tibble(ano=x), ano)), ano >2009 & ano < 2018)))
aux <- lapply(aux, function(x) as.matrix(x))
nomes <- df.reports$cnpj[sele==TRUE]

aux2<-NULL
for(i in 1:length(aux)){
  aux2 <- rbind(aux2 ,cbind(aux[[i]], nomes[i]))
}

colnames(aux2) <- c("ano", "programas", "cnpj")
aux2  <- as_tibble(aux2)
aux2 <- aux2 %>% 
  mutate(ano = as.numeric(ano)) %>%
  mutate(programas = as.numeric(programas))

dados <- left_join(dados, aux2)

# cedentes
sele <- lapply(cedentes, function(x)is.matrix(x))
aux <- lapply(cedentes[sele==TRUE], function(x){ year(dmy(x[,2]))})
aux <- lapply(aux, function(x) ungroup(filter(count(group_by(tibble(ano=x), ano)), ano >2009 & ano < 2018)))
aux <- lapply(aux, function(x) as.matrix(x))
nomes <- df.reports$cnpj[sele==TRUE]

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
nomes <- df.reports$cnpj[sele==TRUE]

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

nomes <- df.reports$cnpj[sele==TRUE]

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



## numero total de marcas

sele <- lapply(marcas, function(x) is.matrix(x))
aux <- lapply(marcas[sele==TRUE], function(x){ year(dmy(x[,2]))})
aux <- lapply(aux, function(x) ungroup(count(group_by(tibble(ano=x), ano))))
aux <- lapply(aux, function(x) as.matrix(x))
aux <- lapply(aux, function(x) {x[,2] <- cumsum(x[,2]) 
return(x) })

nomes <- df.reports$cnpj[sele==TRUE]

aux2<-NULL
for(i in 1:length(aux)){
  aux2 <- rbind(aux2 ,cbind(aux[[i]], nomes[i]))
}

colnames(aux2) <- c("ano", "num_marcas", "cnpj")
aux2  <- as_tibble(aux2)
aux2 <- aux2 %>% 
  mutate(ano = as.numeric(ano)) %>%
  mutate(num_marcas = as.numeric(num_marcas))

dados <- left_join(dados, aux2)

## numero total de desenhos

sele <- lapply(desenhos, function(x) is.matrix(x))
aux <- lapply(desenhos[sele==TRUE], function(x){ year(dmy(x[,2]))})
aux <- lapply(aux, function(x) ungroup(count(group_by(tibble(ano=x), ano))))
aux <- lapply(aux, function(x) as.matrix(x))
aux <- lapply(aux, function(x) {x[,2] <- cumsum(x[,2]) 
return(x) })

nomes <- df.reports$cnpj[sele==TRUE]

aux2<-NULL
for(i in 1:length(aux)){
  aux2 <- rbind(aux2 ,cbind(aux[[i]], nomes[i]))
}

colnames(aux2) <- c("ano", "num_desenhos", "cnpj")
aux2  <- as_tibble(aux2)
aux2 <- aux2 %>% 
  mutate(ano = as.numeric(ano)) %>%
  mutate(num_desenhos = as.numeric(num_desenhos))

dados <- left_join(dados, aux2)

## numero total de programas

sele <- lapply(programas, function(x) is.matrix(x))
aux <- lapply(programas[sele==TRUE], function(x){ year(dmy(x[,2]))})
aux <- lapply(aux, function(x) ungroup(count(group_by(tibble(ano=x), ano))))
aux <- lapply(aux, function(x) as.matrix(x))
aux <- lapply(aux, function(x) {x[,2] <- cumsum(x[,2]) 
return(x) })

nomes <- df.reports$cnpj[sele==TRUE]

aux2<-NULL
for(i in 1:length(aux)){
  aux2 <- rbind(aux2 ,cbind(aux[[i]], nomes[i]))
}

colnames(aux2) <- c("ano", "num_programas", "cnpj")
aux2  <- as_tibble(aux2)
aux2 <- aux2 %>% 
  mutate(ano = as.numeric(ano)) %>%
  mutate(num_programas = as.numeric(num_programas))

dados <- left_join(dados, aux2)


## numero total de cedente

sele <- lapply(cedentes, function(x) is.matrix(x))
aux <- lapply(cedentes[sele==TRUE], function(x){ year(dmy(x[,2]))})
aux <- lapply(aux, function(x) ungroup(count(group_by(tibble(ano=x), ano))))
aux <- lapply(aux, function(x) as.matrix(x))
aux <- lapply(aux, function(x) {x[,2] <- cumsum(x[,2]) 
return(x) })

nomes <- df.reports$cnpj[sele==TRUE]

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

nomes <- df.reports$cnpj[sele==TRUE]

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
dados[is.na(dados$marcas), "marcas"] <-0
dados[is.na(dados$desenhos), "desenhos"] <-0
dados[is.na(dados$programas), "programas"] <-0
dados[is.na(dados$cedentes), "cedentes"] <-0
dados[is.na(dados$cessionaria), "cessionaria"] <-0

dados[is.na(dados$num_patentes), "num_patentes"] <-0
dados[is.na(dados$num_marcas), "num_marcas"] <-0
dados[is.na(dados$num_desenhos), "num_desenhos"] <-0
dados[is.na(dados$num_programas), "num_programas"] <-0
dados[is.na(dados$num_cedentes), "num_cedentes"] <-0
dados[is.na(dados$num_cessionaria), "num_cessionaria"] <-0

dados <- dados %>% 
  mutate(idade = year(ymd(paste0(ano, "12-31"))) - year(constitu))

write.csv2(dados, file = "data-raw/dados.csv")

save(dados, file = "data-raw/dados.RData")
