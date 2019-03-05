
rm(list=ls())
load("data-raw/dados.RData")

#install.packages('janitor') 
#install.packages("skimr")
library(skimr)
library(janitor)
library(tidyverse)
library(lubridate)

# nome das variaveis
dados <- clean_names(dados)

# entradas duplicadas
teste <- get_dupes(dados, cnpj, ref_date)


# retira empresas sem intangivel
dados <- dados %>%
  filter(!is.na(x1_02_04_intangivel)) 

# retira empresas com ativo nulo
dados <- dados %>%
  filter(x1_ativo_total>0) 

# consistencia das informacoes
dados <- dados %>%
  filter(x1_ativo_total>x1_01_ativo_circulante &
           x1_ativo_total > x1_02_ativo_nao_circulante & 
           x1_02_ativo_nao_circulante > x1_02_04_intangivel)


# Harmoniza as variaveis 

dados <- dados %>%
  mutate(pl = ifelse(is.na(x2_03_patrimonio_liquido_consolidado), x2_03_patrimonio_liquido, x2_03_patrimonio_liquido_consolidado)) %>%
  mutate(lucro = ifelse(is.na(x3_11_lucro_prejuizo_consolidado_do_periodo), x3_11_lucro_prejuizo_do_periodo, x3_11_lucro_prejuizo_consolidado_do_periodo)) 


# calcula os indices

dados <- dados %>%
  mutate(liq_cor = x1_01_ativo_circulante/ x2_01_passivo_circulante) %>%
  mutate(liq_geral = x1_ativo_total/ (x2_01_passivo_circulante+x2_02_passivo_nao_circulante)) %>%
  mutate(end_ger =  x2_01_passivo_circulante/(x2_01_passivo_circulante+ x2_02_passivo_nao_circulante)) %>%
  mutate(end_ce =  (x2_01_passivo_circulante+x2_02_passivo_nao_circulante)/(x1_ativo_total)) %>%
  mutate(roa = lucro/x1_ativo_total) %>%
  mutate(roe = lucro/pl) %>%
  mutate(intang_ativo = x1_02_04_intangivel/x1_ativo_total) %>%
  mutate(intang_pl = x1_02_04_intangivel/pl) %>%
  mutate(ch = x3_01_receita_de_venda_de_bens_e_ou_servicos/x7_08_01_pessoal) %>%
  mutate(giro = x7_01_receitas/x1_ativo_total) %>%
  mutate(pro_patente = ifelse(patentes>0,1,0)) %>%
  mutate(lucra = x3_05_resultado_antes_do_resultado_financeiro_e_dos_tributos/x3_01_receita_de_venda_de_bens_e_ou_servicos) %>%
  mutate(segmento = as.factor(segmento)) %>%
  mutate(setor = as.factor(clean_names(setor))) %>%
  mutate(subsetor = as.factor(subsetor)) %>%
  mutate(categoria = as.factor(categ_reg)) %>%
  mutate(ativi = as.factor(setor_ativ)) %>%
  mutate(uf = as.factor(uf)) %>%
  mutate(dano = as.factor(ano)) %>%
  mutate(idade = ano - year(ymd(constitu))) %>%
  mutate(tamanho = log(x1_ativo_total)) %>%
  mutate(vca = (x1_01_04_estoques + x1_02_03_imobilizado)/x1_ativo_total) %>%
  mutate(ief = (x2_03_01_capital_social_realizado + x2_02_passivo_nao_circulante) /(x1_01_04_estoques + x1_02_ativo_nao_circulante)) %>%
  mutate(pagr = x7_07_valor_adicionado_total_a_distribuir/x1_ativo_total) %>%
  mutate(pplgr = x7_07_valor_adicionado_total_a_distribuir/pl) %>%
  mutate(pgpgr = x7_07_valor_adicionado_total_a_distribuir/x7_08_01_pessoal) %>%
  mutate(alta = ifelse(subsetor %in% c("Equipamentos", "Medicamentos e Outros Produtos", "Computadores e Equipamentos", "Material de Transporte",
                                       "Máquinas e Equipamentos", "Automóveis e Motocicletas", "Químicos"), 1, 0)) %>%
  mutate(baixa = ifelse(subsetor %in% c("Siderurgia e Metalurgia", "Petróleo. Gás e Biocombustíveis", "Embalagens", "Tecidos. Vestuário e Calçados", "Utilidades Domésticas", "Alimentos Processados",
                                        "Bebidas", "Madeira e Papel", "Materiais Diversos"), 1, 0)) %>%
  mutate(regiao = ifelse(uf %in% c("SC", "RS", "PR"), "sul", 
                         ifelse(uf %in% c("SP", "RJ", "MG", "ES"), "suldeste", "outras")))

# retira empresas com intang_ativo maior que 1
dados <- dados %>%
  filter(intang_ativo<=1) 

##########################
### outlier
##########################

mod <- lm(pro_patente ~ liq_cor+liq_geral+end_ger+end_ce+roa+roe+intang_ativo+intang_pl+pagr+pplgr+idade+tamanho+alta+baixa, data=dados)
cooksd <- cooks.distance(mod)

# plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
# abline(h = 4*mean(cooksd, na.rm=T), col="red")  # add cutoff line
# text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="red")  # add labels

influential <- as.numeric(names(cooksd)[(cooksd > 4*mean(cooksd, na.rm=T))])  # influential row numbers
#head(dados[influential, ])  # influential observations.

dados <- dados[-influential, ] 
n <- length(influential)


mod <- glm(pro_patente ~ liq_cor+liq_geral+end_ger+end_ce+roa+roe+intang_ativo+intang_pl+pagr+pplgr+idade+tamanho+alta+baixa, family=binomial(link="logit"), data=dados)
cooksd <- cooks.distance(mod)

# plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
# abline(h = 4*mean(cooksd, na.rm=T), col="red")  # add cutoff line
# text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="red")  # add labels

influential <- as.numeric(names(cooksd)[(cooksd > 4*mean(cooksd, na.rm=T))])  # influential row numbers
#head(dados[influential, ])  # influential observations.

dados <- dados[-influential, ]

n <- n + length(influential)


# Remove empresas sem alguma informaccao no periodo 10-17
sele <- dados %>%
  count(cnpj) %>%
  filter(n<8)
dados2 <- dados %>%
  filter(!(cnpj %in% sele$cnpj))

sele <- dados2 %>%
  count(cnpj) %>%
  filter(n>8)
dados <- dados %>%
  filter(!(cnpj %in% sele$cnpj))


# identificador do painel 
aux <- levels(as.factor(dados2$cnpj))
dados2$id <- NA

for(i in 1:length(aux)){
  dados2$id[which(dados2$cnpj %in% aux[i])] <-i
}

# identificador do painel 
aux <- levels(as.factor(dados$cnpj))
dados$id <- NA

for(i in 1:length(aux)){
  dados$id[which(dados$cnpj %in% aux[i])] <-i
}


write.csv2(dados, file = "data-raw/tidy-dados.csv")
save(dados, dados2, file = "data-raw/tidy-dados.RData")
n
