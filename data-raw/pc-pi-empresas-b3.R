#package creation pi empresas b3

##inicia o pacote
#install.packages("devtools")
devtools::setup(rstudio = FALSE)

##preencher o DESCRIPTION

## cria pasta para dados brutos
devtools::use_data_raw()

# Ignora o que tem em data-raw
devtools::use_build_ignore("data-raw")

# Ignora Rproj do Rstudio
devtools::use_build_ignore("pi.empresas.b3.Rproj")


#Escrevas as funcoes e salve em R
