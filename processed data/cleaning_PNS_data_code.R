rm(list = ls())
library(tidyverse)
library(plm)
library(readr)
library(PNSIBGE)
library(mfx)

#############################
# DATA CLEANING
#############################

# Pesquisa Nacional de Saúde (IBGE) 2019
# Uploading it with read_pns function (PNSIBGE package)

#setwd("./raw_data")

dataPNS <- read_pns(microdata="PNS_2019.txt", input_txt="input_PNS_2019.txt")
dataPNS <- pns_labeller(data_pns=dataPNS, dictionary.file="dicionario_PNS_microdados_2019.xls")

# Selecting relevant variables
df_pns <- dataPNS %>% 
  transmute(codigo_mun = V0024,
            uf = V0001,
            ano = V0020,
            genero = C006,
            idade = C008,
            cor = C009,
            afastado = J002,
            acamado = J005,
            dias_afastado = J003,
            dias_acamado = J006,
            causa = J00402,
            educ = VDD004A,
            trab = VDE002,
            setor_trab = VDE014,
            renda_dom = VDF002,
            renda_per_capita = VDF003,
            tipo_abastecimento_agua = A005010,
            distribuicao_agua = A005012, #Este domicílio está ligado à rede geral de distribuição de água?
            banheiro = A01401,
            agua_canalizada = A00601, # A água utilizada neste domicílio chega
            numero_banheiros = A01401, # Quantos banheiros (com chuveiro ou banheira e vaso sanitário ou privada) de uso exclusivo dos moradores existem neste domicílio, inclusive os localizados no terreno ou propriedade
            rede_esgoto = A01501, # Para onde vai o esgoto do banheiro? Ou Para onde vai o esgoto do sanitário ou do buraco para dejeções?
            lixo_coletado = A016010, # Qual o (principal) destino dado ao lixo
            peso_amostral = V00281)


df_pns <- df_pns %>%
  mutate(homem = ifelse(genero == "Homem",1,0),
         branco = ifelse(cor == "Branca",1,0),
         afastado_geral = ifelse(afastado == "Sim",1,0),
         acamado_geral = ifelse(acamado == "Sim",1,0),
         causa_hidrica = ifelse(causa == "Problemas gastrointestinais (Diarreia / vômito / náusea / gastrite / dor de barriga)",1,0),
         causa_respiratoria = ifelse(causa == "Problemas respiratórios (Resfriado / gripe /sinusite/ asma / bronquite / pneumonia)",1,0),
         afastado_causa_hidrica = afastado_geral*causa_hidrica,
         acamado_causa_hidrica = acamado_geral*causa_hidrica,
         afastado_causa_respiratoria = afastado_geral*causa_respiratoria,
         acamado_causa_respiratoria = acamado_geral*causa_respiratoria,
         educ = as.numeric(educ),
         trab = ifelse(trab == "Pessoas Ocupadas",1,0),
         tem_rede_agua = ifelse(tipo_abastecimento_agua == "Rede geral de distribuição",1,0),
         tem_banheiro = ifelse(banheiro >= 1,1, 0),
         agua_canalizada = ifelse(agua_canalizada == "Canalizada em pelo menos um cômodo",1,0),
         rede_esgoto = ifelse(rede_esgoto == "Rede geral de esgoto ou pluvial" |
                                rede_esgoto == "Fossa séptica ligada à rede",1,0),
         lixo_coletado == ifelse(lixo_coletado == " Coletado diretamente por serviço de limpeza (independente da frequência de dias de coleta)" |
                                   lixo_coletado == " Coletado em caçamba de serviço de limpeza",1,0))

# substituindo NA por zero
df_pns$acamado_causa_hidrica[is.na(df_pns$acamado_causa_hidrica)] <- 0
df_pns$afastado_causa_hidrica[is.na(df_pns$afastado_causa_hidrica)] <- 0
df_pns$acamado_causa_respiratoria[is.na(df_pns$acamado_causa_respiratoria)] <- 0
df_pns$afastado_causa_respiratoria[is.na(df_pns$afastado_causa_respiratoria)] <- 0


df_pns <- df_pns %>%
  mutate(regiao = case_when(
    uf %in% c("Rio Grande do Sul", "Paraná", "Santa Catarina") ~ "Sul",
    uf %in% c("São Paulo", "Rio de Janeiro", "Espírito Santo", "Minas Gerais") ~ "Sudeste",
    uf %in% c("Bahia", "Sergipe", "Alagoas", "Pernambuco", "Paraíba", "Rio Grande do Norte", "Ceará", "Piauí", "Maranhão") ~ "Nordeste",
    uf %in% c("Mato Grosso", "Mato Grosso do Sul", "Goiás", "Distrito Federal") ~ "Centro-Oeste",
    uf %in% c("Pará", "Amazonas", "Acre", "Amapá", "Rondônia", "Roraima", "Tocantins") ~ "Norte",
    TRUE ~ NA_character_ # Caso algum estado não se encaixe nas categorias acima
  ))

#writexl::write_xlsx(df_pns, "pns_tratada.xlsx")