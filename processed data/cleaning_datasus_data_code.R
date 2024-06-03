rm(list = ls())
library(tidyverse)
library(plm)
library(readr)
library(PNSIBGE)
library(mfx)

getwd()

#############################
# DATA CLEANING
#############################

# sih sus
read_process_sih <- function(file_path, year){
  readxl::read_xlsx(
    path = file_path,
    sheet = year,
    skip = 9,
    col_names = c("nome_codigo", "internacoes", "custo_internacao",
                  "dias_permanencia", "obitos","taxa_mortalidade"),
    col_types = c("text", "numeric", "numeric", "numeric",
                  "numeric","numeric")) %>% 
    mutate(ano = as.numeric(year),
           codigo_mun = as.numeric(substr(nome_codigo,1,6)))
}

df_sihsus_2014 <- read_process_sih("./raw_data/sih_datasus.xlsx", "2014")
df_sihsus_2015 <- read_process_sih("./raw_data/sih_datasus.xlsx", "2015")
df_sihsus_2016 <- read_process_sih("./raw_data/sih_datasus.xlsx", "2016")
df_sihsus_2017 <- read_process_sih("./raw_data/sih_datasus.xlsx", "2017")
df_sihsus_2018 <- read_process_sih("./raw_data/sih_datasus.xlsx", "2018")
df_sihsus_2019 <- read_process_sih("./raw_data/sih_datasus.xlsx", "2019")
df_sihsus_2020 <- read_process_sih("./raw_data/sih_datasus.xlsx", "2020")
df_sihsus_2021 <- read_process_sih("./raw_data/sih_datasus.xlsx", "2021")

df_sihsus <- bind_rows(df_sihsus_2014,df_sihsus_2015,df_sihsus_2016,df_sihsus_2017,
                       df_sihsus_2018,df_sihsus_2019,df_sihsus_2020,df_sihsus_2021)

rm(df_sihsus_2014,df_sihsus_2015,df_sihsus_2016,df_sihsus_2017,
   df_sihsus_2018,df_sihsus_2019,df_sihsus_2020,df_sihsus_2021)

df_sihsus <- df_sihsus %>%
  arrange(codigo_mun, ano) %>%
  group_by(codigo_mun) %>% 
  mutate(variacao_internacoes = internacoes - dplyr::lag(internacoes),
         variacao_obitos = obitos - dplyr::lag(obitos),
         variacao_custo_internacao = custo_internacao - dplyr::lag(custo_internacao),
         variacao_dias_permanencia = dias_permanencia - dplyr::lag(dias_permanencia)) %>% 
  ungroup()

writexl::write_xlsx(df_sihsus,"./processed data/sihsus_tratada.xlsx")
