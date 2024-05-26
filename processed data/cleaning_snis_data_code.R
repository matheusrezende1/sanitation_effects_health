rm(list = ls())
library(tidyverse)
library(plm)

#############################
# DATA CLEANING
#############################

# 2018_2022
dfraw_18_22 <- readxl::read_xlsx("raw_data/desagregado_snis_2018_2022.xlsx")

# 2014_2017
dfraw_14_17 <- readxl::read_xlsx("raw_data/desagregado_snis_2014_2017.xlsx")

# checando se colunas sao iguais
identical(names(dfraw_18_22), names(dfraw_14_17))

#juntando as bases 
dfraw <- bind_rows(dfraw_18_22, dfraw_14_17) %>% arrange(`Código do Município`, `Ano de Referência`)

# puxando variaveis de interesse e renomeando
dfraw2 <- dfraw %>% 
  transmute(codigo_mun = `Código do Município`,
            nome_mun = `Município`,
            uf = `Estado`,
            ano = `Ano de Referência`,
            codigo_prestador = `Código do Prestador`,
            nome_prestador = `Prestador`,
            abrangencia = `Abrangência`,
            quantidade_municipios_atendidos_agua = `GE001 - Quantidade de municípios atendidos com abastecimento de água com delegação em vigor`,
            quantidade_municipios_atendidos_esgoto = `GE014 - Quantidade de municípios atendidos com esgotamento sanitário com delegação em vigor`,
            populacao_municipio = `POP_TOT - População total do município do ano de referência (Fonte: IBGE):`,
            populacao_urbana_municipio = `POP_URB - População urbana do município do ano de referência (Fonte: IBGE):`,
            populacao_atendida_agua = `AG001 - População total atendida com abastecimento de água`,
            economias_ativas_agua = `AG003 - Quantidade de economias ativas de água`,
            extensao_rede_agua = `AG005 - Extensão da rede de água`,
            populacao_atendida_esgoto = `ES001 - População total atendida com esgotamento sanitário`,
            economias_ativas_esgoto = `ES003 - Quantidade de economias ativas de esgotos`,
            extensao_rede_esgoto = `ES004 - Extensão da rede de esgotos`,
            receita_operacional_total = `FN005 - Receita operacional total (direta + indireta)`,
            arrecadacao_total = `FN006 - Arrecadação total`,
            despesa_pessoal_proprio = `FN010 - Despesa com pessoal próprio`,
            despesa_produtos_quimicos = `FN011 - Despesa com produtos químicos`,
            despesa_energia_eletrica = `FN013 - Despesa com energia elétrica`,
            despesa_terceiros = `FN014 - Despesa com serviços de terceiros`,
            despesa_total = `FN017 - Despesas totais com os serviços (DTS), sendo FN017 = FN015 + FN016 + FN019 + FN022 + FN028`,
            investimento_proprio_agua = `FN023 - Investimento realizado em abastecimento de água pelo prestador de serviços`,
            investimento_proprio_esgoto = `FN024 - Investimento realizado em esgotamento sanitário pelo prestador de serviços`,
            investimento_proprio_total = `FN033 - Investimentos totais realizados pelo prestador de serviços`,
            investimento_municipio_agua = `FN042 - Investimento realizado em abastecimento de água pelo(s) município(s)`,
            investimento_municipio_esgoto = `FN043 - Investimento realizado em esgotamento sanitário pelo(s) município(s)`,
            investimento_municipio_total = `FN048 - Investimentos totais realizados pelo(s) município(s)`,
            investimento_estado_agua = `FN052 - Investimento realizado em abastecimento de água pelo estado`,
            investimento_estado_esgoto = `FN053 - Investimento realizado em esgotamento sanitário pelo estado`,
            investimento_estado_total = `FN058 - Investimentos totais realizados pelo estado`,
            cobertura_agua = `IN055 - Índice de atendimento total de água`,
            indice_coleta_esgoto = `IN015 - Índice de coleta de esgoto`,
            indice_tratamento_esgoto = `IN046 - Índice de esgoto tratado referido à água consumida`,
            tarifa_media = `IN004 - Tarifa média praticada`,
            indice_perdas_agua = `IN051 - Índice de perdas por ligação`
  )    


dfraw2 %>% filter(ano == 2022) %>%  
  summarise(pop = sum(populacao_atendida_agua, na.rm= T))


# ajustando municipios que aparecem mais de uma vez

df <- dfraw2 %>%
  group_by(codigo_mun, nome_mun, uf, ano) %>%
  summarise(populacao_municipio = mean(populacao_municipio),
            populacao_urbana_municipio = mean(populacao_urbana_municipio),
            populacao_atendida_agua = sum(populacao_atendida_agua, na.rm =T),
            economias_ativas_agua = sum(economias_ativas_agua,na.rm =T),
            extensao_rede_agua = sum(extensao_rede_agua, na.rm =T),
            populacao_atendida_esgoto = sum(populacao_atendida_esgoto, na.rm =T),
            economias_ativas_esgoto = sum(economias_ativas_esgoto, na.rm =T),
            extensao_rede_esgoto = sum(extensao_rede_esgoto, na.rm =T),
            receita_operacional_total = sum(receita_operacional_total, na.rm =T),
            arrecadacao_total = sum(arrecadacao_total, na.rm =T),
            despesa_pessoal_proprio = sum(despesa_pessoal_proprio, na.rm =T),
            despesa_produtos_quimicos = sum(despesa_produtos_quimicos, na.rm =T),
            despesa_energia_eletrica = sum(despesa_energia_eletrica, na.rm =T),
            despesa_terceiros = sum(despesa_terceiros, na.rm =T),
            despesa_total = sum(despesa_total, na.rm =T),
            investimento_proprio_agua = sum(investimento_proprio_agua, na.rm =T),
            investimento_proprio_esgoto = sum(investimento_proprio_esgoto, na.rm =T),
            investimento_proprio_total = sum(investimento_proprio_total, na.rm =T),
            investimento_municipio_agua = sum(investimento_municipio_agua, na.rm =T),
            investimento_municipio_esgoto = sum(investimento_municipio_esgoto, na.rm =T),
            investimento_municipio_total = sum(investimento_municipio_total, na.rm =T),
            investimento_estado_agua = sum(investimento_estado_agua, na.rm =T),
            investimento_estado_esgoto = sum(investimento_estado_esgoto,na.rm =T),
            investimento_estado_total = sum(investimento_estado_total, na.rm =T),
            cobertura_agua = sum(cobertura_agua)/100,
            indice_coleta_esgoto = sum(indice_coleta_esgoto),
            indice_tratamento_esgoto = sum(indice_tratamento_esgoto),
            tarifa_media = mean(tarifa_media),
            indice_perdas_agua = mean(indice_perdas_agua)) %>% ungroup()

# somando duplicados (check)
dfraw2 %>% group_by(codigo_mun, ano) %>% tally() %>% filter(n>1) %>% ungroup() %>% 
  mutate(numero = n-1) %>% summarise(duplicadas=sum(numero))

# substituindo NaN por NA
df <- df %>%
  mutate(across(everything(), ~replace(., is.nan(.), NA)))

# criando variaveis 
df <- df %>%
  mutate(regiao = case_when(
    uf %in% c("SP", "MG", "RJ", "ES") ~ "sudeste",
    uf %in% c("PR", "SC", "RS") ~ "sul",
    uf %in% c("MT", "MS", "GO", "DF") ~ "centro-oeste",
    uf %in% c("AC", "RO", "AM", "RR", "PA", "AP", "TO") ~ "norte",
    uf %in% c("MA", "PI", "CE", "RN", "PB", "PE", "AL", "SE", "BA") ~ "nordeste"),
    micro_porte = ifelse(populacao_municipio < 20000,1,0),
    pequeno_porte = ifelse(populacao_municipio %in% (20000:50000),1,0),
    medio_porte = ifelse(populacao_municipio %in% (50000:200000),1,0),
    grande_porte = ifelse(populacao_municipio > 200000,1,0)) %>% 
  filter(!is.na(codigo_mun))

df <- df %>% filter(ano < 2020) %>% 
  mutate(investimento_proprio_agua_MM = investimento_proprio_agua/1000000,
         investimento_proprio_esgoto_MM = investimento_proprio_esgoto/1000000,
         investimento_proprio_total_MM = investimento_proprio_total/1000000,
         economias_ativas_agua_M = economias_ativas_agua/1000,
         pop_M = populacao_municipio/1000,
         cobertura_esgoto = (populacao_atendida_esgoto/populacao_municipio)*100,
         cobertura_agua = (populacao_atendida_agua/populacao_municipio)*100) %>% 
  arrange(codigo_mun, ano) %>%
  group_by(codigo_mun) %>%
  mutate(variacao_economias_ativas_agua = economias_ativas_agua - dplyr::lag(economias_ativas_agua),
         variacao_cobertura_agua = cobertura_agua - dplyr::lag(cobertura_agua),
         variacao_cobertura_esgoto = cobertura_esgoto - dplyr::lag(cobertura_esgoto),
         variacao_populacao = populacao_municipio - dplyr::lag(populacao_municipio),
         variacao_extensao_rede_agua = extensao_rede_agua - dplyr::lag(extensao_rede_agua),
         variacao_economias_ativas_esgoto = economias_ativas_esgoto - dplyr::lag(economias_ativas_esgoto),
         variacao_perdas_agua = indice_perdas_agua - dplyr::lag(indice_perdas_agua),
         investimento_media_movel_agua = (investimento_proprio_agua_MM + dplyr::lag(investimento_proprio_agua_MM,1))/2,
         investimento_media_movel_esgoto = (investimento_proprio_esgoto_MM + dplyr::lag(investimento_proprio_esgoto_MM,1))/2,
         investimento_media_movel_total = (investimento_proprio_total_MM + dplyr::lag(investimento_proprio_total_MM,1))/2) %>%
  ungroup()

writexl::write_xlsx(df, "processed data/snis_tratada_2014_2022.xlsx")
