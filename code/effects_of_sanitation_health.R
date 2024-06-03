rm(list = ls())
library(tidyverse)
library(plm)
library(readr)
library(PNSIBGE)
library(mfx)
library(stargazer)
library(ggthemes)

df_snis <- readxl::read_xlsx("./processed data/snis_tratada_2014_2022.xlsx")
df_datasus <- readxl::read_xlsx("./processed data/sihsus_tratada.xlsx")

df <- df_snis %>% left_join(df_datasus, by = c("codigo_mun", "ano"))

#################################################################################
# PLOTS
#################################################################################

df %>%
  group_by(uf) %>% 
  summarise(cobertura_media_esgoto_uf = sum(populacao_atendida_esgoto, na.rm = T)/ sum(populacao_municipio, na.rm = T),
            internacoes_por_cem_mil = sum(internacoes, na.rm = T)/(sum(populacao_municipio, na.rm = T)/100000),
            populacao_uf = sum(populacao_municipio, na.rm = T)) %>%
  ungroup() %>% 
  mutate(regiao = case_when(
    uf %in% c("SP", "MG", "RJ", "ES") ~ "Sudeste",
    uf %in% c("PR", "SC", "RS") ~ "Sul",
    uf %in% c("MT", "MS", "GO", "DF") ~ "Centro Oeste",
    uf %in% c("AC", "RO", "AM", "RR", "PA", "AP", "TO") ~ "Norte",
    uf %in% c("MA", "PI", "CE", "RN", "PB", "PE", "AL", "SE", "BA") ~ "Nordeste")) %>% 
  ggplot() + geom_point(aes(x = cobertura_media_esgoto_uf, y = internacoes_por_cem_mil, size = populacao_uf, color = regiao), alpha = 0.8) +
  guides(
    size = FALSE,  
    color = guide_legend(override.aes = list(size = 3))) +
  labs(x = "Cobertura Média de Esgoto por UF",
       y = "Internações por Cem Mil Habitantes",
       color = "Região"
  ) +
  theme_minimal() +  
  ggtitle("Relação entre Cobertura de Esgoto e Internações por 100 Mil Habitantes") +
  theme(
    text = element_text(size = 12),  
    plot.title = element_text(hjust = 0.5, size = 12),  
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 12), 
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.position = "bottom"
  )

df %>%
  group_by(uf) %>% 
  summarise(cobertura_media_agua_uf = sum(populacao_atendida_agua, na.rm = T)/ sum(populacao_municipio, na.rm = T),
            internacoes_por_cem_mil = sum(internacoes, na.rm = T)/(sum(populacao_municipio, na.rm = T)/100000),
            populacao_uf = sum(populacao_municipio, na.rm = T)) %>%
  ungroup() %>% 
  mutate(regiao = case_when(
    uf %in% c("SP", "MG", "RJ", "ES") ~ "Sudeste",
    uf %in% c("PR", "SC", "RS") ~ "Sul",
    uf %in% c("MT", "MS", "GO", "DF") ~ "Centro Oeste",
    uf %in% c("AC", "RO", "AM", "RR", "PA", "AP", "TO") ~ "Norte",
    uf %in% c("MA", "PI", "CE", "RN", "PB", "PE", "AL", "SE", "BA") ~ "Nordeste")) %>% 
  ggplot() + geom_point(aes(x = cobertura_media_agua_uf, y = internacoes_por_cem_mil, size = populacao_uf, color = regiao), alpha = 0.8) +
  guides(
    size = FALSE,  
    color = guide_legend(override.aes = list(size = 3))) +
  labs(x = "Cobertura Média de Água por UF",
       y = "Internações por Cem Mil Habitantes",
       color = "Região"
  ) +
  theme_minimal() + 
  ggtitle("Relação entre Cobertura de Água e Internações por 100 Mil Habitantes") +
  theme(
    text = element_text(size = 12), 
    plot.title = element_text(hjust = 0.5, size = 12), 
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 12),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.position = "bottom"
  )

#################################################################################
# REGRESSIONS
#################################################################################

# impact of investment over number of hospitalizations 
ols_internacoes_agua <- lm(variacao_internacoes ~ investimento_media_movel_agua + variacao_populacao + as.factor(uf), data = df)
summary(ols_internacoes_agua) # R$1MM -> 0.18 less hospitalizations

ols_internacoes_esgoto <- lm(variacao_internacoes ~ investimento_media_movel_esgoto + variacao_populacao + as.factor(uf), data = df)
summary(ols_internacoes_esgoto) # no effect


# impact of investment over hospitalized costs 
ols_custo_internacoes_agua <- lm(variacao_custo_internacao ~ investimento_media_movel_agua + variacao_populacao + as.factor(uf), data = df)
summary(ols_custo_internacoes_agua)

ols_custo_internacoes_esgoto <- lm(variacao_custo_internacao ~ investimento_media_movel_esgoto + variacao_populacao + as.factor(uf), data = df)
summary(ols_custo_internacoes_esgoto)


# impact of investment over hospitalization duration
ols_dias_permanencia_agua <- lm(variacao_dias_permanencia ~ investimento_media_movel_agua + variacao_populacao + as.factor(uf), data = df)
summary(ols_dias_permanencia_agua)

ols_dias_permanencia_esgoto <- lm(variacao_dias_permanencia ~ investimento_media_movel_esgoto + variacao_populacao + as.factor(uf), data = df)
summary(ols_dias_permanencia_esgoto)

stargazer(ols_internacoes_agua, 
          ols_internacoes_esgoto)
          
          
stargazer(ols_internacoes_agua, ols_internacoes_esgoto, type = "text",
          title = "Impact of Water and Sewage Investment on Hospitalization",
          omit = c("uf", "Constant"),
          covariate.labels = c("Water Investment", "Sewage Investment", "Population"),
          dep.var.labels.include = F,
          dep.var.caption = "Variation in Water and Sewage Investments",
          add.lines = list(c("UF Dummies?", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes")))

stargazer(ols_dias_permanencia_agua, ols_dias_permanencia_esgoto, type = "text",
          title = "Impact of Water and Sewage Investment on Hospitalization Duration",
          omit = c("uf", "Constant"),
          covariate.labels = c("Water Investment", "Sewage Investment", "Population"),
          dep.var.labels.include = F,
          dep.var.caption = "Variation in Water and Sewage Investments",
          add.lines = list(c("UF Dummies?", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes")))


# impact of coverage over number of hospitalizations 
ols_cobertura_int_agua <- lm(variacao_internacoes ~ variacao_cobertura_agua + variacao_populacao + as.factor(uf), data = df)
summary(ols_cobertura_int_agua)

ols_cobertura_int_esgoto <- lm(variacao_internacoes ~ variacao_cobertura_esgoto +
                                 variacao_populacao + as.factor(uf), data = df)
summary(ols_cobertura_int_esgoto)



stargazer(ols_cobertura_int_agua, ols_cobertura_int_esgoto, type = "text",
          title = "Impact of Water and Sewage Coverage on Hospitalization",
          omit = c("uf", "Constant"),
          covariate.labels = c("Water Investment", "Sewage Investment", "Population"),
          dep.var.labels.include = F,
          dep.var.caption = "Variation in Water and Sewage Investments",
          add.lines = list(c("UF Dummies?", "Yes", "Yes")))


############################################################################
# ROBUSTNESS
############################################################################


ef <- plm(internacoes ~  cobertura_esgoto + cobertura_agua + pop_M,
          data = df, index = c("codigo_mun", "ano"), model = "within")
summary(ef)
