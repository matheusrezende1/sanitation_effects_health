rm(list = ls())
library(tidyverse)
library(plm)
library(readr)
library(PNSIBGE)
library(mfx)
library(stargazer)

df_snis <- readxl::read_xlsx("./processed data/snis_tratada_2014_2022.xlsx")
df_datasus <- readxl::read_xlsx("./processed data/sihsus_tratada.xlsx")

df <- df_snis %>% left_join(df_datasus, by = c("codigo_mun", "ano"))

df %>%
  mutate(internacoes_por_cem_mil = internacoes/(populacao_municipio/100000)) %>% 
  group_by(ano) %>%
  summarise(internacoes_por_cem_mil = mean(internacoes_por_cem_mil, na.rm = TRUE),
            investimento_esgoto = sum(investimento_proprio_esgoto, na.rm = TRUE)/1000000) %>% 
  ggplot(aes(x = investimento_esgoto, y = internacoes_por_cem_mil)) +
  geom_point(aes(color = as.factor(ano)), size = 3) +  # Adicionar pontos coloridos por ano
  geom_text(aes(label = ano), vjust = -1, hjust = 1.5, color = "black")  +
  labs(x = "Investimento em Esgoto",
       y = "Internações por Milhão",
       color = "Ano") +
  ylim(0,450) +
  theme_minimal() +  # Tema minimalista
  ggtitle("Relação entre Investimento em Esgoto e Internações por Milhão por Ano")

# Regressions

ols_internacoes_agua <- lm(variacao_internacoes ~ investimento_media_movel_agua + 
                             variacao_populacao + as.factor(uf), data = dfteste)
summary(ols_internacoes_agua) # 1 milhao, 0.18 menos internacoes
300*0.18*400


ols_internacoes_agua <- lm(variacao_internacoes ~ variacao_cobertura_agua +
                             variacao_populacao + as.factor(uf), data = dfteste)
summary(ols_internacoes_agua)

ols_internacoes_esgoto <- lm(variacao_internacoes ~ variacao_cobertura_esgoto +
                               variacao_populacao + as.factor(uf), data = dfteste)
summary(ols_internacoes_esgoto)

# impacto do investimento sobre custo internacao
ols_custo_internacoes_agua <- lm(variacao_custo_internacao ~ investimento_media_movel_agua + variacao_populacao + as.factor(uf), data = dfteste)
summary(ols_custo_internacoes_agua)

ols_custo_internacoes_esgoto <- lm(variacao_custo_internacao ~ investimento_media_movel_esgoto + variacao_populacao + as.factor(uf), data = dfteste)
summary(ols_custo_internacoes_esgoto)


# impacto do investimento sobre dias permanencia
ols_dias_permanencia_agua <- lm(variacao_dias_permanencia ~ investimento_media_movel_agua + variacao_populacao + as.factor(uf), data = dfteste)
summary(ols_dias_permanencia_agua)

ols_dias_permanencia_esgoto <- lm(variacao_dias_permanencia ~ investimento_media_movel_esgoto + variacao_populacao + as.factor(uf), data = dfteste)
summary(ols_dias_permanencia_esgoto)

# Fixed effects
ef <- plm(internacoes ~  cobertura_esgoto + cobertura_agua + pop_M,
          data = dfteste, index = c("codigo_mun", "ano"), model = "within")
summary(ef)
