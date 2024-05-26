rm(list = ls())
library(tidyverse)
library(plm)
library(readr)
library(PNSIBGE)
library(mfx)
library(stargazer)

df_snis <- readxl::read_xlsx("./processed data/snis_tratada_2014_2022.xlsx")

# Regression analysis

ols_water <- lm(variacao_economias_ativas_agua ~ investimento_media_movel_agua,data = df_snis)
ols_water2 <- lm(variacao_economias_ativas_agua ~ investimento_media_movel_agua + variacao_populacao, data = df_snis)
ols_water3 <- lm(variacao_economias_ativas_agua ~ investimento_media_movel_agua + variacao_populacao + as.factor(uf), data = df_snis)

ols_sewage <- lm(variacao_economias_ativas_esgoto ~ investimento_media_movel_esgoto, data = df_snis)
ols_sewage2 <- lm(variacao_economias_ativas_esgoto ~ investimento_media_movel_esgoto + variacao_populacao, data = df_snis)
ols_sewage3 <- lm(variacao_economias_ativas_esgoto ~ investimento_media_movel_esgoto + variacao_populacao + as.factor(uf), data = df_snis)

stargazer(ols_water, ols_water2, ols_water3, type = "text",
          title = "Impacto do Investimento em Água",
          omit = c("intercept", "uf", "Constant"),
          covariate.labels = c("Water Investment", "Population"),
          dep.var.labels.include = F,
          dep.var.caption = "Variation in Active Water Connections",
          add.lines = list(c("UF Dummies?", "No", "No", "Yes")))

stargazer(ols_sewage, ols_sewage2, ols_sewage3, type = "text",
          title = "Impacts of Sewage Investment on Active Connections",
          omit = c("intercept", "uf", "Constant"),
          covariate.labels = c("Sewage Investment", "Population"),
          dep.var.labels.include = F,
          dep.var.caption = "Variation in Active Sewage Connections",
          add.lines = list(c("UF Dummies?", "No", "No", "Yes")))
