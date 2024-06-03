rm(list = ls())
library(tidyverse)
library(plm)
library(readr)
library(PNSIBGE)
library(mfx)
library(readxl)

df <- read_csv("./processed data/pns_tratada.csv")


ols_all <- lm(acamado_causa_hidrica ~ rede_esgoto + agua_canalizada +
                tem_rede_agua + educ + trab + idade +
                homem, data = df_pns_hidrica)
summary(ols_all)

ols_all_old <- lm(acamado_causa_hidrica ~ rede_esgoto + agua_canalizada + educ + trab +
                    homem, data = df_pns_hidrica)
summary(ols_all_old)

ols_all_afastado <- lm(afastado_causa_hidrica ~ rede_esgoto + educ + trab +
                         homem, data = df_pns_hidrica)
summary(ols_all_afastado)

ols_norte <- lm(acamado_causa_hidrica ~ rede_esgoto + agua_canalizada + educ + trab +
                  homem, data = df_pns_hidrica %>% filter(regiao == "Norte"))
summary(ols_norte)

ols_nordeste <- lm(acamado_causa_hidrica ~ rede_esgoto + agua_canalizada + educ + trab +
                     homem, data = df_pns_hidrica %>% filter(regiao == "Nordeste"))
summary(ols_nordeste)

ols_sul <- lm(acamado_causa_hidrica ~ rede_esgoto + agua_canalizada + educ + trab +
                homem, data = df_pns_hidrica %>% filter(regiao == "Sul"))
summary(ols_sul)

ols_sudeste <- lm(acamado_causa_hidrica ~ rede_esgoto + agua_canalizada + educ + trab +
                    homem, data = df_pns_hidrica %>% filter(regiao == "Sudeste"))
summary(ols_sudeste)

ols_centro_oeste <- lm(acamado_causa_hidrica ~ rede_esgoto + agua_canalizada + educ + trab +
                         homem, data = df_pns_hidrica %>% filter(regiao == "Centro-Oeste"))
summary(ols_centro_oeste)


0.0006689*1000000*26

logit <- glm(acamado_causa_hidrica ~ rede_esgoto + agua_canalizada +
               tem_rede_agua + educ + trab + idade +
               homem, family = binomial(link="logit"),
             data = df_pns_hidrica)
summary(logit)

logitmfx(acamado_causa_hidrica ~ rede_esgoto + agua_canalizada +
           tem_rede_agua + educ + trab + idade +
           homem,data = df_pns_hidrica )

probitmfx(acamado_causa_hidrica ~ rede_esgoto + agua_canalizada +
            tem_rede_agua + educ + trab + idade +
            homem,data = df_pns_hidrica )


