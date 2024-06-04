rm(list = ls())
library(tidyverse)
library(plm)
library(readr)
library(PNSIBGE)
library(mfx)
library(readxl)

df <- read_csv("./processed data/pns_tratada.csv")

#################################################################################
# PLOTS
#################################################################################

prop_all <- df %>% filter(!is.na(rede_esgoto)) %>% 
  group_by(rede_esgoto) %>% 
  summarise(n = n()) %>% 
  mutate(prop_all = n/sum(n))

prop_sick <- df %>% filter(!is.na(rede_esgoto)) %>%
  filter(acamado_causa_hidrica == 1) %>% 
  group_by(rede_esgoto) %>% 
  summarise(n = n()) %>% 
  mutate(prop_sick = n/sum(n))

prop_all %>% left_join(prop_sick, by = "rede_esgoto") %>% 
  pivot_longer(cols = starts_with("prop"),
               names_to = "Grupo",
               values_to = "Proporcao") %>% 
  ggplot(aes(x = factor(rede_esgoto,
                        labels = c("Sem Saneamento", "Com Saneamento")),
             y = Proporcao, fill = Grupo)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.6) +
  scale_fill_brewer(palette = "Dark2") +
  labs(x = "Saneamento Básico", y = "Proporção", fill = "Grupo",
       title = "Proporção de pessoas com e sem saneamento, geral e acamados") +
  theme_minimal()
  
ggsave("./figs/prob_of_sick_sanitation.png")  
#################################################################################
# REGRESSIONS
#################################################################################


ols_all <- lm(acamado_causa_hidrica ~ rede_esgoto + agua_canalizada +
                tem_rede_agua + educ + trab + idade +
                homem, data = df)
summary(ols_all)

ols_all_old <- lm(acamado_causa_hidrica ~ rede_esgoto + agua_canalizada + educ + trab +
                    homem, data = df)
summary(ols_all_old)

ols_all_afastado <- lm(afastado_causa_hidrica ~ rede_esgoto + educ + trab +
                         homem, data = df)
summary(ols_all_afastado)

ols_norte <- lm(acamado_causa_hidrica ~ rede_esgoto + agua_canalizada + educ + trab +
                  homem, data = df %>% filter(regiao == "Norte"))
summary(ols_norte)

ols_nordeste <- lm(acamado_causa_hidrica ~ rede_esgoto + agua_canalizada + educ + trab +
                     homem, data = df %>% filter(regiao == "Nordeste"))
summary(ols_nordeste)

ols_sul <- lm(acamado_causa_hidrica ~ rede_esgoto + agua_canalizada + educ + trab +
                homem, data = df %>% filter(regiao == "Sul"))
summary(ols_sul)

ols_sudeste <- lm(acamado_causa_hidrica ~ rede_esgoto + agua_canalizada + educ + trab +
                    homem, data = df %>% filter(regiao == "Sudeste"))
summary(ols_sudeste)

ols_centro_oeste <- lm(acamado_causa_hidrica ~ rede_esgoto + agua_canalizada + educ + trab +
                         homem, data = df %>% filter(regiao == "Centro-Oeste"))
summary(ols_centro_oeste)


#################################################################################
# ROBUSTNESS
#################################################################################

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



logit <- glm(acamado_causa_hidrica ~ rede_esgoto + agua_canalizada +
               tem_rede_agua + educ + trab + idade +
               homem, family = binomial(link="logit"))


logitmfx(acamado_causa_hidrica ~ rede_esgoto + agua_canalizada +
           tem_rede_agua + educ + trab + idade +
           homem,data = df )

probitmfx(acamado_causa_hidrica ~ rede_esgoto + agua_canalizada +
            tem_rede_agua + educ + trab + idade +
            homem,data = df )



