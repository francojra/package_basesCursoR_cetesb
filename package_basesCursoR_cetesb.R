# Base de dados - Curso R ------------------------------------------------------------------------------------------------------------------
# Autoria do script: Jeanne Franco ---------------------------------------------------------------------------------------------------------
# Data: 03/07/22 ---------------------------------------------------------------------------------------------------------------------------

# Carregar pacotes ----------------------------------------------------------------------------------------------------------------------------

library(basesCursoR)
library(dplyr)
library(ggplot2)
library(magrittr)
library(tidyr)

# Identificar bases disponíveis ------------------------------------------------------------------------------------------------------------

basesCursoR::bases_disponiveis()

# Carregar base de dados -------------------------------------------------------------------------------------------------------------------

cetesb <- basesCursoR::pegar_base("cetesb")
View(cetesb)

# Selecionar dados -------------------------------------------------------------------------------------------------------------------------

cetesb1 <- cetesb %>%
  select(estacao_cetesb, poluente, concentracao) %>%
  filter(estacao_cetesb %in% c("Pinheiros", "Mooca", "Congonhas", "Osasco", 
                         "Ibirapuera", "Santo Amaro", "Santana", 
                       "Paque D.Pedro II")) %>%
  filter(poluente == "O3") %>%
  drop_na()
View(cetesb1)  
glimpse(cetesb1)
cetesb1$estacao_cetesb <- as.factor(cetesb1$estacao_cetesb)

# Análises ---------------------------------------------------------------------------------------------------------------------------------

cetesb2 <- cetesb1 %>%
  group_by(estacao_cetesb) %>%
  summarise(media = mean(concentracao), sd = sd(concentracao),
            n = n(), se = sd/sqrt(n)) 
View(cetesb2)

ggplot(cetesb1, aes(x = estacao_cetesb, y = concentracao)) +
  geom_col()
