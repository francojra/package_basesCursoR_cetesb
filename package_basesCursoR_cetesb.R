# Base de dados - Curso R ------------------------------------------------------------------------------------------------------------------
# Autoria do script: Jeanne Franco ---------------------------------------------------------------------------------------------------------
# Data: 03/07/22 ---------------------------------------------------------------------------------------------------------------------------

# Carregar pacotes ----------------------------------------------------------------------------------------------------------------------------

library(basesCursoR)
library(dplyr)
library(ggplot2)
library(magrittr)

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
  filter(poluente == "O3") 
View(cetesb1)  

# Análises ---------------------------------------------------------------------------------------------------------------------------------

cetesb2 <- cetesb1 %>%
  group_by(estado, semanaEpi) %>%
  summarise(media = mean(casosAcumulado))

ggplot(covid2, aes(x = semanaEpi, y = media, 
                   col = estado, group = estado)) +
  geom_point() +
  geom_line()
