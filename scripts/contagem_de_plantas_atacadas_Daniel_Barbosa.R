rm(list = ls())
ls()

library(tidyverse)
library(readxl)

dadosA <- read_excel("dados/Contagem de Pantas Atacadas/acont.xlsx")

df <- dadosA |> 
  select(!c(Taxa, Adjuvante)) |> 
  mutate(Tratamento = factor(Tratamento),
         Bloco = factor(Bloco)) |> 
  pivot_longer(names_to = "dias",
               values_to = "contagem",
               cols = "0DAA":"7DAB") |> 
  mutate(dias = recode(dias, 
                      "0DAA" = "0",
                      "3DAA" = "3",
                      "7DAA" = "7", 
                      "3DAB" = "10",
                      "7DAB" = "14"),
         dias = as.numeric(dias))


df |>
  filter(Tratamento %in% c(1:2)) |> 
  ggplot(aes(x = dias, y = contagem, color = Tratamento, shape = Bloco))+
  geom_smooth(method = "lm")+
  geom_point()+
  scale_x_continuous(breaks = df$dias)
