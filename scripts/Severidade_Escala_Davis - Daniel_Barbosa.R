rm(list = ls())

library(tidyverse)
library(readxl)
library(easyanova)
library(RColorBrewer)

df_A_Davis <- read_excel("dados/Escala Davis/df_A_Davis.xlsx")

### ESTATÍSTICA --------------------------

## Reduzindo a severidade das 25 plantas para uma média por bloco
df_media_A_Davis <- df_A_Davis |> 
  mutate(Taxa = as.factor(Taxa)) |> 
  # filter(dias == "3DAA") |>
  # filter(dias == "7DAA") |>
  filter(dias == "3DAB") |>
  # filter(dias == "7DAB") |>
  group_by(Adjuvante, Taxa, Bloco, Tratamento) |>
  # summarise(Severidade_sum = sum(Severidade)) |>
  summarise(Severidade_mean = mean(Severidade)) |>
  select(Adjuvante, Taxa, Bloco, Severidade_mean, Tratamento)

## Retirando o controle para não afetar no desenho fatorial 3x4+1
df_sem_controle_media <- df_media_A_Davis |> 
  dplyr::filter(!(Adjuvante == "NA" & Taxa == 0)) |> 
  select(!Tratamento)

## df com apenas controle
df_apenas_controle_media <- df_media_A_Davis |> 
  dplyr::filter(Adjuvante == "NA" & Taxa == 0)

## ANOVA
# Aplicando o teste estatístico nos fatores
easyanova::ea2(data = df_sem_controle_media, design = 2)

# CV = 58% !!!! Como faço para reduzir???


### Gráficos para visualização dos dados  ----------------
## Comparando valores entre blocos
df_media_A_Davis |> 
  ggplot(aes(x  = Taxa, y = Severidade_mean)) +
  geom_point(aes(fill = Bloco), shape = 21, size = 3) +
  # geom_jitter(aes(fill = Bloco), shape = 21, size = 3) +
  facet_grid(cols = vars(Adjuvante)) +
  scale_fill_manual(values = brewer.pal(4, "Set1")) +
  scale_color_manual(values = brewer.pal(4, "Set1")) 

## distribuição dos dados
df_media_A_Davis |>
  ggplot(aes(x = Severidade_mean))+
  geom_histogram()

## Boxplot do adjuvante e taxa
df_media_A_Davis |> 
  ggplot(aes(x = Adjuvante, y = Severidade_mean, fill = Taxa))+
  geom_boxplot(position = position_dodge(width = 0.8))+
  geom_point(position = position_dodge(width = 0.8))


### Gráfico para verificar aseveridade ao longo dos dias -------------------
# 
df_geral_A_Davis <- df_A_Davis |> 
  mutate(dias = as.character(dias),
         dias = recode(dias, 
                       "3DAA" = "3",
                       "7DAA" = "7", 
                       "3DAB" = "10",
                       "7DAB" = "14"),
         dias = as.numeric(dias))

# histograma (distribuição dos dados)
df_geral_A_Davis |> 
  ggplot(aes(x = Severidade))+
  geom_histogram()

# visãp geral dos dados com todos os dias
df_geral_A_Davis |> 
  ggplot(aes(x = dias, y = Severidade, color = Adjuvante))+
  geom_point() +
  geom_jitter(width = 0.4, height = 0.2)+
  scale_x_continuous(breaks = c(3, 7, 10, 14))+
  scale_y_continuous(breaks = c(0,2,4,6,8,10))+
  geom_smooth(method = "glm", method.args = list(family="poisson"), se=FALSE)+
  facet_wrap(~Taxa)+
  labs(title="Efeito da Taxa de Aplicação e Adjuvante na Severidade ao longo do tempo",
       x="Tempo",
       y="Severidade dos Danos (Escala Davis)") 






