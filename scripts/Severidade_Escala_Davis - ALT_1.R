rm(list = ls())

library(tidyverse)
library(readxl)
library(easyanova)
library(RColorBrewer)
library(nortest)
dir()
df_A_Davis <- read.table( "df_A_Davis.txt", h= TRUE)

### ESTATÍSTICA --------------------------

## Reduzindo a severidade das 25 plantas para uma média por bloco
df_media_A_Davis <- df_A_Davis |> 
  mutate(Taxa = as.factor(Taxa), Adjuvante = as.factor(Adjuvante), Bloco = as.factor(Bloco), planta = as.factor(planta), Tratamento = as.factor(Tratamento), dias = as.factor(dias)) |> 
  filter(dias == "3DAA") |>
  # filter(dias == "7DAA") |> 
  # filter(dias == "3DAB") |>
  # filter(dias == "7DAB") |>
  group_by(Adjuvante, Taxa, Bloco, Tratamento) |>
  # summarise(Severidade_mean = sum(Severidade)) |>
  summarise(Severidade_mean = mean(Severidade)) |>
  select(Adjuvante, Taxa, Bloco, Severidade_mean, Tratamento )
attach(df_media_A_Davis)

## Retirando o controle para não afetar no desenho fatorial 3x4+1
df_sem_controle_media <- df_media_A_Davis |> 
  dplyr::filter(!(Adjuvante == "TA" & Taxa == 0)) |> 
  select(!Tratamento)

## df com apenas controle
df_apenas_controle_media <- df_media_A_Davis |> 
  dplyr::filter(Adjuvante == "TA" | Taxa == 0)


## ANOVA
# Aplicando o teste estatístico nos fatores
easyanova::ea2(data = df_sem_controle_media, design = 2)
#Aplicando o teste estatístico com a testemunha 
easyanova::ea2(data = df_media_A_Davis, design = 2)


# CV = 58% !!!! Como faço para reduzir???
# Deu normalidade, pois p> 0.05
#Homogeneidade da variância pois p>0.05



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



#7DAA

df_A_Davis<- read.table( "df_A_Davis.txt", h= TRUE)


df_media_A_Davis2 <- df_A_Davis1 |> 
  mutate(Taxa = as.factor(Taxa), Adjuvante = as.factor(Adjuvante), Bloco = as.factor(Bloco)) |> 
  filter(dias == "7DAA") |>
  # filter(dias == "7DAA") |> 
  # filter(dias == "3DAB") |>
  # filter(dias == "7DAB") |>
  group_by(Adjuvante, Taxa, Bloco, Tratamento) |>
  # summarise(Severidade_mean = sum(Severidade)) |>
  summarise(Severidade_mean = mean(Severidade)) |>
  select(Adjuvante, Taxa, Bloco, Severidade_mean, Tratamento )

## Retirando o controle para não afetar no desenho fatorial 3x4+1
df_sem_controle_media1 <- df_media_A_Davis2 |> 
  dplyr::filter(!(Adjuvante == "NA" & Taxa == 0)) |> 
  select(!Tratamento)

## df com apenas controle
df_apenas_controle_media2 <- df_media_A_Davis2 |> 
  dplyr::filter(Adjuvante == "NA" | Taxa == 0)

## ANOVA
# Aplicando o teste estatístico nos fatores
easyanova::ea2(data = df_sem_controle_media1, design = 2)
# Homogeneidade de variância P>0.05
#Independência P>0.05
easyanova::ea2(data = df_media_A_Davis2, design = 2)# não plota com controle


#3DAB

## Reduzindo a severidade das 25 plantas para uma média por bloco
df_media_A_Davis3 <- df_A_Davis |> 
  mutate(Taxa = as.factor(Taxa), Adjuvante = as.factor(Adjuvante), Bloco = as.factor(Bloco)) |> 
  filter(dias == "3DAB") |>
  group_by(Adjuvante, Taxa, Bloco, Tratamento) |>
  # summarise(Severidade_mean = sum(Severidade)) |>
  summarise(Severidade_mean = mean(Severidade)) |>
  select(Adjuvante, Taxa, Bloco, Severidade_mean, Tratamento )


## Retirando o controle para não afetar no desenho fatorial 3x4+1
df_sem_controle_media3 <- df_media_A_Davis3 |> 
  dplyr::filter(!(Adjuvante == "NA" & Taxa == 0)) |> 
  select(!Tratamento)


## df com apenas controle
df_apenas_controle_media <- df_media_A_Davis3 |> 
  dplyr::filter(Adjuvante == "NA" | Taxa == 0)


## ANOVA
# Aplicando o teste estatístico nos fatores
easyanova::ea2(data = df_sem_controle_media3, design = 2)
#Aplicando o teste estatístico com a testemunha 

# homogeneidade da variância,  pvalor >0.05
# Normalidade, p valor>0.02
easyanova::ea2(data = df_media_A_Davis3, design = 2)# não plota com controle






#7 DAB
df_A_Davis<- read.table( "df_A_Davis.txt", h= TRUE)
## Reduzindo a severidade das 25 plantas para uma média por bloco

df_media_A_Davis4 <- df_A_Davis |> 
  mutate(Taxa = as.factor(Taxa), Adjuvante = as.factor(Adjuvante), Bloco = as.factor(Bloco)) |> 
  group_by(Adjuvante, Taxa, Bloco, Tratamento) |>
  # summarise(Severidade_mean = sum(Severidade)) |>
  summarise(Severidade_mean = mean(Severidade)) |>
  select(Adjuvante, Taxa, Bloco, Severidade_mean, Tratamento )

## Retirando o controle para não afetar no desenho fatorial 3x4+1

df_sem_controle_media4 <- df_media_A_Davis4 |> 
  dplyr::filter(!(Adjuvante == "NA" & Taxa == 0)) |> 
  select(!Tratamento)


## ANOVA
# Aplicando o teste estatístico nos fatores
easyanova::ea2(data = df_sem_controle_media4, design = 2)
#Aplicando o teste estatístico com a testemunha 





