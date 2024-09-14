rm(list = ls())

library(tidyverse) #sintaxe geral do código
library(readxl) # importação dos dados
library(easyanova)  # estatística
library(RColorBrewer) # modificar as cores
library(nortest) # lilliefors test
library(gt)   # tabelas bonitas
library(DescTools) # teste de Dunnettlibrary(tidyverse)

#### Load the data ---------------
df_incidencia_A <- read_excel("dados/Contagem de Pantas Atacadas/df_incidencia_A") |> 
  mutate_if(is.character, as.factor) |> 
  mutate(Dias = fct_relevel(Dias, "0DAA", "3DAA", "7DAA", "3DAB", "7DAB"),
         Adjuvante = fct_relevel(Adjuvante, "NA", "Aureo", "Ochima", "Silwet"),
         Taxa = fct_relevel(Taxa, "NA", "10", "30", "120"))

#### Transforming the data ----------------
## Retirando o controle para não afetar no desenho fatorial 3x4+1
inc_sem_c <- df_incidencia_A |> 
  filter(!(Tratamento == 1)) |> 
  select(!Tratamento)


### Gráfico para verificar a INCIDÊNCIA ao longo dos dias -------------------
## Transformando o tempo em contínuo
df_geral_inc_A <- df_incidencia_A |> 
  mutate(Dias = as.character(Dias),
         Dias = recode(Dias, 
                       "0DAA" = "0",
                       "3DAA" = "3",
                       "7DAA" = "7", 
                       "3DAB" = "10",
                       "7DAB" = "14"),
         Dias = as.numeric(Dias))

# visão geral dos dados com todos os dias
df_geral_inc_A |> 
  ggplot(aes(x = Dias, y = Incidencia_percent, color = Adjuvante))+
  geom_point() +
  geom_jitter(width = 0.4, height = 0.2)+
  scale_x_continuous(breaks = c(0, 3, 7, 10, 14))+
  # scale_y_continuous(breaks = c(0,2,4,6,8,10))+
  geom_smooth(method = "glm", method.args = list(family="poisson"), se=FALSE)+
  facet_wrap(~Taxa)+
  labs(title="Efeito da Taxa de Aplicação e Adjuvante na Incidência ao longo do tempo",
       x="Tempo",
       y="Incidência (%)") 
### Gráficos para visualização dos dados: INCIDÊNCIA ----------------

## Boxplot do adjuvante e taxa
df_incidencia_A |> 
  ggplot(aes(x = Adjuvante, y = Incidencia_percent, fill = Taxa))+
  geom_boxplot(position = position_dodge(width = 0.8))+
  geom_point(position = position_dodge(width = 0.8))

## Boxplot dos tratamentos
# geral
BP_g_INC_trat <- df_incidencia_A |> 
  ggplot(aes(x = reorder(Tratamento, -Incidencia_percent), y = Incidencia_percent, fill = Tratamento))+
  geom_boxplot()+
  geom_point()+
  labs(
    title = "Todos os dias",
    x = "Tratamentos",
    y = "Incidência (%)")+
  facet_wrap(~Dias)
BP_g_INC_trat

# 0DAA
BP_d0A_INC_trat <- df_incidencia_A |> 
  filter(Dias == "0DAA") |> 
  ggplot(aes(x = reorder(Tratamento, -Incidencia_percent), y = Incidencia_percent, fill = Tratamento))+
  geom_boxplot()+
  geom_point()+
  labs(
    title = "0DAA",
    x = "Tratamentos",
    y = "Incidência (%)")
BP_d0A_INC_trat

# 3DAA
BP_d3A_INC_trat <- df_incidencia_A |> 
  filter(Dias == "3DAA") |> 
  ggplot(aes(x = reorder(Tratamento, -Incidencia_percent), y = Incidencia_percent, fill = Tratamento))+
  geom_boxplot()+
  geom_point()+
  labs(
    title = "3DAA",
    x = "Tratamentos",
    y = "Incidência (%)")
BP_d3A_INC_trat

# 7DAA
BP_d7A_INC_trat <- df_incidencia_A |> 
  filter(Dias == "7DAA") |> 
  ggplot(aes(x = reorder(Tratamento, -Incidencia_percent), y = Incidencia_percent, fill = Tratamento))+
  geom_boxplot()+
  geom_point()+
  labs(
    title = "7DAA",
    x = "Tratamentos",
    y = "Incidência (%)")
BP_d7A_INC_trat

# 3DAB
BP_d3B_INC_trat <- df_incidencia_A |> 
  filter(Dias == "3DAB") |> 
  ggplot(aes(x = reorder(Tratamento, -Incidencia_percent), y = Incidencia_percent, fill = Tratamento))+
  geom_boxplot()+
  geom_point()+
  labs(
    title = "3DAB",
    x = "Tratamentos",
    y = "Incidência (%)")
BP_d3B_INC_trat

# 7DAB
BP_d7B_INC_trat <- df_incidencia_A |> 
  filter(Dias == "7DAB") |> 
  ggplot(aes(x = reorder(Tratamento, -Incidencia_percent), y = Incidencia_percent, fill = Tratamento))+
  geom_boxplot()+
  geom_point()+
  labs(
    title = "7DAB",
    x = "Tratamentos",
    y = "Incidência (%)")
BP_d7B_INC_trat

#
### ESTATÍSTICA ------

## Dia d0A -------------

### Anova SEM testemunha
Inc_d0A_sem_c_anova <- inc_sem_c |> 
  filter(Dias == "0DAA") |>
  select(Adjuvante, Taxa, Bloco, Incidencia_percent) |> 
  easyanova::ea2(design = 2, plot = 2)
Inc_d0A_sem_c_anova
## Resultados:
Inc_d0A_sem_c_anova[[10]]$`residual analysis`
# Normalidade do resíduo: p> 0.05
# homocedasticidade da variância: p>0.05

Inc_d0A_sem_c_anova[[1]]
## Tabela bonita da anova 
data.1_0A <- as.data.frame(Inc_d0A_sem_c_anova[[1]])
tratamento_0A.inc <- c("Adjuvante", "Taxa", "Blocos ", "Ajuvante:Taxa", "Resíduos " )
data.1_0A<- cbind( Tratamento  = tratamento_0A.inc, data.1_0A)
data.2_0A <- data.1_0A |> 
  gt() |> 
  tab_header(
  title = md("***Anova da Incidência***"), 
  subtitle = md("**Antes da primeira aplicação (0DAA)**")) |> 
  tab_source_note(source_note = md( "**valor de  α = 0.05** ")) |> 
  tab_options(table.width = pct(100)) 
data.2_0A
# Não há diferença significativa entre os níveis do fator.
# HÁ SIM diferença significativa entre blocos
# Não há interação entre os fatores

### Anova COM testemunha
BP_d0A_INC_trat
# comparando os tratamentos entre si
Inc_d0A_COM_c_anova <- df_incidencia_A |> 
  filter(Dias == "0DAA") |> 
  select(Tratamento, Bloco, Incidencia_percent) |> 
  easyanova::ea1(design = 2, plot = 3)
Inc_d0A_COM_c_anova
## Resultados:
Inc_d0A_COM_c_anova[[4]]$`residual analysis`

lillie.test(Inc_d0A_COM_c_anova$`Residual analysis`$residuals)
# Normalidade do resíduo: p> 0.05 (LILLIEFORS)

Inc_d0A_COM_c_anova[[1]]

# Os tratamentos não diferem estatisticamente entre si!




## Dia d3A ------------
### Anova SEM testemunha
Inc_d3A_sem_c_anova <- inc_sem_c |> 
  filter(Dias == "3DAA") |>
  select(Adjuvante, Taxa, Bloco, Incidencia_percent) |> 
  easyanova::ea2(design = 2, plot = 2)
Inc_d3A_sem_c_anova
## Resultados:
Inc_d3A_sem_c_anova[[10]]$`residual analysis`

lillie.test(Inc_d3A_sem_c_anova$`Residual analysis`$residuals)
# Normalidade do resíduo: p> 0.05 (LILLIEFORS)
# homocedasticidade da variância: p>0.05

Inc_d3A_sem_c_anova[[1]]
## Tabela bonita da anova 
## Tabela bonita da anova 

data.1_3A <- as.data.frame(Inc_d3A_sem_c_anova[[1]])
tratamento_3A.inc <- c("Adjuvante", "Taxa", "Blocos ", "Ajuvante:Taxa", "Resíduos " )
data.1_3A<- cbind( Tratamento  = tratamento_3A.inc, data.1_3A)
data.2_3A <- data.1_3A |> 
  gt() |> 
  tab_header(
    title = md("***Anova da Incidência***"), 
    subtitle = md("**Três dias após a primeira aplicação (3DAA)**")) |> 
  tab_source_note(source_note = md( "**valor de  α = 0.05** ")) |> 
  tab_options(table.width = pct(100)) 
data.2_3A

# Não há diferença significativa entre os níveis do fator.
# HÁ SIM diferença significativa entre blocos
# Não há interação entre os fatores

### Anova COM testemunha
BP_d3A_INC_trat
# comparando os tratamentos entre si
Inc_d3A_COM_c_anova <- df_incidencia_A |> 
  filter(Dias == "3DAA") |> 
  select(Tratamento, Bloco, Incidencia_percent) |> 
  easyanova::ea1(design = 2, plot = 3)
Inc_d3A_COM_c_anova
## Resultados:
Inc_d3A_COM_c_anova[[4]]$`residual analysis`
lillie.test(Inc_d3A_COM_c_anova$`Residual analysis`$residuals)

Inc_d3A_COM_c_anova[[1]]

# Os tratamentos não diferem estatisticamente entre si!



## Dia d7A ------------

### Anova SEM testemunha
Inc_d7A_sem_c_anova <- inc_sem_c |> 
  filter(Dias == "7DAA") |>
  select(Adjuvante, Taxa, Bloco, Incidencia_percent) |> 
  easyanova::ea2(design = 2, plot = 3)
Inc_d7A_sem_c_anova
## Resultados:
Inc_d7A_sem_c_anova[[10]]$`residual analysis`

lillie.test(Inc_d7A_sem_c_anova$`Residual analysis`$residuals)
# Normalidade do resíduo: p = 0.32 (LILLIEFORS) - consider-se normal!
# homocedasticidade da variância: p>0.05

Inc_d7A_sem_c_anova[[1]]
## Tabela bonita da anova 
## Tabela bonita da anova 

data.1_7A <- as.data.frame(Inc_d7A_sem_c_anova[[1]])
tratamento_7A.inc <- c("Adjuvante", "Taxa", "Blocos ", "Ajuvante:Taxa", "Resíduos " )
data.1_7A<- cbind( Tratamento  = tratamento_7A.inc, data.1_7A)
data.2_7A <- data.1_7A |> 
  gt() |> 
  tab_header(
    title = md("***Anova da Incidência***"), 
    subtitle = md("**Sete dias após a primeira aplicação (7DAA)**")) |> 
  tab_source_note(source_note = md( "**valor de  α = 0.05** ")) |> 
  tab_options(table.width = pct(100)) 
data.2_7A

# Não há diferença significativa entre os níveis do fator.
# HÁ SIM diferença significativa entre blocos
# Não há interação entre os fatores

### Anova COM testemunha
BP_d7A_INC_trat
# comparando os tratamentos entre si
Inc_d7A_COM_c_anova <- df_incidencia_A |> 
  filter(Dias == "7DAA") |> 
  select(Tratamento, Bloco, Incidencia_percent) |> 
  easyanova::ea1(design = 2, plot = 2)
Inc_d7A_COM_c_anova
## Resultados:
Inc_d7A_COM_c_anova[[4]]$`residual analysis`

lillie.test(Inc_d7A_COM_c_anova$`Residual analysis`$residuals)

Inc_d7A_COM_c_anova[[1]]

# Os tratamentos não diferem estatisticamente entre si!




## Dia d3B ------------

### Anova SEM testemunha
Inc_d3B_sem_c_anova <- inc_sem_c |> 
  filter(Dias == "3DAB") |>
  select(Adjuvante, Taxa, Bloco, Incidencia_percent) |> 
  easyanova::ea2(design = 2, plot = 3)
Inc_d3B_sem_c_anova
## Resultados:
Inc_d3B_sem_c_anova[[10]]$`residual analysis`

lillie.test(Inc_d3B_sem_c_anova$`Residual analysis`$residuals)
# Normalidade do resíduo: p = 0.32 (LILLIEFORS) - consider-se normal!
# homocedasticidade da variância: p>0.05

Inc_d3B_sem_c_anova[[1]]
## Tabela bonita da anova 
## Tabela bonita da anova 

data.1_3B <- as.data.frame(Inc_d3B_sem_c_anova[[1]])
tratamento_3B.inc <- c("Adjuvante", "Taxa", "Blocos ", "Ajuvante:Taxa", "Resíduos " )
data.1_3B<- cbind( Tratamento  = tratamento_3B.inc, data.1_3B)
data.2_3B <- data.1_3B |> 
  gt() |> 
  tab_header(
    title = md("***Anova da Incidência***"), 
    subtitle = md("**Três dias após a segunda aplicação (3DAB)**")) |> 
  tab_source_note(source_note = md( "**valor de  α = 0.05** ")) |> 
  tab_options(table.width = pct(100)) 
data.2_3B

# Não há diferença significativa entre os níveis do fator.
# HÁ SIM diferença significativa entre blocos
# Não há interação entre os fatores

### Anova COM testemunha
BP_d3B_INC_trat
# comparando os tratamentos entre si
Inc_d3B_COM_c_anova <- df_incidencia_A |> 
  filter(Dias == "3DAB") |> 
  select(Tratamento, Bloco, Incidencia_percent) |> 
  easyanova::ea1(design = 2, plot = 2)
Inc_d3B_COM_c_anova
## Resultados:
Inc_d3B_COM_c_anova[[4]]$`residual analysis`

lillie.test(Inc_d3B_COM_c_anova$`Residual analysis`$residuals)

Inc_d3B_COM_c_anova[[1]]

# Os tratamentos diferem SIM estatisticamente entre si!


## Dia d7B ------------

### Anova SEM testemunha
Inc_d7B_sem_c_anova <- inc_sem_c |> 
  filter(Dias == "7DAB") |>
  select(Adjuvante, Taxa, Bloco, Incidencia_percent) |> 
  easyanova::ea2(design = 2, plot = 3)
Inc_d7B_sem_c_anova
## Resultados:
Inc_d7B_sem_c_anova[[10]]$`residual analysis`

lillie.test(Inc_d7B_sem_c_anova$`Residual analysis`$residuals)
# Normalidade do resíduo: p = 0.32 (LILLIEFORS) - consider-se normal!
# homocedasticidade da variância: p>0.05

Inc_d7B_sem_c_anova[[1]]
## Tabela bonita da anova 
## Tabela bonita da anova 

data.1_7B <- as.data.frame(Inc_d7B_sem_c_anova[[1]])
tratamento_7B.inc <- c("Adjuvante", "Taxa", "Blocos ", "Ajuvante:Taxa", "Resíduos " )
data.1_7B<- cbind( Tratamento  = tratamento_7B.inc, data.1_7B)
data.2_7B <- data.1_7B |> 
  gt() |> 
  tab_header(
    title = md("***Anova da Incidência***"), 
    subtitle = md("**Sete dias após a segunda aplicação (7DAB)**")) |> 
  tab_source_note(source_note = md( "**valor de  α = 0.05** ")) |> 
  tab_options(table.width = pct(100)) 
data.2_7B

# Não há diferença significativa entre os níveis do fator.
# HÁ SIM diferença significativa entre blocos
# Não há interação entre os fatores

### Anova COM testemunha
BP_d7B_INC_trat
# comparando os tratamentos entre si
Inc_d7B_COM_c_anova <- df_incidencia_A |> 
  filter(Dias == "7DAB") |> 
  select(Tratamento, Bloco, Incidencia_percent) |> 
  easyanova::ea1(design = 2, plot = 2)
Inc_d7B_COM_c_anova
## Resultados:
Inc_d7B_COM_c_anova[[4]]$`residual analysis`

lillie.test(Inc_d7B_COM_c_anova$`Residual analysis`$residuals)

Inc_d7B_COM_c_anova[[1]]

# Os tratamentos NÃO diferem estatisticamente entre si!