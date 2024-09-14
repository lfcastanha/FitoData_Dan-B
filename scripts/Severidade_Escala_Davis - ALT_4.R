rm(list = ls())

library(tidyverse) #sintaxe geral do código
library(readxl) # importação dos dados
library(easyanova)  # estatística
library(RColorBrewer) # modificar as cores
library(nortest) # lilliefors test
library(gt)   # tabelas bonitas
library(DescTools) # teste de Dunnett


#### Load data --------------------------
df_A_Davis <- read_excel("dados/Escala Davis/df_A_Davis.xlsx") |> 
  mutate_if(is.character, as.factor) |> 
  mutate(Taxa = as.factor(Taxa))

#### Transformando os dados ----------------------------
## Reduzindo a severidade das 25 plantas para uma média por bloco
df_media_A_Davis <- df_A_Davis |> 
  group_by(Adjuvante, Taxa, Bloco, Tratamento, dias) |>
  summarise(Severidade_mean = mean(Severidade)) |>
  select(Adjuvante, Taxa, Bloco, Severidade_mean, Tratamento, dias) |> 
  ungroup()

## Retirando o controle para não afetar no desenho fatorial 3x4+1
df_sem_controle_media <- df_media_A_Davis |> 
  filter(!(Adjuvante == "NA" & Taxa == 0)) |> 
  select(!Tratamento)

####### VISUALIZAÇÃO -----------------
### Gráficos para visualização dos dados  ----------------

## distribuição dos dados
df_media_A_Davis |>
  ggplot(aes(x = Severidade_mean))+
  geom_histogram()

## Boxplot do adjuvante e taxa
df_media_A_Davis |> 
  ggplot(aes(x = Adjuvante, y = Severidade_mean, fill = Taxa))+
  geom_boxplot(position = position_dodge(width = 0.8))+
  geom_point(position = position_dodge(width = 0.8))

## Boxplot dos tratamentos
# geral
BP_geral_trat <- df_media_A_Davis |> 
  ggplot(aes(x = reorder(Tratamento, -Severidade_mean), y = Severidade_mean, fill = Tratamento))+
  geom_boxplot()+
  labs(
    title = "Todos os dias",
    x = "Tratamento",
    y = "Severidade média (Escala Davis)")+
  facet_wrap(~dias)
BP_geral_trat
# 3DAA
BP_3DAA_trat <- df_media_A_Davis |> 
  filter(dias == "3DAA") |> 
  ggplot(aes(x = reorder(Tratamento, -Severidade_mean), y = Severidade_mean, fill = Tratamento))+
  geom_boxplot()+
  labs(
    title = "3DAA",
    x = "Tratamento",
    y = "Severidade média (Escala Davis)")
BP_3DAA_trat
# 7DAA
BP_7DAA_trat <- df_media_A_Davis |> 
  filter(dias == "7DAA") |> 
  ggplot(aes(x = reorder(Tratamento, -Severidade_mean), y = Severidade_mean, fill = Tratamento))+
  geom_boxplot()+
  labs(
    title = "7DAA",
    x = "Tratamento",
    y = "Severidade média (Escala Davis)")
# 3DAB
BP_3DAB_trat <- df_media_A_Davis |> 
  filter(dias == "3DAB") |> 
  ggplot(aes(x = reorder(Tratamento, -Severidade_mean), y = Severidade_mean, fill = Tratamento))+
  geom_boxplot()+
  labs(
    title = "3DAB",
    x = "Tratamento",
    y = "Severidade média (Escala Davis)")
# 7DAB
BP_7DAB_trat <- df_media_A_Davis |> 
  filter(dias == "7DAB") |> 
  ggplot(aes(x = reorder(Tratamento, -Severidade_mean), y = Severidade_mean, fill = Tratamento))+
  geom_boxplot()+
  labs(
    title = "7DAB",
    x = "Tratamento",
    y = "Severidade média (Escala Davis)")

#
### Gráfico para verificar a severidade ao longo dos dias -------------------
## Transformando o tempo em contínuo
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
####### ESTATÍSTICA --------------------
#### 3DAA ------------------
### Anova SEM testemunha
anova_sc_3DAA <- df_sem_controle_media |> 
  filter(dias == "3DAA") |>
  select(!dias) |> 
  easyanova::ea2(design = 2, plot = 2) 
anova_sc_3DAA
## Resultados:
anova_sc_3DAA[[10]]$`residual analysis`
# CV = 58% !!!! Como faço para reduzir???
# Normalidade do resíduo: p> 0.05
# homocedasticidade da variância: p>0.05

anova_sc_3DAA[[1]]
# Não há diferença significativa entre os níveis do fator.
# Não há diferença significativa entre blocos
# Não há interação entre os fatores

## Tabela bonita da anova 
data1 <- as.data.frame(anova_sc_3DAA[[1]])
data1
tratamento_1 <- c("Adjuvante", "Taxa", "Blocos ", "Ajuvante:Taxa", "Resíduos " )
data1<- cbind( Tratamento  = tratamento_1, data1)
data1
data2 <- data1 %>% gt()%>% tab_header(
  title = md("***Anova da Escala Davies***"), 
  subtitle = md("**Três dias após primeira aplicação (3DAA)**")) %>%
  tab_source_note(source_note = md( "**valor de  α = 0.05** ")) %>%
  tab_options(table.width = pct(100)) 
data2

### Anova COM testemunha
BP_3DAA_trat
# comparando o tratamento 1 (controle) com o tratamento 2 (Adjuvante = Aureo, Taxa = 10)
anova_Cc_3DAA <- df_media_A_Davis |> 
  filter(dias == "3DAA",
         Tratamento %in% c(1, 2)) |> 
  select(Tratamento, Bloco, Severidade_mean)  |> 
  easyanova::ea1(design = 2)
anova_Cc_3DAA
## Resultados:
anova_Cc_3DAA[[1]]

## Teste de Dunnett
df_media_A_Davis |> 
  AgroR::dunnett(trat = Tratamento,
                 resp = Severidade_mean,
                 control = "1",
                 block = Bloco, 
                 model = "DBC")
df_media_A_Davis |> 
  filter(dias == "3DAA") |> 
  select(Tratamento, Bloco, Severidade_mean)  |> 
  DescTools::DunnettTest(Severidade_mean ~ Tratamento + Bloco )

# Os tratamentos 1 e 2 não diferem estatisticamente entre si!

#### 7DAA ---------------------------

## ANOVA
### Anova SEM testemunha
anova_sc_7DAA <- df_sem_controle_media |> 
  filter(dias == "7DAA") |>
  select(!dias) |> 
  easyanova::ea2(design = 2, alpha = 0.05) 
anova_sc_7DAA


## Tabela bonita Anova 

data_3 <- as.data.frame(anova_sc_7DAA[[1]])
data_3
tratamento_1 <- c("Adjuvante", "Taxa", "Blocos ", "Ajuvante:Taxa", "Resíduos " )
data3<- cbind( Tratamento  = tratamento_1, data_3)
data_3
data_4 <- data_3 %>% gt()%>% tab_header(
  title = md("***Anova da Escala Davies***"), 
  subtitle = md("**Sete dias após primeira aplicação (7DAA)**")) %>%
  tab_source_note(source_note = md( "**valor de  α = 0.05** ")) %>%
  tab_options(table.width = pct(100)) 
data_4




## Resultados normalidade
anova_sc_7DAA[[10]]$`residual analysis`
# normalidade do resíduo (P>0.05)
# homocedasticidade  do resíduo (P > 0.05)
anova_sc_7DAA[[1]]
# Não há diferença significativa entre os níveis do fator.
# Não há diferença significativa entre blocos
# Não há interação entre os fatores

### Anova COM testemunha
BP_7DAA_trat
# comparando o tratamento 1 (controle) com o tratamento 2 (Adjuvante = Aureo, Taxa = 10)
anova_Cc_7DAA <- df_media_A_Davis |> 
  filter(dias == "7DAA",
         Tratamento %in% c(1, 2)) |> 
  select(Tratamento, Bloco, Severidade_mean)  |> 
  easyanova::ea1(design = 2)
anova_Cc_7DAA
## Resultados:
anova_Cc_7DAA[[1]]
# Os tratamentos 1 e 2 não diferem estatisticamente entre si!

#### 3DAB --------------------------------

## ANOVA
### Anova SEM testemunha
anova_sc_3DAB <- df_sem_controle_media |> 
  filter(dias == "3DAB") |>
  select(!dias) |> 
  easyanova::ea2(design = 2, alpha = 0.05) 
anova_sc_3DAB

# Tabela bonita anova 
data_5 <- as.data.frame(anova_sc_3DAB[[1]])
data_5
tratamento_1 <- c("Adjuvante", "Taxa", "Blocos ", "Ajuvante:Taxa", "Resíduos " )
data_5<- cbind( Tratamento  = tratamento_1, data_5)
data_6 <- data_5 %>% gt()%>% tab_header(
  title = md("***Anova da Escala Davies***"), 
  subtitle = md("**Três dias após segunda aplicação (3DAB)**")) %>%
  tab_source_note(source_note = md( "**valor de  α = 0.05** ")) %>%
  tab_options(table.width = pct(100)) 
data_6

## Resultados da normalidade
anova_sc_3DAB[[10]]$`residual analysis`
# normalidade do resíduo SHAPIRO (P<0.05)
lillie.test(anova_sc_3DAB$`Residual analysis`$residuals)
# Normalidade LILLIEFORS (p>0.05)
# homocedasticidade  do resíduo (P > 0.05)
anova_sc_3DAB[[1]]
# Não há diferença significativa entre os níveis do fator.
# HÁ SIM diferença significativa entre blocos
# Não há interação entre os fatores

### Anova COM testemunha
BP_3DAB_trat
# comparando o tratamento 1 (controle) com o tratamento 2 (Adjuvante = Aureo, Taxa = 10)
anova_Cc_3DAB <- df_media_A_Davis |> 
  filter(dias == "3DAB",
         Tratamento %in% c(1, 2)) |> 
  select(Tratamento, Bloco, Severidade_mean)  |> 
  easyanova::ea1(design = 2)
anova_Cc_3DAB
## Resultados:
anova_Cc_3DAB[[1]]
# Os tratamentos 1 e 2 NÃO diferem estatisticamente entre si!


#### 7DAB -------------------------------

## ANOVA
### Anova SEM testemunha
anova_sc_7DAB <- df_sem_controle_media |> 
  filter(dias == "7DAB") |>
  select(!dias) |> 
  easyanova::ea2(design = 2) 
anova_sc_7DAB

#Anova tabela bonita 
data_7 <- as.data.frame(anova_sc_7DAB[[1]])
data_7
tratamento_7 <- c("Adjuvante", "Taxa", "Blocos ", "Ajuvante:Taxa", "Resíduos " )
data_7<- cbind( Tratamento  = tratamento_1, data_7)
data_8 <- data_5 %>% gt()%>% tab_header(
  title = md("***Anova da Escala Davies***"), 
  subtitle = md("**Sete  dias após segunda aplicação (7DAB)**")) %>%
  tab_source_note(source_note = md( "**valor de  α = 0.05** ")) %>%
  tab_options(table.width = pct(100)) 
data_8


## Resultados normalidade:
anova_sc_7DAB[[10]]$`residual analysis`
# normalidade do resíduo SHAPIRO (P = 0.021)
lillie.test(anova_sc_7DAB$`Residual analysis`$residuals)
# Normalidade LILLIEFORS (p = 0.048) CONSIDERA-SE NORMAL????
# homocedasticidade  do resíduo (P > 0.05)
anova_sc_7DAB[[1]]
# Não há diferença significativa entre os níveis do fator.
# HÁ SIM diferença significativa entre blocos
# Não há interação entre os fatores

### Anova COM testemunha
BP_7DAB_trat
# comparando os tratamentos entre si
anova_Cc_7DAB <- df_media_A_Davis |> 
  filter(dias == "7DAB") |> 
  select(Tratamento, Bloco, Severidade_mean)  |> 
  easyanova::ea1(design = 2)
anova_Cc_7DAB
## Resultados:
anova_Cc_7DAB[[1]]
# Os tratamentos NÃO diferem estatisticamente entre si!





