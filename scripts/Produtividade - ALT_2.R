rm(list = ls())

library(tinytex)
library(tidyverse)
library(knitr)
library(nortest)
library(gt)
library(gtsummary)
library(car)
library(gridExtra)
library(lmtest)
library(easyanova)

#### Loading and trasnforming the data -------
df_produtividade <- read.table("dados/Produtividade/aprod_ADAPT_1.txt" , h= TRUE) |> 
  mutate(Adjuvante = as.factor(Adjuvante),
         Taxa = as.factor(Taxa), 
         Bloco = as.factor(Bloco))


## Nomeando os tratamentos
df_trat_prod <- df_produtividade |> 
  mutate(
    # Criar uma coluna com a indentificação do tratamento
    Tratamento = factor(case_when(
      Adjuvante == "Ta" ~ "1",
      Adjuvante == "Aureo" & Taxa == "10" ~ "2",
      Adjuvante == "Silwet" & Taxa == "10" ~ "3",
      Adjuvante == "Ochima" & Taxa == "10" ~ "4",
      Adjuvante == "nad" & Taxa == "10" ~ "5",
      Adjuvante == "Aureo" & Taxa == "30" ~ "6",
      Adjuvante == "Silwet" & Taxa == "30" ~ "7",
      Adjuvante == "Ochima" & Taxa == "30" ~ "8",
      Adjuvante == "nad" & Taxa == "30" ~ "9",
      Adjuvante == "Aureo" & Taxa == "120" ~ "10",
      Adjuvante == "Silwet" & Taxa == "120" ~ "11",
      Adjuvante == "Ochima" & Taxa == "120" ~ "12",
      Adjuvante == "nad" & Taxa == "120" ~ "13"
    )),
    # reorganizar os fatores para ordem crescente de 1 a 13
    Tratamento = fct_relevel(
      Tratamento,
      "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13")) |> 
  select(Tratamento, everything())

## Sem controle
prod_SEM_controle <- df_produtividade |> 
  filter(!(Adjuvante == "Ta"))


###### análise descritiva -----------------------
## Transformando os dados
tab_descritiva_prod <- df_produtividade |> 
  group_by(Adjuvante, Taxa )%>% 
  summarise(Mean = mean(Prod), SD = sd(Prod)) %>% 
  arrange(Adjuvante)

## Tabela descritiva
as.data.frame(tab_descritiva_prod[]) |> 
  mutate(
    Adjuvante = c("Aureo", "Aureo", "Aureo", "Sem adjuvante", "Sem adjuvante", "Sem adjuvante", 
                  "Ochima", "Ochima", "Ochima", "Silwet", "Silwet", "Silwet", "Testemunha")) |> 
  gt() %>%
  cols_label(
    Adjuvante = md("**Adjuvante**"), 
    Taxa = md("**Taxa**"),
    Mean = md("**Média**"),
    SD = md("**Desvio**")) %>%
  tab_header(title = md("***Produtividade***")) %>%
  tab_source_note(
    source_note = md("***Não houve significância na interação a α = 0.05***")) %>%
  tab_options(
    table.width = pct(95)  # Define a largura da tabela como 100% da área disponível
  )

#### ESTATÍSTICA ####  -------------------

### ANOVA Sem testemunha -------------
anova_SEM_C_prod <- prod_SEM_controle |> 
  ea2(design = 2, list = FALSE)
anova_SEM_C_prod

## Resultados:
anova_SEM_C_prod[[10]]$`residual analysis`
# Normalidade do resíduo: p>0.05
# homocedasticidade da variância: p>0.05

anova_SEM_C_prod[[1]]
# Tabela bonita da anova sem controle
tab_anova_SEM_c_prod <- as.data.frame(anova_SEM_C_prod[[1]]) %>% 
  mutate(Tratamento = c("Adjuvante", "Taxa", "Blocos ", "Ajuvante:Taxa", "Resíduos ")) |> 
  select(Tratamento, everything()) |> 
  gt()%>%
  tab_header(title = md("***Anova da produtividade***")) %>%
  tab_source_note(source_note = md( "**valor de  α = 0.05** ")) %>%
  tab_options(table.width = pct(100)) 
tab_anova_SEM_c_prod


## Resultados
# Há SIM diferença significativa entre os níveis do fator.
# NÃO há diferença significativa entre blocos
# Há SIM interação entre os fatores


# Tabela post hoc: ADJUVANTE
anova_SEM_C_prod[[2]]
tab_ADJUV_post_hoc_prod <- as.data.frame(anova_SEM_C_prod[[2]]) |> 
  mutate(Adjuvante = c("Aureo","Ochima", "Silwet", "Sem adjuvante")) |> 
  select(Adjuvante, adjusted.mean, tukey) |> 
  gt() |> 
  cols_label(Adjuvante = md("**Adjuvante**"), 
             adjusted.mean = md("**Média Ajustada**"), 
             tukey = md("**Tukey**")) |> 
  tab_header(title = md("***Produtividade***"),
             subtitle = md("**Análise Post Hoc ajduvante**")) |> 
  tab_source_note( 
    source_note = md("***Letras iguais: Níveis não diferem entre si a α = 0.05***")) |> 
  tab_options(table.width = pct(75))
tab_ADJUV_post_hoc_prod

# Tabela post hoc: TAXA
tab_TAXA_post_hoc_prod <- as.data.frame(anova_SEM_C_prod[[4]]) |> 
  select(factor_2, adjusted.mean, tukey) |>
  # tornando a tabela mais bonita
  gt() |> 
  cols_label(factor_2 = md("**Taxa**"), adjusted.mean = md("**Média ajustada**"), tukey = md('**Tukey**')) %>%
  tab_header(title = md("***Produtividade*** "), subtitle = md("**Análise Post Hoc  da Taxa de aplicação**")) %>% 
  tab_source_note(source_note = md("***Letras iguais: Níveis não diferem entre si a α = 0.05***")) %>%
  tab_options(table.width = pct(70))

tab_TAXA_post_hoc_prod

### ANOVA COM testemunha ----------------------

anova_COM_C_prod <- df_trat_prod |> 
  select(Tratamento, Bloco, Prod) |> 
  filter(Tratamento %in% c())
  ea1(design = 2, plot = 3)

anova_COM_C_prod

## Resultados:
anova_COM_C_prod[[4]]$`residual analysis`
# Normalidade do resíduo: p>0.05
# homocedasticidade da variância: p = 0.0002

anova_COM_C_prod[[1]]


## Tabela post hoc TRATAMENTOS
anova_COM_C_prod[[2]]

tab_TRAT_post_hoc_prod <- as.data.frame(anova_COM_C_prod[[2]]) |> 
  select(treatment, adjusted.mean, tukey) |>
  # tornando a tabela mais bonita
  gt() |> 
  cols_label(treatment = md("**Tratamento**"), 
             adjusted.mean = md("**Média ajustada**"), 
             tukey = md('**Tukey**')) %>%
  tab_header(title = md("***Produtividade*** "), 
             subtitle = md("**Análise Post Hoc  dos Tratamentos**")) %>% 
  tab_source_note(source_note = md("***Letras iguais: Níveis não diferem entre si a α = 0.05***")) %>%
  tab_options(table.width = pct(70))
tab_TRAT_post_hoc_prod

## Boxplot tratamentos
BP_prod_tratamento <- df_trat_prod |> 
  ggplot(aes(x = reorder(Tratamento, +Prod), y = Prod, fill = Tratamento))+
  geom_boxplot()+
  geom_point()+
  labs(
    title = "",
    x = "Tratamentos",
    y = "Produtividade/Ha")
BP_prod_tratamento


#### Gráficos -------------------

#gráfico de adjuvante
df_produtividade |> 
  mutate(fct_relevel(Adjuvante, "Ta",  "nad","Ochima",  "Silwet", "Aureo")) |> 
  ggplot(aes(y = Prod, x = reorder(Adjuvante, +Prod), fill = Adjuvante)) + 
  geom_boxplot(position = position_dodge( width = 0.80)) +
  theme(legend.position = "none", 
        axis.text.y = element_text(color = "black", face = "bold"), 
        axis.text.x = element_text(color = "black", face = "bold"),
        axis.title.x = element_text(size = 13, color = "black", face = "bold"),
        axis.title.y = element_text(size =13, color = "black", face = "bold"))+
  labs(x = "Adjuvante ", 
       y = "Produtividade/ha ", 
       size = 15) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 5500)) +
  geom_point() +
  scale_x_discrete(labels= c("nad"= "Sem adjuavente", "Ta"= "Testemunha"))


#Gráfico da taxa de aplicação 
df_produtividade |> 
  ggplot(aes(y = Prod, x = reorder(Taxa, +Prod), fill = Taxa)) + 
  geom_boxplot( position = position_dodge( width = 0.80)) +
  theme(legend.position = "none", 
        axis.text.y = element_text(color = "black", face = "bold"), 
        axis.text.x = element_text(color = "black", face = "bold"),
        axis.title.x = element_text(size = 13, color = "black", face = "bold"),
        axis.title.y = element_text(size =13, color = "black", face = "bold"))+ 
  labs(x = "Taxa aplicação  ", y = "Produtividade/ha", size = 15) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 5500)) +
  geom_point() 



