rm(list = ls())
ls()

library(tidyverse)
library(readxl)
library(lmtest)

#### A_Davis #####
dadosA_davis <- read_excel("dados/Escala Davis/adavis.xlsx")

df_A_Davis <- dadosA_davis |> 
  mutate(
    # Criar uma coluna com a indentificação do tratamento
    Tratamento = factor(case_when(
      Adjuvante == "NA" & Taxa == "NA" ~ "1",
      Adjuvante == "Aureo" & Taxa == "10.0" ~ "2",
      Adjuvante == "Silwet" & Taxa == "10.0" ~ "3",
      Adjuvante == "Ochima" & Taxa == "10.0" ~ "4",
      Adjuvante == "NA" & Taxa == "10.0" ~ "5",
      Adjuvante == "Aureo" & Taxa == "30.0" ~ "6",
      Adjuvante == "Silwet" & Taxa == "30.0" ~ "7",
      Adjuvante == "Ochima" & Taxa == "30.0" ~ "8",
      Adjuvante == "NA" & Taxa == "30.0" ~ "9",
      Adjuvante == "Aureo" & Taxa == "120.0" ~ "10",
      Adjuvante == "Silwet" & Taxa == "120.0" ~ "11",
      Adjuvante == "Ochima" & Taxa == "120.0" ~ "12",
      Adjuvante == "NA" & Taxa == "120.0" ~ "13"
      )),
    # reorganizar os fatores para ordem crescente de 1 a 13
    Tratamento = fct_relevel(
      Tratamento,
      "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13"),
    Bloco = factor(Bloco),
    Sub = factor(Sub))|> 
  # excluir colunas 
  select(!c(Taxa, Adjuvante)) |> 
  # alongar df
  pivot_longer(names_to = "dias",
               values_to = "Escala_Davis",
               cols = "3DAA":"7DAB") |> 
  mutate(dias = factor(dias)) |> 
  rename(planta = "Sub")


df_A_Davis |> 
  filter(dias == "3DAA") |> 
  ggplot(aes(x = Tratamento, y = contagem))+
  geom_boxplot()

df_A_Davis |> 
  filter(dias == "7DAA") |> 
  ggplot(aes(x = contagem))+
  geom_histogram()



#### ESTATÍSTICA ####
# ANOVA

attach(df_A_cont)
modelo = aov(contagem~Tratamento+Bloco)
anova(modelo)

### Normalidade dos erros (Shapiro-Wilk normality test)
shapiro.test(modelo$residuals)
# os resíduos NÃO seguem distribuição normal (p-value = 1.764e-05)

### Homogeneidade das variâncias (Bartlett test)
bartlett.test(modelo$residuals~Tratamento)
# as variâncias NÃO são homogêneas (p-value = 0.00156)

### Independência dos erros (Durbin-Watson test)
# H0 = first order autocorrelation does Not exist
# H1 = first order correlation exists.
lmtest::dwtest(modelo)
# os erros NÃO são independentes (p-value = 3.571e-05) 

### Gráfico dos resíduos
a = anova(modelo)
plot(modelo$residuals/sqrt(a$`Mean Sq`[3]), ylab="Resíduos Padronizados")
abline(h=0)



#### Testando a distribuição de Poisson #####
attach(df_A_Davis)

## aplicando o teste Poisson para criar um modelo comparativo entre as variáveis
## Bloco, Tratamento e Dias.
model1 <- glm(Escala_Davis ~ Bloco*Tratamento*dias, family = poisson, data = df_A_Davis)
summary(model1)
# como o valor do resíduo (Residual deviance: 10431) é maior que os graus de 
# liberdade (4991  degrees of freedom), será preciso utilizar o teste quasipoisson.
model2 <- glm(Escala_Davis ~ Bloco*Tratamento*dias, quasipoisson, df_A_Davis)
summary(model2)

model3 <- glm(Escala_Davis ~ Bloco*Tratamento, quasipoisson, df_A_Davis)
summary(model3)
