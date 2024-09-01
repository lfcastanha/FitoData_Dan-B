rm(list = ls())
ls()

library(tidyverse)
library(readxl)
library(lmtest)
#### A_Davis -------------------------------
dadosA_davis <- read_excel("dados/Escala Davis/adavis-adapted.xlsx")

df_A_Davis <- dadosA_davis |> 
  mutate(
    # Criar uma coluna com a indentificação do tratamento
    Tratamento = factor(case_when(
      Adjuvante == "NA" & Taxa == "0" ~ "1",
      Adjuvante == "Aureo" & Taxa == "10" ~ "2",
      Adjuvante == "Silwet" & Taxa == "10" ~ "3",
      Adjuvante == "Ochima" & Taxa == "10" ~ "4",
      Adjuvante == "NA" & Taxa == "10" ~ "5",
      Adjuvante == "Aureo" & Taxa == "30" ~ "6",
      Adjuvante == "Silwet" & Taxa == "30" ~ "7",
      Adjuvante == "Ochima" & Taxa == "30" ~ "8",
      Adjuvante == "NA" & Taxa == "30" ~ "9",
      Adjuvante == "Aureo" & Taxa == "120" ~ "10",
      Adjuvante == "Silwet" & Taxa == "120" ~ "11",
      Adjuvante == "Ochima" & Taxa == "120" ~ "12",
      Adjuvante == "NA" & Taxa == "120" ~ "13"
      )),
    # reorganizar os fatores para ordem crescente de 1 a 13
    Tratamento = fct_relevel(
      Tratamento,
      "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13"),
    Bloco = factor(Bloco),
    Sub = factor(Sub))|> 
  # alongar df
  pivot_longer(names_to = "dias",
               values_to = "Severidade",
               cols = "3DAA":"7DAB") |> 
  mutate(dias = factor(dias),
         Adjuvante = factor(Adjuvante)) |> 
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

|

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



#### Script do ChatGPT ---------------------------------------------

library(lme4)  # Para ajustar o modelo linear generalizado misto
library(lmerTest)  # Para obter valores de p para o GLMM
library(ggplot2)  # Para visualização dos dados

# Supondo que o seu conjunto de dados se chama 'dados_experimento'
# E as variáveis são:
# - Severidade: variável resposta (escala Davis de 0 a 9)
# - TaxaAplicacao: variável explicativa (taxa de aplicação de pesticida)
# - Adjuvante: variável explicativa (tipo de adjuvante utilizado)
# - Bloco: efeito aleatório correspondente aos blocos casualizados


# selecionar apenas o dia 3DAA
df_3DAA_A_Davis <- df_A_Davis |> 
  filter(dias == "3DAA") |> 
  mutate(Taxa = factor(Taxa))

# Ajuste do Modelo Linear Generalizado Misto
glmm_model <- glmer(Severidade ~ Taxa*Adjuvante + (1|Bloco), 
                    family = poisson(link = "log"), data = df_A_Davis)


model1 <- glm(Severidade ~ Taxa*Adjuvante*Bloco, family = "poisson", df_3DAA_A_Davis)
summary(model1)

# Resumo do modelo
summary(glmm_model)

# Verificação dos resíduos
plot(residuals(glmm_model), main="Resíduos do Modelo GLMM")
qqnorm(residuals(glmm_model))
qqline(residuals(glmm_model), col="red")

# Comparação de modelos (opcional)
# Se você quiser comparar diferentes modelos, por exemplo, com e sem interação:
glmm_model_sem_interacao <- glmer(Severidade ~ Taxa + Adjuvante + (1|Bloco), 
                                  family = poisson(link = "log"), data = df_3DAA_A_Davis)

# Comparação usando AIC
AIC(glmm_model, glmm_model_sem_interacao)

# Análise dos efeitos fixos
anova(glmm_model)

# Gráfico de efeitos principais (usando ggplot2 para visualização)
ggplot(dados_chatGPT, aes(x=Taxa, y=Severidade, color=Adjuvante)) +
  geom_point() +
  geom_smooth(method="glm", method.args=list(family="poisson"), se=FALSE) +
  labs(title="Efeito da Taxa de Aplicação e Adjuvante na Severidade",
       x="Taxa de Aplicação de Pesticida",
       y="Severidade dos Danos (Escala Davis)") +
  theme_minimal()

|> # Se necessário, salvar o modelo ajustado
save(glmm_model, file = "glmm_modelo_ajustado.RData")  
         

# Using Flexplot -----------------------------------

library(flexplot)

# Flexplot
flexplot(Severidade~1, df_A_Davis)

# Flexplot it
flexplot(Severidade~Taxa | Adjuvante, df_A_Davis, 
         method = "poisson", jitter = c(0, .2), ghost.line = "gray")
df_A_Davis |> 
  mutate(Taxa = factor(Taxa)) |> 
  ggplot(aes(x = Taxa, y = Severidade))+
  geom_point()+
  geom_jitter()+
  geom_smooth(method = "lm")+
  facet_wrap(~Adjuvante)

# modeling
full = glm(Severidade~Adjuvante*Taxa, 
           df_A_Davis,
           family = "poisson")
reduced = glm(Severidade~Adjuvante,
              df_A_Davis,
              family = "poisson")

# Visualize the two models
compare.fits(Severidade~Taxa | Adjuvante, df_A_Davis,
             full, reduced, jitter = c(0, .1))

# aid with statistics
model.comparison(full, reduced)

# 
visualize(full, plot = "model", jitter = c(0, .1))
