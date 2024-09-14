rm(list = ls())
ls()

library(tidyverse)
library(readxl)
library(lmtest)

df_A_Davis <- read_excel("dados/Escala Davis/df_A_Davis.xlsx")

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
  filter(dias == "3DAA") 

# rescaling the answer variable
df_A_Davis$Taxa <- scale(df_A_Davis$Taxa)

df_A_Davis |> 
  ggplot(aes(x = Taxa))+
  geom_density()


# Ajuste do Modelo Linear Generalizado Misto
glmm_model <- glmer(Severidade ~ Taxa*Adjuvante + (1|Bloco), 
                    family = poisson(link = "log"), data = df_A_Davis)
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
ggplot(df_A_Davis, aes(x= Taxa, y=Severidade)) +
  geom_point(aes(color = Adjuvante)) +
  geom_jitter(aes(color = Adjuvante))+
  scale_x_continuous(breaks = c(0, 10, 30, 120))+
  scale_y_continuous(breaks = c(0,2,4,6,8,10))+
  geom_smooth(method = "glm", method.args = list(family="poisson"), se=FALSE) +
  labs(title="Efeito da Taxa de Aplicação e Adjuvante na Severidade",
       x="Taxa de Aplicação de Pesticida",
       y="Severidade dos Danos (Escala Davis)") +
  theme_minimal()

# Se necessário, salvar o modelo ajustado
save(glmm_model, file = "glmm_modelo_ajustado.RData")  

??anova

### Reajustandando os dados à escala original ------------

# Supondo que 'TaxaAplicacao' foi padronizada
# E que o modelo foi ajustado com a variável padronizada

# Ajustando o modelo
glm_model <- glm(Severidade ~ scale(TaxaAplicacao) * Adjuvante, 
                 family = poisson(link = "log"), data = dados_experimento)

# Resumo do modelo
summary(glm_model)

# Obter o desvio padrão da variável original 'TaxaAplicacao'
sd_taxa <- sd(dados_experimento$TaxaAplicacao)

# Extrair os coeficientes do modelo
coef_padronizados <- coef(glm_model)

# Transformar os coeficientes de volta para a escala original
coef_originais <- coef_padronizados
coef_originais["scale(TaxaAplicacao)"] <- coef_padronizados["scale(TaxaAplicacao)"] / sd_taxa
coef_originais["scale(TaxaAplicacao):Adjuvante"] <- coef_padronizados["scale(TaxaAplicacao):Adjuvante"] / sd_taxa

# Exibir os coeficientes transformados
coef_originais

# Opcional: criar um sumário manualmente com os coeficientes ajustados
summary_adjusted <- summary(glm_model)
summary_adjusted$coefficients[,1] <- coef_originais

summary_adjusted




### testing out other stuff -----------------

model1 <- glm(Severidade ~ Taxa*Adjuvante*Bloco, family = "poisson", df_3DAA_A_Davis)
summary(model1)

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
visualize(reduced, plot = "model", jitter = c(.2, .2))
?visualize


# Novo script GPT ------------------

# Instala e carrega os pacotes necessários
library(lme4)
library(MuMIn) # Para facilitar a comparação de modelos

# Supondo que seu banco de dados esteja no data frame `dados`
# Variável resposta: `severidade` (medida pela escala Davis)
# Variáveis preditoras: `taxa_aplicacao` (taxa de aplicação de pesticida) e `adjuvante` (tipo de adjuvante)
# Variável aleatória: `bloco` (blocos casualizados)

# Reescalonamento da variável 'taxa_aplicacao'
taxa_media <- mean(df_A_Davis$Taxa)
taxa_sd <- sd(df_A_Davis$Taxa)
df_A_Davis$taxa_aplicacao_esc <- as.factor((df_A_Davis$Taxa - taxa_media) / taxa_sd )

# Modelo completo: Inclui todas as variáveis e a interação entre elas
modelo_completo <- glmer(Severidade ~ taxa_aplicacao_esc * Adjuvante + (1 | Bloco), 
                         data = df_A_Davis, family = poisson(link = "log"))

# Modelos simplificados
modelo_sem_interacao <- glmer(Severidade ~ taxa_aplicacao_esc + Adjuvante + (1 | Bloco), 
                              data = df_A_Davis, family = poisson(link = "log"))

modelo_apenas_taxa <- glmer(Severidade ~ taxa_aplicacao_esc + (1 | Bloco), 
                            data = df_A_Davis, family = poisson(link = "log"))

modelo_apenas_adjuvante <- glmer(Severidade ~ Adjuvante + (1 | Bloco), 
                                 data = df_A_Davis, family = poisson(link = "log"))

# Comparação dos modelos usando o critério de Akaike (AIC)
aic_modelos <- data.frame(
  Modelo = c("Completo", "Sem Interação", "Apenas Taxa", "Apenas Adjuvante"),
  AICc = c(AICc(modelo_completo), 
           AICc(modelo_sem_interacao), 
           AICc(modelo_apenas_taxa), 
           AICc(modelo_apenas_adjuvante))
)

# Ordena os modelos pelo AICc
aic_modelos <- aic_modelos[order(aic_modelos$AICc), ]

# Exibe os resultados da comparação dos modelos
print(aic_modelos)

# Seleciona o modelo com o menor AIC
melhor_modelo <- switch(aic_modelos$Modelo[1],
                        "Completo" = modelo_completo,
                        "Sem Interação" = modelo_sem_interacao,
                        "Apenas Taxa" = modelo_apenas_taxa,
                        "Apenas Adjuvante" = modelo_apenas_adjuvante)

# Resumo do melhor modelo
summary(melhor_modelo)

summary(modelo_completo)
anova(modelo_completo, 
      # Realiza ANOVA no modelo completo
      anova_result <- anova(modelo_completo)
      
      # Exibe o resultado da ANOVA
      print(anova_result)
      
      # Instala e carrega o pacote necessário
      install.packages("car")
      library(car)
      
      # Realiza a ANOVA com a função Anova do pacote car
      anova_result <- Anova(modelo_completo, type = 3)  # Type II ou III são os mais comuns para modelos mistos
      
      # Exibe o resultado da ANOVA com valores de p
      print(anova_result)
      
      # retornar a Taxa para numerico
      df_A_Davis$taxa_aplicacao_esc <- as.numeric(df_A_Davis$taxa_aplicacao_esc)
      # Retorno dos coeficientes da variável 'taxa_aplicacao' ao seu valor original
      coeficientes <- fixef(melhor_modelo)
      coeficientes_originais <- coeficientes
      coeficientes_originais["taxa_aplicacao_esc"] <- coeficientes["taxa_aplicacao_esc"] / taxa_sd
      coeficientes_originais["(Intercept)"] <- coeficientes["(Intercept)"] - coeficientes["taxa_aplicacao_esc"] * taxa_media / taxa_sd
      
      # Exibe os coeficientes ajustados para a escala original
      print(coeficientes_originais)
      
      
# Utilizando o modelo de quasi-poisson ---------------------

# Instala e carrega os pacotes necessários
library(MASS)
library(nlme)
library(MuMIn)

# Supondo que seu banco de dados esteja no data frame `dados`
# Variável resposta: `severidade` (medida pela escala Davis)
# Variáveis preditoras: `taxa_aplicacao` (taxa de aplicação de pesticida) e `adjuvante` (tipo de adjuvante)
# Variável aleatória: `bloco` (blocos casualizados)

# Reescalonamento da variável 'taxa_aplicacao'
taxa_media <- mean(df_A_Davis$Taxa)
taxa_sd <- sd(df_A_Davis$Taxa)
df_A_Davis$taxa_aplicacao_esc <- as.factor((df_A_Davis$Taxa - taxa_media) / taxa_sd)

# Função auxiliar para ajustar modelos Quasi-Poisson com GLMM usando glmmPQL
ajustar_modelo <- function(formula) {
  glmmPQL(fixed = formula, random = ~ 1 | Bloco, 
          family = quasipoisson(link = "log"), data = df_A_Davis)
}

# Modelo completo: Inclui todas as variáveis e a interação entre elas
modelo_completo <- ajustar_modelo(Severidade ~ taxa_aplicacao_esc * Adjuvante)

# Modelos simplificados
modelo_sem_interacao <- ajustar_modelo(Severidade ~ taxa_aplicacao_esc + Adjuvante)

modelo_apenas_taxa <- ajustar_modelo(Severidade ~ taxa_aplicacao_esc)

modelo_apenas_adjuvante <- ajustar_modelo(Severidade ~ Adjuvante)

# Comparação dos modelos usando o critério de Akaike (AIC)
# Nota: AIC não é bem definido para Quasi-Poisson, mas pode ser usado como uma aproximação.
aic_modelos <- data.frame(
  Modelo = c("Completo", "Sem Interação", "Apenas Taxa", "Apenas Adjuvante"),
  AICc = c(AIC(modelo_completo), 
           AIC(modelo_sem_interacao), 
           AIC(modelo_apenas_taxa), 
           AIC(modelo_apenas_adjuvante))
)

# Ordena os modelos pelo AICc
aic_modelos <- aic_modelos[order(aic_modelos$AICc), ]

# Exibe os resultados da comparação dos modelos
print(aic_modelos)

# Seleciona o modelo com o menor AIC
melhor_modelo <- switch(aic_modelos$Modelo[1],
                        "Completo" = modelo_completo,
                        "Sem Interação" = modelo_sem_interacao,
                        "Apenas Taxa" = modelo_apenas_taxa,
                        "Apenas Adjuvante" = modelo_apenas_adjuvante)

# Resumo do melhor modelo
summary(melhor_modelo)

# Retorno dos coeficientes da variável 'taxa_aplicacao' ao seu valor original
coeficientes <- fixef(melhor_modelo)
coeficientes_originais <- coeficientes
coeficientes_originais["taxa_aplicacao_esc"] <- coeficientes["taxa_aplicacao_esc"] / taxa_sd
coeficientes_originais["(Intercept)"] <- coeficientes["(Intercept)"] - coeficientes["taxa_aplicacao_esc"] * taxa_media / taxa_sd

# Exibe os coeficientes ajustados para a escala original
print(coeficientes_originais)
### graficos para visualizar a regressão de poisson --------
df_A_Davis |> 
  ggplot(aes(x= Taxa, y =Severidade, color = Adjuvante)) +
  geom_point() +
  geom_jitter()+
  scale_x_continuous(breaks = c(0, 10, 30, 120))+
  scale_y_continuous(breaks = 0:9)+
  geom_smooth(method = "glm", method.args = list(family="poisson"), se=FALSE) +
  labs(title="Efeito da Taxa de Aplicação e Adjuvante na Severidade",
       x="Taxa de Aplicação de Pesticida",
       y="Severidade dos Danos (Escala Davis)") +
  theme_minimal()


df_A_Davis |> 
  mutate(Taxa = as.factor(Taxa)) |> 
  group_by(Adjuvante, Taxa) |> 
  summarise(Severidade_media = mean(Severidade)) |> 
  mutate(Taxa = factor(Taxa)) |> 
  ggplot(aes(x= Adjuvante, y = Severidade_media, fill = Taxa))+
  geom_bar(stat = "identity")

