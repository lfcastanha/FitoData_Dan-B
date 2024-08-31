rm(list = ls())
ls()

library(tidyverse)
library(readxl)
library(lmtest)

dadosA_contagem <- read_excel("dados/Contagem de Pantas Atacadas/acont.xlsx")

df_A_cont <- dadosA_contagem |> 
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


#
df_A_cont |>
  filter(Tratamento %in% c(1:2)) |> 
  ggplot(aes(x = dias, y = contagem, color = Tratamento))+
  geom_point()+
  geom_line()+
  scale_x_continuous(breaks = df$dias)+
  facet_wrap(~Bloco)

# Gráfico exploratório
df_A_cont |> 
  ggplot(aes(x = Tratamento, y = contagem))+
  geom_boxplot(alpha = 0.6)+
  geom_point()+
  facet_wrap(~Bloco)


######## testar!!!###########
anova_tabela <- df_A_cont %>%
  #select the columns you are interested in
  select(Bloco, Tratamento, dias, contagem) %>%
  #Divide the data in a list of dataframes
  split(.$dias) |> 
  #For each dataframe
  map_df(~{
    #Get the mean value for each engine 
    .x %>% 
      group_by(Bloco) %>% 
      summarise(value = mean(contagem)) |> 
      #get the data in wide format
      pivot_wider(names_from = Bloco) |> 
      #Combine it with t.test result
      bind_cols(t_test(.x, formula = contagem ~ Bloco,
                       alternative = "two-sided",
                       order = c("Orgânica", "Convencional")))
  }, .id = "Dia")

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

#### Testando a distribuição de Poisson ####






df_A_cont |> 
  filter(dias == 14) |> 
  ggplot(aes(x = contagem))+
  geom_histogram()




