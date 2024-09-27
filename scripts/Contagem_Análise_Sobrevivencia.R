rm(list = ls())

library(tidyverse)
library(readxl)
library(survival)

### Loading and transforming the data ---------
df_A_cont <- read_excel("dados/Contagem de Pantas Atacadas/acont.xlsx") |> 
  pivot_longer(names_to = "Dias",
               values_to = "contagem",
               cols = "0DAA":"7DAB") |> 
  mutate_if(is.character, as.factor) |> 
  mutate(Tratamento = factor(Tratamento),
         Bloco = factor(Bloco)) |> 
  mutate(Dias = recode(Dias, 
                       "0DAA" = "0",
                       "3DAA" = "3",
                       "7DAA" = "7", 
                       "3DAB" = "10",
                       "7DAB" = "14"),
         Dias = as.character(Dias),
         Dias = as.numeric(Dias))

## Substituindo os valores de "contagem" por repetições de linhas
df_An_Sobrev <- df_A_cont |> 
  # a função uncount expande o df de acordo com a coluna determinada
  uncount(weights = contagem) |> 
  mutate(Status = 1)

view (df_An_Sobrev)

# writexl::write_xlsx(df_An_Sobrev, "dados/Contagem de Pantas Atacadas/df_An_Sobrev.xlsx")


### Análise de sobrevivência ------------------
attach(df_An_Sobrev)
names(df_An_Sobrev)

plot(survfit(Surv(Dias, Status)∼Tratamento),
     col=c(2,3,4),
     xlab="Age at death (months)")

# POR QUE NÃO DEU CERTO???