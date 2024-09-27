rm(list = ls())

library(tidyverse)
library(readxl)

df_original_A_contagem <- read_excel("dados/Contagem de Pantas Atacadas/acont.xlsx")

### transformar contagem em "porcentagem de plantas atacadas" ------- 
# Se cada parcela de tratamento tem 25 plantas, então divirei o valor por 25 para 
# obter a porcentagem de incidencia

## Realizar a transformação
df_incidencia_A <- df_original_A_contagem |> 
  pivot_longer(names_to = "Dias",
               values_to = "contagem",
               cols = "0DAA":"7DAB") |>  
  mutate(Tratamento = factor(Tratamento),
         Bloco = factor(Bloco),
         Dias = factor(Dias),
         Incidencia_percent = contagem/25 * 100)

#writexl::write_xlsx(df_incidencia_A, "dados/Contagem de Pantas Atacadas/df_incidencia_A")




## alongamento do df, reduzindo as colunas de datas para apenas uma: tempo. -----
## Ideal para gráfico de contagem x tempo!!
df_A_cont <- dadosA_contagem |> 
  mutate(dias = recode(dias, 
                       "0DAA" = "0",
                       "3DAA" = "3",
                       "7DAA" = "7", 
                       "3DAB" = "10",
                       "7DAB" = "14"),
         dias = as.numeric(dias))