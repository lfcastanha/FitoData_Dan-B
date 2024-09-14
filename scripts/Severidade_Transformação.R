rm(list = ls())
ls()

library(tidyverse)
library(readxl)
library(writexl)

#### A_Davis -------------------------------
df_original_A_davis <- read_excel("dados/Escala Davis/adavis-adapted.xlsx")

df_A_Davis <- df_original_A_davis |> 
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
  mutate(Taxa = as.numeric(Taxa),
         Adjuvante = factor(Adjuvante)) |> 
  rename(planta = "Sub") |> 
  #retirar valores NA
  drop_na() 

# writexl::write_xlsx(df_A_Davis, "dados/Escala Davis/df_A_Davis.xlsx")