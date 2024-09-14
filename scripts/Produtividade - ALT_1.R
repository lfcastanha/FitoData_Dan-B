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
dir()
data_1 <- read.table("aprod.txt" , h= TRUE)
data_1
attach(data_1)
str(data_1)
#transformando as variáveis em fatores





data_2 <- data_1 %>% 
  transmute(Adjuvante = as.factor(Adjuvante),
            Taxa = as.factor(Taxa), 
            Bloco = as.factor(Bloco), Prod) 
  data_2
str(data_2)
attach(data_2)



#análise descritiva

data_5 <- data_2 %>%group_by(Adjuvante, Taxa )%>% 
  summarise(Mean = mean(Prod), SD = sd(Prod)) %>% arrange(Adjuvante)
data_5
is.data.frame(data_5)


# fazendo uma tabela bonita de análise descritiva
data_6 <-as.data.frame(data_5[])
data_6
data_6$Adjuvante<-NULL
data_6
attach(data_6)
Adjuvante_1 <- c("Aureo", "Aureo", "Aureo", "Sem adjuvante", "Sem adjuvante", "Sem adjuvante", 
               "Ochima", "Ochima", "Ochima", "Silwet", "Silwet", "Silwet", "Testemunha")
data_6 <- cbind(Adjuvante= Adjuvante_1, data_6)
data_6
data_7 <- data_6 %>% gt() %>% cols_label(
  Adjuvante = md("**Adjuvante**"), 
  Taxa = md("**Taxa**"), Mean = md("**Média**"), SD = md("**Desvio**")) %>%
  tab_header(title = md("***Produtividade***")) %>%
  tab_source_note(source_note = md("***Não houve significância na interação a α = 0.05***")) %>%
  tab_options(
    table.width = pct(95)  # Define a largura da tabela como 100% da área disponível
  )

data_7







#### ESTATÍSTICA #### isto é sem testemunha 
data_3 <- ea2(data_2, design = 2, list = FALSE) # sem o adjuvante testemunha
data_3
names(data_3)
# os resíduos  são normalizados, pois p-valor > 0.05
# as variâncias  são homegêneas, pois p-valor >0.05
#Independências dos erros ???
# Descriptive analysis:
# deu diferença significativa somente dentro dos níveis dos dois tratamentos 

#gráficos histograma 

names(data_2)
data_4 <- data_2 %>%group_by(Adjuvante)%>% summarise(Mean = mean(Prod), SD = sd(Prod))
attach(data_4)
data_4



ggplot( data = data_2, aes(x= Prod)) + geom_histogram( 
  ,binwidth = 50, closed = c("right", "left"),
  fill= "pink", color = "black")



min(Prod)
max(Prod)


#gráfico de adjuvante 
data_2$Adjuvante

data_adjuvante <- factor(data_2$Adjuvante , levels = c("Ta",  "nad","Ochima",  "Silwet", "Aureo"))
data_adjuvante


ggplot(data = data_2 , aes(y = Prod, x = data_adjuvante, fill = data_adjuvante)) + geom_boxplot( position = position_dodge( width = 0.80)) +
  theme(legend.position = "none", axis.text.y = element_text(color = "black", face = "bold"), 
        axis.text.x = element_text(color = "black", face = "bold"),
        axis.title.x = element_text(size = 17, color = "black", face = "bold"),
        axis.title.y = element_text(size =17, color = "black", face = "bold"))+
  labs(x = "Adjuvante ", y = "Produtividade/ha ", size = 15) + scale_y_continuous(expand = c(0, 0), limits = c(0, 5500)) +
  geom_point() +scale_x_discrete(labels= c("nad"= "Sem adjuavente", "Ta"= "Testemunha"))
  
 

#Gráfico da taxa de aplicação 


ggplot(data = data_2 , aes(y = Prod, x = Taxa, fill = Taxa)) + geom_boxplot( position = position_dodge( width = 0.80)) +
  theme(legend.position = "none", axis.text.y = element_text(color = "black", face = "bold"), 
        axis.text.x = element_text(color = "black", face = "bold"),
        axis.title.x = element_text(size = 17, color = "black", face = "bold"),
        axis.title.y = element_text(size =17, color = "black", face = "bold"))+ 
  labs(x = "Taxa aplicação  ", y = "Produtividade/ha", size = 15) + scale_y_continuous(expand = c(0, 0), limits = c(0, 5500)) +
  geom_point() 









 min(Prod)
  max(Prod)
# Analisando outliers
 # há um  um outlier acima 
  
  
data_5 <-data_2 %>% filter(Prod < quantile(Prod, 0.25, na.rm = TRUE)- 1.5*IQR(Prod, na.rm =  TRUE))
data_6 <- data_2 %>% filter( Prod > quantile(Prod, 0.75, na.rm = TRUE) + 1.5*IQR(Prod, na.rm =  TRUE))



# o que fazer agora?
#teste  para testemunhas Anova o que vale 





data_1 <- read.table("aprod.txt" , h= TRUE)
data_1
attach(data_1)

data_2 <- data_1 %>% transmute(Adjuvante = as.factor(Adjuvante),
                               Taxa = as.factor(Taxa), 
                               Bloco = as.factor(Bloco), Prod)



        
data_2
attach(data_2)
names(data_2)

  
anova1 <- ea2(data_2, design = 2)
anova1
str(anova1)
# como não houve significancia entre a interação dos fatores, então avaliou-se a interação dentro dos níveis dos fatores. Somente deu significativo entre 
# os níveis fatores adjuvantes


# Essa tabela bonita  foi para teste de tukey de adjuvante 
data_12 <- as.data.frame(anova1[[2]])
data_12
data_12$sd <- NULL
attach(data_12)
data_12$sem <-NULL
data_12$snk <-NULL
data_12$duncan <-NULL
data_12$scott_knott <-NULL
data_12$t <- NULL
data_12$factor_1 <- NULL
#data_12$factor_1 <- NULL
Adjuvante_2 <- c("Aureo","Ochima", "Silwet", "Sem adjuvante", "Testemunha")
data_12<- cbind(Adjuvante= Adjuvante_2, data_12)
data_12
data_13 <- data_12 %>% gt() %>% cols_label(Adjuvante = md("**Adjuvante**"), adjusted.mean = md("**Média Ajustada**"), tukey = md("**Tukey**")) %>%
  tab_header(title = md("***Produtividade***"),subtitle = md("**Análise Post Hoc ajduvante**")) %>%
  tab_source_note( source_note = md("***Letras iguais: Níveis não diferem entre si a α = 0.05***")) %>%
  tab_options(table.width = pct(75))
data_13


#Tabela da Anova bonita
data14 <- as.data.frame(anova1[[1]])
data14 
tratamento_1 <- c("Adjuvante", "Taxa", "Blocos ", "Ajuvante:Taxa", "Resíduos " )
data14 <- cbind( Tratamento  = tratamento_1, data14)
data14
data15 <- data14 %>% gt()%>% tab_header(title = md("***Anova da produtividade***")) %>%
  tab_source_note(source_note = md( "**valor de  α = 0.05** ")) %>%
  tab_options(table.width = pct(100)) 
data15


# Essa tabela foi feita para o teste de tukey de Taxa
data_16 <- as.data.frame(anova1[[4]])
data_16
data_16
data_16$sd <- NULL
attach(data_12)
data_16$sem <-NULL
data_16$snk <-NULL
data_16$duncan <-NULL
data_16$scott_knott <-NULL
data_16$t <- NULL
data_16
attach(data_16)
#tabela bonita
data_17 <- data_16 %>% gt() %>%
  cols_label( factor_2 = md("**Taxa**"), adjusted.mean = md("**Média ajustada**"), tukey = md('**Tukey**')) %>%
  tab_header(title = md("***Produtividade*** "), subtitle = md("**Análise Post Hoc  da Taxa de aplicação**")) %>% 
  tab_source_note(source_note = md("***Letras iguais: Níveis não diferem entre si a α = 0.05***")) %>%
  tab_options(table.width = pct(70))

data_17
