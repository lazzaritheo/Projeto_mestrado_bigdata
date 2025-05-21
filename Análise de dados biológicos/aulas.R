library(ggplot2)
# Importar arquivo txt com separador de tabulação
dados <- read.table("AntSpeed.txt", header = TRUE, sep = "\t")

#Graphic dispersion 
plot(dados$seedweight_mg)

#boxplot----
boxplot(seedweight_mg~substrate, subset= substrate %in% "sand", data=dados, xlab = "Substrato", ylab = "Peso da semente") #Boxplot da abundância da especie Aba.par nas 3 áreas. 

#Filtrar----
B1 <- dados[dados$colonyID %in% 'B1',]# filtra as linhas que tem o valor TRUE na coluna bar
peso_B1 <- c1$seedweight_mg

#outra possibilidade de filtrar 
B1_peso <- dados[dados$colonyID %in% 'B1',"seedweight_mg"]

#tabela dinamica relacionando duas variáveis categóricas 

table(dados$colonyID, dados$substrate)

#importar dados
library(readxl)
dados_amostra <-read_xlsx("dados_amostra.xlsx", sheet=1, col_names = T)

dados_local <-read_xlsx("dados_local.xlsx", sheet=1, col_names = T)

#Manipular dados dentro de uma tabela
library(dplyr)
tabela_curta <- dados_amostra %>%
  group_by(Local) %>%
  summarise(MedRiqAmo=mean(RiqAmos),
            Med.MO = mean(MO))

#Jutar dados de tabelas

dados_local$RiqAmost <- tabela_curta$MedRiqAmo
dados_local$MOAmost <- tabela_curta$Med.MO

#Grafico de dispersion
par(mfrow=c(1,2)) #Para mostrar os dois plots abaixo na mesma imagem
plot(dados_local$Alti, dados_local$RiquezaTotal, xlab = "Altitude (m)", ylab = "Riqueza total no local")
plot(dados_local$Temp, dados_local$RiquezaTotal, xlab = "Temperatura (C)", ylab = "Riqueza total no local")

#boxplot
boxplot(dados_local$RiquezaTotal ~ dados_local$Estado, main="Riqueza total", xlab = "Estado", ylab = "Riqueza total")
boxplot(dados_local$RiquezaTotal ~ dados_local$Protecao, main="Riqueza amostral média", xlab = "Proteção", ylab = "Riqueza total")

#Separar a tabela em dois

localPrim <- dados_local[dados_local$Estado %in%
                           'Primaria',]
localSec <- dados_local[dados_local$Estado %in%
                          'Secundaria',]
#Teste de variância da riqueza total no estado das florestas(olhar o valor de p):
var.test(RiquezaTotal~Estado, data = dados_local)
bartlett.test(RiquezaTotal~Estado, data = dados_local)#Mais indicado para dados biológicos

library(ggplot2)
ggplot(data = localPrim, aes(x=RiquezaTotal))+
  geom_histogram(aes(y=after_stat(density)),
                 binwidth = 1)+
  geom_density(lwd=1.2,
               linetype=2,
               colour=2)
#Para saber se os dados seguem uma distribuição normal
shapiro.test(resid(lm(RiquezaTotal~Estado, dados_local)))
#isso verifica o quanto cada ponto se afasta da média (nesse caso a média é 0)
resid(lm(RiquezaTotal~Estado, dados_local))
shapiro.test(resid(lm(RiquezaTotal~Protecao, dados_local)))

#Avaliação visual dos resíduos para avaliar normalidade
Riq.estado <- lm(RiquezaTotal~Estado, dados_local)
names(Riq.estado)
qqnorm(Riq.estado$residuals)
qqline(Riq.estado$residuals)
#gráficos
boxplot(RiquezaTotal~Estado, xlab = "Estado sucessional", 
        ylab = "Riqueza total", col="lightblue", data = dados_local)

boxplot(RiquezaTotal~Estado, notch =T, xlab = "Estado sucessional", 
        ylab = "Riqueza total", col="purple", data = dados_local)

boxplot(RiquezaTotal~Protecao, notch =T, xlab = "Estado sucessional", 
        ylab = "Riqueza total", col="lightblue", data = dados_local)

#Teste t

t.test(RiquezaTotal~Estado, data = dados_local)
t.test(RiquezaTotal~Protecao, data = dados_local)

#Comparações não paramétricos:

wilcox.test(Temp~Estado, dados_local)

#Avaliação geral inicial para modelo linear

#Relacionar duas variáveis 

plot(RiquezaTotal~Temp, data = dados_local, 
     xlab="Temperatura Cº", ylab = "Riqueza total no local",
     xlim = c(0, 45),  # Limites do eixo X
     ylim = c(0, 15))# Limites do eixo y

#Construção inicial do modelo
Temp.lm.tot <- lm(RiquezaTotal~Temp, data = dados_local)

#Verificar as premissas do teste
par(mfrow = c(2,2), oma = c(0,0,2,0))
plot(Temp.lm.tot)
dev.off()# Volta a conf. padrão dos gráficos

#Para testar homogeneidade
library(performance)
hetero.temp.tot <- check_heteroscedasticity(Temp.lm.tot)
hetero.temp.tot
plot(hetero.temp.tot)

#Para testar a premissa de normalidade
ResidualModel.tot <- residuals(Temp.lm.tot)
hist(ResidualModel.tot)
#teste de normalidade
shapiro.test(ResidualModel.tot)
#teste de normalidade visual:
qqnorm(ResidualModel.tot)
qqline(ResidualModel.tot)
#teste com o pacote car
library(car)
qqPlot(Temp.lm.tot)#com o intervalo de confiança 

#Normalidade com o pacote performance
library(performance)
library(qqplotr)
check_normality(Temp.lm.tot)
normality.tem.tot <- check_normality(Temp.lm.tot)
plot(normality.tem.tot)

#Verificação completa com o pacote performance
library(performance)
check_model(Temp.lm.tot)

#Checar outliers
library(performance)
check_outliers(Temp.lm.tot)
outliers.temp.tot <- check_outliers(Temp.lm.tot)
plot(outliers.temp.tot)

#graficar os residuos do modelo
plot(RiquezaTotal~Temp, data = dados_local, 
     xlab="Temperatura Cº", ylab = "Riqueza total no local",
     xlim = c(0, 50),  # Limites do eixo X
     ylim = c(0, 13))
abline(Temp.lm.tot, col="purple")
#linha de tendencia
fitted.temp.tot <- predict(Temp.lm.tot)
fitted.temp.tot

#montar graficamente os residuos 

for (i in 1:26) {
  lines(c(dados_local$Temp[i], dados_local$Temp[i]), 
        c(dados_local$RiquezaTotal[i],fitted.temp.tot[i]), 
        col="red")}

#Ver os resutados consolidados do modelo
summary(Temp.lm.tot)#Resultado do modelo
summary.aov(Temp.lm.tot)#Resultado em tabela tipo ANOVA
#Com outro pacote
library(car)
Anova(Temp.lm.tot)

#Gráficos diagnósticos
plot(Temp.lm.tot)
library(ggplot2)
library(sjPlot)
plot_model(Temp.lm.tot, type = "pred", show.data = T)


# Regressão linear multipla (+variáveis) ----------------------------------

plot(RiquezaTotal~Precipit, data = dados_local, xlab="Precipitação mm", 
     ylab = "Riqueza total no local")
plot(RiquezaTotal~Alti, data = dados_local, xlab="Altitude m",
     ylab = "Riqueza total no local")
pairs(dados_local[c(7,2,3,4,10)], panel = panel.smooth)

#Modelo
Reg.mult.full <- lm(RiquezaTotal~Temp+Alti+Precipit+MOAmost, data = dados_local)#Calcula o modelo linear simples

#Testando as premissas
library(performance)
check_heteroscedasticity(Reg.mult.full)#Testa hogeneidade das variâncias das variáveis
plot(check_heteroscedasticity(Reg.mult.full))#Para visualizar o gráfico
check_normality(Reg.mult.full)#Testa a normalidade dos resíduos
plot(check_normality(Reg.mult.full))
check_outliers(Reg.mult.full)
plot(check_outliers(Reg.mult.full))
check_model(Reg.mult.full)

#Checar a multicolinearidade
check_collinearity(Reg.mult.full)
plot(check_collinearity(Reg.mult.full))

#Modelo sem a variável temperatura
Reg.mult.v2 <- lm(RiquezaTotal~Alti+Precipit+MOAmost, data = dados_local)
check_model(Reg.mult.v2)
summary(Reg.mult.v2)

#Modelo nulo
Reg.mult.nulo <- lm(RiquezaTotal~1, data = dados_local)
check_model(Reg.mult.nulo)

Reg.mult.v3 <- lm(RiquezaTotal~Alti+Precipit, data = dados_local)

##Likelihood-ratio test (LRT)
library(lmtest)

lrtest(Reg.mult.v2, Reg.mult.v3)
##atenção nos valores de Pr para saber se os modelos estão semelhantes
##se o valor de P estiver alto (>0,05) os modelos estão parecidos e a variável não é tão explicativa.


##verificar sobre o modelo completo para entender a relação das variáveis
anova(Reg.mult.v2)

library(car)
##para entender qual o papel dessas variáveis no modelo completo de maneira mais prática
Anova(Reg.mult.v2)

## modelo 4 agora sem precipitação

Reg.mult.v4 <- lm(RiquezaTotal~Alti, data= dados_local)
lrtest(Reg.mult.v3, Reg.mult.v4)
##o valor de P mostra qual a probabilidade de os modelos não serem iguais
##dado os valores negativos, o maior valor vai se o melhor modelo
##no caso o modelo 1 é melhor

anova(Reg.mult.v3)

Anova(Reg.mult.v3)

##modelo nulo
Reg.mult.nulo <- lm(RiquezaTotal~1, data=dados_local)
lrtest(Reg.mult.v3, Reg.mult.nulo)

##2.6 Seleção stepwise UP por AIC
step(Reg.mult.nulo,
     scope = list(upper=Reg.mult.v2),
     direction = "both",
     data = dados_local)

##ver os valores de AIC de baixo pra cima pra saber quais os valores que se diferenciam em pelo menos 2 unidades pra entender quais variáveis são mais fundamentais

##modelo nulo <none> valor = 38 .. adicionando a MOAmost = 39..

##menos 2 unidades de diferença, logo a MOAmost não afeta o modelo


## seleção stewise down por AIC (USADO MAIS FREQUENTEMENTE)
step(Reg.mult.v2,
     scope = list(lower=Reg.mult.nulo),
     direction="both",
     data=dados_local)



#Modelo força bruta
library(MuMIn)
options(na.action = "na.fail")
Model.selec.bruto <- dredge(Reg.mult.v2)
Model.selec.bruto

#Modelos utilizando variáveis categóricas

#Anova Fatorial
barplot(tapply(dados_amostra$RiqAmos, 
               list(dados_amostra$Estado, dados_amostra$SoloAmos),mean),
               beside = TRUE, ylab = "Riqueza por amostra", xlab = "Tipo de solo", 
               ylim = c(0,6))
legend(locator(1), legend = c("Primaria","Secundria"), 
       title = "Estado sucessional", fill = c("black","lightgray"))
boxplot(RiqAmos~SoloAmos*Estado, data = dados_amostra, frame=F,
        col = c("blue", "yellow"), ylab = "Riqueza por amostra")

#Construir o modelo
model.Fact.Aov <- aov(RiqAmos~Estado*SoloAmos, data = dados_amostra)
summary(model.Fact.Aov)
#outra maneira de escrever o códico
model.Facto.lm <- lm(RiqAmos~Estado+SoloAmos+Estado:SoloAmos, data = dados_amostra)
summary(model.Facto.lm)

#criando contrastes 
dados_amostra$Solo2 <- factor(dados_amostra$SoloAmos)
levels(dados_amostra$Solo2)
levels(dados_amostra$Solo2)[c(1,4)] <- "NaoInteragem"
levels(dados_amostra$Solo2)[c(2,3)] <- "Interagem"
levels(dados_amostra$Solo2)

#Recriar o modelo com a nova variável(Solo2)
model.Facto.lm.2 <- lm(RiqAmos~Estado*Solo2, data = dados_amostra)
summary(model.Facto.lm.2)
library(car)
Anova(model.Facto.lm.2)

#Graficos
library(interactions)
interactions::cat_plot(model.Facto.lm.2, pred = Solo2, modx=Estado,
                       plot.points=T, interval=T,
                       errorbar.width=0.4,colors= "Qual1", 
                       y.label="Riqueza por amostra",x.label="Grupo de solo", 
                       geom= "line", dodge.width= 0.6 )

###  A tabela antspeed esta com o nome (dados)
###  Criar modelo com: lm(ColonyID+Subtrate+Seedweight+subtrate:Seweight)






