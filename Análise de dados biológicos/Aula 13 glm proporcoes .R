
###Análise de dados biológicos: Modelos lineares, generalizados e mistos
#Script produzido para a disciplina
#Autor: Prof. Sebastian Felipe Sendoya

###Aula 13 Construindo modelos generalizados GLMs. Binomial, proporções

#lista de Pacotes
library(MASS)
library(vcd)
library(readxl)
library(ggplot2)
library(performance)
library(AER)
library(car)
library(effectsize)
library(sjPlot)
library(jtools)
library(interactions)
library(broom.mixed)
library(dotwhisker)
library(sjmisc)
library(ggeffects)



#Carregando os dados
library(readxl)
DataFlores <-read_xlsx("exemplo_flores.xlsx", sheet=1) #importa diretamente do arquivo excel
#esta planilha tem dadso novos diferentes aos que estavamos usando antes

#conferir o cabeçalho da sua tabela:
head(DataFlores)
str(DataFlores)

#Nosso objetivo é avaliar se dentro de um conjunto de plantas a quantidade delas (proporção)
# que teve produção flores que era afetada pela aplicação de um promovedor de cresimento ("dose").
#O mesmo experiento foi feito com 5 variedades diferentes da planta. As hipoteses giram ao redor 
#de que o diferntes variaedaes de plantas podem responder ao produto de forma diferntes para produzir flores.

#1. Fazer avaliação geral. 
plot(DataFlores)

#1.1. Vamos criar duas colunas especificas, uma para calcular diretamente a proporção (pf)
#e outra (PropoF)em que vamos relacionar (com o cbind) quantas plantas produciram flores ("florered")
#com quantas não produziram ("number"-"florered")
DataFlores$pf <- DataFlores$flowered/DataFlores$number #proporção de plantas que floriram (pf)
#Função cbind relaciona duas colunas (nesse caso relacionando as floridas e as não floridas)
DataFlores$PropoF <- cbind(DataFlores$flowered,DataFlores$number-DataFlores$flowered)


#1.2 Graficos especificos avaliando a proporção
plot(pf~dose, data= DataFlores, xlab="Dose",
     ylab="Proportion flowered")

library(ggplot2)
ggplot(DataFlores,aes(dose,pf,group=variety))+#geom_point()+
  geom_jitter(aes(shape=variety, color= variety),size=3, alpha=0.8,)+
  theme_minimal()

boxplot(pf~variety, data= DataFlores, xlab="Variedade de planta", ylab="Proportion flowered")

#2. teste de ajuste da distribuição aos dados da variavel resposta:
#Por ser uma contagem estamos comparando a dsitribuição de poisson e binomial negativa

#2.1. Os testes de ajuste usam relação de verosimilhança comparando com a ditribuição teorica
#De forma geral entre menor o vaor de X2 e menor o p melhor
library(vcd)
fitbin <- goodfit(DataFlores$PropoF, type= "binomial")
summary(fitbin)
rootogram(fitbin)

#3. neste caso vamos diretamente ao modelo
modelFlor.bin.1 <- glm(PropoF ~ variety*dose, data = DataFlores, 
                       family = binomial, na.action = "na.fail")
plot(modelFlor.bin.1)


#3.1.PARA CHECAR MULTICOLINEARIDADE (tem que tirar as interações)
library(performance)
modelFlor.bin.2 <- update(modelFlor.bin.1, .~. -variety:dose )
Mod.colinear.flo <- check_collinearity(modelFlor.bin.2)
plot(Mod.colinear.flo)

#3.2PARA CHECAR outliers
Mod.Outlier.flo <- check_outliers(modelFlor.bin.1)
Mod.Outlier.flo
plot(Mod.Outlier.flo)#caso haja outliers aqui pode visualizar eles
#temos problema de outliers


#3.3.overdispersion
#eviter valores maiores de 1
#uma alternativa para faores de overdispersion muito maiores que 1 é usar family= quasipoisson
check_overdispersion(modelFlor.bin.1)

#3.4.parachecar uniformidade
plot(simulate_residuals(modelFlor.bin.1))
check_residuals(modelFlor.bin.1)

library(DHARMa)#pacote para analisar ajuste e premisas de modelos
testDispersion(modelFlor.bin.1)# este test ajuda a reforçaro dito
aveiaid <-simulateResiduals(modelFlor.bin.1, plot = T)
testUniformity(modelFlor.bin.1) 


#3.5.reconstruir com quasibinomial
modelFlor.quabin.1 <- glm(PropoF ~ variety*dose, data = DataFlores, 
                          family = quasibinomial, na.action = "na.fail")

#checando novamente as premisas

Mod.Outlier.flo.2 <- check_outliers(modelFlor.quabin.1)
Mod.Outlier.flo.2

plot(Mod.Outlier.flo.2)#observe queainda tem outliers, deve avaliar se é necessario retirar eles.
check_model(modelFlor.quabin.1)
check_overdispersion(modelFlor.quabin.1)

check_distribution(modelFlor.bin.1)

#Tirando 1 outlier (nesse caso a observação 10)
DataFlores[10,]
modelFlor.bin.1.out <- glm(PropoF ~ variety*dose, data = DataFlores[-c(10),], 
                           family = binomial, na.action = "na.fail")
plot(modelFlor.bin.1.out)

testDispersion(modelFlor.bin.1.out)
Mod.Outlier.flo.out <- check_outliers(modelFlor.bin.1.out)
plot(Mod.Outlier.flo.out)
aveiaid <-simulateResiduals(modelFlor.bin.1.out, plot = T)
testUniformity(modelFlor.bin.1.out)



#3.1.inicia a seleção de modelos 
library(car)
Anova(modelFlor.bin.1.out)
modelFlor.bin.2 <- update(modelFlor.bin.1.out, .~. -variety:dose )
anova(modelFlor.bin.1.out, modelFlor.bin.2, test="Chisq") #perceva que devemos indicar o tipo de test a ser aplicado (qui cuadrado)


#3.2 R2
library(performance)
#Calcular o R2
r2(modelFlor.bin.1.out)
#r2_nakagawa(mod392.bet.rand)
#r2_nakagawa(mod392.nor.rand)


#3.3. outras abordagens de seleção
library(car)
summary(modelFlor.bin.1.out)
plot(modelFlor.bin.1.out)

#Partição da deviance (analogo a variancia)
Anova(modelFlor.bin.1.out)


#4. odds ratio / effect size
#coeficintes transformados
exp(cbind(coef(modelFlor.bin.1.out), confint(modelFlor.bin.1.out)))  

#coeficientes padronizados
library(effectsize)
effecsize.model.flor.qua1<- effectsize(modelFlor.bin.1.out, method = "basic")
effecsize.model.flor.qua1

library(ggplot2)
library(sjPlot)
library(jtools)
library(interactions)
library(broom.mixed)
library(dotwhisker)
library(sjmisc)
library(ggeffects)

#5. aqui o grafico de coeficientes brutos

#5.1. grafico de coeficientes brutos
plot_summs(modelFlor.bin.1.out, scale = T)
#5.1.grafico com coef padronizados
plot_model(modelFlor.bin.1.out, show.values = T, value.offset = .3,type = "std" )
#5.1.grafico com coef transformados
plot_model(modelFlor.bin.1.out, show.values = T, value.offset = .2)


#agora o grafico final de resultados com as predições do modelo
#Desta vez esta,os fazendo manualmente com o ggplot, usando a função ggpredict
#para gerar uma tabela de dados preditos pelo modelo para pode colcoar com os pontos
#das observações. 
#O grafico apresenta o predito para cada nivel da variavel variedade
#o tamamnho dos pontos é proporcional ao numero totoal de flores para cada unidade amostral.

predictions.flo<-ggpredict(modelFlor.bin.1, type = "fixed", 
                           terms = c("dose [0:40]","variety")) 
ls(predictions.flo)
predictions.flo
predictions.flo$group


ggplot(DataFlores, aes(x = dose, y = pf))+
  #geom_point(aes(size = carcasses, colour = factor(radius)), position = position_dodge(.3))+
  geom_jitter(aes(x = dose, y = pf, size = number, colour = factor(variety)), alpha=0.9, width = 0.3)+
  scale_size_continuous(range = c(1, 5))+
  #scale_color_manual(values = c("#173F5f", "#3CAEA3", "#ED5538"))+
  scale_colour_brewer(palette = "Set1")+
  geom_line(data=predictions.flo, aes(x = x, y = predicted, colour = factor(group))) +
  #geom_path()+
  xlab("Dose")+ylab("Proportion flowered")+ ggtitle("")+theme_minimal()+
  theme(
    legend.position="right",
    plot.title = element_text(size=11),
    axis.line.y = element_line(size = 1, colour = 1),
    axis.line.x = element_line(size = 1, colour = 1),
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    axis.text.x = element_text(size = 13),
  )



#vamos agrupar os niveis da variavel, de 4 passamos somente 2


#-----------------------

DataFlores$variety3 <- factor(DataFlores$variety)#duplicamos a coluna com o fator solo
levels(as.factor(DataFlores$variety))
levels(as.factor(DataFlores$variety3))
levels(DataFlores$variety3)[c(1,5)]<- "Grande efeito"
levels(DataFlores$variety3)[c(2,3,4)]<- "Pouco efeito"
levels(DataFlores$variety3)

DataFlores$variety2<-as.factor(DataFlores$variety)
contrasts(DataFlores$variety2) <-cbind(c(4,-1,-1,-1,-1),c(0,-1,-1,-1,3), c(0,-1,2,-1,0), c(0,-1,0,1,0))

contrasts(DataFlores$variety2)
modelFlor.quabin.4 <- glm(PropoF ~ variety2*dose, data = DataFlores,
                          family = quasibinomial, na.action = "na.fail")

summary(modelFlor.quabin.4)

#-----------------------
DataFlores$variety3 <- factor(DataFlores$variety)#duplicamos novamente a coluna com o fator variedade
levels(as.factor(DataFlores$variety))
levels(DataFlores$variety3)

#redefinimos os niveis da variavel nova de acordo ao resultado dos contrastes anteriores:
levels(DataFlores$variety3)[c(1,5)] <- "A-E"
levels(DataFlores$variety3)[c(2,3,4)] <- "B-C-D"

#rodamos novamente o modelo
modelFlor.quabin.5<- glm(PropoF ~ variety3*dose, data = DataFlores,
                          family = quasibinomial, na.action = "na.fail")
summary(modelFlor.quabin.5)

#Grafico agrupado
predictions.flo2<-ggpredict(modelFlor.quabin.5, type = "fixed", terms = c("dose [0:40]","variety3")) 
ls(predictions.flo2)
predictions.flo2
predictions.flo2$group


ggplot(DataFlores, aes(x = dose, y = pf))+
  geom_jitter(aes(x = dose, y = pf, size = number, colour = factor(variety3)), alpha=0.9, width = 0.3)+
  scale_size_continuous(range = c(1, 5))+
  scale_colour_brewer(palette = "Set1")+
  geom_line(data=predictions.flo2, aes(x = x, y = predicted, colour = factor(group))) +
  xlab("Dose")+ylab("Proportion flowered")+ ggtitle("")+theme_minimal()+
  theme(
    legend.position="right",
    plot.title = element_text(size=11),
    axis.line.y = element_line(size = 1, colour = 1),
    axis.line.x = element_line(size = 1, colour = 1),
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    axis.text.x = element_text(size = 13),
  )

#checagem do novo modelo
library(performance)
Mod.hetero.flo.3 <- check_heteroscedasticity(modelFlor.quabin.5)
plot(Mod.hetero.flo.3)
Mod.Outlier.flo.3 <- check_outliers(modelFlor.quabin.5)
Mod.Outlier.flo.3
plot(Mod.Outlier.flo.3)
r2(modelFlor.quabin.5)
#melhorou muito a questão dos outliers!!!


#alternativa para comparações a posteriori
library(emmeans)
