
###Análise de dados biológicos: Modelos lineares, generalizados e mistos
#Script produzido para a disciplina
#Autor: Prof. Sebastian Felipe Sendoya

###Aula 12 Construindo modelos generalizados GLMs


#lista de Pacotes
library(MASS)
library(vcd)
library(readxl)


#Carregando os dados
library(readxl)
DataM4 <-read_xlsx("dados_amostra.xlsx", sheet=1) #importa diretamente do arquivo excel
#esta planilha tem dadso novos diferentes aos que estavamos usando antes

#conferir o cabeçalho da sua tabela:
head(DataM4)
str(DataM4)

#Nosso objetivo é avaliar se a Materia organica no solo (MO), tipo de floresta e o tipo de solo
#inlfuenciam a riqueza de invetebrado das armadilhas (Variável RiqAmos).
#Temos motivos para pensar possam exisitir relações de dependência nos efeitos da MO e o tipo de solo (SoloAmos) 
#assim como do tipo de floresta com a MO 

#1. Fazer avaliação geral. (Análise exploratória) 
plot(DataM4[,c(9,7,10,11)])
dev.off()
#1.1 Graficos especificos
plot(RiqAmos~MO, data= DataM4, xlab="Materia organica no solo (%)",
     ylab="Riqueza por amostra")

boxplot(RiqAmos~Estado, data= DataM4, xlab="Tipo de floresta",
     ylab="Riqueza por amostra")

boxplot(RiqAmos~SoloAmos, data= DataM4, xlab="Tipo de floresta",
        ylab="Riqueza por amostra")

interaction.plot(x.factor = DataM4$SoloAmos, trace.factor = DataM4$Estado , 
                 response = DataM4$RiqAmos, fun = mean, 
                 type = "b", legend = TRUE, 
                 xlab = "Tipo de solo", ylab="Riqueza por amostra",
                 pch=c(15,19), col = c("blue", 8))


#1.2. lembrando do modelo geral

Riq.geral<-glm(RiqAmos ~ (Estado+MO+SoloAmos)^2, data = DataM4, family = gaussian(link="identity"))

anova(Riq.geral)

#2. teste de ajuste da distribuição aos dados da variavel resposta:
#Por ser uma contagem estamos comparando a dsitribuição de poisson e binomial negativa

#2.1. Os testes de ajuste usam relação de verosimilhança comparando com a ditribuição teorica
#De forma geral entre menor o vaor de X2 e menor o p melhor
library(vcd)
#somente compara entre "poisson", "binomial", "nbinomial"
fitnbin <- goodfit(DataM4$RiqAmos, type= "nbinomial")
fitpois <- goodfit(DataM4$RiqAmos, type= "poisson") 
summary(fitpois)
summary(fitnbin)
rootogram(fitpois)
rootogram(fitnbin)

#2.2. o pacote fitdistrplus permite uma representação grafica, mostrando possiveis distribuições e qual distribuição tem mais chance de se ajustar (veja a distancia de ponto azul)

library(fitdistrplus) #PACOTE PARA COMAPRAR DISTRIBUIÇÕES
descdist(DataM4$RiqAmos, discrete = T, boot = 1000)

require(fitdistrplus)
fit.poisson <-fitdist(DataM4$RiqAmos,"pois")
fit.nbi <- fitdist(DataM4$RiqAmos,"nbinom")
fit.norm <- fitdist(DataM4$RiqAmos,"norm")

plot(fit.norm)
plot(fit.norm, demp = TRUE)

par(mfrow=c(1,2), mar=c(4,4,3,3))
cdfcomp(list(fit.poisson, fit.nbi, fit.norm),horizontals=F, addlegend=T, legendtext=c("Poisson","biNeg", "Norm"))
qqcomp(list(fit.poisson, fit.nbi, fit.norm),addlegend=T, legendtext=c("Poisson","biNeg", "Norm"))
dev.off()

#2.3. Olhar os valores de AIC:
gofstat(list(fit.poisson,fit.nbi,fit.norm))

#2.4.Vc pode calcular o likelihood de ser cada tipo de dsitribuição
fitsIII <- list(fit.poisson, fit.nbi, fit.norm)
fitsIII
#confira abaixo os valores de log-likelihood
sapply(fitsIII, function(i) i$loglik) #o maior valor (menos negativo) tipicamente é a melhor distribuição


#2.5. Podemos construir modelos com distribuições diferentes e comparar os performances

modelRiq.1.poi <- glm(RiqAmos ~ (Estado+MO+SoloAmos)^2, data = DataM4, family = poisson, na.action = "na.fail")
modelRiq.1Nbin <- glm.nb(RiqAmos ~ (Estado+MO+SoloAmos)^2, data = DataM4, na.action = "na.fail")
modelRiq.1.gaus <- glm(RiqAmos ~ (Estado+MO+SoloAmos)^2, data = DataM4, family = gaussian, na.action = "na.fail")

#2.5.1 Comparar com rcompanion
library(rcompanion)
compareGLM(modelRiq.1.poi, modelRiq.1Nbin, modelRiq.1.gaus)

# ***** Para escolher o modelo tem que escolher o que tiver o menos valor de AICc

#2.5.2 Comparaçao com pacote perfomance
library(performance)
library(see)
library(qqplotr)

Model.dist <- compare_performance(modelRiq.1.poi, modelRiq.1Nbin, modelRiq.1.gaus)
Model.dist
plot(Model.dist)
check_distribution(modelRiq.1.gaus)

library(MuMIn)
model_selection <- model.sel(modelRiq.1.poi, modelRiq.1Nbin, modelRiq.1.gaus)
print(model_selection)


#2.5.3. Calcular o R2
r2(modelRiq.1Nbin)
#r2_nakagawa(mod392.bet.rand)
#r2_nakagawa(mod392.nor.rand)

#2.5.4. com o pacote bbmle
library(bbmle)#Pacote com ferramentas para comparar modelos, do bolker
AICctab(modelRiq.1.poi, modelRiq.1Nbin, modelRiq.1.gaus)


#3. Escolhida a dsitribuição vamos ver as premissas
plot(modelRiq.1Nbin)
summary(modelRiq.1Nbin)#modelo binomial negativo
summary(modelRiq.1.poi)#modelo poisson

library(performance)
library(see)
library(qqplotr)

#3.1.PARA CHECAR MULTICOLINEARIDADE

modelRiq.1Nbin.col <- glm.nb(RiqAmos ~ Estado+MO+SoloAmos, data = DataM4)
Mod.colinear <- check_collinearity(modelRiq.1Nbin.col)
plot(Mod.colinear)

#3.2PARA CHECAR outliers
Mod.Outlier <- check_outliers(modelRiq.1Nbin)
Mod.Outlier
plot(Mod.Outlier)#caso haja outliers aqui pode visualizar eles

#3.3.parachecar uniformidade
check_model(modelRiq.1Nbin)
plot(simulate_residuals(modelRiq.1Nbin))
check_residuals(modelRiq.1Nbin)

#3.4.overdispersion

check_overdispersion(modelRiq.1Nbin)

#para distribuição poisson, 
#a distribuição binomial negativa é robusta contra overdispersion
#uma alternativa para faores de overdispersion muito maiores que 1 é usar family= quasipoisson


library(DHARMa)#pacote para analisar ajuste e premisas de modelos
testDispersion(modelRiq.1Nbin)# este test ajuda a reforçaro dito
aveiaid <-simulateResiduals(modelRiq.1Nbin, plot = T)


#4. seleção dos modelos
library(car)
summary(modelRiq.1Nbin)
plot(modelRiq.1Nbin)
#Ao análisar a anova sempre levar em consideração as interações e depois as variáveis sozinhas
Anova(modelRiq.1Nbin)

#4.1. vamos testar a interação Estado:SoloAmos 

modelRiq.2Nbin <- update(modelRiq.1Nbin, .~. -Estado:SoloAmos)#Para alterar um modelo que já existe o sinal de -retira algo e +adiciona
summary(modelRiq.2Nbin)
anova(modelRiq.1Nbin, modelRiq.2Nbin) 
#ao ser p=0,79 podemos remover a interação Estado:SoloAmos

#4.1B. vamos testar a interação MO:SoloAmos 
modelRiq.2NbinB <- update(modelRiq.1Nbin, .~. -MO:SoloAmos)
summary(modelRiq.2NbinB)
anova(modelRiq.1Nbin, modelRiq.2NbinB) 
#ao ser p=0,6 podemos remover a interação

#4.1C. vamos testar a interação Estado:SoloAmos 
modelRiq.2NbinC <- update(modelRiq.1Nbin, .~. -Estado:MO)
summary(modelRiq.2NbinC)
anova(modelRiq.1Nbin, modelRiq.2NbinC) 
#ao ser p<0.05 não podemos remover a interação

####Uma opção melhor!!!
Anova(modelRiq.1Nbin)

#4.2. vamos testar tirando a interação Estado:SoloAmos e a interação MO:SoloAmos e continuar a selação a aprtir dai
modelRiq.3Nbin <- update(modelRiq.1Nbin, .~. -MO:SoloAmos - Estado:SoloAmos )
#vamor usar a funação Anova para ver testar MO:SoloAmos no novo modelo
Anova(modelRiq.3Nbin)  
#ao ser p<0,05 não podemos remover a interação Estado:MO

#4.3.  Resta testar os termos soltos, no caso SoloAmos
modelRiq.4Nbin <- update(modelRiq.3Nbin, .~. -SoloAmos )
anova(modelRiq.4Nbin, modelRiq.3Nbin) 
Anova(modelRiq.3Nbin)
#ao ser p=0,75  podemos remover o termo

#o melhor modelo seria modelRiq.4Nbin

#4.5. Por garantia podemos construir um modelo nulo
modelRiq.Null.Nbin <- glm.nb(RiqAmos ~ 1, data = DataM4)
anova(modelRiq.4Nbin, modelRiq.Null.Nbin) 
#ao ser p<0,05  Nosso modelo é melhor

Anova(modelRiq.4Nbin)
summary(modelRiq.4Nbin)


#4.6. outra aproximação apoiada em AIC
library(rcompanion)
compareGLM(modelRiq.1Nbin, modelRiq.2Nbin, modelRiq.3Nbin, modelRiq.4Nbin, modelRiq.Null.Nbin) 
library(bbmle)
AICctab(modelRiq.1Nbin, modelRiq.2Nbin, modelRiq.3Nbin, modelRiq.4Nbin, modelRiq.Null.Nbin)

#4.7. Pelo pacote MuMIn
library(MuMIn)
Multimodeltotal<-dredge(modelRiq.1Nbin)
Multimodeltotal
model.sel(Multimodeltotal)


#5. o melhor modelo adequado seria então:
summary(modelRiq.4Nbin)
Anova(modelRiq.4Nbin)
coef(modelRiq.4Nbin) #mostra os coeficientes do modelo
confint(modelRiq.4Nbin) #mostra os intervalos de confiança do modelo

#6. Calcular o R2
library(performance)
r2(modelRiq.4Nbin)

#7. odds ratio / effect size
exp(cbind(coef(modelRiq.4Nbin), confint(modelRiq.4Nbin)))  

#para comaprar tamanho de efeito corretamente entre preditores é necessário um apadronização, isto siginifica que cada variavel seja transformada para ter média 0 e desvio padrão 1
#O pacote performance faz isso "a posteriori" re-ajustando os modelos, mas vc pode padronizar as variavel antes de construir o modelo. 
library(effectsize)
effecsize.model.4.Nbin.basic<- effectsize(modelRiq.4Nbin, method = "basic")
effecsize.model.4.Nbin.basic
effecsize.model.4.Nbin<- effectsize(modelRiq.4Nbin, method = "refit")#metodo refit não padroniza os fatores. émelhor usar basic nesses casos ou padronizar antes de rodar o modelo. 
effecsize.model.4.Nbin


library(ggplot2)
library(sjPlot)
library(jtools)
library(interactions)
library(broom.mixed)
library(dotwhisker)
library(sjmisc)
library(ggeffects)

#8. aqui o grafico de coeficientes brutos
dw <- dwplot(modelRiq.4Nbin, show_intercept = TRUE)
dw
print(dw+geom_vline(xintercept=0,lty=2))

#aqui o grafic de coeficientes padronizados, com escala do exixo logaritmica
plot_summs(modelRiq.4Nbin, scale = T, exp = TRUE)

#aqui os coeficientes já tranformados
plot_model(modelRiq.4Nbin, show.values = T, value.offset = 0.2)
#vejam mais detalhes em : https://cran.r-project.org/web/packages/sjPlot/vignettes/plot_model_estimates.html


#9. grafico final do modelo
interactions::interact_plot(modelRiq.4Nbin, pred=MO, modx=Estado, 
              plot.points=T, interval=T, outcome.scale="response")


library(glmmTMB)
riqmodel.TMB.1<-glmmTMB(RiqAmos ~ Estado + MO + Estado:MO, data = DataM4,
                        family = "nbinom1")
riqmodel.TMB.Zero<-glmmTMB(RiqAmos ~ Estado + MO + Estado:MO, data = DataM4,
        family = "nbinom1", ziformula = ~Estado + MO)
AICctab(riqmodel.TMB.1, riqmodel.TMB.Zero)

check_overdispersion(riqmodel.TMB.Zero)
library(DHARMa)#pacote para analisar ajuste e premisas de modelos
testDispersion(riqmodel.TMB.Zero)# este test ajuda a reforçaro dito
aveiaid.zero <-simulateResiduals(riqmodel.TMB.Zero, plot = T)
check_model(riqmodel.TMB.Zero)

Anova(riqmodel.TMB.Zero)

