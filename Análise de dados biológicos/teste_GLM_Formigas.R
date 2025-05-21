# Testando GLM com os dados das formigas
###### OLHAR O PACOTES NECESSÁRIOS QUE ESTÃO NA AULA 12 GLM CONTAGEM
# Baixando os dados das formigas:
AntSpeed <- read.table("AntSpeed.txt", header = TRUE, sep = "\t")

#conferir o cabeçalho da sua tabela:
head(AntSpeed)
str(AntSpeed)

#1. Fazer avaliação geral. (Análise exploratória) 
plot(AntSpeed[,c(2,3,4,5)])
dev.off()

# pacote fitdistrplus permite uma representação grafica, mostrando possiveis distribuições e qual distribuição tem mais chance de se ajustar (veja a distancia de ponto azul)

library(fitdistrplus) #PACOTE PARA COMAPRAR DISTRIBUIÇÕES
descdist(AntSpeed$antspeed_cm.s, discrete = F, boot = 1000)

require(fitdistrplus)
fit.gamma <-fitdist(AntSpeed$antspeed_cm.s,"gamma")
fit.lognormal <- fitdist(AntSpeed$antspeed_cm.s,"lnorm")
fit.weibull <- fitdist(AntSpeed$antspeed_cm.s,"weibull")

cdfcomp(list(fit.gamma, fit.lognormal, fit.weibull),horizontals=F, addlegend=T, legendtext=c("Gamma","Lognormal", "Weibull"))
#2.3. Olhar os valores de AIC:
gofstat(list(fit.gamma,fit.lognormal,fit.weibull))

### Corrigindo o exercício 1 com GLM:
# Construir modelos com distribuições diferentes e comparar os performances

model.glm.gamma.log <- glm(antspeed_cm.s~(colonyID+substrate+seedweight_mg+substrate:seedweight_mg)^2,
                       data = AntSpeed, family = Gamma(link="log"))
model.glm.gamma.sqrt <- glm(antspeed_cm.s~(colonyID+substrate+seedweight_mg+substrate:seedweight_mg)^2, 
                       data = AntSpeed, family = Gamma(link="sqrt"))
model.glm.gamma.log2 <- glm(antspeed_cm.s~(colonyID+substrate+seedweight_mg+substrate:seedweight_mg),
                           data = AntSpeed, na.action = "na.fail",family = Gamma(link="log"))
model.glm.gamma.log3 <- glm(antspeed_cm.s~(colonyID+substrate+seedweight_mg),
                            data = AntSpeed, family = Gamma(link="log"))
model.glm.gamma.sqrt2 <- glm(antspeed_cm.s~(colonyID+substrate+seedweight_mg+substrate:seedweight_mg), 
                            data = AntSpeed, family = Gamma(link="sqrt"))
# ver AICc dos modelos
AICctab(model.glm.gamma.log, model.glm.gamma.sqrt)
AICctab(model.glm.gamma.log, model.glm.gamma.log2, model.glm.gamma.log3, model.glm.gamma.sqrt,model.glm.gamma.sqrt2)
AICctab(model.glm.gamma.log2, model.glm.gamma.sqrt2)
#Criar graficamente os modelos 
Models.comp <- compare_performance(model.glm.gamma.log, model.glm.gamma.log2)
Models.comp
plot(Models.comp)

# Testar multcolineariedade
#3.1.PARA CHECAR MULTICOLINEARIDADE
model.gamma.log.col <- glm(antspeed_cm.s~(colonyID+substrate+seedweight_mg+substrate:seedweight_mg)^2,
                           data = AntSpeed)
Mod.colinear.gammalog <- check_collinearity(model.gamma.log.col)
plot(Mod.colinear.gammalog)

# CHECAR outliers
Mod.Outlier.glm <- check_outliers(model.glm.gamma.log)
Mod.Outlier.glm
plot(Mod.Outlier.glm)#caso haja outliers aqui pode visualizar eles

#3.3.parachecar uniformidade
check_model(model.glm.gamma.sqrt2)
plot(simulate_residuals(model.glm.gamma.sqrt2))
check_residuals(model.glm.gamma.sqrt2)

# overdispersion
check_overdispersion(model.glm.gamma.log2)
check_overdispersion(model.glm.gamma.sqrt2)
library(DHARMa)#pacote para analisar ajuste e premisas de modelos
testDispersion(model.glm.gamma.log2)# este test ajuda a reforçaro dito
aveiaid <-simulateResiduals(model.glm.gamma.log2, plot = T)

# seleção dos modelos analoisando os valores de p e ir removendo as interações 
library(car)
summary(model.glm.gamma.log2)
plot(model.glm.gamma.log2)
#Ao análisar a anova sempre levar em consideração as interações e depois as variáveis sozinhas
Anova(model.glm.gamma.log2)
coef(model.glm.gamma.log2)
confint(model.glm.gamma.log2)

# Ver AICc Pelo pacote MuMIn 
library(MuMIn)
Multimodel.log2 <- dredge(model.glm.gamma.log2)
Multimodel.log2
model.sel(Multimodel.log2)

# odds ratio / effect size
exp(cbind(coef(model.glm.gamma.log2), confint(model.glm.gamma.log2)))  

#para comaprar tamanho de efeito corretamente entre preditores é necessário um apadronização, isto siginifica que cada variavel seja transformada para ter média 0 e desvio padrão 1
#O pacote performance faz isso "a posteriori" re-ajustando os modelos, mas vc pode padronizar as variavel antes de construir o modelo. 
library(effectsize)
effecsize.model.glm.basic<- effectsize(model.glm.gamma.log2, method = "basic")
effecsize.model.glm.basic
effecsize.glm.refit<- effectsize(model.glm.gamma.log2, method = "refit")#metodo refit não padroniza os fatores. émelhor usar basic nesses casos ou padronizar antes de rodar o modelo. 
effecsize.glm.refit

library(ggplot2)
library(sjPlot)
library(jtools)
library(interactions)
library(broom.mixed)
library(dotwhisker)
library(sjmisc)
library(ggeffects)
# Graficos de coeficientes:
# aqui o grafico de coeficientes brutos
dw.glm <- dwplot(model.glm.gamma.log2, show_intercept = TRUE)
dw.glm
print(dw.glm+geom_vline(xintercept=0,lty=2))
#aqui o grafic de coeficientes padronizados, com escala do exixo logaritmica
plot_summs(model.glm.gamma.log2, scale = T, exp = TRUE)
#aqui os coeficientes já tranformados
plot_model(model.glm.gamma.log2, show.values = T, value.offset = 0.2)
#vejam mais detalhes em : https://cran.r-project.org/web/packages/sjPlot/vignettes/plot_model_estimates.html

#Ver os resultados brutos:
#9. grafico final do modelo
interactions::interact_plot(model.glm.gamma.log2, pred=seedweight_mg, modx=substrate, 
                            plot.points=T, interval=T, outcome.scale="response")

