
###Análise de dados biológicos: Modelos lineares, generalizados e mistos
#Script produzido para a disciplina
#Autor: Prof. Sebastian Felipe Sendoya

###Aula 10 Construindo modelos clássicos no R Parte 3



#segundo exemplo usando a escala amostral na armadilha(não dá parametrico)

#1. Anova simples relacionando riqueza total
#1.1.Aqui é o grafico diagnistico onde podemos ver a variação total a ser explicada
plot(1:26, xdatLocal$RiquezaTotal, xlab="Ordem dos locais",  ylab="Riqueza total por local")
abline(h=mean(xdatLocal$RiquezaTotal),col="blue")
for (i in 1:26){
  lines(c(i,i),c(mean(xdatLocal$RiquezaTotal),xdatLocal$RiquezaTotal[i]),col="green")
}


#1.2.aqui vamos construir um grafico para ver qual é a variação entre os niveis (entre as médias)
plot(1:26,xdatLocal$RiquezaTotal,ylab="Riqueza total por local",xlab="Ordem dos locais",
     pch=21,col=c("red","blue")[as.factor(xdatLocal$Protecao)])
abline(h=mean(xdatLocal$RiquezaTotal[xdatLocal$Protecao=="ResLeg"]), col= "red")
abline(h=mean(xdatLocal$RiquezaTotal[xdatLocal$Protecao=="UniCons"]), col="blue")


index <- 1:length(xdatLocal$RiquezaTotal)
for (i in 1:length(index)){
  if (xdatLocal$Protecao[i]=="ResLeg")
    lines(c(index[i],index[i]),c(mean(xdatLocal$RiquezaTotal[xdatLocal$Protecao=="ResLeg"]),
                                 xdatLocal$RiquezaTotal[i]),col="red")
  else
    lines(c(index[i],index[i]),c(mean(xdatLocal$RiquezaTotal[xdatLocal$Protecao=="UniCons"]),
                                 xdatLocal$RiquezaTotal[i]),col="blue" )
}                                


#Abordagem com boxplot

boxplot(RiquezaTotal ~ Protecao,
        data = xdatLocal,
        xlab = "Estado proteção da área",
        ylab = "Riqueza total de espécies")

library(ggplot2)
ggplot(xdatLocal, aes(x = Protecao, y = RiquezaTotal)) +
  geom_boxplot(aes(fill = RiquezaTotal), outlier.shape = NA) +
  geom_jitter(aes(color = RiquezaTotal), width = 0.2, alpha = 0.5) +
  theme_minimal() +
  labs(title = "Box Plot com Densidade", x = "Estado proteção da área" , y = "Riqueza total de espécies")


#Estabelecer u₼fator para Protecao:
xdatLocal$Protecao2<-factor(xdatLocal$Protecao, 
                            levels= c("UniCons", "ResLeg"))

RiqProMod<- lm(RiquezaTotal~Protecao2, data= xdatLocal)

#1.3. Verificar presupostos do modelo
#análise grafico da adequabilidade do modelo
plot(RiqProMod)
#plot 1 comparar as vriancias, aparenta problemas
#plot 2: qq plot. Razonable linearidad (com excção do ponto 6)
#plot 4, confirma o efeito extremo do ponto 6.

library(performance)
check_model(RiqProMod)
check_normality(RiqProMod)
check_heteroscedasticity(RiqProMod)
check_outliers(RiqProMod)

#1.5. Resultados
#Consultar os contrastes do fator (0 é a referência usada pelo R)
contrasts(xdatLocal$Protecao2)
#Análise Tipo 1
summary(aov(xdatLocal$RiquezaTotal~xdatLocal$Protecao))
summary(lm(xdatLocal$RiquezaTotal~xdatLocal$Protecao))
summary(lm(xdatLocal$RiquezaTotal~xdatLocal$Protecao2))

summary(RiqProMod)

#Análise Tipo II

library(car)
Anova(RiqProMod)

#1.6. Vamos graficar os resultados
#Figura base
boxplot(RiquezaTotal~Protecao, data= xdatLocal, xlab= "Estado de proteção da floresta", ylab= "Riqueza total", notch=T)#grafico descritivo dos dados

#Estes gráficos permitem ver os dados e ao memso tempo as predições (coeficeintes) do modelo
#neste caso as médias e os IC
library(ggplot2)
library(sjPlot)
library(jtools)
library(interactions)
#opção1
cat_plot(RiqProMod, pred = Protecao2, plot.points=T, interval=T, errorbar.width=0.3, outcome.scale = "link"  )
#opção 2
plot_model(RiqProMod, type = "eff",terms= "Protecao2", show.data=T, show.p=T, jitter=c(0.1,0))


#Para comparaçãos a posteriori em caso de vários niveis

RiqAov<-aov(xdatLocal$RiquezaTotal~xdatLocal$Protecao)
TukeyHSD(RiqAov)




#2. Anova fatorial
#Vamos comparar os efeitos de tipo de floresta e tipo de solo na escla amostral de armadilha

#2.1. Grafico exploratorio
barplot(tapply(xdat.amostra$RiqAmos,list(xdat.amostra$Estado,xdat.amostra$SoloAmos),mean),beside=T, ylab="Riqueza por amostra",xlab="Tipo de solo",ylim=c(0,6))

legend(locator(1),legend=c("Primaria","Secundaria"),title="Estado sucessional",
       fill=c("black","lightgrey")) 

boxplot(RiqAmos~SoloAmos*Estado, data=xdat.amostra, frame = FALSE, 
        col = c("#00AFBB", "#E7B800"), ylab="Riqueza por amostra")

interaction.plot(x.factor = xdat.amostra$SoloAmos, trace.factor = xdat.amostra$Estado, response = xdat.amostra$RiqAmos, fun = mean, type = "b",legend = TRUE, xlab = "Estado sucessional", ylab="Riqueza por amostra", pch=c(1,19), col = c("#00AFBB", "#E7B800"))


#2.2. Construimos o modelo
model.Facto.Aov <- aov(RiqAmos~Estado*SoloAmos, data= xdat4 )
summary(model.Facto.Aov)

model.Facto.lm <- lm(RiqAmos~Estado+SoloAmos+Estado:SoloAmos, data= xdat.amostra )
summary(model.Facto.lm)

#2.3. agora vamos a aprimorar os contrastes
xdat.amostra$Solo2 <- factor(xdat.amostra$SoloAmos)#duplicamos a coluna com o fator solo
levels(xdat.amostra$Solo2)

#vamos agrupar os niveis da variavel, de 4 passamos somente 2
levels(xdat.amostra$Solo2)[c(1,4)] <- "NaoInteragem"
levels(xdat.amostra$Solo2 )[c(2,3)] <- "Interagem"
levels(xdat.amostra$Solo2 )

#recriams o modelo com a nova variavel
model.Facto.Aov.2 <- aov(RiqAmos~Estado*Solo2, data= xdat.amostra )
summary(model.Facto.Aov.2)
model.Facto.lm.2 <- lm(RiqAmos~Estado*Solo2, data= xdat.amostra )
summary(model.Facto.lm.2)

#2.4.vamos comparar a verossimilhasa do modelo com novo agrupamento de niveis
#(estimada pela soma de quadrados do residuo)
anova(model.Facto.lm.2,model.Facto.lm) # p>0.05 indica que os modelos são equivalentes
library(car)
Anova(model.Facto.lm.2)

#2.5. Vamos testar as premissas
plot(model.Facto.lm.2)
library(performance)
check_model(model.Facto.lm.2)
hetero.temp <- check_heteroscedasticity(model.Facto.lm.2)
hetero.temp
check_outliers(model.Facto.lm.2)
check_normality(model.Facto.lm.2)
#normalidade
Model.F.lm2.Resi<- residuals(model.Facto.lm.2 )
shapiro.test(x = Model.F.lm2.Resi )# normalidade marginal



#2.7. Gráficos
#opção 1
library(interactions)
interactions::cat_plot(model.Facto.lm.2, pred = Solo2, modx=Estado ,plot.points=T, interval=T,
         errorbar.width=0.4,colors= "Qual1", y.label="Riqueza por amostra",x.label="Grupo de solo", geom= "line", dodge.width= 0.6 )

#opção 2
plot_model(model.Facto.lm.2, terms = c("Solo2","Estado"), type = "eff",
           show.data=T, jitter=c(0.1,0), grid.breaks=0.5, 
           dot.size=2, line.size=1.2)+
  xlab("Efeito do solo")+ ggtitle("")+theme_minimal()


#2.8. Apresentando resultados
plot_model(model.Facto.lm.2, type = "std", se=T,show.values=T, digits=4 )+
  theme_minimal()

#2.9. Saida do programa
library(MASS)
library(stargazer)
stargazer(model.Facto.lm.2,  type = "text", style= "all", single.row=TRUE, 
          ci.level=0.95, out = "resultsFactorial.html", report= "vcsp*")



#3. Analise de co-variancia

#Vamos comparar os efeitos de tipo unidade de conservação e Precipitação 
#na riqueza total na escala amostral de localidade

#3.1. Graficos exploratorio----

plot(xdatLocal$Precipit, xdatLocal$RiquezaTotal,
     ylab="Riqueza Total por local",xlab="Precipitação (mm)")

boxplot(RiquezaTotal~Protecao, data=xdatLocal, 
        ylab="Riqueza Total por local", col=c(2,3))
plot(xdatLocal$Precipit, xdatLocal$RiquezaTotal,
     ylab="Riqueza Total por local",xlab="Precipitação (mm)", 
     col=c(2,3)[as.factor(xdatLocal$Protecao)], pch= 7)

#3.2. Construimos o modelo

model.cova.lm <- lm(RiquezaTotal~Precipit*Protecao, data= xdatLocal )
summary(model.cova.lm)
anova(model.cova.lm)
library(car)
Anova(model.cova.lm)
#3.3. Começamos a simplificação do modelo
model.cova.lm.2 <- lm(RiquezaTotal~Precipit+Protecao, data= xdatLocal )
summary(model.cova.lm.2)

anova(model.cova.lm, model.cova.lm.2)


model.cova.lm.3 <- lm(RiquezaTotal~Precipit, data= xdatLocal )
anova(model.cova.lm.2, model.cova.lm.3)



library(performance)
check_model(model.cova.lm.2)

#3.4. Gerar gráfico final, duas aboradegnes de gráfico
plot(xdatLocal$Precipit,xdatLocal$RiquezaTotal,pch=21,
     bg=c(2,3)[as.factor(xdatLocal$Protecao)])
legend(locator(1),c("Unidade de conservação","Reserva legal"),col=c(2,3),pch=16)

library(interactions)
interact_plot(model.cova.lm.2, pred=Precipit, modx=Protecao, 
              plot.points=T, interval=T, int.type="prediction", 
              outcome.scale="response",
              y.label="Riqueza Total por local",x.label="Precipitação (mm)",
              modx.labels= c("Unidade de conservação","Reserva legal"))

plot_model(model.cova.lm, type = "std", se=T,show.values=T, digits=4 )+
  theme_minimal()
