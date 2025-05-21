
###Análise de dados biológicos: Modelos lineares, generalizados e mistos
#Script produzido para a disciplina
#Autor: Prof. Sebastian Felipe Sendoya

###Aula 9 Construindo modelos clássicos no R Parte 2


#1. Avaliação geral inicial para modelo linear

#1.1. Avaliação inicial: vamos relacionar a Riqueza total com a temperatura
plot(RiquezaTotal~Temp, data= xdat.local, xlab="Temperatura (Cº)",
     ylab="Riqueza total no local")
#1.2. Construção inicial do modelo
lm(RiquezaTotal~Temp, data= xdatLocal) #calcula o modelo linear simples
#preste atenção aos coeficientes que o modelo produz no console. O que significam?
Temp.lm.tot<-lm(RiquezaTotal~Temp, data= xdatLocal) #guarda o modelo em um objeto

## Verificar as premissas do teste
par(mfrow = c(2, 2), oma = c(0, 0, 2, 0))
plot(Temp.lm.tot)
dev.off() # volta a configuração dos gráficos para o formato padrão

#1.3. para testar homogeneidade de variancia (ou seja, heterocedasticidade)
library(performance)
hetero.temp.tot <- check_heteroscedasticity(Temp.lm.tot)
hetero.temp.tot
plot(hetero.temp.tot)# o plot deve mostrar uma linha de tendencia o mais horizontal possivel

#1.4. para testar a premissa de normalidade
ResidualModel.tot <- residuals(Temp.lm.tot)
hist(ResidualModel.tot)#mostra a distribuição dos residuos (ver normalidade)
shapiro.test(ResidualModel.tot)#test de normalidae
shapiro.test(residuals(Temp.lm.tot))

qqnorm(ResidualModel.tot)#test visual de normalidade
qqline(ResidualModel.tot)
#qplot com pacote car
library(car)
qqPlot(Temp.lm.tot)

#Normalidade com pacote performance
library(performance)
check_normality(Temp.lm.tot)
normality.temp.tot <- check_normality(Temp.lm.tot)
plot(normality.temp.tot)

#verificação completa com pacote performance
library(performance)
check_model(Temp.lm.tot)

#Checar outliers, pontos muito influentes
library(performance)
check_outliers(Temp.lm.tot)
outliers.temp.tot <- check_outliers(Temp.lm.tot)
plot(outliers.temp.tot)

#1.6. vamos graficar os residuos do modelo
plot(RiquezaTotal~Temp, data= xdat.local, xlab="Temperatura (Cº)",
     ylab="Riqueza total no local", xlim=c(0, 50), ylim=c(0, 13))
abline(Temp.lm.tot,col="green") #adiciona a linha de tendencia
fitted.Temp.tot <- predict(Temp.lm.tot)
fitted.Temp.tot

#agora vamos mostrar graficamente os residuos
for (i in 1:26){
  lines(c(xdatLocal$Temp[i],xdatLocal$Temp[i]),c(xdatLocal$RiquezaTotal[i],fitted.Temp.tot[i]),col="red")
}

#1.5.  Ver um resultado consolidado do modelo
summary(Temp.lm.tot) #preste atenção nos valrs dos parametros (Estimate) dos erros,
#valores de p, F e R2.
#Resultado em tabela tipo ANOVA
summary.aov(Temp.lm.tot)
#Outra opção
library(car)
Anova(Temp.lm.tot)

#1.6. Graficos diagnosticos
plot(Temp.lm.tot)
#plot 1. Ideal: Sem padrão, homogeneo ao longo de x
#PLot 2: Ideal linha reta diagional. Forma de "s" ou de arco indicam falata de normalidade
#PLot 3: Ideal similar ao P1. Evitar dados formando triangulo
#PLot 4: POntos influentes. Leverage é uma medida da influencia do ponto no modelo. 
#Prestar atenção a pontos perto ou depois das linhas vermelhas (alto leverage e alto residuo)
influence.measures(Temp.lm.tot)#detalahmento do plot4. Observar valores maiore a 2*p/n, p = no de parametros

#1.7. gráfico final
library(ggplot2)
library(sjPlot)
plot_model(Temp.lm.tot, type = "pred",show.data=T)

plot_model(Temp.lm.tot, type = "eff", terms = "Temp" , show.values=T, 
           show.data=T, show.p=T, colors = "gs",
           dot.size=2, line.size=1.1, ci.lvl=0.95) +
  xlab("Temperatura (Cº)")+ ylab("Riqueza total no local")+ 
  ggtitle("")+theme_minimal()+
  geom_line(color="#000000")+
  theme(
    legend.position="none",
    plot.title = element_text(size=11),
    axis.line.y = element_line(linewidth = 1, colour = 1),
    axis.line.x = element_line(linewidth = 1, colour = 1),
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    axis.text.x = element_text(size = 13),
  )



##1.8 Gráfico no ggplot2
library(ggplot2)
library(ggpubr)
ggplot(data = xdatLocal, aes(x = Temp, y = RiquezaTotal)) + 
  labs(x = "Temperatura média anual (°C)", 
       y = "Riqueza amostrada de espécies") +
  geom_point(size = 4, shape = 21, fill = "chocolate3", alpha = 0.7) +
  geom_smooth(method = lm, se = FALSE, color = "black") +
  theme_minimal() +
  theme(legend.position = "none")+
  stat_regline_equation(label.x = 10, label.y = 10)

#1.9. Gráfico detalhado
library(ggstatsplot)
ggscatterstats(
  data  = xdatLocal,
  x     = Temp,
  y     = RiquezaTotal,
  title = "Riqueza de espécies"
)

#Mostrar intervalos de confiança
confint(Temp.lm.tot)


###2. Regressão multipla
#2.1. vamos relacionar a Riqueza total com a temperatura
plot(RiquezaTotal~Precipit, data= xdatLocal, xlab="Precipitação anual (mm)",
     ylab="Riqueza total no local")
plot(RiquezaTotal~Alti, data= xdatLocal, xlab="Altitude (m)",
     ylab="Riqueza total no local")
pairs(xdatLocal[c(7,2,3,4,10)], panel=panel.smooth)

#2.2 Modelo
Reg.mult.full<- lm(RiquezaTotal~Temp+Alti+Precipit+MOAmost, data= dados_local) #calcula o modelo linear simples

#2.3. Testando premisas:
library(performance)
check_heteroscedasticity(Reg.mult.full)
plot(check_heteroscedasticity(Reg.mult.full))
check_normality(Reg.mult.full)
plot(check_normality(Reg.mult.full))
check_outliers(Reg.mult.full)
plot(check_outliers(Reg.mult.full))
check_model(Reg.mult.full)

library(corrplot)
#matriz de correlação
Matriz.cor<- cor(dados_local[,c(2:4,10)])
Matriz.cor
#valores de P
testRes <- cor.mtest(xdatLocal[,c(2:4,10)], 
                     conf.level = 0.95)
testRes

#checar multicolinearidade:
check_collinearity(Reg.mult.full)
plot(check_collinearity(Reg.mult.full))


#Se alguma das premissas não se cumpre o modelo não é confiavel
#No caso o alto valor de VIF indica colinearidade, por tanto se recomenda remover
#uma variavel (Exemplo: Temperatura)

#Modelo 2 sem temperatura
Reg.mult.v2<- lm(RiquezaTotal~Alti+Precipit+MOAmost, data= xdatLocal) #calcula o modelo linear multiplo
check_model(Reg.mult.v2)

#2.4. Resultados
summary(Reg.mult.v2)

#2.5. Teste de verosimilhança
##Modelo 3 sem MOAmost
Reg.mult.v3<- lm(RiquezaTotal~Alti+Precipit, data= dados_local)
## Likelihood-ratio test (LRT)
library(lmtest)
lrtest(Reg.mult.v2, Reg.mult.v3)
anova(Reg.mult.v2)
library(car)
Anova(Reg.mult.v2)

##Modelo 4 sem Precipit
Reg.mult.v4<- lm(RiquezaTotal~Alti, data= dados_local)
library(lmtest)
lrtest(Reg.mult.v3, Reg.mult.v4)
anova(Reg.mult.v3)
library(car)
Anova(Reg.mult.v3)

##Modelo nulo
##
Reg.mult.nulo<- lm(RiquezaTotal~1, data= dados_local)
lrtest(Reg.mult.v3, Reg.mult.nulo)

#2.6. Seleção stepwise UP por AIC
step(Reg.mult.nulo,
     scope = list(upper=Reg.mult.v2),
     direction="both",
     data=xdatLocal) 

#2.7. Seleção stepwise down por AIC
step(Reg.mult.v2,
     scope = list(lower=Reg.mult.nulo),
     direction="both",
     data=xdatLocal) 


#2.9. Força bruta: MuMin
library(MuMIn)
options(na.action = "na.fail")
Model.sele <- dredge(Reg.mult.v2)
Model.sele


#2.10. Apresentando resultados
library(ggplot2)
library(sjPlot)
plot_model(Reg.mult.v3, type = "eff", terms = c("Alti", "Precipit") , se=T, show.values=T, digits=3, show.data = T )+
theme_minimal()

#2.15. Consolidabdo resultados:
#Resultado do modelo escolhido
summary(Reg.mult.v3)#resumo
confint(Reg.mult.v3)#Intervalos de confiança

library(arm)
fig10_3 <- coefplot(Reg.mult.v3)# Gráfico de coeficientes

#2.12. Saida do programa
library(MASS)
library(stargazer)
stargazer(Reg.mult.v3,  type = "text", style= "all", single.row=TRUE, 
          ci.level=0.95, out = "resultsMulti.html", report= "vcsp*")

#2.13. Graficos alternativa 2
ggplot(dados_local,aes(y=RiquezaTotal,x=Alti, color=Precipit))+
  geom_point(cex=3)+
  stat_smooth(method="lm",se=FALSE)

#2.14. Graficos alternativa 3
library(ggiraphExtra)
ggPredict(Reg.mult.v3,interactive=TRUE)
