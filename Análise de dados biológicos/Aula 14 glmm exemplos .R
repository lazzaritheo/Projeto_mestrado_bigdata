###Análise de dados biológicos: Modelos lineares, generalizados e mistos
#Script produzido para a disciplina
#Autor: Prof. Sebastian Felipe Sendoya

###Aula 14 Construindo modelos generalizados mistos GLMMs.

# exemplo


library(lme4)
library(MASS)
library(car)
library(afex)
library(Rcpp)
library(Matrix)
library(readxl)
library(lmerTest)
library(performance)
library(see)
library(qqplotr)
library(DHARMa)
library(effectsize)
library(ggplot2)
library(sjPlot)
library(jtools)
library(interactions)
library(broom.mixed)
library(dotwhisker)
library(sjmisc)
library(ggeffects)



#carregandoos dados
library(readxl)
Flores.Nec <-read_xlsx("exemplo_mistos.xlsx", sheet=1) #importa diretamente do arquivo excel
str(Flores.Nec)
Flores.Nec$Sexo<- factor(Flores.Nec$Sexo) #transforma a variavel em um fator
Flores.Nec$Individuo<- factor(Flores.Nec$Individuo) #transforma a variavel em um fator
Flores.Nec$Fac.Horario<- factor(Flores.Nec$Horario) #transforma a variavel em um fator

levels(Flores.Nec$Sexo)
levels(Flores.Nec$Fac.Horario)

#1. Analise exploratória: tendencias gerais
plot(Flores.Nec)
plot( Flores.Nec$Horario, Flores.Nec$Nectar)
boxplot(Nectar~Sexo, data=Flores.Nec, col=c("blue","green")) #mostra varia'cao entre individuos



#2. construção do modelo
library(lme4)
library(lmerTest)

# 2.1. comparar com o modelo sem componente aleatorio
Nectar.lme4.1 <- lmer(Nectar~Sexo+ (1|Horario), data = Flores.Nec)
summary(Nectar.lme4.1) #Mostra resultados

Nectar.lme4.1B <- lm(Nectar~Sexo, data = Flores.Nec)
summary(Nectar.lme4.1B) #Mostra resultados
anova(Nectar.lme4.1,Nectar.lme4.1B, test='Chisq')# este test mostra que adicionar a variavel aleatoria melhora muito a explicação


#2.2. Premisas
library(performance)
library(see)
library(qqplotr)
check_model(Nectar.lme4.1)
check_normality(Nectar.lme4.1)
check_outliers(Nectar.lme4.1)
testDispersion(Nectar.lme4.1)# Teste de dispersão com o Dharma
aveiaid <-simulateResiduals(Nectar.lme4.1, plot = T)

#qqplot:
qqnorm(resid(Nectar.lme4.1));qqline(resid(Nectar.lme4.1))

#2. agora vamos iniciar a simplificação
Nectar.lme4.2 <- lmer(Nectar~1+ (1|Horario),  data = Flores.Nec) 
summary(Nectar.lme4.2) #Comparar resultados com modelo anterior
#comparação de modelos:
anova(Nectar.lme4.1,Nectar.lme4.2, test='Chisq') 
Anova(Nectar.lme4.1)

#3. Analizemos o modelo
summary(Nectar.lme4.1)
#preste atenção à parte inicial com as variacia explicada pela variavel aleatoria
#e a variaça residual
coef(Nectar.lme4.1) #veja os intereptos para cada horario variam
#o intercepto no summary é a media dos interceptos de cada horario


#3.1 Calcular o R2
library(performance)
r2(Nectar.lme4.1) #perceba a diferença entre R2 marginal e o condicional


#4. coeficientes padronizados
library(effectsize)
effecsize.model.nectar<- effectsize(Nectar.lme4.1, method = "refit")
effecsize.model.nectar


library(ggplot2)
library(sjPlot)
library(jtools)
library(interactions)
library(broom.mixed)
library(dotwhisker)
library(sjmisc)
library(ggeffects)

#4.1. grafico de coeficientes brutos
plot_summs(Nectar.lme4.1, scale = T)
#4.1.grafico com coef padronizados
plot_model(Nectar.lme4.1, show.values = T, value.offset = .3,type = "std" )
#4.1.grafico com coef transformados
plot_model(Nectar.lme4.1, show.values = T, value.offset = .2)

#função para mostar resultados compilados
tab_model(Nectar.lme4.1, show.re.var= TRUE)



#Grafico agrupado
predictions.nectar<-ggpredict(Nectar.lme4.1, type ="re" , terms = c("Sexo","Horario")) 
ls(predictions.nectar)
predictions.nectar
predictions.nectar$group
predictions.nectar$x

plot(predictions.nectar)
plot(predictions.nectar$x, predictions.nectar$predicted)


ggplot(Flores.Nec,  aes(x=Sexo, y=Nectar)) +
  geom_boxplot(aes(fill=Sexo), fill= c("blue", "green"), alpha=0.4) +
  #scale_fill_viridis() +
  geom_jitter(aes(x=Sexo, y=Nectar, color=Fac.Horario), size=2, alpha=0.9, width = 0.1) +
  geom_line(predictions.nectar, mapping= aes(x = x, y = predicted, colour = factor(group), 
                                              group=factor(group) ))+
  scale_color_brewer(palette="Spectral")+
  theme_minimal() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  xlab("Sexo da flor")+
  ylab("Nectar coletado")+
  theme(
    plot.title = element_text(size=11),
    axis.line.y = element_line(linewidth  = 1, colour = 1),
    axis.line.x = element_line(linewidth  = 1, colour = 1),
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    axis.text.x = element_text(size = 13),
  )






#--------------------------


####Exemplo 2: Presença de fungos em diferentes variedades de aveia

Aveia.dat <-read_xlsx("exemplo_mistos_2.xlsx", sheet=1) #importa diretamente do arquivo excel
str(Aveia.dat)
Aveia.dat$variety<- factor(Aveia.dat$variety) #transforma a variavel em um fator
Aveia.dat$block    <- factor(Aveia.dat$block) #transforma a variavel em um fator
Aveia.dat$Plot    <- factor(Aveia.dat$Plot) #transforma a variavel em um fator
Aveia.dat$Categoria   <- factor(Aveia.dat$Categoria) #transforma a variavel em um fator


levels(Aveia.dat$Plot)
levels(Aveia.dat$variety)
levels(Aveia.dat$Categoria)

#1. analises visual
plot(Aveia.dat)
boxplot(Fungi ~ variety, data = Aveia.dat)

ggplot(Aveia.dat,  aes(x=variety, y=Fungi, fill=Categoria)) +
  geom_boxplot(aes(fill=Categoria), alpha=0.4) +
  theme_minimal() +
  theme(
    legend.position="top",
    plot.title = element_text(size=11)
  ) +
  xlab("Variedade")+
  ylab("Riqueza de fungos")+
  theme(
    plot.title = element_text(size=11),
    axis.line.y = element_line(size = 1, colour = 1),
    axis.line.x = element_line(size = 1, colour = 1),
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    axis.text.x = element_text(size = 13),
  )



#2. Distribuição do erro
# distribuição tem mais chance de se ajustar (veja a distancia de ponto azul)
library(fitdistrplus) #PACOTE PARA COMAPRAR DISTRIBUIÇÕES
descdist(Aveia.dat$Fungi, discrete = T, boot = 1000)


#2.1. Podemos construir modelos com distribuições diferentes e comparar os performances

model.ave.poi.1 <- glmer(Fungi ~ variety+(1|block/Categoria), data = Aveia.dat, family = poisson, na.action = "na.fail")
summary(model.ave.poi.1)

model.ave.Gau.1 <- lmer(Fungi ~ variety+(1|block/Categoria), data = Aveia.dat, na.action = "na.fail")
summary(model.ave.Gau.1)

model.ave.poi.2 <- glmer(Fungi ~ variety+(1|block)+(1|Plot), data = Aveia.dat, family = poisson, na.action = "na.fail")
summary(model.ave.poi.2)

#Percebam que o resultado de model.ave.poi.1 é igual ao de model.ave.poi.2

model.ave.Nb.1 <- glmer.nb(Fungi ~ variety+(1|block/Categoria), data = Aveia.dat, na.action = "na.fail")
#temos um erro no calculo de um dos parametros

#Para evitar o  erro na estimação de theta vamos rodar todos os modelos no pacote glmmTMB

library(glmmTMB)
library(bbmle)
model.ave.Nb.2<- glmmTMB(Fungi ~ variety+(1|block/Categoria), data = Aveia.dat, na.action = "na.fail", family = nbinom1)
model.ave.Gau.2<- glmmTMB(Fungi ~ variety+(1|block/Categoria), data = Aveia.dat, na.action = "na.fail", family = gaussian)
model.ave.poi.2<- glmmTMB(Fungi ~ variety+(1|block/Categoria), data = Aveia.dat, na.action = "na.fail", family = poisson)


#comparamos os modelos com as 3 dsitribuições
AICctab(model.ave.Nb.2, model.ave.Gau.2, model.ave.poi.2)
summary(model.ave.poi.2)

#Comparaçao de distribuiçãocom pacote perfomance
library(performance)
library(see)
library(qqplotr)

Model.dist <- compare_performance(model.ave.Nb.2, model.ave.Gau.2, model.ave.poi.2)
Model.dist
plot(Model.dist)

#3. construção do modelo

# 3.1. comparar com o modelo sem componente aleatorio
model.ave.poi.3 <- glmmTMB(Fungi ~ variety+(1|Plot), data = Aveia.dat, na.action = "na.fail", family = poisson)
anova(model.ave.poi.2,model.ave.poi.3, test='Chisq')# este test mostra que adicionar a variavel aleatoria bloco não afeta a explicação

model.ave.poi.4 <- glmmTMB(Fungi ~ variety, data = Aveia.dat, na.action = "na.fail", family = poisson)
anova(model.ave.poi.3,model.ave.poi.4, test='Chisq')# este test mostra que adicionar a variavel aleatoria plot afeta os resultados

#o desenho aninhado e a variaçãop em maior escala não afeta os padrões da resposta, as variações locais (plot) sim.

#3.2. agora vamos testar os componentes fixos
model.ave.poi.5  <- glmmTMB(Fungi ~ 1+(1|Plot), data = Aveia.dat,
                            na.action = "na.fail", family = poisson)
anova(model.ave.poi.3,model.ave.poi.5, test='Chisq')# este test mostra que adicionar a variavel aleatoria plot afeta os resultados

#Modelo escolhido
summary(model.ave.poi.3) #Comparar resultados com modelo anterior

#4. Analizemos o modelo
summary(model.ave.poi.3)
#preste atenção à parte inicial com as variacia explicada pela variavel aleatoria
#e a variaça residual
coef(model.ave.poi.3) #veja os intereptos para cada horario variam
#o intercepto no summary é a media dos interceptos de cada horario


#3.1 Calcular o R2
library(performance)
r2(model.ave.poi.3) #perceba a diferença entre R2 marginal e o condicional

#3.2 premissas

library(performance)
library(see)
library(qqplotr)


#checar normlidade dos residuos
normality2 <- check_normality(model.ave.poi.3, effects = "random")
normality2
check_model(model.ave.poi.3)
check_zeroinflation(model.ave.poi.3)

library(DHARMa)#pacote para analisar ajuste e premisas de modelos
testDispersion(model.ave.poi.3)# este test ajuda a reforçaro dito
aveiaid <-simulateResiduals(model.ave.poi.3, plot = T)
check_overdispersion(model.ave.poi.3)

#4. coeficientes padronizados
library(effectsize)
effecsize.model.aveia<- effectsize(model.ave.poi.3, method = "basic")
effecsize.model.aveia


#4.1. grafico de coeficientes brutos

#4.1.grafico com coef transformados
plot_model(model.ave.poi.3, show.values = T, value.offset = .2)

#função para mostar resultados compilados
tab_model(model.ave.poi.3, show.re.var= TRUE, show.aicc = T)

plot_model(model.ave.poi.3)

#Grafico agrupado
predictions.aveia<-ggpredict(model.ave.poi.3, type ="re" , terms = c("variety","Plot")) 
ls(predictions.aveia)
predictions.aveia
predictions.aveia$group

plot(predictions.aveia$x, predictions.aveia$predicted)




ggplot(Aveia.dat,  aes(x=variety, y=Fungi, fill=Plot)) +
  geom_boxplot(aes(fill=variety), alpha=0.4) +
  #scale_fill_viridis() +
  #scale_fill_brewer(palette="Spectral")+
  geom_jitter(aes(x=variety, y=Fungi, color=Plot), size=2, alpha=0.9, width = 0.1) +
  geom_line(predictions.aveia, mapping= aes(x = x, y = predicted, colour = factor(group),fill = factor(group), group=factor(group) ))+
  scale_color_brewer(palette="Spectral")+
  theme_minimal() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  xlab("Variedade")+
  ylab("Riqueza de fungos")+
  theme(
    plot.title = element_text(size=11),
    axis.line.y = element_line(size = 1, colour = 1),
    axis.line.x = element_line(size = 1, colour = 1),
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    axis.text.x = element_text(size = 13),
  )







# Exemplo 3 considerando a variação dos individuos no tempo

#vamos utilizar nosso exemplo das flores novamente agora imaginando que as medidas foram tiradas nos mesmos individuos

Flores.Nec$Individuo<- factor(Flores.Nec$Individuo) #transforma a variavel em um fator
Flores.Nec$Nectar <- as.numeric(Flores.Nec$Nectar)
Flores.Nec$Sexo <- as.factor(Flores.Nec$Sexo)
Flores.Nec$Horario <- as.numeric(Flores.Nec$Horario)

plot(Nectar~Sexo*Horario, data=Flores.Nec) #dispersão dos dados muito alta, será que tem tendência?

lm.out.1 <- lm(Nectar~Horario*Sexo, data=Flores.Nec) #cria modelos linear simples
summary(lm.out.1)

lm.out.2 <- lm(Nectar~Horario+Sexo, data=Flores.Nec) #cria modelos linear simples
summary(lm.out.2)

par(mfrow=c(1,1))
plot(Nectar~Horario, data=Flores.Nec, col=c("blue2","green")[Sexo]) #separa os pontos por sexo da flor
abline(lm.out.2$coefficients[1:2], col=c("blue2"))# desenha perdi??o do modelo linear para ambos sexos
abline(lm.out.2$coefficients[1]+lm.out.2$coefficients[3],lm.out.2$coefficients[2] , col=c("green"))# desenha perdi??o do modelo linear

#varia a quantidade nectar entre individuos?
boxplot(Nectar~Individuo, data=Flores.Nec) #mostra varia'cao entre individuos

plot(Nectar~Horario, data=Flores.Nec, col=c(1:10)[as.factor(Flores.Nec$Individuo)])

#já que os individuos quebram a independencia devemos incluir eles ocmo variavel aleatória

#2. construção do modelo com os interceptos aleatorios
library(lme4)
library(lmerTest)

#2.1 testando efeito s aleatorios
flor.lme4.1 <- lmer(Nectar~Horario*Sexo+ (1|Individuo), data = Flores.Nec, REML=FALSE)
summary(flor.lme4.1) #Mostra resultados

anova(flor.lme4.1,lm.out.1, test='Chisq') #comparação de modelos

coef(flor.lme4.1)


#2.2. testando efeitos fixos
flor.lme4.2 <- lmer(Nectar~Horario +Sexo+ (1|Individuo), data = Flores.Nec, REML=FALSE) #Duas varaiveis
summary(flor.lme4.2) #Comparar resultados com modelo anterior
anova(flor.lme4.1,flor.lme4.2, test='Chisq') #comparação de modelos

library(car)
Anova(flor.lme4.2)

coef(flor.lme4.2)# Ver os coeficientes para cada individuo




#  3. apresentação grafica
library(ggeffects)
predictions.flower1<-ggpredict(flor.lme4.2, type ="re" , 
                               terms = c( "Horario", "Sexo", "Individuo")) 
ls(predictions.flower1)
predictions.flower1
predictions.flower1$group
predictions.flower1$facet
predictions.flower1$x

plot(predictions.flower1, ci=T)

predictions.flower2<-ggpredict(flor.lme4.2, type ="re" , terms = c("Horario", "Individuo")) 
plot(predictions.flower2, color = c(1:10))
predictions.flower2$group


predictions.flower3<-ggpredict(flor.lme4.2, type ="re" , terms = c("Sexo", "Individuo")) 
plot(predictions.flower3, colors = c(1:10))


#2.3. Mas os indivduos não podem variar ao longo do tempo? Inclinação aleatoria
flor.lme4.3 <- lmer(Nectar~Horario*Sexo+ (Horario|Individuo), data = Flores.Nec, REML=FALSE) #Duas varaiveis
library(DHARMa)
check_model(flor.lme4.3)
check_outliers(flor.lme4.3)
check_heteroscedasticity(flor.lme4.3)
testDispersion(flor.lme4.3)# este test ajuda a reforçaro dito
aveiaid <-simulateResiduals(flor.lme4.3, plot = T)

summary(flor.lme4.3) #Comparar resultados com modelo anterior
anova(flor.lme4.1,flor.lme4.3, test='Chisq') #compara'c~ao de modelos
Anova(flor.lme4.3)

coef(flor.lme4.3)# Ver os coeficientes para cada individuo

flor.lme4.4 <- lmer(Nectar~Horario+Sexo+ (Horario|Individuo), data = Flores.Nec, REML=FALSE) #Duas varaiveis
anova(flor.lme4.3,flor.lme4.4, test='Chisq')


predictions.flower4<-ggpredict(flor.lme4.3, type ="re" , terms = c("Horario", "Individuo")) 
plot(predictions.flower4, color = c(1:10), show_data = F)
predictions.flower4$group


predictions.flower5<-ggpredict(flor.lme4.3, type ="re" , terms = c("Horario", "Sexo", "Individuo")) 
plot(predictions.flower5, show_data = T )



ggplot(Flores.Nec,  aes(x=Horario, y=Nectar)) +
  geom_jitter(aes(x=Horario, y=Nectar, color=Individuo), size=2, alpha=0.9, width = 0.1) +
  geom_line(predictions.flower5, mapping= aes(x =  x, y = predicted, colour = factor(facet), group=factor(group)))+
  #scale_color_brewer(palette="Paired")+
  theme_minimal() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)) +
  xlab("Horario de coleta")+
  ylab("Nectar coletado")+
  theme(
    plot.title = element_text(size=11),
    axis.line.y = element_line(size = 1, colour = 1),
    axis.line.x = element_line(size = 1, colour = 1),
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    axis.text.x = element_text(size = 13),
  )









