#################################################################################
#                      Universidade Federal de Pelotas                          #
#               Departamento de Zooligia, Ecologia e Genética                   #
#                   Mestrado em Biodiversidade Animal                           #
#                            Theo Lazzari                                       #
#################################################################################

# Script elaborado para criação de um modelo linear utilizando os dados das formigas

# Importação dos dados 

AntSpeed <- read.table("AntSpeed.txt", header = TRUE, sep = "\t")
#Isso garante que o modelo reconheça as variáveis categóricas em um fator
AntSpeed$colonyID <- as.factor(AntSpeed$colonyID)
AntSpeed$substrate <- as.factor(AntSpeed$substrate)

#  Análise exploratória dos dados
plot(AntSpeed[,c(1,2,3,5,5)])

# Criando um modelo com: lm(ColonyID+Subtrate+Seedweight+subtrate:Seedweight)
library(performance)
model.geral <- lm(antspeed_cm.s~colonyID+substrate+seedweight_mg+substrate:seedweight_mg,
                  data= AntSpeed)
check_model(model.geral)

#  Testar homogeneidade de variancia (ou seja, heterocedasticidade)
hetero.model.geral <- check_heteroscedasticity(model.geral)
hetero.model.geral
plot(hetero.model.geral)
# >Warning: Heteroscedasticity (non-constant error variance) detected (p < .001).

#  Para deixar os dados mais homogenios 
model.log <- lm(log(antspeed_cm.s) ~ colonyID + substrate + seedweight_mg + 
                  substrate:seedweight_mg, data = AntSpeed)
check_model(model.log)
hetero.model.log <- check_heteroscedasticity(model.log)
hetero.model.log
plot(hetero.model.log)
# >OK: Error variance appears to be homoscedastic (p = 0.557).

# Testar a  normalidade
ResidualModel.log <- residuals(model.log)
hist(ResidualModel.log)#mostra a distribuição dos residuos (ver normalidade)
#test visual de normalidade
qqnorm(ResidualModel.log)
qqline(ResidualModel.log)
shapiro.test(ResidualModel.log)#test de normalidae
# >p-value = 5.682e-06 os resíduos não seguem uma distribuição normal
# Outra opção para testar normalidade
check_normality(model.log)
plot(check_normality(model.log))
# >os resíduos não seguem uma distribuição normal


# Checar outliers
check_outliers(model.log)
outliers.model.log <- check_outliers(model.log)
plot(outliers.model.log)

#checar multicolinearidade:
check_collinearity(model.log)
plot(check_collinearity(model.log))

# Ver resultado dos modelos
summary(model.geral)
summary(model.log)

library(ggplot2)
library(sjPlot)
plot_model(model.log, type = "pred" ,show.data=T, jitter = T)


# Ajustando preditoras (criando modelos com outras preditoras)
# 
#removendo interação substrate e seedweight_mg
model.log.ajust1 <- lm(log(antspeed_cm.s) ~ colonyID + substrate + seedweight_mg,
                data = AntSpeed)
check_model(model.log.ajust1)
hetero.model.log.ajust1 <- check_heteroscedasticity(model.log.ajust1)
hetero.model.log.ajust1
summary(model.log.ajust1)
#Para comparar os dois modelos
anova(model.log.ajust1, model.log)
# > O modelo sem a interação se mostrou um pouco melhor

#Plotando o modelo sem interação
plot_model(model.log.ajust1, type = "pred" ,show.data=T, jitter = T)

#Simplificar mais ainda o modelo
model.log.ajust2 <- lm(log(antspeed_cm.s) ~ colonyID + seedweight_mg,
                      data = AntSpeed)
check_model(model.log.ajust2)
hetero.model.log.ajust2 <- check_heteroscedasticity(model.log.ajust2)
hetero.model.log.ajust2
summary(model.log.ajust2)
anova(model.log.ajust1, model.log.ajust2)
# A variável substrate é importante para o modelo

# Modelo somente com o peso da semente influenciando na velocidade

model.log.ajust3 <- lm(log(antspeed_cm.s) ~ seedweight_mg,
                       data = AntSpeed)
summary(model.log.ajust3)
check_model(model.log.ajust3)
hetero.model.log.ajust3 <- check_heteroscedasticity(model.log.ajust3)
hetero.model.log.ajust3

# Teste de verosimilhança
library(lmtest)
lrtest(model.log, model.log.ajust1, model.log.ajust2, model.log.ajust3)

## Conclusão: Dada a homocedasticidade observada nos modelos logarítmicos e a 
## importância do peso da semente em todos os modelos, a melhor abordagem para 
## explicar a relação entre o peso da semente e a velocidade das formigas é utilizando
## os modelos em logarítmicos. O modelo logarítmico mais simples (model.log.ajust1) 
## seria uma escolha sólida, pois oferece um bom ajuste (R² = 0.2065) e atende à 
## suposição de homocedasticidade.Porém o modelo (model.log.ajust2) também pode ser considerado
## caso a variável substrato seja relevante para análise

# Para decidir o modelo escolhido:
anova(model.log.ajst1, model.log.ajust2, model.log, model.log.ajust3)
#Representação gráfica 
library(ggplot2)

ggplot(AntSpeed, aes(x = substrate, y = log(antspeed_cm.s), color = colonyID)) +
  geom_boxplot() +
  labs(title = "Efeito do Substrato na Velocidade das Formigas", x="Substrato",
       y="Velocidade das formigas log(cm.s)", color="Colônias")

# O substrato é importante para o modelo, então o modelo escolhido é o model.log.ajust1

