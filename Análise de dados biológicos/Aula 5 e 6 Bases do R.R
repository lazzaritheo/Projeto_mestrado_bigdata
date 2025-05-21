###Análise de dados biológicos: Modelos lineares, generalizados e mistos. V3

# Thu Oct 17 13:34:15 2024 ------------------------------


#Script produzido para a disciplina
#Autor: Prof. Sebastian Felipe Sendoya

###Aula5 organização e dados, excel e R 

###INSTALAR----
install.packages("swirl")#instalar um pacote
library(swirl)
update.packages(checkBuilt=TRUE) #atualiza os pacotes
updateR() # Atuallizar

###Comandos básicos do R----


##demos----
demo( )
demo(graphics)
#Ajudas----
library(lme4) # carrega libraria
?lme4 # ajuda do pacote ou funcao
help(lme4)#igual a anterior, veja atambem o nais do rstudio
help.search("lme4") # Procurar ajuda sobre uma funcao ou pacote não precisa estar escrita perfeitamente
args("rnorm")#argumentos da funcao "rnorm"
rnorm# se coloca o nome da funcao obtem o codigo usado na funcao

#Diretorio----
getwd() # saber o diretorio de trabalho WD
setwd("D:/Users/User/Disciplinas/ecol com desenho/Aulas")
ls() #objetos no WD


##citação----
citation( ) # use para o R e para os pacotes. deve citar no seu texto

##Natureza do R---
#R funciona na logica da criação e manipulação de objetos de diferentes naturezas


#trazendo os dados para o R
##Data frame. Uma tabela com elementos da mesma longitude, mas não necessariamente do mesmo tipo
#Cada elemnto (coluna tem um nome)

#Dataframes:----
#exemplo de dados pregravado no r
data(trees)# carrega os dados na area de trbalho
head(trees)#mostra o cabeçalho
summary(trees) #mostra um "resumo" da tabela (media, quartis etc.. )
str(trees)# estrutura da tabela

#Criando dataframe no r----
x<- data.frame(foo=1:4, bar= c(T, T, F, F))#foo e bar s?o s? os nomes da duas colunas creadas
ncol(x) #mostra numero de colunas de x
nrow(x) #mostra numero de linhas de x
names(x)<-c("foo", "bar", "norf")#coloca nomes às colunas de x

#lendo de uma tabela: Opção 1----
read.table()
read.table("exemplo.csv")#lendo de uma tabela no arquivo especificado
xdat1 <- read.csv2("exemplo.csv", header = T, sep = ";")#muda separado para "tabulação"," ou ";" segundo seu ssistema
##MOstrar o objeto----
head(xdat1)

#lendo de uma tabela: Opção 2----
xdat2 <- read.csv2("exemplo.csv", header = T, sep = ";", row.names = 1)#Inclui a primera coluna como nomes das linhas
str(xdat2)


#lendo de uma tabela: Opção 3----
xdat3 <- read.table("clipboard", header = T, sep = "\t")#importa do "clipboard" (memoria) do windows.


#lendo de uma tabela: Opção 4----
library(readxl)
xlsxdat2 <-read_xlsx("ExercicioM2A5.xlsx", sheet=1, col_names = T) #importa diretamente do arquivo excel
#conferir o cabeçalho da sua tabela:
head(xdat4)
str(xlsxdat2)

#lendo de uma tabela: Opção 5----
xdat5 = read.csv(file.choose())#escolher o arquivo a ser aberto



#Aula5 organização de dados, no R


#Outros----
#attach(testedados)
#detach(testedados)
#missing values NA or NaN----
is.na(xdat4)#testa si ha NA, resposta é um vetor logico

##objetos----
class(xdat2) # ver a classe de um objeto
rm(xdat1)#remover o objeto
str(xdat2)
summary(xdat2)

#vetores----
v1<-vector(mode="numeric",5) #crea um vetor numerico de 5 numeros
rm(v1)#remove o v1
v1<-vector(mode="numeric",10) #criamos o novo v1
attributes(v1)# atributos de um objeto
print(v1)#mostra o objeto
#vetor com sequencia de dados----
v2<-1:20 #crear um vetor numerico de 1 a 20
print(v2)#mostr o objeto

v3<-c(1, 3, 5, 2) #função de concatenar
print(v3)
#coercion:
v2<-as.numeric(v2) #trasnforma em numeros
v4<-as.character(v2) #trasnforma em carateres



###Matrizes: alternativa 1----
m<-matrix(nrow=2, ncol=3) #cria um amatrix vazia
dim(m) #dimenções da matriz
attributes(m)
m<-matrix(1:6, nrow=2, ncol=3)#Colocal objetos. Objetos sãoo inlcuidos na matriz em ordem das COLUNAS
#alternativa 2:----
m<-1:6 #sequencia de numero s de 1-10
dim(m) <- c(2,3) #dimensiona o objeto m com 2 filas e 5 colunas
dimnames(m)<- list(c("a", "b"), c("c", "d", "e"))#coloca nomes as colunas e linas da matrz
#alternativa 3: matriz juntando linhas----
x<-1:3
y<-10:12
cbind(x,y)#junta os vetores pela coluna
rbind(x,y)#junta os vetores pela fila

#Alternativa 4 (preenchedo)----
z <- matrix(nrow = 4, ncol = 3) #crea uma matrix de 4 linhas e 4 colunas
z
z[,1]= c(1,2,3,4)
z[,2]= c( 3, 5, 6 ,2)
z[,3]= c(3,7,6,5)
colnames(z)=c("Ax", "Bx", "Cx") #coloca os nomes das colunas (rownames para lineas)
rownames(z)=c("1", "2", "3", "4")
zt = t(z)  #transpor a matrix z. 
zt

#listas: vetores que podem conter varios tipos de elementos----
x<-list (1, "a", TRUE, 1+4i) #os elemnetos s?o mostrados com doble caixa

#Fatores: Vetores de dados categoricos----

xf<- factor(c("yes", "yes", "no", "yes", "no"))
table(xf) # resumo da tabela contando quantos de cada categoria tem
xf<- factor(c("yes", "yes", "no", "yes", "no"), levels=c("yes", "no"))# especifica quais os niveis do fator, e mais importante a ordem em que o R usa eles
factor3<-c(1,2,3,4,1,4,2,3,1,2,3,2,4,1)
factor3<- as.factor(factor3) 
class(factor3)  
#Melhor opção
factor4<- gl(2, 8, labels = c("Control", "Treat"))# gerar um fator especificando quantos niveis e quantosreplicações


#edicao um objeto no r----
z2= edit(z) #abre um editor para modificar z e o resultado o guarda em z2
fix(z)   #abre o editor diretamente para modificar z
z

trans_xlsx <- t(xlsxdat2) #Para transpor o arquivo do excel

##subsets----
#[] #extrai elementos da mesma clase que o original, em forma de lista

x<- c("a", "b", "c", "d", "c", "a")
x[3]# extrae o priemiro elemento de x
x[1:4]# extrae os priemiros 4 elementos de x
x[x>"b"] #extrae elemntos maiores que a (ordem alfabetica)
u<-x>"a" #cria um vetor logico u que indica (T o F) que elementos maiores que a
t<-x[u]# extrae os elemntos de x que seguem a logica do vetor u

xdat4[1,2]#extre o primer elememto da segunda coluna (em forma de vetor)
xdat4[2,1]#extrae o segundo elemnto da priemira coluna
xdat4[1,]#extrae a priemira fila inteira
objetovec<-xdat4[,4]#extrae a quarta coluna inteira
xdat4[,2:3, drop=FALSE]# extrae a 2 coluna interia com o mesmo formato do original (matriz 1x1)
xdat4[5:15,]#linhas intermediarias

#Filtrar----
xdat4Prim<- xdat4[xdat4$Estado %in% 'Primaria',]# filtra as linhas que tem o valor TRUE na coluna bar
xdat4$Alti
xdat4Secun<- xdat4[xdat4$Estado %in% 'Secundaria',]# filtra as linhas que tem o valor TRUE na coluna bar
xdat4[xdat4$Alti>500 & xdat4$Alti<1000,]#Filtra as linhas que tem valores de altitude entre 500 e 1000

#Funcao Subset----

xdat4Prec<-subset(xdat4, Precipit>1000) #filtra linhas da tablea com prcipitação maior a 1000
subset(xdat4, RiqAmos==3) #extrai a parte da tabela com valores de RiqAmos iguais a 3

#Ordenar----

xdat4Order<-xdat4[order(xdat4$Alti, decreasing=T),] #ordena de maior a menos a linhas pela altitude
xdat2Order<-xdat2[,order(colSums(xdat2), decreasing=T)] #organizar de maior a menor segundo os totais marginais das colunas

#Tabelas consolidadas
#mostra a media para cad tipo de floresta de cada variavel numerica
aggregate(xdat4[,c(4,5,6,8, 9, 10)],list(Floresta=xdat4$Estado),mean) # escolher os numneros das colunas que são de naturez numerica

#checando os dados para possiveis erros----
#
plot(xdat4$Alti)
plot(xdat4)
plot(xdat4[,c(4,5,6,7,10)], xdat4[,c(4,5,6,7,10)])


#subtituir por 0 e 1----

set.seed(2)
M=matrix(sample(c(0:3),25, rep=T),5,5)#criar uma matriz aleatoria  de 5x5
M=data.frame(M)#transforma a matriz M em data frame
M[M>0] <-1 #transforma os valores da Matriz em 0s e 1s


xdat4$PreAus<-xdat4$RiqAmos#Cria uma nova coluna para os valores 0 e 1
xdat4$PreAus[xdat4$PreAus>0] <-1 #transforma os valores da nova coluna em 0s e 1s


#remover missing values----
xmis<-c(1,2,NA, 4,NA, 5)
bad<-is.na(xmis) #cria um vetor logico indicado NA de xmis
xmis[!bad]#extrae elementos de x que não são NAs
ymis<-c("a", "b", NA, "d", NA, "f")
good<- complete.cases(xmis,ymis)#vetor logico com os casos validos de Xmis e de ymis
#complete.cases pode ser usado sobre um data frame, deixando somente os casos (linhas) que estejam completos)
xdat <- read.table("clipboard", header = T, sep = "\t")
goodm<-complete.cases (xdat4)
xdat[goodm,]


#tabelas dinamicas----

table(xdat4$Estado, xdat4$Local)
ftable(Local ~ RiqAmos, data = xdat4)# tabela de contingencia permite definir os fatores
ftable(Local ~ Estado+SoloAmos, data = xdat4)# tabela de contingencia 3 fatores (separa tipos de solo e estado para cada local)

#resumos----
summary(xdat4)# resumo geral dos dados
colSums(xdat4[,4:6])#total marginal das colunas
rowSums(xdat4[,4:6])#total marginal das filas

#Tabelas dinâmicas especificando funções a serem executadas. Exemplos usam max(ou seja valor amximo, teste com mean, sum, etc) 
tapply(xdat4$Alti, INDEX= xdat4$Estado, FUN=max)# devolve os valores maximos de cada tipo de floresta. saida em forma de matrix
aggregate(Alti~Estado, data=xdat4, FUN=max)# devolve os valores maximos de cada tipo de floresta. saida em forma de dataframe
tapply(xdat4$Alti, INDEX= list(xdat4$Estado, xdat4$SoloAmos), FUN=max)#maximo por categoria de habitat
#para aplicar a média de ariqueza amostral por tipo de floresta e tipo de solo
RiqMedTab<-tapply(xdat4$RiqAmos, INDEX= list(xdat4$Estado, xdat4$SoloAmos), FUN=mean)
RiqMedTab

#RiqMedTab.Pri<-tapply(xdat4$RiqAmos[xdat4$Estado=="Primaria"], INDEX= list(xdat4$Estado, xdat4$SoloAmos), FUN=mean)
#RiqMedTab.Pri


#Graficos basicos----
#Barras----
barplot(xdat4$RiqAmos, width = 1, names.arg = NULL, legend.text = NULL, beside = FALSE, horiz = FALSE)
title(xlab="Amostra", ylab="Riqueza de especies por amostra")# Colocar titulo nos eixos

barplot(RiqMedTab, width = 1, names.arg = NULL, legend.text = T, beside = T, horiz = T)# frequencias de duas espcies (cal.fus e bem.lam) em caa área
title(xlab="Media da Riqueza Amostral", ylab="Tipo de solo")# Colocar titulo nos eixos

#boxplot----
boxplot(RiqAmos~SoloAmos, data=xdat4, xlab= "Tipo de solo", ylab="Riqueza da amostra") #Boxplot da abundância da especie Aba.par nas 3 áreas. 
boxplot(RiqAmos~SoloAmos+Estado, data=xdat4, xlab= "Tipo de solo por Floresta", ylab="Riqueza da amostra") #Boxplot da interação de duas variaveis categóricas

#scarterplot----
plot(RiqAmost~Temp, data=dados_local, subset= Estado  %in%"Primaria")# dispersão  da abundância da especie Aba.par em relação á variavel max.Ht
points(RiqAmost~Temp, data=dados_local, subset= Estado %in% "Secundaria", col= "#624000", pch= "ç") #adicionar outra espcie ao gráfico
legend("topright", c("Floresta primaria", "Floresta secundaria"), pch= c("o","ç"), col= c(1,"#624000")) #Adicionar a legenda

#Histograma (dsitribuição de freqeuncias----
hist(xdat4$RiqAmos)# Histogrma da variavel 

##argumentos gráficos uteis: 
#main: Titulo
#Xlim= c(), ylim=c() : Escala dos eixos
#xlab, ylab: titulo dos eixos
#col: cores em formato númerico ou por extenso 1,2,... blue, red
#pch: tipo de ponto
#lwd=1 ; largura ds linhas
#Type='p' ; plotar pontos, linhas ('l'), ou ambos ('b')


