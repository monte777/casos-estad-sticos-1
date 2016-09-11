setwd("C:/Users/pmontenegro/Desktop/Documentos UCR/Casos estadísticos/Clase 2 caso 1")
#librarys requeridas
library(car)
library(MASS)
library(pscl)
library(foreign)
library(mlogit)
library(epitools)
library(corrplot)
library(Rcmdr)
library(survey)
library(ggplot2)
library(knitr)
library(rmarkdown)

#Enaho 2011, extración de la base, el único cambio de la base original es colapsar ocupados.
enaho_2011=(read.csv(file="Encuesta Nacional de Hogares (ENAHO) 2011.csv",header=TRUE,sep=","))
names(enaho_2011)
dim(enaho_2011)

enaho <- subset(enaho_2011, Linea==1)
enaho2 <- subset(enaho_2011, Linea==2)
enaho3 <- subset(enaho_2011, Linea==3)
enaho4 <- subset(enaho_2011, Linea==4)

#rm(enaho_2011) #para eliminar una base

names(enaho)
attach(enaho)
dim(enaho)

#Representación gráfica
id=seq(1:dim(enaho)[1])
enaho<-data.frame(id,enaho)

#Análisis de la variable ingreso per cápital
summary(ipcn)
hist(ipcn)#No se distribuye normalmente
plot(tapply(ipcn,ipcn,mean))
percentiles1=quantile(ipcn,c(0.01,0.025,0.5,0.975,0.99))
percentiles1

var(ipcn)
plot(ipcn)#valores extremos

#corregir los valores extremos
InerciaIC=rep(0,50)
for(k in 1:50) {
   grupos=kmeans(ipcn,k)
   InerciaIC[k]=grupos$tot.withinss
}
plot(InerciaIC,col="blue",type="b",main= "Gráfico Codo de Jambu")#entre 8 y 9 cluster

cluster<-kmeans(ipcn,8)
points(cluster$centers,pch=19,col="blue",cex=2)
points(enaho,col=cluster$cluster,pch=19)

centros<-cluster$centers
centros#tengo los centroides de la agrupación.

#de la manera anterior con los percentiles y codo Jambu recodifico el ingreso

ipcn2=recode(ipcn,"min(ipcn):19080=19080;1961567:max(ipcn)=1961567")
table(ipcn2)
plot(ipcn2)
percentiles3=quantile(ipcn2,c(0.01,0.025,0.5,0.975,0.99))
percentiles3#Nótese que no afecte el 3% inferior y el 1% superior de los datos

InerciaIC=rep(0,50)
for(k in 1:50) {
   grupos=kmeans(ipcn2,k)
   InerciaIC[k]=grupos$tot.withinss
}
plot(InerciaIC,col="blue",type="b")

cluster<-kmeans(ipcn2,8)
points(cluster$centers,pch=19,col="blue",cex=2)
points(enaho,col=cluster$cluster,pch=19)

centros<-cluster$centers
centros

#trasformo la variable de ingreso
income=log(ipcn2)
plot(tapply(income,income,mean))
hist(income)


h <- hist(income, breaks = 10, col = "white", main = "Histograma del ingreso per cápita curva Normal")
xfit <- seq(min(income), max(income), length = 40)
yfit <- dnorm(xfit, mean = mean(income), sd = sd(income))
yfit <- yfit * diff(h$mids[1:2]) * length(income)
lines(xfit, yfit, col = "blue", lwd = 2) 

summary(income)

#correlación de variables.
correla<-data.frame(ipcn,Región,Zona,V8,HacDor)
corrplot(cor(correla), method = "square", type = "lower")
corrplot.mixed(cor(correla))

plot(ipcn,Zona)
plot(income)

percentiles=quantile(income,c(0.01,0.025,0.5,0.975,0.99))
percentiles

#######################################################################################################

#INDICADORES 6
#Indicador 1
#Hacinamiento
#ENAHO
summary(R4T3)
summary(V8)
table(V8)
V8<-recode(V8,"0=1")
table(V8)

hacinamiento<-R4T3/V8
summary(hacinamiento)
table(hacinamiento)

table(hacinamiento)
cor(income, hacinamiento)#correlacción inversa 32%

ind_hacina<-(hacinamiento-0.0202/(11-0.0202))*100
hist(ind_hacina)

ind_hacina_t<-log(ind_hacina)

cor(income,ind_hacina_t)
hist(ind_hacina_t)
h <- hist(ind_hacina_t, breaks = 10, col = "white", main = "Histograma del log hacinamiento curva Normal")
xfit <- seq(min(ind_hacina_t), max(ind_hacina_t), length = 40)
yfit <- dnorm(xfit, mean = mean(ind_hacina_t), sd = sd(ind_hacina_t))
yfit <- yfit * diff(h$mids[1:2]) * length(ind_hacina_t)
lines(xfit, yfit, col = "blue", lwd = 2) 

summary(ind_hacina_t)

#Censo
#hacinamiento<-total/dormitorios*100

#Indicador 2
#Pertenencias
#ENAHO 11 variables, satelital y cable unidas

#cantidad_cel<-V18A1
tel_resid<-	recode(V18B,"3=1;4=0")
agua_caliente<-	recode(V18D,"7=1;8=0")
almacenar_agua<-	recode(V18E,"1=1;2=0")
computadora<-	recode(V18F,"3=1;4=0")
radio<-		recode(V18H,"7=1;8=0")
carro<-	recode(V18I,"1=1;2=0")
moto<-	recode(V18J,"3=1;4=0")
tv_moderno<-	recode(V18K,"5=1;6=0")
tv_convencional<-	recode(V18L,"7=1;8=0")
satelital<-	recode(V18N,"3=1;4=0")+recode(V18M,"1=1;2=0")
satelital<-recode(satelital,"2=1")
internet<-	V19A
agua<-recode(V12,"1:4=1;5:7=0")#tiene o no agua
ele<-recode(V15,"1:6=1;0=0")#tiene o no electricidad
cocina<-recode(V16,"1=1;2=1;0=0;3=0;4=0;9=0")#cocina o no alimentos

#table(cantidad_cel,R4T3)
table(tel_resid)
table(agua_caliente)
table(almacenar_agua)
table(computadora)
table(radio)
table(carro)
table(moto)
table(tv_moderno)
table(tv_convencional)
table(satelital)
table(internet)
table(agua)
table(ele)
table(cocina)

chisq.test(table(income,tel_resid))# se cumple el supuesto de asociación
chisq.test(table(income,agua_caliente))# se cumple el supuesto de asociación
chisq.test(table(income,almacenar_agua))# se cumple el supuesto de asociación
chisq.test(table(income,computadora))# se cumple el supuesto de asociación
chisq.test(table(income,radio))#no se cumple el supuesto de asociación
chisq.test(table(income,carro))# se cumple el supuesto de asociación
chisq.test(table(income,moto))# se cumple el supuesto de asociación
chisq.test(table(income,tv_moderno))# se cumple el supuesto de asociación
chisq.test(table(income,tv_convencional))#no se cumple el supuesto de asociación
chisq.test(table(income,satelital))# se cumple el supuesto de asociación
chisq.test(table(income,internet))# se cumple el supuesto de asociación
chisq.test(table(income,agua))# no se cumple el supuesto de asociación
chisq.test(table(income,ele))#no se cumple el supuesto de asociación
chisq.test(table(income,cocina))#no se cumple el supuesto de asociación


modelo1=glm(income~tel_resid+agua_caliente+almacenar_agua+computadora+radio+carro+moto+tv_moderno+tv_convencional+satelital+internet+agua+ele+cocina, family=gaussian)
summary(modelo1)
plot(modelo1$fitted,modelo1$residuals)

base1<-data.frame(income,tel_resid,agua_caliente,almacenar_agua,computadora,radio,carro,moto,tv_moderno,tv_convencional,satelital,internet,agua,ele,cocina)

#Observar matriz de correlaciones
corrplot(cor(base1), method = "square", type = "lower")
corrplot.mixed(cor(base1))

#Escoger el modelo con el AIC y BIC, pero no nos ayuda mucho.
stepwise(modelo1, direction = c("backward/forward"), criterion = "AIC")
formula(modelo1)
stepwise(modelo1, direction = c("backward/forward"), criterion = "BIC")
formula(modelo1)

#segundo modelo con la asociación y correlacción entre el ingreso
modelo2=glm(income~tel_resid+agua_caliente+almacenar_agua+computadora+carro+tv_moderno+satelital+internet, family=gaussian)
summary(modelo2)
plot(modelo2$fitted,modelo2$residuals)

#incluyo un factor de expasión, contrayendo la base.
Factor.elev<-round(Factor/(mean(Factor)),1)

datos.muestrales <- data.frame(base1,Factor.elev)

ddatos <- svydesign(id=~1,weights=~Factor.elev,data=datos.muestrales)
modelo <- svyglm(income~tel_resid+agua_caliente+almacenar_agua+computadora+radio+carro+moto+tv_moderno+tv_convencional+satelital+internet+agua+ele+cocina,family=gaussian,design=ddatos)
modelo
summary(modelo)#dejamos por fuera moto y tv_convencional

#Usando 11 variables, moto, tv_convecional, electricidad
pertenencias<-tel_resid+agua_caliente+almacenar_agua+computadora+radio+carro+tv_moderno+satelital+internet+agua+cocina

hist(pertenencias)

h <- hist(pertenencias, breaks = 10, col = "white", main = "Histograma de las pertenencias curva Normal")
xfit <- seq(min(pertenencias), max(pertenencias), length = 40)
yfit <- dnorm(xfit, mean = mean(pertenencias), sd = sd(pertenencias))
yfit <- yfit * diff(h$mids[1:2]) * length(pertenencias)
lines(xfit, yfit, col = "blue", lwd = 2) 

summary(pertenencias)
table(pertenencias)

cor(income,pertenencias)#correlación del 57%

#creo el indicador, reescalando.

ind_pert<-(pertenencias/11)*100
hist(ind_pert)
h <- hist(ind_pert, breaks = 10, col = "white", main = "Histograma de indicador de pertenencias curva Normal")
xfit <- seq(min(ind_pert), max(ind_pert), length = 40)
yfit <- dnorm(xfit, mean = mean(ind_pert), sd = sd(ind_pert))
yfit <- yfit * diff(h$mids[1:2]) * length(ind_pert)
lines(xfit, yfit, col = "blue", lwd = 2) 

summary(ind_pert)

#Censo

#Indicador 3
#ocupación
#ENAHO
table(OcupEmpPri)
summary(OcupEmpPri)
OcupEmpPri<-recode(OcupEmpPri,"NA=999")

table(OcupFuerzaTrab)
summary(OcupFuerzaTrab)

i=1
for (i in 1:length(OcupEmpPri))
{
	if (OcupEmpPri[i]==999)
	{
		OcupEmpPri[i]=OcupFuerzaTrab[i]
	}else
	OcupEmpPri[i]=OcupEmpPri[i]
}

#Con la segunda persona que trabaja.
enaho2$OcupEmpPri

OcupEmpPri<-recode(OcupEmpPri,"NA=999")
i=1
for (i in 1:length(OcupEmpPri))
{
	if (OcupEmpPri[i]==999)
	{
		OcupEmpPri[i]=enaho2$OcupEmpPri[i]
	}else
	OcupEmpPri[i]=OcupEmpPri[i]
}


OcupEmpPri<-recode(OcupEmpPri,"NA=999")
i=1
for (i in 1:length(OcupEmpPri))
{
	if (OcupEmpPri[i]==999)
	{
		OcupEmpPri[i]=enaho3$OcupEmpPri[i]
	}else
	OcupEmpPri[i]=OcupEmpPri[i]
}

summary(OcupEmpPri)

OcupEmpPri<-recode(OcupEmpPri,"10=9;22=9")

table(OcupEmpPri)

#1Nivel direct adm públ y empr priv
#2Nivel prof, científ e intelect
#3Nivel técnico y profesional medio
#4Apoyo administrativo
#5Venta locales prestac serv direct a personas
#6Agropecuarias, agrícolas y pesqueras "calificadas"
#7Prod artes, const, mecán, art gráf y manuf "calificadas
#8Montaje y operación de instalaciones y de máquinas
#9Ocupaciones no calificadas
#10No bien especificadas

table(PosiEmpAgruPri)
summary(PosiEmpAgruPri)

PosiEmpAgruPri<-recode(PosiEmpAgruPri,"NA=999")

i=1
for (i in 1:length(PosiEmpAgruPri))
{
	if (PosiEmpAgruPri[i]==999)
	{
		PosiEmpAgruPri[i]=enaho2$PosiEmpAgruPri[i]
	}else
	PosiEmpAgruPri[i]=PosiEmpAgruPri[i]
}

#Con la segunda persona que trabaja.
enaho2$PosiEmpAgruPri

PosiEmpAgruPri<-recode(PosiEmpAgruPri,"NA=999")
i=1
for (i in 1:length(PosiEmpAgruPri))
{
	if (PosiEmpAgruPri[i]==999)
	{
		PosiEmpAgruPri[i]=enaho3$PosiEmpAgruPri[i]
	}else
	PosiEmpAgruPri[i]=PosiEmpAgruPri[i]
}


PosiEmpAgruPri<-recode(PosiEmpAgruPri,"NA=999")
i=1
for (i in 1:length(PosiEmpAgruPri))
{
	if (PosiEmpAgruPri[i]==999)
	{
		PosiEmpAgruPri[i]=enaho4$PosiEmpAgruPri[i]
	}else
	PosiEmpAgruPri[i]=PosiEmpAgruPri[i]
}

summary(PosiEmpAgruPri)
table(PosiEmpAgruPri)

categ_ocup<-paste(OcupEmpPri,OcupFuerzaTrab,sep = "",collapse = NULL)
categ_ocup
categ_ocup<-as.numeric(categ_ocup)
class(a)
enaho<-data.frame(enaho,categ_ocup)

#PosiEmpAgruPri
#11Asalariada de hogar privado(5)
#12Asalariada de empresas o instituciones (3,4)
#13Auxiliares no remunerados(6) 
#21Empleadora (1)
#22Cuenta Propia (2)

out <- tapply(ipcn, list(OcupEmpPri, PosiEmpAgruPri), mean) 
out
table(OcupEmpPri,PosiEmpAgruPri)
#Censo
P46_OCUPACION_GRAN_GRUPO
#1Nivel direc. público y privado
#2Nivel prof. y cientifico
#3Nivel técnico y prof. medio
#4Apoyo administrativo
#5Vent. en locales y  serv. direct
#6Agropecuario y pesca calific
#7Prod.artesanal; otros ofici.calific
#8Operación maq. y ensamblaje
#9Ocupaciones elementales

P28_CATEGORIA_OCUPACIONAL
#1Patrono(a)
#2Trabajador(a) por cuenta propia
#3Empleado(a) de empresa privada
#4Empleado(a) del sector público
#5Empleado(a) de casas particulares
#6Ayudante sin recibir pago

#hago lo siguiente
a<-recode(a,"x==0 & y==5=1")

ind_ocupacion = function(xm, xmax)
{
	i=1
	B=rep(0,length(xm))
	for(i in 1:length(xm))
	{
		B[i]=rnorm(1,xm[i],xmax[i])
		i=i+1	
	}
round(B,0)
}


#Nivel de instrucción
#ENAHO
table(Escolari)
summary(Escolari)

#Censo
table(P40_AÑOS_ESCOLARIDAD)
summary(P40_AÑOS_ESCOLARIDAD)


#proporción de dependientes
#ENAHO
names(enaho)
p_dependientes<-(menos15_sum+mas65)/R4T3

summary(R4T3)
summary(p_dependientes)
hist(p_dependientes)
ind_dependientes<-p_dependientes/3*100
hist(ind_dependientes)


ind_dependientes_t<-log(ind_dependientes)
hist(ind_dependientes_t)
summary(ind_dependientes_t)


cor(income,p_dependientes)#-0.28
#Censo


#número de personas ocupadas
n_ocupados<-ocup_sum
table(n_ocupados)
hist(n_ocupados)
summary(n_ocupados)

ind_n_ocupados<-n_ocupados/9*100
hist(ind_n_ocupados)

cor(income,ind_n_ocupados_t)#-0.27





#ajuste de la linea recta
abline(lm(y ~ x))

#sí es líneal
model<-lm(y ~ x)

#ajuste resumen
summary(model)

#ajustes
model$fitted
model$resid
model$coef

#plot
plot(model,which=1)

#Para poder generalizar un modelo de regresión debemos comprobar los supuestos del modelo, y una vez seguros de que se cumplen, para comprobar si el modelo se puede generalizar utilizaremos la validación cruzada. Empezamos analizando gráficamente los supuestos.
#Este primer gráfico enfrenta los errores residuales frente a sus valores ajustados. El residuos deben estar distribuidos al azar alrededor de la línea horizontal que representa un error residual de cero; es decir, no debe haber una tendencia clara en la distribución de puntos. Una tendencia en la variabilidad de los residuos sugiere que la varianza está relacionada con la media, violando el supuesto de varianza constante.En este caso los datos parecen exhibir una ligera tendencia con un incremento de la varianza en los extremos.
plot(modelCirf, which = 1, pch = 20)
#Si hubiera algún tipo de curva en la gráfica entonces se ha violado el supuesto de linealidad. Y si los datos parecen seguir un patrón y además están más extendidos por en algunos puntos de la gráfica que en otros entonces probablemente se incumplan los supuestos de homogeneidad de varianza y linealidad.
plot(modelCirf, which = 2, pch = 20)

#En este gráfico los residuos tipificados se trazan contra los cuantiles de una distribución normal estándar. Si los residuos se distribuyen normalmente los datos se deben situar a lo largo de la línea. En este caso, los datos no hacen parecen tener una distribución normal.
plot(modelCirf, which = 3, pch = 20)
#El tercero es el gráfico escala-ubicación en el que los residuos están estandarizados por sus desviaciones estándar estimadas. Esta gráfica se utiliza para detectar si la difusión de los residuos es constante en el rango de valores ajustados. Una vez más, se aprecia una tendencia muy leve en los datos de tal manera que los valores altos muestran una mayor variación.

plot(modelCirf, which = 5, pch = 20)

dfbeb$fitted.modelCirf <- fitted(modelCirf)
dfbeb$residuals.modelCirf <- residuals(modelCirf)
dfbeb$rstudent.modelCirf <- rstudent(modelCirf)

#normalidad
ks.test(dfbeb$rstudent.modelCirf, "pnorm")

#hist
hist(dfbeb$rstudent.modelCirf, xlab = "residuos", main = "Histograma residuos")

#homogenidad de varianza
bptest(modelCirf, studentize = FALSE, data = dfbeb)

#Autocorrelación, Aceptamos la hipótesis nula de que no existe correlación entre los residuos con un p-valor superior a 0.05
dwtest(modelCirf, alternative = "two.sided", data = dfbeb)

#atipicos
outlierTest(modelCirf)

#influenciales
infl <- influence.measures(modelCirf)
summary(infl)

influencePlot(modelCirf, id.n = 2)

cook <- cooks.distance(modelCirf)
labels <- rownames(dfbeb)
halfnorm(cook, 3, labs = labels, ylab = "Distancia de Cook"
         
#predicción
# Definiendo un intervalo para la vble vino.
x0 <- seq(min(dfbeb$vino), max(dfbeb$vino), length = length(dfbeb$vino))
dbp <- data.frame(poblacion = 56, cerveza = 41, vino = x0, licorDuro = 58)
pred <- predict(modelCirf, dbp, interval = "prediction", se.fit = TRUE, data = dfbeb)
head(pred$fit)

################RESUMEN######################################
# Leer los datos de un fichero .csv
df <- read.table("files/40A-file.csv", sep = ";", head = TRUE)

### Primera aproximación a los datos
str(df)
summary(df)

# Correlación Gráfico de dispersión multivariante
pairs(df, panel = panel.smooth)

# Matriz de correlación
cor(df, use = "everything", method = "pearson")
corr.test(df, use = "complete", method = "pearson")

## Correlación parcial (si fuera necesario)
library("ppcor")
pcor.test(df$var1, df$var2, df$var3)


# Modelo de regresión múltiple

## Creamos el modelo de regresión
modelo <- lm(var1 ~ var2 + var3 + ..., data = df)
summary(modelo)  # analizamos el modelo inicial

## Comparación de modelos (encajados)
anova(model3, model1)
anova(model3, model2)

## Selección del modelo mediante los métodos paso a paso Método hacia atrás
step(modelo, direction = "backward")

### Método de dos sentidos
step(modelo, direction = "both")

### Método hacia delante
mdlCir0 <- lm(var1 ~ 1, data = df)
step(mdlCir0, direction = "forward", ~var1 + var2 + var3 + var4)
modelo <- lm(var1 ~ var2 + var3, data = df)


# Análisis del modelo final
summary(modelo)
anova(modelo)

## Diagnósitco del modelo


# Gráficamente
plot(modelo, which = 1)
plot(modelo, which = 2)
plot(modelo, which = 3)
plot(modelo, which = 5)

## Contrastes Obtenemos los residuos del modelo y valores ajustados
df$fitted.modelo <- fitted(modelo)
df$residuals.modelo <- residuals(modelo)
df$rstudent.modelo <- rstudent(modelo)

### Normalidad
ks.test(df$rstudent.modelo, "pnorm")
hist(df$rstudent.modelo, xlab = "residuos", main = "histograma residuos")

### Homogeneidad de varianzas
library(lmtest)
bptest(modelo, studentize = FALSE, data = df)

### Autocorrelación
dwtest(modelo, alternative = "two.sided", data = df)

### Valores atípicos
library(car)
outlierTest(modelo)

### Análisis de la influencia Tabla con las medidas de influencia
infl <- influence.measures(modelo)
summary(infl)

#### Gráfico medidas influyentes
influencePlot(modelo, id.n = 2)

#### Gráfico de las distancias de Cook
cook <- cooks.distance(modelo)
labels <- rownames(df)
library(faraway)
halfnorm(cook, 3, labs = labels, ylab = "Distancia de Cook")

## validación cruzada
library(DAAG)
cv.lm(df, modelo, m = 2)

# Predicción.  Valores concretos de cada vble
predict(modelo, data.frame(var1 = 39, var = 62, var3 = 18), interval = "prediction", 
    data = df)


# Poniendo un intervalo para una de las vbles.
x0 <- seq(min(df$var2), max(df$var2), length = length(df$var2))
pred <- predict(modelo, data.frame(var2 = x0), interval = "prediction", data = df)
head(pred)


# Multicolinealidad
library(car)
vif(modelo)
sqrt(vif(modelo)) > 2
################################################################################################

dffest <- read.table("files/40A-festival.csv", sep = ";", head = TRUE)
head(dffest)

levels(dffest$musica)

plot(cambio ~ musica, data = dffest)

#crear variables dummys
contrasts(dffest$musica) <- contr.treatment(4, base = 4)

Indie_dum <- c(1, 0, 0, 0)
Metal_dum <- c(0, 1, 0, 0)
Pop_dum <- c(0, 0, 1, 0)
contrasts(dffest$musica) <- cbind(Indie_dum, Metal_dum, Pop_dum)

modelFesti <- lm(cambio ~ musica, data = dffest)
summary(modelFesti)


modelo1=glm(satell~weight+width, family=gaussian)
summary(modelo1)
boxplot(satell~as.factor(color))
plot(modelo1$fitted,modelo1$residuals)
lrtest(modelo1,modelo2)#razón verosimilitudes

tab=table(cep$estrogen[cep$case==1],cep$estrogen[cep$case==0])
tab[2,1]/tab[1,2]


datos2=datos[order(datos$id),] 
table(lsoa2$i)
lsoa2=na.omit(lsoa2)
lsoa2=lsoa2[order(lsoa2$id,lsoa2$year),]

head(lsoa2)
tapply(i,year,mean, na.omit=TRUE)

agreg2$hta=agreg2$Var1#construir una variable

chisq.test(table(malasalud,hta))#x2 pearson

###Ahora un gráfico tomando en cuenta la transformación logito
prob=tapply(malasalud,age,mean)
logito=log(prob/(1-prob))
plot(tapply(age,age,mean),logito)
edad=tapply(age,age,mean)
edad
plot(tapply(age,age,mean),tapply(c1,age,mean))
saludar2=recode(c1, "1=5;2=4;3=3;4=2;5=1")

scatter.smooth(edad[edad<105],logito[edad<105])

cc$moro[hist.credit==0 & no.tarjetas >=5]=rbinom(1446,1,0.45)
ind <- sample(2, nrow(cc), replace = TRUE, prob = c(0.8, 0.2))
data1 <- cc[ind == 1, ]
data2 <- cc[ind == 2, ]
moro=subset.data.frame(data1)

nbase1 <- subset(base1, cunicah >= 4500 & cunicah < 5201) 






