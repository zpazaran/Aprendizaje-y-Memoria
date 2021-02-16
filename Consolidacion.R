setwd("D:/UNIVERSIDAD/4to Trimestre/Aprendizaje_y_Memoria/R")
library(lme4)
library(car)
library(multcomp)
library(plotrix) #std.error() no categoriza elementos de una variable o columna, si uso esta debo usar adicionalmente as.factor(). O puedo usar tapply y hace todo

#Declaracion de Tablas
CortoPlazo <- read.csv("CortoPlazo.csv")
LargoPlazo <-read.csv("LargoPlazo.csv")

#cálculo de la media y error estandar para las tablas de Memoría a Corto y Largo Plazo
MCP <- aggregate(Aciertos ~ grupo, data = CortoPlazo, mean)
MCP$Error_Estandar <- tapply(CortoPlazo$Aciertos, INDEX = CortoPlazo$grupo, FUN = std.error)
names(MCP) <- c("Grupo", "Media", "Error Estandar")
MLP <- aggregate(Aciertos ~ grupo, data = LargoPlazo, mean)
MLP$Error_Estandar <- tapply(LargoPlazo$Aciertos, INDEX = LargoPlazo$grupo, FUN = std.error)
names(MLP) <- c("Grupo", "Media", "Error Estandar")
MCP$`Error Estandar`
#Graficacion
Grafica_MCP<-barplot(MCP$Media, axes=FALSE,axisname=FALSE, ylim=c(0,10),
                     col=c('purple', 'orange','yellow'),main="Media de Prueba de Memoria a Corto Plazo",
                     xlab="Grupo", ylab="Media de Aciertos")
axis(1,labels=c("Control", "Ejercicio","Agua Fría"), at=Grafica_MCP)
axis(2,at=seq(0,9,by=0.5))
segments(Grafica_MCP - 0.1, MCP$Media + MCP$`Error Estandar`, Grafica_MCP + 0.1, MCP$Media + MCP$`Error Estandar`, lwd=3)
segments(Grafica_MCP - 0.1, MCP$Media - MCP$`Error Estandar`, Grafica_MCP + 0.1, MCP$Media - MCP$`Error Estandar`, lwd=3)
segments(Grafica_MCP, MCP$Media - MCP$`Error Estandar`, Grafica_MCP, MCP$Media + MCP$`Error Estandar`, lwd=3)
#
Grafica_MLP<-barplot(MLP$Media, axes=FALSE,axisname=FALSE, ylim=c(0,10),
                     col=c('purple', 'orange','yellow'),main="Media de Prueba de Memoria a Largo Plazo",
                     xlab="Grupo", ylab="Media de Aciertos")
axis(1,labels=c("Control", "Ejercicio","Agua Fría"), at=Grafica_MLP)
axis(2,at=seq(0,9,by=0.5))
segments(Grafica_MLP - 0.1, MLP$Media + MLP$`Error Estandar`, Grafica_MLP + 0.1, MLP$Media + MLP$`Error Estandar`, lwd=3)
segments(Grafica_MLP - 0.1, MLP$Media - MLP$`Error Estandar`, Grafica_MLP + 0.1, MLP$Media - MLP$`Error Estandar`, lwd=3)
segments(Grafica_MLP, MLP$Media - MLP$`Error Estandar`, Grafica_MLP, MLP$Media + MLP$`Error Estandar`, lwd=3)

#anova y tukey para Memoria a Corto Plazo
resumen_ANOVA_CP <- summary(ANOVA_MCP<-aov(data=CortoPlazo, formula = Aciertos ~ grupo))
TUKEY_ANOVA_CP <- TukeyHSD(ANOVA_MCP)
#anova y tukey para Memoria a Largo Plazo
resumen_ANOVA_C <- summary(ANOVA_MLP<-aov(data=LargoPlazo, formula = Aciertos ~ grupo))
TUKEY_ANOVA_C <- TukeyHSD(ANOVA_MLP)
#visualizar tablas del anova y tukey en MCP y MLP
View(data.frame(resumen_ANOVA_CP[[1]], resumen_ANOVA_C[[1]]))
View(data.frame(TUKEY_ANOVA_CP[[1]], TUKEY_ANOVA_C[[1]]))