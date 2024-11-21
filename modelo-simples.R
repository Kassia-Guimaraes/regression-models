### MODELO REGRESSÃO LINEAR SIMPLES ###

library(readr)
mobility <- read_csv("mobility.csv")


###########################
###### Estudo modelo ######

modelo <- lm(Mobility ~ Commute, data=mobility)

summary(modelo)


plot(mobility$Commute, mobility$Mobility, 
     main='Mobility em função da Commute',xlab='Commute', ylab='Mobility')+
  abline(modelo, col=2)+
  legend("topleft",
         legend = "Reta Regressão do Modelo",
         col = 2,
         lty = 1,
         cex = 0.8)



########################################
###### Correlação entre variáveis ######

cor(mobility$Mobility, mobility$Commute, method = 'pearson')
cor.test(mobility$Mobility, mobility$Commute, method = 'pearson')



##########################
###### Tabela ANOVA ######

anova(modelo)


################################################
###### Qualidade de Ajustamento do Modelo ######

plot(mobility$Mobility,fitted(modelo),
     main='Estudo da Qualidade de Ajustamento',
     xlab='Valores Observados do Mobility',
     ylab='Valores Estimados para Mobility')+
  abline(a = 0, b = 1, col = 2, lwd = 2)+
  legend("topright",
         legend ="Igualdade dos valores 
esperados e observados",
         col = 2,
         lty = 1,
         cex = 0.8)

summary(modelo)

##################################
###### Análise dos Resíduos ######
residuals(modelo)
rstandard(modelo)

sum(residuals(modelo))


####################################
###### Pressupostos do Modelo ######


#ver gráfico dos resíduos
qqnorm(rstandard(modelo), main='Estudo Normalidade dos Resíduos',
       xlab='Quantis teóricos de uma Normal(0,1)', 
       ylab='Quantis dos Resíduos Observados')
qqline(rstandard(modelo), col=2)


#teste da normalidade dos resíduos
shapiro.test(rstandard(modelo))

require(nortest)
lillie.test(rstandard(modelo))
sf.test(rstandard(modelo))
cvm.test(rstandard(modelo))
ad.test(rstandard(modelo))


#teste da homocedasticidade
plot(fitted(modelo), rstandard(modelo), xlab='Mobility Estimada', ylab='Erros Padronizados',
     main='Estudo da homocidasticidade')+
abline(h=0, col=2)+
legend("topleft",
       legend = "Média Zero",
       col = 2,
       lty = 1,
       cex = 0.8)

library(lmtest)
bptest(modelo)  # Teste de Breusch-Pagan para homocedasticidade



# independência dos erros
plot(1:length(mobility$Mobility), rstandard(modelo), 
     main='Estudo da Indepência dos Erros', xlab='Índice', ylab='Erros Padronizados')
