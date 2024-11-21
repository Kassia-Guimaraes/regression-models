### MODELO REGRESSÃO LINEAR SIMPLES ###

library(readr)
mobility <- read_csv("mobility.csv")

## Mobility e Commute 
modelo <- lm(Mobility ~ Commute, data=mobility)

plot(mobility$Commute, mobility$Mobility, 
     main='Mobility em função da Commute',xlab='Commute', ylab='Mobility')
abline(modelo, col=2)

cor(mobility$Mobility, mobility$Commute, method = 'pearson')
cor.test(mobility$Mobility, mobility$Commute, method = 'pearson')

summary(modelo)

length(mobility$Mobility)

2*(1 - pt(0.03679597,727))
confint(modelo, level=0.95)


### Análise dos resíduos
rstandard(modelo)

#ver gráfico dos resíduos
qqnorm(rstandard(modelo))
qqline(rstandard(modelo), col=2)


#teste da normalidade dos resíduos
shapiro.test(rstandard(modelo))


#teste da homocedasticidade
plot(fitted(modelo), rstandard(modelo), xlab='Mobility Estimada', ylab='Erros Padronizados',
     main='Estudo da homocidasticidade')
abline(h=0, col=2)
legend("topright",                # Localização da legenda no gráfico
       legend = "Média dos Erros", # Texto da legenda
       col = "red",                # Cor da linha
       lty = 1,                    # Tipo de linha (1 para linha contínua)
       cex = 0.8)                  # Tamanho do texto da legenda


# independência dos erros
plot(1:length(mobility$Mobility), rstandard(modelo), 
     main='Estudo da Indepência dos Erros', xlab='Índice', ylab='Erros Padronizados')
