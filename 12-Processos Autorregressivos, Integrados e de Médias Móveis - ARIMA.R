                          #Aula 12 - Modelos  ARIMA

library("urca")                                #Carrega Pacote URCA
library(readxl)                                #Carrega Pacote readxl
library(pwt8)                                  #Carrega o pacote PWT8.0


data("pwt8.0")                                 #Carrega os dados elencados "pwt8.0" dispoin�veis no pacote
View(pwt8.0)                                   #Visualiza os dados na tabela pwt8.0


br <- subset(pwt8.0, country=="Brazil", 
             select = c("rgdpna","emp","xr"))  #Cria a tabela "br" com dados das linhas que assumem o valor "country" (pa�s) igual a "Brazil", selecionando as colunas cujas vari�veis s�o "rgdpna" (PIB), "avh" (TRABALHO)  e "xr" (C�MBIO)

colnames(br) <-  c("PIB","Emprego","C�mbio")   #Renomeia as colunas para PIB, Trabalho e C�mbio
View(br)

                                        #Separando as vari�veis
PIB <- br$PIB[45:62]                    #Cria o vetor para vari�vel PIB                  
EMPREGO <- br$Emprego[45:62]            #Cria o vetor para vari�vel EMPREGO
CAMBIO <- br$C�mbio[45:62]              #Cria o vetor para vari�vel CAMBIO
Anos <- seq(from=1994, to=2011, by=1)   #Cria um vetor para o tempo em anos de 1994 at� 2011 


                                    #Analise para o Emprego

plot(EMPREGO, type = "l")                            #Cria gr�fico para o PIB
emprego <- ts(EMPREGO, start = 1994, frequency = 1)  #Define como S�rie Temporal
plot(emprego, main="Pessoa Empregadas no Brasil", 
     ylab="Qte de Pessoas Empregadas-milh�es", 
     xlab="Ano")                                      #Cria gr�fico da S�rie Temporal

acf(emprego)                                          #Fun��o de Autocorrela��o
pacf(emprego)                                         ##Fun��o de Autocorrela��o Parcial
reglinEMP <- lm(EMPREGO ~ Anos)                       #Regress�o linear simples do emprego em rela��o ao tempo
reglinEMP                                             #Exibe os resultados da regress�o linear
summary(reglinEMP)
plot(emprego)                                         #Gr�fcio dos dados
abline(reglinEMP, col="Blue")                         #Insere a linha de regress�o linear estimada


#Removendo Tend�ncia

residuosEMP <- reglinEMP$residuals                    #Salva os res�duos no vetor residuosEMP
reglinEMPres <- lm(residuosEMP ~ Anos)                #Regress�o linear dos res�duos em fun��o do tempo
plot(residuosEMP,type="l")                            #Gr�fico dos res�duos
abline(reglinEMPres, col="Blue")                      #Insere a linha de regress�o linear dos res�duos


#Removendo Tend�ncia por meio da diferen�a

pdemprego <- diff(EMPREGO)                                #Calcula a primeira diferen�a da s�rie de dados
diferenca1 <- (data.frame(EMPREGO[2:18],pdemprego))       #Exibe a tabela da s�rie original coma diferen�a <- 
DIFERENCA <- ts(diferenca1, start = 1994, frequency = 1)  #Define serie temporal para a tabela diferenca1
plot(DIFERENCA, plot.type="single", col=c("Black","Green")) #Cria o grafico com as duas series
plot(pdemprego, type="l")                                   #Cria gr�pafico somente para a serie da diferen�a

#Teste Dick-Fuller Aumentado conferindo se a serie se tornou estacionaria

pdemprego1 <- diff(emprego)                                            #Calculando-se a primeira diferen�a
TesteDF_Emprego1_trend <- ur.df(pdemprego1, "trend", lags = 1)         #Teste DF-DickFuller com drift e com tendencia
summary(TesteDF_Emprego1_trend) 

pdemprego2 <- diff(diff(emprego))                                      #Calculando-se a segunda diferen�a
TesteDF_Emprego2_trend <- ur.df(pdemprego2, "trend", lags = 1)         #Teste DF-DickFuller com drift e com tendencia
summary(TesteDF_Emprego2_trend)

#Estimando a s�rie temporal

arima123 <- arima(emprego, c(1,2,3))

#ARMA
arima120 <- arima(emprego, c(1,2,0))
arima121 <- arima(emprego, c(1,2,1))
arima122 <- arima(emprego, c(1,2,2))

arima220 <- arima(emprego, c(2,2,0))
arima221 <- arima(emprego, c(2,2,1))
arima222 <- arima(emprego, c(2,2,2))
arima223 <- arima(emprego, c(2,2,3))
#MA
arima021 <- arima(emprego, c(0,2,1))
arima022 <- arima(emprego, c(0,2,2))
arima023 <- arima(emprego, c(0,2,3))
#AR
arima120 <- arima(emprego, c(1,2,0))

#Escolher o melhor modelo com base no menor AIC/BIC
estimacoes <- list(arima123,arima120,arima121,
                   arima122,arima220,arima221,
                   arima222,arima223,arima021,arima021, arima022,
                   arima023,arima120)
AIC <- sapply(estimacoes, AIC)
BIC <- sapply(estimacoes, BIC)
Modelo <-c(list("arima123","arima120","arima121",
                "arima122","arima220","arima221",
                "arima222","arima223","arima021","arima021", "arima022",
                "arima023","arima120")) 
Resultados <- data.frame(Modelo,AIC,BIC)

#An�lise para o C�mbio

plot(CAMBIO, type = "l")                            
CAMBIO <- ts(CAMBIO, start = 1994, frequency = 1)  
plot(CAMBIO, main="Cambio", 
     ylab="Cambio", 
     xlab="Ano") 

acf(CAMBIO)                                          
pacf(CAMBIO)                                         
reglinEMP <- lm(CAMBIO ~ Anos)                      
reglinEMP                                            
plot(CAMBIO)                                       
abline(reglinEMP, col="Blue")    

residuosEMP <- reglinEMP$residuals                    
reglinEMPres <- lm(residuosEMP ~ Anos)                
plot(residuosEMP,type="l")                           
abline(reglinEMPres, col="Blue")   

pdcambio <- diff(CAMBIO)                                
diferenca1 <- (data.frame(CAMBIO[2:18],pdcambio))       
DIFERENCA <- ts(diferenca1, start = 1994, frequency = 1)  
plot(DIFERENCA, plot.type="single", col=c("Black","Green")) 
plot(pdcambio, type="l")                                   
                 
pdcambio1 <- diff(CAMBIO)                                           
TesteDF_cambio1_trend <- ur.df(pdcambio1, "trend", lags = 1)         
summary(TesteDF_cambio1_trend) 

pdcambio2 <- diff(diff(CAMBIO))                                      
TesteDF_cambio2_trend <- ur.df(pdcambio2, "trend", lags = 1)        
summary(TesteDF_cambio2_trend) 

arima123 <- arima(CAMBIO, c(1,2,3))

arima120 <- arima(CAMBIO, c(1,2,0))
arima121 <- arima(CAMBIO, c(1,2,1))
arima122 <- arima(CAMBIO, c(1,2,2))

arima220 <- arima(CAMBIO, c(2,2,0))
arima221 <- arima(CAMBIO, c(2,2,1))
arima222 <- arima(CAMBIO, c(2,2,2))
arima223 <- arima(CAMBIO, c(2,2,3))

arima021 <- arima(CAMBIO, c(0,2,1))
arima022 <- arima(CAMBIO, c(0,2,2))
arima023 <- arima(CAMBIO, c(0,2,3))

arima120 <- arima(CAMBIO, c(1,2,0))

estimacoes <- list(arima123,arima120,arima121,
                   arima122,arima220,arima221,
                   arima222,arima223,arima021,arima021, arima022,
                   arima023,arima120)
AIC <- sapply(estimacoes, AIC)
BIC <- sapply(estimacoes, BIC)
Modelo <-c(list("arima123","arima120","arima121",
                "arima122","arima220","arima221",
                "arima222","arima223","arima021","arima021", "arima022",
                "arima023","arima120")) 
Resultados <- data.frame(Modelo,AIC,BIC)

#An�lise para o PIB

plot(PIB, type = "l")                            
PIB <- ts(PIB, start = 1994, frequency = 1)  
plot(PIB, main="PIB", 
     ylab="PIB", 
     xlab="Ano")

acf(PIB)                                          
pacf(PIB)                                         
reglinEMP <- lm(PIB ~ Anos)                      
reglinEMP                                            
plot(PIB)                                       
abline(reglinEMP, col="Blue")    

residuosEMP <- reglinEMP$residuals                    
reglinEMPres <- lm(residuosEMP ~ Anos)                
plot(residuosEMP,type="l")                           
abline(reglinEMPres, col="Blue")   

pdpib <- diff(PIB)                                
diferenca1 <- (data.frame(PIB[2:18],pdpib))       
DIFERENCA <- ts(diferenca1, start = 1994, frequency = 1)  
plot(DIFERENCA, plot.type="single", col=c("Black","Green")) 
plot(pdpib, type="l")                                   

pdpib1 <- diff(PIB)                                           
TesteDF_pib1_trend <- ur.df(pdpib1, "trend", lags = 1)         
summary(TesteDF_pib1_trend) 

pdpib2 <- diff(diff(PIB))                                      
TesteDF_pib2_trend <- ur.df(pdpib2, "trend", lags = 1)        
summary(TesteDF_pib2_trend) 

arima123 <- arima(PIB, c(1,2,3))

arima120 <- arima(PIB, c(1,2,0))
arima121 <- arima(PIB, c(1,2,1))
arima122 <- arima(PIB, c(1,2,2))

arima220 <- arima(PIB, c(2,2,0))
arima221 <- arima(PIB, c(2,2,1))
arima222 <- arima(PIB, c(2,2,2))
arima223 <- arima(PIB, c(2,2,3))

arima021 <- arima(PIB, c(0,2,1))
arima022 <- arima(PIB, c(0,2,2))
arima023 <- arima(PIB, c(0,2,3))

arima120 <- arima(PIB, c(1,2,0))

estimacoes <- list(arima123,arima120,arima121,
                   arima122,arima220,arima221,
                   arima222,arima223,arima021,arima021, arima022,
                   arima023,arima120)
AIC <- sapply(estimacoes, AIC)
BIC <- sapply(estimacoes, BIC)
Modelo <-c(list("arima123","arima120","arima121",
                "arima122","arima220","arima221",
                "arima222","arima223","arima021","arima021", "arima022",
                "arima023","arima120")) 
Resultados <- data.frame(Modelo,AIC,BIC)
