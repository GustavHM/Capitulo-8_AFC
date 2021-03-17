#instalar e habilitar os pacotes lavaan e semTools
library(lavaan)
library(semTools)

#carregar o banco de dados
banco<-readRDS (url(
  "https://github.com/GustavHM/Capitulo-8_AFC/raw/main/banco_DASS21.rds"))
#No banco homem = 1 e mulher = 2.

############################## AFC ###################################
#Modelo bifactor
modelo_BF <- '
Estresse =~ DASS1  + DASS6  + DASS8  + DASS11 + DASS12 + DASS14 + DASS18 
Ansi     =~ DASS2  + DASS4  + DASS7  + DASS9  + DASS15 + DASS19 + DASS20
Depre    =~ DASS3  + DASS5  + DASS10 + DASS13 + DASS16 + DASS17 + DASS21

Distress =~ DASS1  + DASS2  + DASS3  + DASS4  + DASS5  + DASS6  + DASS7  + 
            DASS8  + DASS9  + DASS10 + DASS11 + DASS12 + DASS13 + DASS14 +
            DASS15 + DASS16 + DASS17 + DASS18 + DASS19 + DASS20 + DASS21

Distress ~~ 0*Estresse + 0*Ansi + 0*Depre
Estresse  ~~ 0*Ansi + 0*Depre
Ansi  ~~ 0*Depre
'
#rodar os resultados da AFC
resultados_BF <- sem(modelo_BF, data=banco, 
                     ordered=names(banco), estimator="WLSMV")

#checar os resultados da AFC
summary(resultados_BF, standardized=TRUE, fit.measures=TRUE)

############################# AFCMG #############################
#CONFIGURAL
configural <-measEq.syntax(configural.model=modelo_BF, data=banco,
                           ordered=names(banco), estimator="WLSMV",
                           group="Sexo", return.fit=TRUE)

#THRESHOLDS
thresholds <-measEq.syntax(configural.model=modelo_BF, data=banco,
                           ordered=names(banco), estimator="WLSMV",
                           group="Sexo", return.fit=TRUE,
                           group.equal="thresholds")

#MÉTRICA
metrica <- measEq.syntax(configural.model=modelo_BF, data=banco,
                         ordered=names(banco), estimator="WLSMV",
                         group="Sexo", return.fit=TRUE,
                         group.equal=c("thresholds","loadings"))

#ESCALAR (em modelos bifactor é necessário liberar manualmente as 
#médias/interceptos dos fatores)
#para saber mais consultar: https://github.com/simsem/semTools/issues/60
#gerar a sintaxe
sintaxe_escalar <-measEq.syntax(configural.model=modelo, data=banco,
                                ordered=names(banco), estimator="WLSMV",
                                group="Sexo",  ID.fac = "Uv",
                                group.equal=c("thresholds","loadings",
                                                   "intercepts"))
#abir a sintaxe
cat(as.character(sintaxe_escalar_sexo))

#modelo manual
modelo_manual <-'
## LOADINGS:
Segur =~ c(NA, NA)*ETD1 + c(lambda.1_1, lambda.1_1)*ETD1
Segur =~ c(NA, NA)*ETD2 + c(lambda.2_1, lambda.2_1)*ETD2
Segur =~ c(NA, NA)*ETD3 + c(lambda.3_1, lambda.3_1)*ETD3
Saude =~ c(NA, NA)*ETD4 + c(lambda.4_2, lambda.4_2)*ETD4
Saude =~ c(NA, NA)*ETD5 + c(lambda.5_2, lambda.5_2)*ETD5
Saude =~ c(NA, NA)*ETD6 + c(lambda.6_2, lambda.6_2)*ETD6
Remune =~ c(NA, NA)*ETD7 + c(lambda.7_3, lambda.7_3)*ETD7
Remune =~ c(NA, NA)*ETD8 + c(lambda.8_3, lambda.8_3)*ETD8
Remune =~ c(NA, NA)*ETD9 + c(lambda.9_3, lambda.9_3)*ETD9
Tempo =~ c(NA, NA)*ETD10 + c(lambda.10_4, lambda.10_4)*ETD10
Tempo =~ c(NA, NA)*ETD11 + c(lambda.11_4, lambda.11_4)*ETD11
Tempo =~ c(NA, NA)*ETD12 + c(lambda.12_4, lambda.12_4)*ETD12
Valores =~ c(NA, NA)*ETD13 + c(lambda.13_5, lambda.13_5)*ETD13
Valores =~ c(NA, NA)*ETD14 + c(lambda.14_5, lambda.14_5)*ETD14
Valores =~ c(NA, NA)*ETD15 + c(lambda.15_5, lambda.15_5)*ETD15
TD =~ c(NA, NA)*ETD1 + c(lambda.1_6, lambda.1_6)*ETD1
TD =~ c(NA, NA)*ETD2 + c(lambda.2_6, lambda.2_6)*ETD2
TD =~ c(NA, NA)*ETD3 + c(lambda.3_6, lambda.3_6)*ETD3
TD =~ c(NA, NA)*ETD4 + c(lambda.4_6, lambda.4_6)*ETD4
TD =~ c(NA, NA)*ETD5 + c(lambda.5_6, lambda.5_6)*ETD5
TD =~ c(NA, NA)*ETD6 + c(lambda.6_6, lambda.6_6)*ETD6
TD =~ c(NA, NA)*ETD7 + c(lambda.7_6, lambda.7_6)*ETD7
TD =~ c(NA, NA)*ETD8 + c(lambda.8_6, lambda.8_6)*ETD8
TD =~ c(NA, NA)*ETD9 + c(lambda.9_6, lambda.9_6)*ETD9
TD =~ c(NA, NA)*ETD10 + c(lambda.10_6, lambda.10_6)*ETD10
TD =~ c(NA, NA)*ETD11 + c(lambda.11_6, lambda.11_6)*ETD11
TD =~ c(NA, NA)*ETD12 + c(lambda.12_6, lambda.12_6)*ETD12
TD =~ c(NA, NA)*ETD13 + c(lambda.13_6, lambda.13_6)*ETD13
TD =~ c(NA, NA)*ETD14 + c(lambda.14_6, lambda.14_6)*ETD14
TD =~ c(NA, NA)*ETD15 + c(lambda.15_6, lambda.15_6)*ETD15

## THRESHOLDS:
ETD1 | c(NA, NA)*t1 + c(ETD1.thr1, ETD1.thr1)*t1
ETD1 | c(NA, NA)*t2 + c(ETD1.thr2, ETD1.thr2)*t2
ETD1 | c(NA, NA)*t3 + c(ETD1.thr3, ETD1.thr3)*t3
ETD1 | c(NA, NA)*t4 + c(ETD1.thr4, ETD1.thr4)*t4
ETD1 | c(NA, NA)*t5 + c(ETD1.thr5, ETD1.thr5)*t5
ETD1 | c(NA, NA)*t6 + c(ETD1.thr6, ETD1.thr6)*t6
ETD2 | c(NA, NA)*t1 + c(ETD2.thr1, ETD2.thr1)*t1
ETD2 | c(NA, NA)*t2 + c(ETD2.thr2, ETD2.thr2)*t2
ETD2 | c(NA, NA)*t3 + c(ETD2.thr3, ETD2.thr3)*t3
ETD2 | c(NA, NA)*t4 + c(ETD2.thr4, ETD2.thr4)*t4
ETD2 | c(NA, NA)*t5 + c(ETD2.thr5, ETD2.thr5)*t5
ETD2 | c(NA, NA)*t6 + c(ETD2.thr6, ETD2.thr6)*t6
ETD3 | c(NA, NA)*t1 + c(ETD3.thr1, ETD3.thr1)*t1
ETD3 | c(NA, NA)*t2 + c(ETD3.thr2, ETD3.thr2)*t2
ETD3 | c(NA, NA)*t3 + c(ETD3.thr3, ETD3.thr3)*t3
ETD3 | c(NA, NA)*t4 + c(ETD3.thr4, ETD3.thr4)*t4
ETD3 | c(NA, NA)*t5 + c(ETD3.thr5, ETD3.thr5)*t5
ETD3 | c(NA, NA)*t6 + c(ETD3.thr6, ETD3.thr6)*t6
ETD4 | c(NA, NA)*t1 + c(ETD4.thr1, ETD4.thr1)*t1
ETD4 | c(NA, NA)*t2 + c(ETD4.thr2, ETD4.thr2)*t2
ETD4 | c(NA, NA)*t3 + c(ETD4.thr3, ETD4.thr3)*t3
ETD4 | c(NA, NA)*t4 + c(ETD4.thr4, ETD4.thr4)*t4
ETD4 | c(NA, NA)*t5 + c(ETD4.thr5, ETD4.thr5)*t5
ETD4 | c(NA, NA)*t6 + c(ETD4.thr6, ETD4.thr6)*t6
ETD5 | c(NA, NA)*t1 + c(ETD5.thr1, ETD5.thr1)*t1
ETD5 | c(NA, NA)*t2 + c(ETD5.thr2, ETD5.thr2)*t2
ETD5 | c(NA, NA)*t3 + c(ETD5.thr3, ETD5.thr3)*t3
ETD5 | c(NA, NA)*t4 + c(ETD5.thr4, ETD5.thr4)*t4
ETD5 | c(NA, NA)*t5 + c(ETD5.thr5, ETD5.thr5)*t5
ETD5 | c(NA, NA)*t6 + c(ETD5.thr6, ETD5.thr6)*t6
ETD6 | c(NA, NA)*t1 + c(ETD6.thr1, ETD6.thr1)*t1
ETD6 | c(NA, NA)*t2 + c(ETD6.thr2, ETD6.thr2)*t2
ETD6 | c(NA, NA)*t3 + c(ETD6.thr3, ETD6.thr3)*t3
ETD6 | c(NA, NA)*t4 + c(ETD6.thr4, ETD6.thr4)*t4
ETD6 | c(NA, NA)*t5 + c(ETD6.thr5, ETD6.thr5)*t5
ETD6 | c(NA, NA)*t6 + c(ETD6.thr6, ETD6.thr6)*t6
ETD7 | c(NA, NA)*t1 + c(ETD7.thr1, ETD7.thr1)*t1
ETD7 | c(NA, NA)*t2 + c(ETD7.thr2, ETD7.thr2)*t2
ETD7 | c(NA, NA)*t3 + c(ETD7.thr3, ETD7.thr3)*t3
ETD7 | c(NA, NA)*t4 + c(ETD7.thr4, ETD7.thr4)*t4
ETD7 | c(NA, NA)*t5 + c(ETD7.thr5, ETD7.thr5)*t5
ETD7 | c(NA, NA)*t6 + c(ETD7.thr6, ETD7.thr6)*t6
ETD8 | c(NA, NA)*t1 + c(ETD8.thr1, ETD8.thr1)*t1
ETD8 | c(NA, NA)*t2 + c(ETD8.thr2, ETD8.thr2)*t2
ETD8 | c(NA, NA)*t3 + c(ETD8.thr3, ETD8.thr3)*t3
ETD8 | c(NA, NA)*t4 + c(ETD8.thr4, ETD8.thr4)*t4
ETD8 | c(NA, NA)*t5 + c(ETD8.thr5, ETD8.thr5)*t5
ETD8 | c(NA, NA)*t6 + c(ETD8.thr6, ETD8.thr6)*t6
ETD9 | c(NA, NA)*t1 + c(ETD9.thr1, ETD9.thr1)*t1
ETD9 | c(NA, NA)*t2 + c(ETD9.thr2, ETD9.thr2)*t2
ETD9 | c(NA, NA)*t3 + c(ETD9.thr3, ETD9.thr3)*t3
ETD9 | c(NA, NA)*t4 + c(ETD9.thr4, ETD9.thr4)*t4
ETD9 | c(NA, NA)*t5 + c(ETD9.thr5, ETD9.thr5)*t5
ETD9 | c(NA, NA)*t6 + c(ETD9.thr6, ETD9.thr6)*t6
ETD10 | c(NA, NA)*t1 + c(ETD10.thr1, ETD10.thr1)*t1
ETD10 | c(NA, NA)*t2 + c(ETD10.thr2, ETD10.thr2)*t2
ETD10 | c(NA, NA)*t3 + c(ETD10.thr3, ETD10.thr3)*t3
ETD10 | c(NA, NA)*t4 + c(ETD10.thr4, ETD10.thr4)*t4
ETD10 | c(NA, NA)*t5 + c(ETD10.thr5, ETD10.thr5)*t5
ETD10 | c(NA, NA)*t6 + c(ETD10.thr6, ETD10.thr6)*t6
ETD11 | c(NA, NA)*t1 + c(ETD11.thr1, ETD11.thr1)*t1
ETD11 | c(NA, NA)*t2 + c(ETD11.thr2, ETD11.thr2)*t2
ETD11 | c(NA, NA)*t3 + c(ETD11.thr3, ETD11.thr3)*t3
ETD11 | c(NA, NA)*t4 + c(ETD11.thr4, ETD11.thr4)*t4
ETD11 | c(NA, NA)*t5 + c(ETD11.thr5, ETD11.thr5)*t5
ETD11 | c(NA, NA)*t6 + c(ETD11.thr6, ETD11.thr6)*t6
ETD12 | c(NA, NA)*t1 + c(ETD12.thr1, ETD12.thr1)*t1
ETD12 | c(NA, NA)*t2 + c(ETD12.thr2, ETD12.thr2)*t2
ETD12 | c(NA, NA)*t3 + c(ETD12.thr3, ETD12.thr3)*t3
ETD12 | c(NA, NA)*t4 + c(ETD12.thr4, ETD12.thr4)*t4
ETD12 | c(NA, NA)*t5 + c(ETD12.thr5, ETD12.thr5)*t5
ETD12 | c(NA, NA)*t6 + c(ETD12.thr6, ETD12.thr6)*t6
ETD13 | c(NA, NA)*t1 + c(ETD13.thr1, ETD13.thr1)*t1
ETD13 | c(NA, NA)*t2 + c(ETD13.thr2, ETD13.thr2)*t2
ETD13 | c(NA, NA)*t3 + c(ETD13.thr3, ETD13.thr3)*t3
ETD13 | c(NA, NA)*t4 + c(ETD13.thr4, ETD13.thr4)*t4
ETD13 | c(NA, NA)*t5 + c(ETD13.thr5, ETD13.thr5)*t5
ETD13 | c(NA, NA)*t6 + c(ETD13.thr6, ETD13.thr6)*t6
ETD14 | c(NA, NA)*t1 + c(ETD14.thr1, ETD14.thr1)*t1
ETD14 | c(NA, NA)*t2 + c(ETD14.thr2, ETD14.thr2)*t2
ETD14 | c(NA, NA)*t3 + c(ETD14.thr3, ETD14.thr3)*t3
ETD14 | c(NA, NA)*t4 + c(ETD14.thr4, ETD14.thr4)*t4
ETD14 | c(NA, NA)*t5 + c(ETD14.thr5, ETD14.thr5)*t5
ETD14 | c(NA, NA)*t6 + c(ETD14.thr6, ETD14.thr6)*t6
ETD15 | c(NA, NA)*t1 + c(ETD15.thr1, ETD15.thr1)*t1
ETD15 | c(NA, NA)*t2 + c(ETD15.thr2, ETD15.thr2)*t2
ETD15 | c(NA, NA)*t3 + c(ETD15.thr3, ETD15.thr3)*t3
ETD15 | c(NA, NA)*t4 + c(ETD15.thr4, ETD15.thr4)*t4
ETD15 | c(NA, NA)*t5 + c(ETD15.thr5, ETD15.thr5)*t5
ETD15 | c(NA, NA)*t6 + c(ETD15.thr6, ETD15.thr6)*t6

## INTERCEPTS:
ETD1 ~ c(0, 0)*1 + c(nu.1, nu.1)*1
ETD2 ~ c(0, 0)*1 + c(nu.2, nu.2)*1
ETD3 ~ c(0, 0)*1 + c(nu.3, nu.3)*1
ETD4 ~ c(0, 0)*1 + c(nu.4, nu.4)*1
ETD5 ~ c(0, 0)*1 + c(nu.5, nu.5)*1
ETD6 ~ c(0, 0)*1 + c(nu.6, nu.6)*1
ETD7 ~ c(0, 0)*1 + c(nu.7, nu.7)*1
ETD8 ~ c(0, 0)*1 + c(nu.8, nu.8)*1
ETD9 ~ c(0, 0)*1 + c(nu.9, nu.9)*1
ETD10 ~ c(0, 0)*1 + c(nu.10, nu.10)*1
ETD11 ~ c(0, 0)*1 + c(nu.11, nu.11)*1
ETD12 ~ c(0, 0)*1 + c(nu.12, nu.12)*1
ETD13 ~ c(0, 0)*1 + c(nu.13, nu.13)*1
ETD14 ~ c(0, 0)*1 + c(nu.14, nu.14)*1
ETD15 ~ c(0, 0)*1 + c(nu.15, nu.15)*1

## SCALING FACTORS:
ETD1 ~*~ c(1, NA)*ETD1
ETD2 ~*~ c(1, NA)*ETD2
ETD3 ~*~ c(1, NA)*ETD3
ETD4 ~*~ c(1, NA)*ETD4
ETD5 ~*~ c(1, NA)*ETD5
ETD6 ~*~ c(1, NA)*ETD6
ETD7 ~*~ c(1, NA)*ETD7
ETD8 ~*~ c(1, NA)*ETD8
ETD9 ~*~ c(1, NA)*ETD9
ETD10 ~*~ c(1, NA)*ETD10
ETD11 ~*~ c(1, NA)*ETD11
ETD12 ~*~ c(1, NA)*ETD12
ETD13 ~*~ c(1, NA)*ETD13
ETD14 ~*~ c(1, NA)*ETD14
ETD15 ~*~ c(1, NA)*ETD15

## LATENT MEANS/INTERCEPTS:
Segur ~ c(0, NA)*1 + c(alpha.1.g1, alpha.1.g2)*1
Saude ~ c(0, NA)*1 + c(alpha.2.g1, alpha.2.g2)*1
Remune ~ c(0, NA)*1 + c(alpha.3.g1, alpha.3.g2)*1
Tempo ~ c(0, NA)*1 + c(alpha.4.g1, alpha.4.g2)*1
Valores ~ c(0, NA)*1 + c(alpha.5.g1, alpha.5.g2)*1
TD ~ c(0, NA)*1 + c(alpha.6.g1, alpha.6.g2)*1

## COMMON-FACTOR VARIANCES:
Segur ~~ c(1, NA)*Segur + c(psi.1_1.g1, psi.1_1.g2)*Segur
Saude ~~ c(1, NA)*Saude + c(psi.2_2.g1, psi.2_2.g2)*Saude
Remune ~~ c(1, NA)*Remune + c(psi.3_3.g1, psi.3_3.g2)*Remune
Tempo ~~ c(1, NA)*Tempo + c(psi.4_4.g1, psi.4_4.g2)*Tempo
Valores ~~ c(1, NA)*Valores + c(psi.5_5.g1, psi.5_5.g2)*Valores
TD ~~ c(1, NA)*TD + c(psi.6_6.g1, psi.6_6.g2)*TD

## COMMON-FACTOR COVARIANCES:
Segur ~~ c(0, 0)*Saude + c(psi.2_1.g1, psi.2_1.g2)*Saude
Segur ~~ c(0, 0)*Remune + c(psi.3_1.g1, psi.3_1.g2)*Remune
Segur ~~ c(0, 0)*Tempo + c(psi.4_1.g1, psi.4_1.g2)*Tempo
Segur ~~ c(0, 0)*Valores + c(psi.5_1.g1, psi.5_1.g2)*Valores
Segur ~~ c(0, 0)*TD + c(psi.6_1.g1, psi.6_1.g2)*TD
Saude ~~ c(0, 0)*Remune + c(psi.3_2.g1, psi.3_2.g2)*Remune
Saude ~~ c(0, 0)*Tempo + c(psi.4_2.g1, psi.4_2.g2)*Tempo
Saude ~~ c(0, 0)*Valores + c(psi.5_2.g1, psi.5_2.g2)*Valores
Saude ~~ c(0, 0)*TD + c(psi.6_2.g1, psi.6_2.g2)*TD
Remune ~~ c(0, 0)*Tempo + c(psi.4_3.g1, psi.4_3.g2)*Tempo
Remune ~~ c(0, 0)*Valores + c(psi.5_3.g1, psi.5_3.g2)*Valores
Remune ~~ c(0, 0)*TD + c(psi.6_3.g1, psi.6_3.g2)*TD
Tempo ~~ c(0, 0)*Valores + c(psi.5_4.g1, psi.5_4.g2)*Valores
Tempo ~~ c(0, 0)*TD + c(psi.6_4.g1, psi.6_4.g2)*TD
Valores ~~ c(0, 0)*TD + c(psi.6_5.g1, psi.6_5.g2)*TD'

#invari?ncia escalar manual
escalar_sexo <- cfa(modelo_manual, data = banco, group = "Sexo", 
                    ordered=names(banco), estimator="WLSMV",std.lv = TRUE)
#summary(escalar_sexo, fit.measures=T, standardized=T)

#RESIDUAL
residual <- measEq.syntax(configural.model=modelo_BF, data=banco,
                          ordered=names(banco), estimator="WLSMV",     
                          group="Sexo", return.fit=TRUE,
                          group.equal=c("thresholds","loadings", 
                                        "intercepts","residuals"),
                          parameterization="theta") 

#comparar os modelos de invariância
invariancia <- compareFit(configural, thresholds, metrica, escalar, residual)
summary(invariancia, 
        fit.measures=c("cfi.scaled","rmsea.scaled","srmr","mfi"))

#Gamma hat
moreFitIndices(configural, fit.measures="gammaHat.scaled")
moreFitIndices(metrica, fit.measures="gammaHat.scaled")
moreFitIndices(escalar, fit.measures="gammaHat.scaled")
moreFitIndices(residual, fit.measures="gammaHat.scaled")
