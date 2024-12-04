#PROBLEMA 2 RAFA 

#una máquina empaqueta hojas de papel con una media de 498 hojas por paquete
#y con prob aproximada de empaquetar más de 489 hojas de 0.99865. 
#un paquete se considera aceptable si tiene entre 492 y 501 hojas.
#el procesos sigue una distribución normal 
media <- 498
p <- 0.9986501 #prob mas de 489 hojas

#1 prob de que un paquete elegido al azar no sea aceptable?
z <- qnorm(0.9986501, 0, 1)
z*(489-498)/z
sd<- abs(z)
sd
#otra forma (MEJOR)
z_489 <- qnorm(1-0.9986501) #porq nos dicen (Px>489)
# 1 - p porque estamos buscando la probabilidad acumulada hasta 489
# La fórmula de la normal estándar es Z = (X - mu) / sigma
# Entonces σ = (X - mu) / Z
sigma <- (489 - mu) / z_489
sigma
#
p492 <- pnorm(491, media, sd) #P(X<492) IMPORTANTEE
p501 <- 1-pnorm(501, media, sd) #P(X>501) = 1-P(X<=501)
pnoaceptable <- p492+p501
pnoaceptable


#2 prob de que un paquete elegido al azar contenga más de 501 hojas:
#P(X>501) = 1-P(X<=501)
1-pnorm(501, media, sd)

#3 cual es el grafico correcto para la función de densidad del numero de hojas agrupadas en un paquere?
x <- seq(480, 520, by=0.1)
densidad <- dnorm(x, media, sd)
plot(x, densidad, type ='l', xlab='número de hojas', ylab='Densidad', main='Función de densidad')


#2prob de que un paquete elegido al azar contenga 502 hojas 
dnorm(502, media, sd)

#3
#la máquina se ajusta para que la media de hojas sea de 499,5 hojas por paquete.
#prob de que un paquete contenga menos de 495 hojas es de 0.025, desviacion estandard?
media2 <- 499.5
p2 <- 0.025
z <- qnorm(0.025, 0, 1)
sd <- abs((495 - media2) / z)
sd

# OTRA FORMA
media2 <- 499.5
p_value <- 0.025
X <- 495
# Paso 1: Encontrar el valor z correspondiente a P(X < 495) = 0.025
z_value <- qnorm(p_value) #ponemos directamente p ya que P(X < 495) = 0.025
# Paso 2: Usar la fórmula Z = (X - mu) / sigma para despejar sigma
sigma <- (X - media2) / z_value
sigma

#4 genera una simulación de 10 000 paquetes usando la semilla 321 
#calcula la proporción de paquetes que contienen entre 497 y 507 hojas. 
med <- 502
desv <- 2

set.seed(321)
experimento <- rnorm(10000, med, desv)
mean(experimento >= 497 & experimento <= 507)

#5 Supon que en cada paquete la probabilidad de que una hoja adicional sea agregada o quitada 
#durante el proceso es de 0.02 . Si cada paquete se compone de 500 hojas, cual es la prob de que 10 hojas
#sean quitadas o agregadas?

dbinom(10, 500, 0.02)

#--------------------------------------------------------------------------
#EJ 2 MERY 
#una fabrica de llaunes produeix una mitjana de 330 ml. segeueix una distrib normal.
# la prob que una llauna tingui més de 320ml es de 0.995. 
#Una llauna es acceptable si contre entre 325 i 335 
rm(list = ls())
media <- 330 
p <- 0.995

#P(X>320) = 1-P(X<=320)
#1 prob de que no sigui acceptable 
z_320 <- qnorm(1-0.995)
z_320#porq nos dicen (Px>489)
# 1 - p porque estamos buscando la probabilidad acumulada hasta 489
# La fórmula de la normal estándar es Z = (X - mu) / sigma
# Entonces σ = (X - mu) / Z
sigma <- (320 - media) / z_320
sigma

#P(X<325) 
p325 <- pnorm(324, media, sigma)
#P(X>335)=1-P(X>=335)
p335<-1-pnorm(335, media, sigma)
p1 <- p325+p335
p1


#2 prob de que una llauna al atzar contingui 336 ml 
dnorm(336, media, sigma)


#3la mitjana ara es de 331ml prob de que una llauna contingui menys de 327 ml es de 0.05
#determina la desv estandard:

media <- 331
p <- 0.05 #(PX<327)


z <- qnorm(p)
sd <- (327 - media) / z
sd


#4 simulacio en R mitjana de 329ml sd de 2 ml.
#utilizant 10000 experiments i la llavor 456 
#calcula proporció de llaunes que contenen entre 324 i 334 ml

media <- 329
sd <- 2

set.seed(456)
experimento <- rnorm(10000, media, sd)
mean(experimento>= 324 & experimento<=334)


#5 durant el proces hi ha una probabilitat del 3% de que s'afegeixi o es tregui una mica de líquid
# cada llauna esta dissenyada per a contenir 330 ml 
#calcula la prob de que exacatament 5ml siguin afegits o retirats ç
dbinom(5, 330, 0.03)

