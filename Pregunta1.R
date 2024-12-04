#ejercicio exp/pois:

#el nombre de trucades telefoniques es modela com una variable aleatoria de poisson.
#Suposem que de mitjana hi ha 25 trucades per hora:
lambda <- 25
#1 probabilitat de que passi un temps de 0.033 hores entre dues trucades?
dpois(0.033, lambda) #nos da error non-integer 
0
#2 probabilitat de que passi un temps de al menos 0.052 hores entre dues trucades?
1-ppois(0.052, lambda)
#3 se hacen 500000 experimentos, la mitjana del temps entre dues trucades es proper a:
rpois(500000,lambda)
mean(rpois(500000,lambda))

#------------------------------------------------------
#el tiempo de duración de una componente sigue una distribución exponencial con media 10000 horas:
lambda <- 1/10000 #CUANDO NOS DICEN COMPONENTE HACEMOS 1/NUMERO

#1 probabilidad de que una componente dure por lo menos 13000 horas?
#P(x>13000)= 1-P(X<=13000)
1-pexp(13000, lambda)

#2 cual es la duración que superan las componentes con una probabilidad de 0.2?
qexp(0.8, lambda)

#realiza 100 simulaciones de la variable X. Utiliza la semilla 85:
#3 el valor de la media de los resultados de las simulaciones
set.seed(85)
rexp(100, lambda)
mean(rexp(100, lambda))
#4 mediana
median(rexp(100, lambda))
#5 varianza 
var(rexp(100, lambda))

#--------------------------------------------------------------------------
#una investigacion previa ha demostrado que el numero de imperfecciones en un 
#alambre fino  de cobre tiene una media de 8 imperfecciones por centimetro de longitud. 
lambda <- 8
#1cual es la probabilidad de que haya una distancia de 0.106 centímetros entre dos imperfecciones del alambre?
dpois(0.106, lambda)
#SIEMPRE ES CERO 
#2 cual es la probabilidad de que haya una distancia de menos de 0.146 centímetros entre dos imperfecciones del alambre?
#P(X<0.146)
pexp(0.145, lambda)
#3 Si se simulan 400000 experimentos, cual es la media de la distancia entre dos imperfecciones?
rexp(400000, lambda)
mean(rexp(400000, lambda))


