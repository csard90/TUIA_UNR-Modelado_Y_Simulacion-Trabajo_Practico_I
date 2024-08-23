# -----------------------------------------------------------------------------

# Modelado y Simulacion - TUIA - TP 01 - Donnarumma Cesar Julian

# Ejercicios Practicos

# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------

# Practica 01 - Ejercicio 8: Coincidencia de items

# Sobre una mesa hay diez cartas y diez sobres, uno correspondiente a cada carta. 
# Un asistente administrativo muy cansado coloca las cartas en los sobres al azar. 
# En este problema, interesa calcular la probabilidad de que ninguna carta sea 
# colocada en su sobre correspondiente, asi como el numero esperado de cartas 
# colocadas en los sobres correctos. El siguiente programa en R busca aproximar 
# estas cantidades mediante simulacion:

m = 100000 # Asigna a la variable m el valor 100000 (nro de repeticiones)
n = 10 # Asigna a la variable n el valor 10 (cantidad de cartas/sobres)
x = numeric(m) # Crea un vector numerico de m elementos y lo asiga a la
# variable x. Aqui luego se guardaran los resultados de guardar
# las cartas en sobres

for (i in 1:m) { # Bucle que hace las m simulaciones
  
  perm = sample(1:n, n) # Extrae una muestra sin reposicion de tamaño n (10) de 
  # la poblacion de 10 cartas.
  
  x[i] = sum(1:n == perm) # Se compara si el orden de las cartas (perm) extraidas
  # es el mismo que el de los sobres (ordenados del 1 al 10)
  # esto arroja vector logico donde TRUE=1 cuando coinciden, 
  # luego se suman la cantidad de coincidencias (cartas
  # bien guardadas) y guarda el resultado de la simulacion
  # la posicion i del vector x.
}

cutp = (0:(n+1)) - 0.5 # Se generan los valores que van a servir como 
# intervalos para el histograma.

hist(x, breaks = cutp, prob = T) # Se crea el histograma con el vector x
# que contiene la cantidad de coincidencias
# de cada iteracion agrupadas por intervalos
# calculados arriba

mean(x == 0) # Calculo de la proporcion de no coincidencias

mean(x) # Calcula la media de la distribucion generada

sd(x) # Calcula el desvio estandar de la distribucion generada

# ----------------------------------------------------------------------------

# 1. Explicar que es lo que hace cada una de las lineas del codigo anterior

# Resuelto directamente arriba

# ----------------------------------------------------------------------------

# 2. Ejecutar el programa con n=10 cartas y sobres, luego con n=5, y nuevamente 
# con n=20. Registrar y comparar los resultados obtenidos. Intuitivamente, 
# ¿como esperaria que varie la probabilidad de no coincidencia, llamemosle pn, 
# con n?

sim_cartas = function(n, flag=F) {
  
  m = 100000; x = numeric(m)
  
  for (i in 1:m) {
    perm = sample(1:n, n)
    x[i] = sum(1:n == perm)
  }
  
  pn = mean(x == 0)
  
  if (flag) {
    
    return(pn)
    
  } else {
    
    cutp = (0:(n+1)) - 0.5
    hist(x, breaks = cutp, prob = T)
    media = mean(x)
    desv_est = sd(x)
    return(list(pn, media, desv_est))
    
  }
  
}

# Simulacion con distintos valores de n
n_5 = sim_cartas(n=5)
n_10 = sim_cartas(n=10)
n_20 = sim_cartas(n=20)
# Agregamos mas valores de n solo para ver que pasa
n_50 = sim_cartas(n=50)
n_100 = sim_cartas(n=100)

# Tabla comparativa
comparacion = rbind(n_5, n_10, n_20, n_50, n_100)
colnames(comparacion) = c('pn', 'media', 'desv_est')
comparacion

# Rta: ¿como esperaria que varie pn con n? A menor cantidad de sobres esperaria
# mayor probabilidad de coincidencia, por lo tanto menor probabilidad
# de no coincidencia, y a mayor cantidad de sobres menor probabilidad
# de coincidencias y mayor de no coincidencia. Eso es lo que se me viene
# intuitivamente.
# De todos modos de la comparacion en la tabla comparativa generada arriba
# vemos que la probabilidad de no coincidencia se mantiene bastante estable
# en torno a 0.36 y su media y desv estandar denotan valores muy concentrados
# alrededor de 0, 1 y 2 lo cual se refleja en los histogramas.

# ----------------------------------------------------------------------------

# 3. Utilizando el principio de inclusion-exclusion, probar que:

#     1 − pn = 1 − 1/2! + 1/3! − ... + ( (−1)**(n+1) )/n!

# Funcion que utiliza el principio de inclusion-exclusion
inc_exc = function(n) {
  
  suma = 0
  
  for (i in 1:n) {
    termino = ( (-1)**(i+1) ) / factorial(i)
    suma <- suma + termino
  }
  
  return(suma)
  
}

# Volvemos a simular para calcular pn para n=20
pn_n20 = as.numeric(unlist(sim_cartas(n=20)[1]))

# Calculamos con el principio de inclusion-exclusion
inc_exc_n20 = inc_exc(n=20)

sprintf('1 - pn (simulacion n=20): %f', 1 - pn_n20)
sprintf('1 - pn (inclusion-exclusion n=20): %f', inc_exc_n20)

# Rta: por medio del algoritmo anterior se puede verificar que la igualdad
# es correcta.

# ----------------------------------------------------------------------------

# 4. Evaluar y graficar (en R) las probabilidades pn para n = 2, 3, ... , 10 y, 
# utilizando el desarrollo exponencial 
# e**x = 1 + x + (x**2)/2! + (x**3)/3! + ... , para todo x perteneciente a R, 
# deducir que pn ≈ 1 − e**1 = 0.6321... para n grande

# Evaluar las probabilidades pn para n= 2, ... , 10
# Lo abordaremos con la simualacion anterior.

# Valores de n (agregamos algunos mas para que se vea mejor en la grafica del final)
ns = 2:20

# Aca se guardan los valores de pn para los distintos n
pns = numeric(length(ns))

# Simulamos
for (i in ns) {
  pns[i-1] = sim_cartas(n=i, flag=T) 
}

# Segun la consigna pn ≈ 1 − e**-1 = 0.6321 para n grande:
1 - exp(-1)

# Pero segun las simulaciones pn se distribuye 0.36.. para n grande
pns
# y segun el despeje del punto anterior con el principio de inc-exc, 
# 1-pn = inc-exc -> 1-inc_exc = pn, tambien se distribuje 0.36 aprox
1-inc_exc_n20

# Si vemos que exp(-1) es igual a 0.3678


# Asumimos un error en la consigna del ejercicio y lo correcto seria decir que
# pn ≈ e**-1 = 0.3678 ... para n grande.

# Grafico
plot(ns, pns, pch = 16, cex = 0.8, xaxt = "n", col = "darkblue", 
     xlab = "n de la simulacion", ylab = "pn", main='Grafico de puntos n vs pn')
axis(1, at = ns, labels = ns)
abline(h = exp(-1), col = "red", lwd = 1)
legend("topright", legend = "exp(-1)", col = "red", lty = 1, lwd = 1)

# Rta: luego de la simulacion y de la correccion anterior sumado al grafico
# concluimos lo que dice el enunciado.

# ----------------------------------------------------------------------------

# 5. Si X es la v.a que cuenta el numero de coincidencias para n cartas y sobres, 
# puede probarse que para n "grande" su distribucion es aproximadamente 
# Pois(λ = 1) y, por lo tanto, E(X) = Var(X) ≈ 1. 
# Comparar numerica y graficamente estas probabilidades agregando el siguiente 
# codigo al de la simulacion:

# x.obs = sort(unique(x))
# cbind(’sim’ = prop.table(table(x)), ’Pois’ = round(dpois(x.obs, 1), 5))
# points(0:10, dpois(0:10, 1), pch = 19)

# Simulacion anterior
m = 100000; n = 10; x = numeric(m)
for (i in 1:m) {perm = sample(1:n, n); x[i] = sum(1:n == perm)}
cutp = (-1:n) - .5; hist(x, breaks = cutp, prob = T)
mean(x == 0); mean(x); sd(x)

x.obs = sort(unique(x))
cbind("sim" = prop.table(table(x)), "Pois" = round(dpois(x.obs, 1), 5))
points(0:10, dpois(0:10, 1), pch = 19)

# Rta: se puede observar en la tabla que las densidades en los valores 
# simulados y de la distribucion pois son muy similares, ademas en el 
# histograma la altura de las barras es tambien similar a la de las 
# densidades generadas con pois. Por lo que parece ser cierto lo que afirma
# el enunciado.

# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------

# Practica 01: Ejercicio 12 - Probabilidades de cobertura en IC para proporciones

# ---------------------------------------------------------------------------- 

# 1. Modificar el programa en R del Ejemplo sobre la probabilidad de 6 en un 
# dado, para verificar que la probabilidad (exacta) de cobertura del IC 
# correspondiente  a n = 30 y p = 0.79 es 0.88756... Tambien para n = 30, 
# hallar las probabilidades de cobertura para p = 1/6, 0.700 y 0.699.

# ---------------------------------------------------------------------------- 

# Funcion para reutilizar codigo:

prob_ic = function(p) { # p es la proporcion poblacional de cantidad de 6
  # en 30 tiradas
  
  n = 30 # cantidad de experimentos
  
  x = 0:n # vector con posibles valores de cantidad de 6 en 30 tiradas
  
  p.est = x/n # proporcion estimada
  
  alpha = .05 # fijamos riesgo
  
  z.alpha = qnorm(1-alpha/2) # 1.96 aprox. para alpha = 0.05
  
  error = z.alpha * sqrt(p.est * (1 - p.est) / n) # calculo del error para IC
  
  IC.izq = p.est - error # limite izquierdo de cada IC
  
  IC.der = p.est + error # limite derecho de cada IC
  
  prob = dbinom(x, n, p) # probabilidad exacta de X=x
  
  cober = (p >= IC.izq) & (p <= IC.der) # ¿p pertenece a cada IC?
  
  round(cbind(x, p.est, IC.izq, IC.der, prob, cober), 4)
  
  # Probabilidad de cobertura del IC
  prob[cober] # P{X=x} para x cuyos IC cubren p = 0.79
  
  return(sum(prob[cober])) # P(cobertura)
  
}

# ---------------------------------------------------------------------------- 

# n=30, p=0.79

prob_ic(p=0.79)

# Rta: Se verifica lo que dice el enunciado, la probabilidad de cobertura del IC
# es de 0.88756...

# ---------------------------------------------------------------------------- 

# n=30, p=1/6

prob_ic(p=1/6)

# Rta: Para p=1/6 la probabilidad de cobertura del IC es de 0.8905

# ----------------------------------------------------------------------------

# n=30, p=0.7

prob_ic(p=0.7)

# Rta: Para p=0.7 la probabilidad de cobertura del IC es de 0.9529

# ----------------------------------------------------------------------------

# n=30, p=0.699

prob_ic(p=0.699)

# Rta: Para p=0.699 la probabilidad de cobertura del IC es de 0.9076

# ----------------------------------------------------------------------------

# 2. Repetir lo anterior para un vector de valores de p de largo 2000, que 
# vaya de 1/30 a 1−(1/30) 
# ( en R: n = 30; m = 2000; p = seq(1/n, 1-1/n, length = m) ) y graficar las 
# probabilidades de cobertura correspondientes (ver Figura 1.6 y Ejemplos 
# 1.5 y 1.6 en el libro IPSGSwR).

prob_ic_2 = function(n) {
  
  p = seq(1/30, 1-1/30, length = 2000) # vector con las 2000 proporciones
  # poblacionales de cantidad de 6
  
  x = 0:n # vector con posibles valores de cantidad de 6 en 30 tiradas
  
  p.est = x/n # proporcion estimada
  
  alpha = .05 # fijamos riesgo
  
  z.alpha = qnorm(1-alpha/2) # 1.96 aprox. para alpha = 0.05
  
  error = z.alpha * sqrt(p.est * (1 - p.est) / n) # calculo del error para IC
  
  IC.izq = p.est - error # limite izquierdo de cada IC
  
  IC.der = p.est + error # limite derecho de cada IC
  
  p_ic = numeric(2000)
  
  for (i in 1:2000) {
    
    prob = dbinom(x, n, p[i]) # probabilidad exacta de X=x
    
    cober = (p[i] >= IC.izq) & (p[i] <= IC.der) # ¿p pertenece a cada IC?
    
    p_ic[i] = sum(prob[cober]) # P(cobertura)
    
  }
  
  x = sprintf('Proporcion de cantidades de 6 en %i tiradas', n)
  y = 'Prop. cant. int. que cubren el verdadero valor'
  
  
  # Crear el gráfico de puntos
  plot(p, p_ic, main="Gráfico de Puntos", xlab=x,
       ylab=y, pch=20, col="black", bg="black", cex=0.4)
  lines(p, p_ic, type = "l", col = "red")
  abline(h = 0.95, col = "blue", lty = 2)
  
  
}

prob_ic_2(30)

# ----------------------------------------------------------------------------

# 3. A partir de esta evidencia, ¿que parece ser mas comun: probabilidades de 
# cobertura por debajo del 95% o por encima del 95%? 
# En el Ejemplo visto en clase, la probabilidad de obtener un 6 es p = 1/6, 
# y 14/20 IC cubrieron p. ¿Es esto mejor, peor o aproximadamente lo esperado?

# Rta: a partir de la evidencia para ser mas comun probabilidades
# de cobertura por debajo del 95%.

# Teniendo en cuenta que para p=1/6 lo esperado es que el 0.8904 de los intervalos
# cubran el valor esperado, 0.7 (14/20) es peor de lo esperado.

prob_ic(p=1/6) # Calculo exacto de cantidad de intervalos que cubren el 
# verdadero valor del parametro

14/20 # Calculo por simulacion

# -----------------------------------------------------------------------------

# 4. Repetir el punto 2 pero para muestras de tamaño n = 50 y n = 100 
# (comparar con Figura 1.7 en IPSGSwR).

prob_ic_2(50)
# Con muestra de tamaño 50 tenemos mas cantidad de coberturas por encima del 95%

prob_ic_2(100)
# Con muestra de tamaño 100 tenemos mas cantidad de coberturas por encima del 95%

# La segunda figura es bastante parecida a la figura 1.7.

# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------

# Practica 02 - Ejercicio 10: Muestreando de una poblacion finita

# Se considera el experimento de lanzar 3 veces un dado equilibrado y se definen 
# las v.a X: "suma de los 3 dados" e Y que vale 1 si los dados coinciden y 0 si 
# no.

# -----------------------------------------------------------------------------

# 1. Simular el lanzamiento de 3 dados una vez. ¿Cuanto valen X e Y en la 
# simulacion? ¿Cuales son las probabilidades respectivas de estos resultados?

# Semilla para repetir la simulacion con mismos resultados
set.seed(1000)

# Simulamos la tirada de dados, x son los posibles valores, con reposicion
# porque una vez que sale un numero tiene que poder seguir saliendo
# y queremos 3 tiradas por eso size=3
dados = sample(x=1:6, size=3, replace = TRUE); dados

# Calculo de x que es la sumatoria de los valores que salieron en los 3 dados,
# se suman los valores del vector
x=sum(dados); x

# Comparacion logica para ver si coincideron en caso de si va a dar TRUE(1)
y=(dados[1] == dados[2]) & (dados[2] == dados[3]); y

# Calculo de probabilidades exactas:

# P(X=x): para calcular esta probabilidad exacta utilizamos un enfoque
# donde para cada posible valor de la suma de los 3 dados generamos
# todas las tiradas posibles, contamos cuantas veces da ese valor
# y por ultimo dividimos la cantidad de veces que dio ese valor por 
# la cantidad de combinaciones posibles teniendo en cuenta todos los dados

# Valores que puede tomar la suma
suma = 1:18

# Vector donde se guardaran las probabilidades exactas
probabilidades = numeric(18)


for (i in suma){ # Por cada posible valor que pueda dar la suma
  c=0 # Inicializamos el contador de veces que dio la suma correspondiente a i
  for (j in 1:6){ # Generamos todas las combinaciones para ver si coincide con
    for (k in 1:6){ # la suma
      for (l in 1:6){
        tirada=c(j,k,l) # Representa cada combinacion posible
        if (sum(tirada)==i) c = c+1 # Si la suma de la tirada es igual al valor
      }                           # para el que queremos calcular la probabilidad
    }                             # el contador suma
  }
  prob = c/6**3 # Luego las veces que se dio esa suma / la cantidad de combinaciones posibles de los dados es la probabilidad
  probabilidades[i]=prob # Guardamos el valor en el indice correspondiente a dicho valor de suma
}

# Por ultimo mostramos todos los valores posibles y sus probabilidades exactas
cbind(suma, probabilidades)

# P(Y=y): solamente en 6 casos de 6**3 pueden coincidir los 3 dados e Y ser 1
# por lo tanto la probabilidad exacta es 6/6**3 y para el caso P(Y=0) entonces
# 1-6/6**3

# Rta final: Abajo se muestran los resultados
print(paste('Tirada de dados:', toString(dados)))
sprintf('P(X=%d)=%4f', x, probabilidades[x])
sprintf('P(Y=%d)=%4f', y, ifelse(y==1, 6/6**3, 1-6/6**3))

# -----------------------------------------------------------------------------

# 2. Simular el lanzamiento de los 3 dados 10000 veces y aproximar las 
# distribuciones de X e Y con los valores simulados. Comparar numerica y 
# graficamente con los resultados exactos.

# Semilla para poder reproducir los mismos resultados
set.seed(1000)
# Cantidad de simulaciones
s=10000
# Lanzamientos de dados
dado_1 = sample(x=1:6, size=s, replace = TRUE); dado_1
dado_2 = sample(x=1:6, size=s, replace = TRUE); dado_2
dado_3 = sample(x=1:6, size=s, replace = TRUE); dado_3

# Calculo de X para cada simulacion
x= dado_1+dado_2+dado_3; x
# Calculo de Y para cada simulacion
y=(dado_1 == dado_2) & (dado_2 == dado_3); y


# Aproximacion de Y: 
y_1 = mean(y); y_1 # P(Y=1) es la sumatoria de 1s divido s (media)
y_0 = 1-y_1 # P(Y=0) le restamos a 1 P(Y=1)

# Comparacion numerica de exactas y aproximadas:
resultados_y = cbind(0:1, round(c(1-6/6**3, 6/6**3), 5), round(c(y_0, y_1),5))
colnames(resultados_y) = c("y", "prob exacta", "prob aproximada")
resultados_y

# Las distribuciones por simulacion y exacta son similares para Y.

# Aproximacion de X:
# Calculamos P(X=x) sacando la frecuencia relativa para cada valor
# en la simulacion
proporciones_aprox = prop.table(table(x))
proporciones_aprox <- as.numeric(proporciones_aprox)

# Comparacion numerica de exactas y aproximadas (las exactas ya las calculamos antes):
resultados_x = cbind(suma, round(probabilidades, 5), round(c(0,0,proporciones_aprox),5))
colnames(resultados_x) = c("x", "prob exacta", "prob aproximada")
resultados_x

# Comparacion en Histograma:
cortes = (1:18) + .5
hist(x, cortes,prob = T, ylab = "P(X=x)", xlab="x: suma", col = "skyblue", main = "", label = T)
axis(side = 1, at = 1:18, labels = 1:18)
points(1:18, probabilidades, pch = 19, col = "blue")

# Nuevamente tanto por tabla como graficamente, la distribuciones exactas
# y aproximadas para X son similares. Si aumentamos s a 600000 en X 
# se parecen mucho mas aun las distribuciones exactas y aproximadas.

# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------

# Practica 03 - Ejercicio 06: Otro mal generador y las consecuencias que trae

# Se considera m = 50000 valores ui=(r + 0·5)/d provenientes del generador con 
# a=1093; b=252; d=86436 y s=6. Intentaremos utilizar este generador para simular 
# lanzamientos de una moneda equilibrada.

# ------------------------------------------------------------------------------

# a) Para un n ≤ m particular, se puede usar el codigo sum(u[1:n] < 0.5)/n para 
# simular la proporcion de caras en los primeros n lanzamientos. Si los valores 
# ui son uniformes en el intervalo (0,1), entonces cada una de las n comparaciones 
# dentro del parentesis tiene una probabilidad de 1/2 de ser TRUE y, por lo tanto, 
# de contribuir con 1 a la suma. Evaluar esto para n = 10000, 20000, 30000, 40000 
# y 50000. Para cada n, el margen de error del 95 % es aproximadamente n**(1/2). 
# Mostrar que todos los valores estan dentro de este margen del valor verdadero 
# P{cara}=0.5. Por lo tanto, uno podria verse tentado a concluir que el generador 
# esta funcionando correctamente. Pero observar que todas estas proporciones
# estan por encima de 0.5, y por cantidades similares. Es esto una coincidencia 
# aleatoria o un patron? (ver parte c)).

# Parametros del generador
a=1093; b=252; d=86436; s=6
m = 50000
# Vector con resultados
r = numeric(m); r[1] = s

for (i in 1:(m-1)) r[i+1] = (a * r[i] + b) %% d # Generador

u = (r + 1/2) / d # A intervalo 0,1

length(unique(u)) # Periodo de 343

test_gen = function(n) {
  
  prop = sum(u[1:n] < 0.5)/n # De los primeros n valores tomamos los 
  # valores menores a 0.5 como cara, sumamos la 
  # cantidad y calculamos la proporcion
  print(sprintf("Proporcion de caras en la muestra: %f", prop))
  # Construccion de intervalos de confianza
  error = n**(-1/2) # Lo da la consigna, error con 95% de confianza
  # Limites
  lim_inf = prop-error
  lim_sup = prop+error
  # Comprobacion
  contiene_pi = (1/2 >= lim_inf) && (1/2 <= lim_sup)
  
  if (contiene_pi) { 
    print(sprintf("El valor de pi=0.5 ESTA contenido en el intervalo de confianza al 95/100 para n=%i", n))
  } else {
    print(sprintf("El valor de pi=0.5 NO ESTÁ contenido en el intervalo de confianza al 95/100 para n=%i", n))
  }
  
}

# Rta: aparentemente todo los intervalos construidos sobre la proporcion 
# estimada contienen el verdadero valor de la proporcion poblacional.

# Probamos el generador con los distintos valores de n
for (i in 1:5)
  test_gen(n=(i*10000))


# ------------------------------------------------------------------------------

# b) Este generador tiene serios problemas. Primero, ¿cuantos valores distintos 
# hay entre los m? Utilizar length(unique(r)). Entonces, este generador repite 
# unos pocos valores muchas veces en m = 50000 iteraciones. 
# En segundo lugar, el periodo depende en gran medida de la semilla s. 
# Reportar los resultados para  s = 2,8 y 17.

length(unique(r))

# Rta: El generador tiene un periodo de 343 valores distintos en m=50000

# ------------------ s=2 --------------------

a=1093; b=252; d=86436; s=2
m = 50000; r = numeric(m); r[1] = s
for (i in 1:(m-1)) r[i+1] = (a * r[i] + b) %% d
u = (r + 1/2) / d 
length(unique(u)) 

# Rta: El generador tiene un periodo de 1029 numeros distintos con s=2

# ------------------ s=8 --------------------

a=1093; b=252; d=86436; s=8
m = 50000; r = numeric(m); r[1] = s
for (i in 1:(m-1)) r[i+1] = (a * r[i] + b) %% d
u = (r + 1/2) / d 
length(unique(u)) 

# Rta: El generador tiene un periodo de 1029 numeros distintos con s=8

# ------------------ s=17 --------------------

a=1093; b=252; d=86436; s=17
m = 50000; r = numeric(m); r[1] = s
for (i in 1:(m-1)) r[i+1] = (a * r[i] + b) %% d
u = (r + 1/2) / d 
length(unique(u)) 

# Rta: El generador tiene un periodo de 147 numeros distintos con s=17

# ------------------------------------------------------------------------------

# c) Basado en este generador, el siguiente codigo crea un grafico de la proporcion 
# de caras en n lanzamientos para todos los valores n = 1, 2, ... , m. A modo de 
# comparacion, se hace lo mismo para los valores generados por runif (que luego 
# utiliza sample), que se sabe que simula de Unif(0,1) en forma precisa. Explicar 
# el codigo, ejecutar el programa y comentar los resultados. 
# En particular, ¿que te parece que sucederia hacia el borde derecho del grafico 
# si hubiera millones de iteraciones m? (aprenderemos mas sobre este tipo de graficos 
# en el tema siguiente).


a = 1093; b = 252; d = 86436; s = 6 # Parametros del generador
m = 50000 # Cantidad de lanzamientos
n = 1:m # Vector que representa el indice de cada lanzamiento
r = numeric(m) # Vector para guardar los resultados de los lanzamientos
r[1] = s # La semilla es el primer numero generado

for (i in 1:(m-1)) {r[i+1] = (a*r[i] + b) %% d} # Generacion de los m-1 numeros restantes

u = (r + 1/2)/d # Transformacion a intervalo 0,1

f = cumsum(u < .5)/n

# u < .5 vector logico que transforma los valores menores 0.5 en TRUE (cara)
# cumsum(u < .5) suma acumulada que representa el numero de caras acumuladas
# por indice del vector, seria el numero de caras que se obtuvo
# en la tirada del indice i

# cumsum(u < .5)/n proporcion (frec relativa) de caras en cada tirada (frec relativa
# de caras en el indice i)

plot(n, f, type="l", ylim = c(.49, .51), col = "red")
# grafico de puntos: eje x cantidad de tiradas, eje y proporcion de caras
abline(h = .5, col = "green") # linea horizontal en y=0.5 (probabilidad verdadera de que toque cara)

g = cumsum(sample(0:1, m, repl=T))/n
# En g pasa lo mismo que mas arriba pero con sample, se generan m tiradas de dados
# con reposicion, en el indice que indica cuantas tiradas se hicieron se suma la 
# cantidad de caras que salieron y luego por indice se genera la proporcion de caras

lines(n, g) # Se agrega la linea al grafico para cada indice

# Rta: al borde derecho del grafico si hubiera millones de iteraciones la grafica
# en el eje y va oscilar cada vez en un rango menor en torno a la proporcion de
# caras en el indice donde termina el periodo. Esto va a pasar porque a partir
# de ese punto los numeros empiezan a repetirse, la cantidad de caras son las mismas
# y en los indices que son multiplos del periodo la proporcion se repite.
# En la grafica con sample(runif) primero se puede notar que la linea no siempre
# es la misma (no esta seteada la semilla) pero ademas como el periodo es mucho
# mas largo la cantidad de caras no se empiezan a repetir en intervalos de "n"
# tiradas entonces a simple vista no hay un valor tan claro en y sobre el cual
# la grafique oscile tan perfectamente.

# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------

# Practica 03 - Ejercicio 7: Evaluacion del generador implementado en runif
# 
# En R, la sentencia runif(m) crea un vector de m observaciones simuladas de la 
# distribucion Unif(0,1). Observar que no se requiere ningun bucle explicito para 
# generar este vector.

# m = 10^6; u = runif(m)
# # genera 1 millon iid Unif(0,1)

# u1 = u[1:(m-2)]; u2 = u[2:(m-1)]; u3 = u[3:m] # 3 dimensiones
# par(mfrow = c(1, 2), pty = "s")
# plot(u1, u2, pch = ".", xlim = c(0, .1), ylim = c(0, .1))
# plot(u1[u3 < .01], u2[u3 < .01], pch = ".", xlim = c(0, 1), ylim = c(0, 1))
# par(mfrow = c(1, 1), pty = "m")

# ------------------------------------------------------------------------------

# a) Ejecutar el programa y comentar los resultados. Aproximadamente cuantos puntos 
# aparecen en cada grafico?

# Creacion de 1millon de numeros aleatorios en unif(0,1)
m = 10^6
u = runif(m)

# Grafico en 3 dimensiones
u1 = u[1:(m-2)]; u2 = u[2:(m-1)]; u3 = u[3:m]
par(mfrow = c(1, 2), pty = "s")
plot(u1, u2, pch = ".", xlim = c(0, .1), ylim = c(0, .1))
plot(u1[u3 < .01], u2[u3 < .01], pch = ".", xlim = c(0, 1), ylim = c(0, 1))
par(mfrow = c(1, 1), pty = "m")

# Rta: a simple vista no se detectan patrones en los graficos por lo que podemos
# hablar de independencia de a pares (cosa que ya sabiamos de la teoria).
# ¿Cuantos puntos aparecen en cada grafico? Si estamos hablando de que runif
# es uniforme en el intervalo (0,1) a intervalos de igual longitud dentro de (0,1)
# mismas proporciones. Lo graficos van de 0 a 0.1 la probabilidad en la distribucion
# uniforme es del ancho del intervalo por lo que estamos hablando de (10**6)*0.1,
# es decir (10**5), 100000 puntos.

# ------------------------------------------------------------------------------

# b) Realizar pruebas de bondad de ajuste chi-cuadrado, como en el Ejercicio 4, 
# basandose en el millon de observaciones uniformes simuladas.

# Calculo del estadistico chi-cuadrado
h = 10 # Cantidad de intervalos
E = m/h # Valor esperado por intervalo, tengo 60 numeros, 10 intervalos, espero 60/10 en cada intervalo
cut = (0:h)/h # Intervalos
N = hist(u, breaks=cut, plot=F)$counts # Agrupo por intervalos y saco el conteo por intervalo
Q = sum((N - E)^2/E) # Estadistico Q

( qchisq(.025,df=h-1) < Q ) && ( Q < qchisq(.975,df=h-1) )

# Rta: segun las pruebas de ajuste chi-cuadrado, el millon de observaciones
# simuladas corresponden a una distribucion uniforme (0,1)

# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------

# Practica 03 - Ejercicio 10: El metodo de la distribucion inversa: otro ejemplo 
# en la familia Beta
# 
# En el ejemplo visto en clase, utilizamos la funcion runif para simular de la 
# distribucion Beta(1/2, 1). En este ejercicio nos interesa simular de la 
# distribucion Beta(2, 1).

# -----------------------------------------------------------------------------

# b) Implementar el metodo anterior en R escribiendo un codigo que simule 100000 
# observaciones independientes de una v.a Beta(2, 1) y comparar el histograma 
# respectivo con la fdp de la distribucion objetivo.

# Valor del parametro Beta(α=2, 1)
alfa = 2

# Cantidad de valores generados
n = 100000

# Paso 1, generar valores en Unif (0, 1)
u = runif(n)

# Paso 2, pasar los valores uniformes generados antes por 
# x = Fx**(-1)(u) = u**(1/α=2)
x = u**(1/alfa) # Los valores en x son las simulaciones en la distribucion deseada

# Grafico
hist(x, prob = T, ylab = "Densidad", col = "darkgreen",
     main = "Histograma de valores \n simulados de Beta(2,1)")
# fdp
fX = function(u) 2*u
curve(fX(x), lwd = 2, col = 'red', add = T)

# Rta: como se ve puede observar en el grafico la funcion de densidad
# se corresponde con el histograma de los valores generados, lo que indica
# que los valores generados tienen distribucion Beta (2, 1).

# -----------------------------------------------------------------------------