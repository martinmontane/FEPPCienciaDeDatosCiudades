#### Objetivo ####
# Este código de R tiene como objetivo repasar algunos de los conteidos básicos necesarios
# para el curso. Una introducción mucho más detallada se puede encontrar en 
# https://martinmontane.github.io/CienciaDeDatosBook/ capitulos 1, 2 y 3

#### Carga de librerias/paquetes ####
# R tiene un montón de funciones que fueron añadidas al conjunto con el que ya viene inicialmente.
# Acá vamos a cargar tidyverse, que es muy particular porque, a su vez, tiene un conjunto de paquetes
# adicionales. Nos va a servir para hacer muchas cosas, desde cargar datos hasta graficarlos
# Antes de poder cargarlo, hay que tenerlo instalado. Los paquetes/librerias se instalan
# solo una vez con install.packages(). Por ejemplo,install.packages("tidyverse")
library(tidyverse)

#### Lectura de datos ####

# En R tenemos que cargar los datos, van a aparecer en la pestaña de "environment" en el panel de arriba a la derecha.
# Vamos a leer ¡un millón de datos! que vienen de Properati y son una muestra de los más de 3 millones de publicaciones
# de inmuebles que tienen para Argentina. Vamos a trabajar con estos datos en otras clases, asi que tengamos una introducción
# con estos.
# El archivo csv está en un archivo .zip en la siguiente dirección. Tienen que descargarlo y descomprimirlo en la carpeta
# del proyecto de R donde estén trabajando
datosArgentina <- read_delim("datosArgentina.csv",delim = ";")

# read_delim() es una función de R que importa archivos separados por algún caracter especial, como en este caso el ";".

#### Exploración de los datos ####

# En esta parte solemos ver qué hay en los datos... veamos algunas funciones útiles
glimpse(datosArgentina)
# Arriba nos dice que tenemos 1.015.783 filas y 16 columnas (variables) en el objeto datosArgentina
# Podemos ver que en "property type" tenemos varias cosas... me gustaría trabajar solo inmuebles que no tienen
# fines comerciales. Veamos qué hay
datosArgentina %>% 
  pull(property.type) %>% 
  unique()
# pull() elige el vector (columna) por el nombre, mientras que unique nos dice los valores únicos que toma.
# %>% se llama pype y lo que hace es establecer un orden entre las operaciones. Primero tomamos
# "datosArgentina" LUEGO seleccionamos la variable property.type y LUEGO nos quedamos con los valores únicos
# Fijense que nos mostró los valores abajo, en la pestaña de "console". Eso nos sirve para ver los valores que toma,
# pero no podemos hacer más operaciones en R porque no creamos un objeto con eso ! para crear un objeto
# debemos asignar usando "<-"
tipoPropiedad <- datosArgentina %>% 
  pull(property.type) %>% 
  unique()
# Ahora no apareció en la consola, sino que creamos un vector que se llama "tipoPropiedad" arriba a la derecha

# Ahora veamos los datos faltantes. En R se mustran como NA, son un valor reservado y nos indican justamente eso.
# Para saber cuantos faltos faltantes hay en una variable podemos hacer lo siguiente:
datosArgentina %>%
  pull(property.price) %>%
  is.na() %>%
  sum()
# Tenemos 30.935 datos faltantes en el precio de la propiedad... más adelante vamos a eliminarlos

#### Procesamiento de los datos ####
# En general, procesar los datos suele ocupar una parte muy importante de nuestro tiempo... veamos algunas
# de las cosas que podemos hacer

# La columna "created_on" nos dice cuándo fue publicado el inmueble. La clase del vector (de la columna)
# es Date, es decir es una fecha. 
datosArgentina %>% 
  pull(created_on) %>% 
  class()

# Vamos a hacer resumenes por años ¿Cómo podemos crear una variable 
# que tenga años? Hay varias formas, pero podemos quedarnos con los primeros cuatro digitos !
# mutate() nos sirve para modificar variables, mientras que substr() nos permite tomar una parte del texto
datosArgentina <- datosArgentina %>% mutate(year=substr(created_on,1,4))

# Veamos una función más: table(), hace una tabla de frecuencias, en este caso la cantidad de anuncios
# por cada uno de los años
table(datosArgentina %>% pull(year))

# Ahora generemos el precio promedio por metro cuadrado. También tenemos que usar mutate(). Usamos
# ifelse() que es muy importante. Lo que hace es preguntar algo en la primera parte. Si se cumple, hace algo.
# si no se cumple, hace otra cosa. En esta caso le preguntamos si la superficie total es mayor 0,
# porque como queremos dividir por esa superficie, si lo hacemos estaríamos dividiendo por cero y es imposible

datosArgentina <- datosArgentina %>% 
  mutate(precioM2 = ifelse(property.surface_total>0, property.price/property.surface_total,NA))

# Saquemos el precio promedio por año y región del país. Pero solo de aquellos valores que
# tengan un valor en precioM2 y se haya anunciado en USD y sea una venta.
# group_by() sirve para decirle en base a qué combinaciones de variables queremos que haga el calculo
# y summarise() resume a cada una de estas combianciones, en este caso queremos tener el promedio (usamos mean())
# por precioM2
resumenYearRegion <- datosArgentina %>%
  filter(!is.na(precioM2) & precioM2 > 0 & property.currency == "USD" & property.operation =="Venta") %>%
  group_by(year,place.l2) %>%
  summarise(precioPromedio=mean(precioM2))

glimpse(resumenYearRegion)

# Hagamos un gráfico de precios en 2020

ggplot(resumenYearRegion %>% filter(year == 2020), aes(y = place.l2,x=precioPromedio)) + 
  geom_col() +
  theme_minimal()
# Parece haber datos raros... Cortemos por lo sano y eliminemos los valors que ya sabemos que son extraños
# inmuebles por abajo de 500 usd el metro cuadrado e imuebles por arriba de los 10.000 usd el metro cuadrado
resumenYearRegion <- datosArgentina %>%
  filter(!is.na(precioM2) & precioM2 > 500 & precioM2 < 10000 & property.currency == "USD" & property.operation =="Venta") %>%
  group_by(year,place.l2) %>%
  summarise(precioPromedio=mean(precioM2))


ggplot(resumenYearRegion %>% filter(year == 2020), aes(y = place.l2,x=precioPromedio)) + 
  geom_col() +
  theme_minimal()

# Ahora pareciera tener mucho más sentido, a pesar de que hay algunos outliers probablemente en corrientes y tierra del fuego

# Nos quedó en formato largo, podemos a los años en las columnas para ver todo un poco más fácil y exportarlo, no?
# La función clave es pivot_wider()
datosParaExportar <- pivot_wider(resumenYearRegion,names_from = "year",values_from = "precioPromedio")

# Exportamos los datos
write_delim(x = datosParaExportar,path="tablaPrecioPromedio.csv",delim =";")

#### Análisis ####

# Estamos en condiciones de hacernos una pregunta relevante ¿aumentó la dólarización de los anuncios de los inmuebles?
# ¿Para venta y para alquiler? Veamos
tablaResumenProporcion <- datosArgentina %>% 
  filter(property.operation %in% c("Alquiler","Venta")) %>% 
  group_by(year,property.operation) %>%
  summarise(propDolares=sum(property.currency %in% "USD")/n())
glimpse(tablaResumenProporcion)
# Atención, el dataset está agrupado (lo pueden ver en Groups: year [6]). Esto nos puede traer problemas para
# algunos gráficos. Recomiendo desagruparlos
tablaResumenProporcion <- tablaResumenProporcion %>% ungroup()

ggplot(tablaResumenProporcion) +
  geom_line(aes(x=year,y=propDolares,group=property.operation,color=property.operation)) +
  theme_minimal()

# A veces conviene solo mostrar tablas...
pivot_wider(tablaResumenProporcion,names_from = "year",values_from = "propDolares")
# Vemos un fuerte aumento en la proporción de anuncios en dólares para la venta, pero no tanto para los
# alquileres, que salvo un valor alto en 2015, se mantienen en torno al 6%-8% entre 2015 y 2020