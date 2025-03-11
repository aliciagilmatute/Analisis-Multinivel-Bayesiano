# Una Aproximación Bayesiana al Análisis Multinivel

El análisis multinivel es un procedimiento estadístico que permite dar cuenta de la idiosincrasia y, por ende, de la variabilidad no solo de los individuos, sino de sus agrupaciones: instituciones, centros educativos, territorios, etc. **Todo depende de la articulación teórica desde la cual se parta**. Por ejemplo, se puede establecer como objeto de estudio a un grupo de personas como seres individuales y estudiar sus diferencias a la vez que estudiamos esas mismas diferencias entre los grupos en los que esas personas dicen agruparse. O por otro lado, entender a cada individuo como un grupo y considerar ciertas características de su persona como objeto de estudio. 

Esto da cuenta de la flexibilidad y utilidad de los análisis multinivel, característica muy apreciada por el enfoque bayesiano dado que: “la propia lógica de los métodos bayesianos se adapta de forma natural a los métodos de efectos aleatorios, en los que los paramétros pasan a ser considerados variables aleatorias.” (Revuelta & Ximénez, 2014). 

Por ello, este trabajo se compromete a explorar las posibilidades que ofrece el análisis multinivel bayesiano con datos recogidos en 10 centros educativos sobre el rendimiento de los estudiantes en matemáticas, centrando nuestro interés en la variabilidad existente entre centros, pero también en cada centro.

## MÉTODO

Los datos se recogieron en 10 centros distintos (“schid”) cuya muestra total es de 260 estudiantes y las variables medidas son:

- El nivel de matemáticas de cada estudiante operativizada en una escala de 0-100 que refleja el rendimiento. Ejerce como variable dependiente. (“math”)
- El número de horas diarias que dedica cada estudiante a los deberes de matemáticas. Ejerce como variable predictora de primer nivel. (“homework”)

Puesto que nuestro interés reside en la exploración de la flexibilidad de los modelos multinivel se han estimado 3 modelos que dan cuenta de las posibilidades que ofrecen estos procedimientos:

- **Modelo 0**: no se tiene en cuenta ninguna variable predictora. Las intersecciones se consideran aleatorias entre centros. Por lo tanto, se estimarán 3 parámetros: la parte fija de la intersección ($\gamma_{00}$), la parte aleatoria de la intersección ($\sigma_{U0}^2$) y la varianza error ($\sigma_{e}^2$).

  Este modelo estaría proponiendo que las medias en el desempeño en matemáticas difieren entre centros, sin tener en cuenta ninguna variable predictora. Es lo único que quiere comprobar, por eso resulta el modelo nulo porque es el modelo con peor capacidad explicativa.

- **Modelo 1**: se introduce una variable predictora de primer nivel (“homework”). Se consideran las intersecciones aleatorias entre los centros y las pendientes fijas. Por lo tanto, pasaremos a estimar 4 parámetros: la parte fija de la intersección ($\gamma_{00}$), la parte aleatoria de la intersección ($\sigma_{U0}^2$), la parte fija de la pendiente ($\gamma_{10}$) y la varianza error ($\sigma_{e}^2$).

  Este modelo estaría proponiendo que las medias en el desempeño en matemáticas difieren entre centros teniendo en cuenta la variable predictora “homework”. Pero asumiría que el comportamiento de esta variable es la misma en todos los colegios, es decir, que ejerce su influencia en el desempeño en matemáticas de los alumnos de la misma manera independientemente del colegio.

- **Modelo 2**: igual al modelo 1 pero considerando las pendientes aleatorias entre los centros. Por lo tanto, pasaremos a estimar 6 parámetros: la parte fija de la intersección ($\gamma_{00}$), la parte aleatoria de la intersección ($\sigma_{U0}^2$), la parte fija de la pendiente ($\gamma_{10}$), la parte aleatoria de la pendiente ($\sigma_{U1}^2$), la covarianza entre la parte aleatoria de la intersección y de la pendiente ($\sigma_{U0,U1}$) y la varianza error ($\sigma_{e}^2$).

  Este modelo estaría proponiendo que las medias en el desempeño en matemáticas difieren entre centros teniendo en cuenta la variable predictora “homework”. Y asumiría que el comportamiento de esta variable NO es la misma en todos los colegios, es decir, que ejerce su influencia en el desempeño en matemáticas de los alumnos de manera distinta en función del colegio.

