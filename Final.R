# Importar paquetes -------------------------------------------------------

library(data.table)
library(dplyr)
library(ech) # descargar y transformar datos ECH 
library(weights) # t test comparando medias de series ponderadas
library(huxtable) # imprimir tabla de medias en la consola
library(magrittr)
library(gridExtra)

Sys.setenv(TZ='GMT')

# Descargar y transformar datos --------------------------------------------

#ech19 <- data.table::fread("ech19.csv") # el comando siguiente descarga los datos de la pagina del INE, yo ya los tengo descargados y los abro desde mi computadora con los valores calculados por la funcion years_of_schooling del paquete ech
ech19 <- get_microdata(year = "2019", # eliminar los numerales a las izquierda de estas 3 lineas para poder usar esta funcion
                       folder = tempdir(),
                       toR = TRUE)
ech19 <- years_of_schooling(ech19)
ech19 <- enrolled_school(ech19)
ech19 <- unsatisfied_basic_needs(ech19)

setDT(ech19)                          # transforma la tabla descargada a formato data.table, usado de aqui en adelante

# datos de canastas alimentarias y no alimentarias para 2019 (los ingresos de la encuesta son los del mes anterior a la entrevista, asi que los meses van de dic-18 a nov-19)
lineaspobech <- cbind.data.frame(ech::cba_cbna_mdeo[1:3], ech::cba_cbna_int[,2:3], ech::cba_cbna_rur[,2:3])[216:227, ]

#transformaciones a los datos, creando las variables necesarias para el analisis
datos2 <- ech19[, .(numero, nper, mes, region_4, pesoano, pesotri, pesomen, e27, e30, ht11, ysvl, ht19, subempleo, f82, f269, f270, f272, f273, d10, d25, c2, c4, d19, d15, d16, e51_2, e51_3, e51_4, e51_5, e51_6, e51_7, e51_8, e51_9, e51_10, e51_11, e193, e197, e201, years_schooling, d260, d21_3, d21_1, d21_2, d21_16, pobre06, UBN, UBN_q)]
datos2[, `:=`(ingvl=ht11, hacinamiento=d25/d10>2, sinempleo=(!(f269==1|f270==1|f272==1|f273==1)&e27>17), informalidad=((f269==1|f270==1|f272==1|f273==1)&f82==2&e27>17), subempleo=(subempleo==1&e27>17), 
              precariedad=(c2==6|c4==5), sincocina=d19==3, sinbano=(d15==2|d16==3|d16==4),
              anoseducacion=e51_2+e51_3+e51_4+e51_5+e51_6+e51_7+e51_8+e51_9+e51_10+e51_11, noasistencia4_17=(e193==3|e197==3|e201==3)&e27>=4&e27<=17,
              nocalefaccion=d260==6, sinheladera=d21_3==2, nocalefaccionagua=(d21_1==2&d21_2==2), nointernet=d21_16==2, region=ifelse(region_4==2|region_4==3, "inturb", ifelse(region_4==1, "montevideo", "rural")))]
datos2[, `:=`(ingpcvl=ingvl/ht19, lineapob=ifelse(region=="montevideo", lineaspobech[mes, 2]*ht19+lineaspobech[mes, 3]*ht19^.8, ifelse(region=="inturb", lineaspobech[mes, 4]*ht19+lineaspobech[mes, 5]*ht19^.8, lineaspobech[mes, 6]*ht19+lineaspobech[mes, 7]*ht19^.8)))][, `:=`(pobres=ingvl<lineapob, diflp=(ingvl*100/lineapob))]
datos2[, segmento:=ifelse(ingvl>lineapob*.9&ingvl<lineapob, "90100", ifelse(ingvl>lineapob&ingvl<lineapob*1.05, "100105", ifelse(ingvl>lineapob*1.05&ingvl<lineapob*1.1, 105110, ifelse(ingvl>lineapob*1.1&ingvl<lineapob*1.15, "110115", ifelse(ingvl>lineapob*1.15&ingvl<lineapob*1.2, "115120", ifelse(ingvl>lineapob*1.2&ingvl<lineapob*1.25, "120125", ifelse(ingvl<lineapob*.9, "90menos", "125mas")))))))]
datos3 <- datos2[, !c("ht11", "ysvl", "region_4", "e51_2","e51_3","e51_4","e51_5","e51_6","e51_7","e51_8","e51_9","e51_10","e51_11","e193","e197","e201")]

totalpop2019 <-  datos3[, sum(pesoano)] # total de la variable pesoano, equivalente a la poblacion del pais

rm(ech19, datos2) # eliminar esas dos tablas, todos los datos a usar se encuentran en datos3

# Tabla de medias de variables analizadas ------------------------------------------------------------

calcmeans <-  function() { # incluyo la variable años de educacion dos veces, primero calculada sesncillamente como la suma de las variables relevantes que figuran en la ECH, y mas abajo 
  vars <- c("sinempleo", "informalidad", "subempleo", "precariedad", "hacinamiento", "sincocina", "sinbano", "noasistencia4_17", "anoseducacion", "nocalefaccion", "sinheladera", "nocalefaccionagua", "nointernet", "years_schooling")
  mat <- matrix(nrow=14, ncol=3)
  colnames(mat) <- c("Pobres INE", "Pobres ISCS", "No pobres")
  rownames(mat) <- c("Sin empleo", "Informal", "Subempleo", "Precariedad", "En hacinamiento", "Sin cocina", "Sin baño", "Inasistencia escolar", "Años educación formal", "Sin calefacción", "Sin heladera", "Sin calentador agua", "Sin acceso internet", "años de educacion (ech)")
  for (var in vars) {
    if (var %in% c("subempleo", "sinempleo", "informalidad")) mat[which(vars==var), 1] <- datos3[pobre06==1&e27>17, sum(get(var)*pesoano)/sum(pesoano)]
    else if (var=="noasistencia4_17") mat[which(vars==var), 1] <- datos3[pobre06==1, sum(get(var)*pesoano)/sum(pesoano)]
    else mat[which(vars==var), 1] <- datos3[pobre06==1, sum(get(var)*pesoano)/sum(pesoano)]
  }
  for (var in vars) {
    if (var %in% c("subempleo", "sinempleo", "informalidad")) mat[which(vars==var), 2] <- datos3[segmento%in%c("100105", "105110", "110115", "115120")&e27>17, sum(get(var)*pesoano)/sum(pesoano)]
    else if (var=="noasistencia4_17") mat[which(vars==var), 2] <- datos3[segmento%in%c("100105", "105110", "110115", "115120"), sum(get(var)*pesoano)/sum(pesoano)]
    else mat[which(vars==var), 2] <- datos3[segmento%in%c("100105", "105110", "110115", "115120"), sum(get(var)*pesoano)/sum(pesoano)]
  }
  for (var in vars) {
    if (var %in% c("subempleo", "sinempleo", "informalidad")) mat[which(vars==var), 3] <- datos3[segmento%in%c("120125", "125mas")&e27>17, sum(get(var)*pesoano)/sum(pesoano)]
    else if (var=="noasistencia4_17") mat[which(vars==var), 3] <- datos3[segmento%in%c("120125", "125mas"), sum(get(var)*pesoano)/sum(pesoano)]
    else mat[which(vars==var), 3] <- datos3[segmento%in%c("120125", "125mas"), sum(get(var)*pesoano)/sum(pesoano)]
  }
  mat[c(9,14), ] <- mat[c(9,14), ]/100 # Años de educacción es la única variable no expresada en %, así que la multiplico por 100 para que se cancele con la siguiente operación
  mat <- round(mat*100, digits=1)
  return(mat)
}
meansTable <- calcmeans()
hux(meansTable) %>% add_colnames() %>% set_bold(row = 1, col = everywhere, value = TRUE) %>% set_all_borders(TRUE) %>% print()

# Sensibilidad de tasa de pobreza a parametro θ -----------------------------

calclineaspob <-  function(scale) { # calcula lineas de pobreza para distintos valores del parametro que representa economias de escala en el gasto no alimentario de los hogares
  data <- datos3[, .(ingvl, ht19, mes, region, pesoano)]
  data[, lineapob:=ifelse(region=="montevideo", lineaspobech[mes, 2]*ht19+lineaspobech[mes, 3]*ht19^scale, ifelse(region=="inturb", lineaspobech[mes, 4]*ht19+lineaspobech[mes, 5]*ht19^scale, lineaspobech[mes, 6]*ht19+lineaspobech[mes, 7]*ht19^scale))][, sum((ingvl<lineapob)*pesoano)/totalpop2019]
} 
thetas <- seq(.7, .9, by=0.01)
tasasDePobreza <- purrr::map_dbl(thetas, function(x) calclineaspob(x))
plot(thetas, tasasDePobreza, main="Tasa de pobreza en 2019 según valor de θ", pch=21, bg="blue", xlab="θ", ylab="tasa de pobreza")
      legend("topleft", expression(frac(sum((w[i]*symbol(I)("Y"[i]<"CBA"[tr]*"n"[i]+CBNA[tr]*n[i]^theta)), i=1, N), sum(w[i], i=1, N))), bty='n', cex=1.5, y.intersp=-3)

# Encontrar y contar diferencias no significativas por segmento ----------------------------------------------

diferenciasSignif <- function() { # usar parametros bootse=T, bootp=T para usar bootstrapping (aunque ignora estratificacion)
  segs <- c("100105", "105110", "110115", "115120", "120125")
  vars <- c("sinempleo", "informalidad", "subempleo", "precariedad", "hacinamiento", "sincocina", "sinbano", "noasistencia4_17", "anoseducacion", "nocalefaccion", "sinheladera", "nocalefaccionagua", "nointernet")
  mat <-  matrix(nrow=length(vars), ncol=length(segs))
  colnames(mat) <- segs
  rownames(mat) <- c("Sin empleo", "Informal", "Subempleo", "Precariedad", "En hacinamiento", "Sin cocina", "Sin baño", "Inasistencia escolar", "Años educación formal", "Sin calefacción", "Sin heladera", "Sin calentador agua", "Sin acceso internet")
  for (var in vars){
    for (seg in segs){
      if (var %in% c("subempleo", "sinempleo", "informalidad")) mat[which(vars==var), which(segs==seg)] <- wtd.t.test(datos3[segmento=="90100"&e27>17, get(var)], datos3[eval(substitute(segmento==seg&e27>17)), get(var)], datos3[segmento=="90100"&e27>17, pesoano], datos3[eval(substitute(segmento==seg&e27>17)), pesoano])$coefficients[1]
      #else if (var=="noasistencia4_17") mat[which(vars==var), which(segs==seg)] <-  wtd.t.test(datos3[segmento=="90100"&e27>3&e27<18, get(var)], datos3[eval(substitute(segmento==seg))&e27>3&e27<18, get(var)], datos3[segmento=="90100"&e27>3&e27<18, pesoano], datos3[eval(substitute(segmento==seg))&e27>3&e27<18, pesoano])$coefficients[1] #en documento no se restringe muestra solo a ninos de 4 a 17
      else mat[which(vars==var), which(segs==seg)] <-  wtd.t.test(datos3[segmento=="90100", get(var)], datos3[eval(substitute(segmento==seg)), get(var)], datos3[segmento=="90100", pesoano], datos3[eval(substitute(segmento==seg)), pesoano])$coefficients[1]
    }
  }
  return(mat)
} # Calcular matriz de estadisticos t
mat <- diferenciasSignif() %>% round(digits=2) # los dos comandos de abajo generan tablas donde TRUE corresponde a las diferencias que NO son estadisticamente significativas, y FALSE a las que SI lo son
mat2 <- ifelse(abs(mat<1.96), T, F) # dos colas
mat3 <- ifelse(mat<1.65, T, F)      # una cola
print("diferencias no significativas en cada segmento a dos colas respecto al segmento 90-100%")
colSums(mat2)
print("diferencias no significativas en cada segmento a una cola  respecto al segmento 90-100%")
colSums(mat3)

diferenciasSignifSegmentos <- function(segmentos) { # esta funcion nos permite calcular un vector de estadisticos t eligiendo los segmentos de ingreso que queremos comparar
  segs <- paste(segmentos, collapse="|segmento==") %>% {c("(segmento==", ., ")")} %>% paste0(collapse='')
  vars <- c("sinempleo", "informalidad", "subempleo", "precariedad", "hacinamiento", "sincocina", "sinbano", "noasistencia4_17", "anoseducacion", "nocalefaccion", "sinheladera", "nocalefaccionagua", "nointernet")
  tstats <- vector(mode="double", length=length(vars))
  names(tstats) <- c("Sin empleo", "Informal", "Subempleo", "Precariedad", "En hacinamiento", "Sin cocina", "Sin baño", "Inasistencia escolar", "Años educación formal", "Sin calefacción", "Sin heladera", "Sin calentador agua", "Sin acceso internet")
  for (var in vars) {
    if (var %in% c("subempleo", "sinempleo", "informalidad")) tstats[which(vars==var)] <- wtd.t.test(datos3[segmento=="90100"&e27>17, get(var)], datos3[eval(parse(text=segs))&e27>17, get(var)], datos3[segmento=="90100"&e27>17, pesoano], datos3[eval(parse(text=segs))&e27>17, pesoano])$coefficients[1]
    else if (var=="noasistencia4_17") tstats[which(vars==var)] <- wtd.t.test(datos3[segmento=="90100"&e27>3&e27<18, get(var)], datos3[eval(parse(text=segs))&e27>3&e27<18, get(var)], datos3[segmento=="90100"&e27>3&e27<18, pesoano], datos3[eval(parse(text=segs))&e27>3&e27<18, pesoano])$coefficients[1]
    else tstats[which(vars==var)] <- wtd.t.test(datos3[segmento=="90100", get(var)], datos3[eval(parse(text=segs)), get(var)], datos3[segmento=="90100", pesoano], datos3[eval(parse(text=segs)), pesoano])$coefficients[1]
  }
  return (tstats %>% round(digits=2))
}
print("comparacion del segmento de referencia con todo el segmento ISCS (100-120%)")
print(diferenciasSignifSegmentos(c("'100105'", "'105110'", "'110115'", "'115120'")))
print(paste0(diferenciasSignifSegmentos(c("'100105'", "'105110'", "'110115'", "'115120'")) %>% {sum(abs(.)<1.96)}, " de 13 diferencias no son significativas"))


diferenciasSignifSegmentos2 <- function(segmento1, segmento2) { # esta funcion nos permite calcular un vector de estadisticos t eligiendo tanto el segmento de referencia como el que queremos contrastar
  vars <- c("sinempleo", "informalidad", "subempleo", "precariedad", "hacinamiento", "sincocina", "sinbano", "noasistencia4_17", "anoseducacion", "nocalefaccion", "sinheladera", "nocalefaccionagua", "nointernet")
  tstats <- vector(mode="double", length=length(vars))
  names(tstats) <- c("Sin empleo", "Informal", "Subempleo", "Precariedad", "En hacinamiento", "Sin cocina", "Sin baño", "Inasistencia escolar", "Años educación formal", "Sin calefacción", "Sin heladera", "Sin calentador agua", "Sin acceso internet")
  for (var in vars) {
    if (var %in% c("subempleo", "sinempleo", "informalidad")) tstats[which(vars==var)] <- wtd.t.test(datos3[eval(parse(text=segmento1))&e27>17, get(var)], datos3[eval(parse(text=segmento2))&e27>17, get(var)], datos3[eval(parse(text=segmento1))&e27>17, pesoano], datos3[eval(parse(text=segmento2))&e27>17, pesoano])$coefficients[1]
    else if (var=="noasistencia4_17") tstats[which(vars==var)] <- wtd.t.test(datos3[eval(parse(text=segmento1))&e27>3&e27<18, get(var)], datos3[eval(parse(text=segmento2))&e27>3&e27<18, get(var)], datos3[eval(parse(text=segmento1))&e27>3&e27<18, pesoano], datos3[eval(parse(text=segmento2))&e27>3&e27<18, pesoano])$coefficients[1]
    else tstats[which(vars==var)] <- wtd.t.test(datos3[eval(parse(text=segmento1)), get(var)], datos3[eval(parse(text=segmento2)), get(var)], datos3[eval(parse(text=segmento1)), pesoano], datos3[eval(parse(text=segmento2)), pesoano])$coefficients[1]
  }
  return (tstats %>% round(digits=2))
}
print("Las siguientes comparaciones usan la linea de pobreza hipotetica de 120%, con un segmento de referencia de hasta 10% menos que ese valor")
print(diferenciasSignifSegmentos2("ingvl>lineapob*1.2*.9&ingvl<lineapob*1.2", "ingvl>=lineapob*1.2&ingvl<lineapob*1.2*1.05"))
print(paste0(diferenciasSignifSegmentos2("ingvl>lineapob*1.2*.9&ingvl<lineapob*1.2", "ingvl>=lineapob*1.2&ingvl<lineapob*1.2*1.05") %>% {sum(abs(.)<1.96)}, " de 13 diferencias no son significativas en segmento 100-105%"))
print(diferenciasSignifSegmentos2("ingvl>lineapob*1.2*.9&ingvl<lineapob*1.2", "ingvl>=lineapob*1.2*1.05&ingvl<lineapob*1.2*1.1"))
print(paste0(diferenciasSignifSegmentos2("ingvl>lineapob*1.2*.9&ingvl<lineapob*1.2", "ingvl>=lineapob*1.2*1.05&ingvl<lineapob*1.2*1.1") %>% {sum(abs(.)<1.96)}, " de 13 diferencias no son significativas en segmento 105-110%"))
print(diferenciasSignifSegmentos2("ingvl>lineapob*1.2*.9&ingvl<lineapob*1.2", "ingvl>=lineapob*1.2*1.1&ingvl<lineapob*1.2*1.15"))
print(paste0(diferenciasSignifSegmentos2("ingvl>lineapob*1.2*.9&ingvl<lineapob*1.2", "ingvl>=lineapob*1.2*1.1&ingvl<lineapob*1.2*1.15") %>% {sum(abs(.)<1.96)}, " de 13 diferencias no son significativas en segmento 110-115%"))

# Contrastes de hipotesis usando la variable nro de NBIs (UBN_q)
NBItest <- function(segmento1, segmento2, var) wtd.t.test(datos3[eval(parse(text=segmento1)), UBN_q], datos3[eval(parse(text=segmento2)), UBN_q], datos3[eval(parse(text=segmento1)), pesoano], datos3[eval(parse(text=segmento2)), pesoano])$coefficients[1]
print("contraste de #NBIs segmento 90-100% vs 100-105%")
NBItest("ingvl>lineapob*.9&ingvl<lineapob", "ingvl>=lineapob&ingvl<lineapob*1.05")
print("contraste de #NBIs segmento 90-100% vs 105-110%")
NBItest("ingvl>lineapob*.9&ingvl<lineapob", "ingvl>=lineapob*1.05&ingvl<lineapob*1.1")
print("contraste de #NBIs segmento 90-100% vs 110-115%")
NBItest("ingvl>lineapob*.9&ingvl<lineapob", "ingvl>=lineapob*1.1&ingvl<lineapob*1.15")
print("contraste de #NBIs segmento 90-100% vs 110-115%")
NBItest("ingvl>lineapob*.9&ingvl<lineapob", "ingvl>=lineapob*1.15&ingvl<lineapob*1.2")
print("contraste de #NBIs segmento 85-100% vs 100-105%")
NBItest("ingvl>lineapob*.85&ingvl<lineapob", "ingvl>=lineapob&ingvl<lineapob*1.05")

# Graficos Linea de Pobreza-NBIs-Empleo -------------------------------------------------

smoothingSpline = smooth.spline(datos3[diflp>60&diflp<150, diflp], datos3[diflp>60&diflp<150, UBN_q], spar=0.35, w=datos3[diflp>60&diflp<150, pesoano])
plot(datos3[diflp>60&diflp<150, diflp], datos3[diflp>60&diflp<150, UBN_q], type="n", main="Promedio de NBIs por nivel de ingresos relativo a la línea de pobreza (suavizado por spline)", xlab="ingreso (% de la línea de pobreza del hogar)", ylab="nro. de NBIs", ylim=c(0.20, 1.65))
lines(smoothingSpline)
abline(v=100, col="blue")
abline(v=90)
abline(v=120)
abline(v=80)
y0 <-  sum(datos3[segmento=="90100", UBN_q*pesoano])/sum(datos3[segmento=="90100", pesoano])
y1 <-  sum(datos3[diflp>100&diflp<120, UBN_q*pesoano])/sum(datos3[diflp>100&diflp<120, pesoano])
y2<-  sum(datos3[diflp>80&diflp<100, UBN_q*pesoano])/sum(datos3[diflp>80&diflp<100, pesoano])
segments(90, y0, 100, lty="dashed")
segments(100, y1, 120, lty="dashed")
segments(80, y2, 100, lty="dashed")

findSmooths <- function() {
  vars <- c("sinempleo", "informalidad", "subempleo", "precariedad", "hacinamiento", "sincocina", "sinbano", "noasistencia4_17", "nocalefaccion", "sinheladera", "nocalefaccionagua", "nointernet")
  smooths <- list()
  #for (i in 1:12) smooths[i] <- list()
  for (var in vars) {
    if (var %in% c("subempleo", "sinempleo", "informalidad")) smooths[which(vars==var)] <- list(smooth.spline(datos3[diflp>60&diflp<150&e27>17, diflp], datos3[diflp>60&diflp<150&e27>17, get(var)*100], spar=.35, w=datos3[diflp>60&diflp<150&e27>17, pesoano]))
    else smooths[which(vars==var)] <- list(smooth.spline(datos3[diflp>60&diflp<150, diflp], datos3[diflp>60&diflp<150, get(var)*100], spar=.35, w=datos3[diflp>60&diflp<150, pesoano]))
  }
  names(smooths) <- c("Sin empleo", "Informal", "Subempleo", "Precariedad", "En hacinamiento", "Sin cocina", "Sin baño", "Inasistencia escolar", "Sin calefacción", "Sin heladera", "Sin calentador agua", "Sin acceso internet")
  return(smooths)
}
smooths <- findSmooths()

plot(datos3[diflp>60&diflp<150&e27>17, diflp], datos3[diflp>60&diflp<150&e27>17, sinempleo*100], type="n", main="Empleo por nivel de ingresos relativo a la línea de pobreza (suavizado por spline)", xlab="ingreso (% de la línea de pobreza del hogar)", ylab="promedio carencia (%)", ylim=c(0, 60))
for (i in 1:3) lines(smooths[[i]], col=i)
legend("bottomleft", c("sin empleo", "informalidad", "subempleo"), col=1:3, lty="solid", cex=0.6, bty="n")
