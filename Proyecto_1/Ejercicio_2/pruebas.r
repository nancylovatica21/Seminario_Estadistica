# Descripción: Este script contiene el código de la función de selección de modelos en base a su AIC y BIC|
# Autor: Fernando Alvarado Palacios
# Fecha: 2020-12-15


#------------------------------------------------------------------------------------------------------------------------------------------
#Algoritmo de selección de modelos en base a su AIC y BIC
#------------------------------------------------------------------------------------------------------------------------------------------


#------------------------------------------------------------------------------------------------------------------------------------------
#Algoritmo malla (Aqui hare la malla para poder elevar a las variables a la potencia que se requiera)


malla <- function(n, liminf, limsup, finura, des_Ponderada = 0 , limInfpon = 0, limSuppon = 2){
  #Parametros
  #n: número de parametros que tiene nuestro modelo 
  #liminf: límite inferior de la malla, para las variables x
  #limsup: límite superior de la malla, para las variables x
  #finura: finura de la malla que deseamos
  #des_Ponderada: si se requiere usar regresion ponderada
  #limInfpon: límite inferior de la regresion ponderada
  #limSuppon: límite superior de la regresion ponderada  
  
  # Crear un data frame con el número correcto de filas antes de agregar columnas
  seq_values <- seq(liminf, limsup, by = finura)
  df <- data.frame(matrix(ncol = 0, nrow = length(seq_values))) # Se crea con las filas correctas

  for (i in 1:n) {
    col_name <- paste("var_", i, sep = "")  # Crear nombre de columna
    df[[col_name]] <- seq_values  # Asignar los valores de la secuencia
  }
  
  if(des_Ponderada != 0){
    df[["Ponderada"]] <- seq(limInfpon, limSuppon, length.out = length(seq_values))
  }

  return(expand.grid(df))
}
