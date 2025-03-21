# Descripción: Este script contiene el código de la función de selección de modelos en base a su AIC y BIC|
# Autor: Fernando Alvarado Palacios
# Fecha: 2020-12-15


#------------------------------------------------------------------------------------------------------------------------------------------
#Algoritmo de selección de modelos en base a su AIC y BIC
#------------------------------------------------------------------------------------------------------------------------------------------


#------------------------------------------------------------------------------------------------------------------------------------------
#Algoritmo malla (Aqui hare la malla para poder elevar a las variables a la potencia que se requiera)


malla <- function(n, liminf, limsup, finura, des_Ponderada = 0 , limInfpon = 0, limSuppon = 2, distribucion  = c(), funcion_liga= c()){
  #Parametros
  #n: número de parametros que tiene nuestro modelo 
  #liminf: límite inferior de la malla, para las variables x
  #limsup: límite superior de la malla, para las variables x
  #finura: finura de la malla que deseamos
  #des_Ponderada: si se requiere usar regresion ponderada
  #limInfpon: límite inferior de la regresion ponderada
  #limSuppon: límite superior de la regresion ponderada 
  #distribucion: distribucion que se requiere para el GLM
  #funcion_liga: funcion liga que se requiere para el GLM
  
  

  
  
  seq_values <- seq(liminf, limsup, by = finura)  #Creando las recuencias para la malla y posteriormente elevar la variable 
  df <- data.frame(matrix(ncol = 0, nrow = length(seq_values))) # Se creo el df, para poder poner las variables

  for (i in 1:n) {
    col_name <- paste("var_", i, sep = "")  # Crear nombre de columna
    df[[col_name]] <- seq_values  # Asignar los valores de la secuencia
  }
  
  if(!is.null(distribucion) ){ #Agregando los GLM
    df["GLM"] <- c(distribucion, rep(NA, times = length(seq_values) - length(distribucion )))
    df["liga"] <- c(funcion_liga, rep(NA, times = length(seq_values) - length(funcion_liga)))
  }
  
  if(des_Ponderada != 0){ #En caso de necesitar regresion donderada se agrega una columan con una secuencia de ponderada 
    df[["Ponderada"]] <- seq(limInfpon, limSuppon, length.out = length(seq_values)) #Maya de regresion ponderada donde se puede cntrolar los limites de la reg ponderda, pero su finura depede de la longitud del df
    return(expand.grid(df))
  } else{
    expan <- expand.grid(df)
    expan[["Ponderada"]] <- rep(0, times = length(expan$var_1))
    return(expan)
  }

 
}





length()