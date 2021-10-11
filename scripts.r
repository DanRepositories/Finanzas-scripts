payback <- function(inversion_inicial, tasa_descuento, FNF)
{
  periodos_totales = length(FNF)

  for (i in 1:periodos_totales) {
    van_actual = van_constante(inversion_inicial, tasa_descuento, FNF[1:i], FALSE)

    if (van_actual >=0) {
      cat("PAYBACK: ", i, "\n")
      cat("VAN ACUMULADO: ", van_actual, "\n")
      break
    }
  }

}

van_constante <- function(inversion_inicial, tasa_descuento, FNF, verbose=TRUE)
{
  cant_periodos = length(FNF)
  suma = 0

  for (i in 1:cant_periodos) {
    suma = suma + (FNF[i] / ((1 + tasa_descuento) ^ i))
  }

  van = suma - inversion_inicial

  if (verbose) {
    mostrar_info_van(van)
  }

  return(van)
}

van_variable <- function(inversion_inicial, tasas_descuento, FNF, verbose=TRUE)
{
  cant_periodos = length(FNF)
  suma = 0
  multiplicatoria_tasas = 1

  for (i in 1:cant_periodos) {
    multiplicatoria_tasas = multiplicatoria_tasas * (1 + tasas_descuento[i])
    suma = suma + (FNF[i] / (multiplicatoria_tasas))
  }

  van = suma - inversion_inicial

  if (verbose) {
    mostrar_info_van(van)
  }

  return(van)
}


mostrar_info_van <- function(van)
{
  if (van < 0) {
    print("VAN NEGATIVO: NO se financia el proyecto")
  } else {
    print("VAN POSITIVO O CERO: SI se financia el proyecto")
  }
}

