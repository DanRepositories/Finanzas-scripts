van_constante <- function(inversion_inicial, tasa_descuento, FNF)
{
  cant_periodos = length(FNF)
  suma = 0

  for (i in 1:cant_periodos) {
    suma = suma + (FNF[i] / ((1 + tasa_descuento) ^ i))
  }

  van = suma - inversion_inicial
  return(van)
}

van_variable <- function(inversion_inicial, tasas_descuento, FNF)
{
  cant_periodos = length(FNF)
  suma = 0
  multiplicatoria_tasas = 1

  for (i in 1:cant_periodos) {
    multiplicatoria_tasas = multiplicatoria_tasas * (1 + tasas_descuento[i])
    suma = suma + (FNF[i] / (multiplicatoria_tasas))
  }

  van = suma - inversion_inicial
  return(van)
}

