caue <- function(inversion_inicial, tasa_descuento, FNF)
{
  cant_periodos = length(FNF)
  van = van_constante(inversion_inicial, tasa_descuento, FNF, FALSE)

  denominador = 1 - ((1 + tasa_descuento) ^ (-cant_periodos))
  valor_caue = van * tasa_descuento / (denominador)

  return(valor_caue)
}

payback <- function(inversion_inicial, tasa_descuento, FNF, verbose=FALSE)
{
  periodos_totales = length(FNF)

  for (i in 1:periodos_totales) {
    van_actual = van_constante(inversion_inicial, tasa_descuento, FNF[1:i], FALSE)

    if (van_actual >=0) {
      cat("PAYBACK: ", i, "\n")
      cat("VAN ACUMULADO: ", van_actual, "\n")
      break
    }

    if (verbose) {
      cat("VAN al ", i," periodo: ", van_actual, "\n")
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

beneficio_costo <- function(inversion_inicial, beneficios, costos, tasa, verbose=TRUE)
{
  periodos_totales = length(beneficios)
  beneficios_vactual = 0
  costos_va = 0

  for (i in 1:periodos_totales) {
    beneficios_vactual = beneficios_vactual + calcular_valor_actual(beneficios[i], tasa, i)
    costos_va = costos_va + calcular_valor_actual(costos[i], tasa, i)
  }

  if (verbose) {
    cat("SUMA BENEFICIOS VALOR ACTUAL: ", beneficios_vactual, "\n")
    cat("SUMA COSTOS VALOR ACTUAL: ", costos_va, "\n")
  }

  razon_beneficio_costo = beneficios_vactual / (costos_va + inversion_inicial)
  return(razon_beneficio_costo)
}

calcular_valor_actual <- function(monto, tasa, cantidad_periodos)
{
  valor_actual = monto * ((1 + tasa) ^ cantidad_periodos)
  return(valor_actual)
}

