#' Parse CMC Markets report from exported CSV file.
#'
#' \code{readCMCreport} returns a data.frame with the operations one by one and
#' to add useful information and statistics calculations.
#'
#' @author Mario Pisa
#' @param report exported from CMC Market as csv file
#' @return data.frame of operations one by one and added useful info and
#'    calculations.
#' @examples
#' readCMCreport("../myCMCreports/Historico20160909.csv")
#' operations <- readCMCreport("../myCMCreports/Historico20160909.csv")
#' @seealso \url{https://www.cmcmarkets.com/}
#' @export
readCMCreport <- function(file) {
   # internal functions
      parseCSV <- function(df) {
         # Fecha...Hora	character
         #df$Fecha...Hora <- strptime(df$Fecha...Hora, "%d.%m.%Y %H:%M:%S") # convertir a POSIX
         df$Fecha...Hora <- as.POSIXct(df$Fecha...Hora, format = "%d.%m.%Y %H:%M:%S", tz = "") # tz vacio es la actual time zone
         # Tipo...Ref	character
         # Orden.Relacionada	character
         # Orden			character
         # Operacion		character
         # Producto		character
         # Cantidad      numeric
         df$Cantidad <- type.convert(gsub("\\.", "", df$Cantidad), na.strings = "-", as.is = FALSE, dec = ",", numerals = c("allow.loss", "warn.loss", "no.loss"))
         #df$Cantidad <- type.convert(df$Cantidad, na.strings="-", as.is=FALSE, dec=",", numerals=c("allow.loss", "warn.loss", "no.loss"))
         # Unidades.Cantidad character
         # Precio		character
         df$Precio <- type.convert(gsub("\\.", "", df$Precio), na.strings = "-", as.is = FALSE, dec = ",", numerals = c("allow.loss", "warn.loss", "no.loss"))
         # Rango.de.Precio		character
         df$Rango.de.Precio <- type.convert(gsub("\\.", "", df$Rango.de.Precio), na.strings="-", as.is=FALSE, dec=",", numerals=c("allow.loss", "warn.loss", "no.loss"))
         df$Rango.de.Precio <- as.numeric(df$Rango.de.Precio)
         # SL		character
         # df$SL <- gsub("\\.", "", df$SL)
         # df$SL <- gsub(",", ".", df$SL)
         df$SL <- gsub("\\(D\\) ", "", df$SL)
         # df$SL <- type.convert(df$SL, na.strings="-", as.is=FALSE, dec=",", numerals=c("allow.loss", "warn.loss", "no.loss"))
         # df$SL <- as.numeric(df$SL)
         df$SL <- type.convert(gsub("\\.", "", df$SL), na.strings="-", as.is=FALSE, dec=",", numerals=c("allow.loss", "warn.loss", "no.loss"))
         df$SL <- as.numeric(df$SL)
         # TP		character
         df$TP <- type.convert(gsub("\\.", "", df$TP), na.strings="-", as.is=FALSE, dec=",", numerals=c("allow.loss", "warn.loss", "no.loss"))
         df$TP <- as.numeric(df$TP)
         # Tipo.de.Conversion	char
         df$Tipo.de.Conversion <- type.convert(gsub("\\.", "", df$Tipo.de.Conversion), na.strings="-", as.is=FALSE, dec=",", numerals=c("allow.loss", "warn.loss", "no.loss"))
         # Garantía.EUR.		character
         df$Garantía.EUR. <- type.convert(gsub("\\.", "", df$Garantía.EUR.), na.strings="-", as.is=TRUE, dec=",", numerals=c("allow.loss", "warn.loss", "no.loss"))
         df$Garantía.EUR. <- as.numeric(df$Garantía.EUR.)
         # Nominal.EUR.	char
         df$Nominal.EUR. <- type.convert(gsub("\\.", "", df$Nominal.EUR.), na.strings="-", as.is=FALSE, dec=",", numerals=c("allow.loss", "warn.loss", "no.loss"))
         # Cantidad	char
         df$Cantidad.EUR. <- type.convert(gsub("\\.", "", df$Cantidad.EUR.), na.strings="-", as.is=FALSE, dec=",", numerals=c("allow.loss", "warn.loss", "no.loss"))
         #df$Cantidad.EUR. <- as.numeric(df$Cantidad.EUR.)
         # Saldo.EUR.	numeric
         df$Saldo.EUR. <- type.convert(gsub("\\.", "", df$Saldo.EUR.), na.strings="-", as.is=FALSE, dec=",", numerals=c("allow.loss", "warn.loss", "no.loss"))
         # Cantidad.excluyendo.costes	logical
         df$Cantidad.excluyendo.costes <- as.numeric(df$Cantidad.excluyendo.costes)
         # Coste->num
         df$Coste <- as.numeric(df$Coste)
         # Tasa.Anual.de.Financiacion	logical->num
         df$Tasa.Anual.de.Financiacion <- as.numeric(df$Tasa.Anual.de.Financiación)
         # Tipo.de.Conversión.EUR.	logical->num
         df$Tipo.de.Conversion.EUR. <- as.numeric(df$Tipo.de.Conversión.EUR.)
         # Cantidad.Financiada.EUR.	logical->num
         df$Cantidad.Financiada.EUR. <- as.numeric(df$Cantidad.Financiada.EUR.)
         # Tasa.Anual.de.Costes.Carrying.	logical->num
         df$Tasa.Anual.de.Costes.Carrying. <- as.numeric(df$Tasa.Anual.de.Costes.Carrying.)
         # Tipo.de.Conversión	logical->num
         df$Tipo.de.Conversion <- as.numeric(df$Tipo.de.Conversión)
         # Cantidad.Costes.Carrying.EUR. 	logical
         df$Cantidad.Costes.Carrying.EUR. <- as.numeric(df$Cantidad.Costes.Carrying.EUR.)
         # Tipo.de.Mantenimiento.de.Posición 	logical
         df$Tipo.de.Mantenimiento.de.Posicion <- as.numeric(df$Tipo.de.Mantenimiento.de.Posición)
         # Tipo.de.Conversión.1 	logical
         df$Tipo.de.Conversion.1 <- as.numeric(df$Tipo.de.Conversión.1)
         # Costes.de.Mantenimiento.Posición.EUR.	logical
         df$Costes.de.Mantenimiento.Posicion.EUR. <- as.numeric(df$Costes.de.Mantenimiento.Posición.EUR.)
         # Ajuste.EUR.	logical
         df$Ajuste.EUR. <- as.numeric(df$Ajuste.EUR.)
         # Total.EUR.	logical
         df$Total.EUR. <- as.numeric(df$Total.EUR.)
         # Prima	logical
         df$Prima <- as.numeric(df$Prima)
         # Devolución.. 	logical
         df$Devolución.. <- as.numeric(df$Devolución.)
         # Objetivo
         df$Objetivo <- as.numeric(df$Objetivo)
         #df$Objetivo <- type.convert(gsub("\\.", "", df$Objetivo), na.strings="-", as.is=FALSE, dec=",", numerals=c("allow.loss", "warn.loss", "no.loss"))
         # Desembolso.con.Ganancia
         df$Desembolso.con.Ganancia <- as.numeric(df$Desembolso.con.Ganancia)
         #df$Desembolso.con.Ganancia <- type.convert(gsub("\\.", "", df$Desembolso.con.Ganancia), na.strings="-", as.is=FALSE, dec=",", numerals=c("allow.loss", "warn.loss", "no.loss"))
         #Desembolso.por.empate
         df$Desembolso.por.empate <- as.numeric(df$Desembolso.por.empate)
         #df$Desembolso.por.empate <- type.convert(gsub("\\.", "", df$Desembolso.por.empate), na.strings="-", as.is=FALSE, dec=",", numerals=c("allow.loss", "warn.loss", "no.loss"))
         # Apertura
         df$Apertura <- as.numeric(df$Apertura)
         #df$Apertura <- type.convert(gsub("\\.", "", df$Apertura), na.strings="-", as.is=FALSE, dec=",", numerals=c("allow.loss", "warn.loss", "no.loss"))
         # Vencimiento
         df$Vencimiento <- as.numeric(df$Vencimiento)
         #df$Vencimiento <- type.convert(gsub("\\.", "", df$Vencimiento), na.strings="-", as.is=FALSE, dec=",", numerals=c("allow.loss", "warn.loss", "no.loss"))
         # Liquidación
         df$Liquidacion <- as.numeric(df$Liquidación)
         #df$Liquidación <- type.convert(gsub("\\.", "", df$Liquidación), na.strings="-", as.is=FALSE, dec=",", numerals=c("allow.loss", "warn.loss", "no.loss"))
         # Precio.de.Liquidación
         df$Precio.de.Liquidacion <- as.numeric(df$Precio.de.Liquidación)
         #df$Precio.de.Liquidación <- type.convert(gsub("\\.", "", df$Precio.de.Liquidación), na.strings="-", as.is=FALSE, dec=",", numerals=c("allow.loss", "warn.loss", "no.loss"))
         # Resultado
         df$Resultado <- as.numeric(df$Resultado)
         #df$Resultado <- type.convert(gsub("\\.", "", df$Resultado), na.strings="-", as.is=FALSE, dec=",", numerals=c("allow.loss", "warn.loss", "no.loss"))
         # Desembolso
         df$Desembolso <- as.numeric(df$Desembolso)
         #df$Desembolso <- type.convert(gsub("\\.", "", df$Desembolso), na.strings="-", as.is=FALSE, dec=",", numerals=c("allow.loss", "warn.loss", "no.loss"))
         # Devolución.
         df$Devolucion. <- as.numeric(df$Devolución.)
         #df$Devolución. <- type.convert(gsub("\\.", "", df$Devolución.), na.strings="-", as.is=FALSE, dec=",", numerals=c("allow.loss", "warn.loss", "no.loss"))

         df
   }
      # extract complete operations from dataframe given by parseCSV
      readOperations <- function(df) {
         # ATENCION: SOLO FUNCIONA SI LAS OPERACIONES COMPLETAS SON CONSECUTIVAS SI MEZCLA HAY QUE REPENSAR LOS INDICES Y SI CIERRO UNA COMPRA EN DOS OPERACIONES FALLARÁ TB.
         operaciones <- data.frame()
         ii <- nrow(df)
         while (ii  >= 2) {
            subindice <- ii - 1
            for (j in subindice:1) {
               if ((df$Orden[ii] == df$Orden[j] | df$Orden[ii] == df$Orden.Relacionada[j]) &
                   (df$Tipo...Ref[j] == "Cierre de Posición" | df$Tipo...Ref[j] == "Stop Loss Ejecutado") &
                   df$Orden[ii] != "-" & df$Tipo...Ref[ii] != "Stop Loss Pendiente Modificado") {
                  operacion <- as.character(df$Tipo...Ref[ii])
                  operacion$Producto <- df$Producto[ii]
                  operacion$Cant <- df$Cantidad[ii] # fallo si compro x contratos y vendo x-1 contrato
                  operacion$FechaEntrada <- df$Fecha...Hora[ii]
                  operacion$PrecioEnt <- df$Precio[ii]
                  operacion$TipoSalida <- df$Tipo...Ref[j]
                  operacion$FechaSalida <- df$Fecha...Hora[j]
                  operacion$PrecioSal <- df$Precio[j]
                  operacion$Puntos <- df$Precio[[j]] - df$Precio[[ii]]
                  operacion$WLEUR <- df$Cantidad.EUR.[[j]]
                  operacion$Saldo <- df$Saldo.EUR.[[j]]
                  names(operacion)[1] <- paste("TipoOpe")
                  operacion$TimeOp <- round(difftime(df$Fecha...Hora[[j]], df$Fecha...Hora[[ii]], units = "mins"), digits = 2)
                  operacion <- as.data.frame(operacion)
                  operaciones <- rbind(operaciones, operacion)
                  #ii <- j
               }
            }
            ii <- ii - 1
         }
         # calcula la diferencia de saldo; es lo mismo que operacion$WLEUR
         # operaciones$diff[2:nrow(operaciones)] <- diff(operaciones$Saldo)
         # calcula el retorno de la operación
         operaciones$ret_pct[2:nrow(operaciones)] <- round(diff(log(operaciones$Saldo)) * 100, digits = 2)
         # calcula el ratio recompensa/riesgo
         operaciones$brratio <- round(operaciones$WLEUR / 3, 2) # riesgo fijo de 3eur
         # devuelve las operaciones
         operaciones
   }
      # count consecutive Winner or Losser operations
      findWinLos <- function(df) {
         for (i in 1:nrow(df)) {
            if (df[i, 10] > 0) {
               df$wl[i] <- "w"
            } else if (df[i, 10] <= 0) {
               df$wl[i] <- "l"
            } else {
               df$wl[i] <- "?"
            }
            # calcula la tipología de la operacion
            if (df$brratio[i] < -0.33) {
               df$nota[i] <- "VB"
            } else if (df$brratio[i] < 0.33) {
               df$nota[i] <- "B"
            } else if (df$brratio[i] >= 0.33 & df$brratio[i] <
                       1) {
               df$nota[i] <- "G"
            } else if (df$brratio[i] >= 1) {
               df$nota[i] <- "VG"
            }

         }
         df$seqnota <- sequence(rle(df$nota)$lengths)
         df$seqwl <- sequence(rle(df$wl)$lengths)
         df
   }

   # read cvs file
   df <- read.csv(file, header = TRUE, sep = ",", quote = "\"", dec = ",",
                  fill = TRUE, stringsAsFactors = FALSE)
   report <- parseCSV(df)
   operations <- readOperations(report)
   operations <- findWinLos(operations)
   print(paste("CMC Markets report parsed file: ", file))
   operations
}
