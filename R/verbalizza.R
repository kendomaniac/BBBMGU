#' @title Una funzione che selezione gli studenti da verbalizzare.
#' @description Una funzione che seleziona dal output di aggregaVotiModuli il set di studenti registrati per la verbalizzazione.
#' @param excel.verbalizzazione, file excel della prenotazione per appello verbalizzante.
#' @param voti.aggregati, file tab delimited con i voti aggregati generati da aggregaVotiModul.
#' @param voti.verbalizzabili, tab delimited output con solo i voti aggregati per gli studenti registratiall'appello verbalizzante.
#' @param studenti.missing, tab delimited output con la matrice del file di verbalizzazione per gli studenti che non trovo nei miei dati.
#' @author Raffaele Calogero, raffaele.calogero [at] unito [dot] it, University of Torino, Italy
#' @return  Il file tab delimited  riempito con i risultati parziali ed il voto integrato per gli studenti da verbalizzare.
#'
#' @examples
#' \dontrun{
#
#' verbalizza(excel.verbalizzazione="verbalizzante.xls", 
#'                   voti.aggregati="canaleA_studenti_votiBBBMGU_aggregati_190227.txt",
#'                   voti.verbalizzabili="canaleA_studenti_da_verbalizzare.txt", 
#'                   studenti.missing="canaleA_studenti_da_verbalizzare_missing.txt")
#' }
#'
#' @export

verbalizza <- function(excel.verbalizzazione, voti.aggregati, voti.verbalizzabili, studenti.missing){
  #carica dati da prenotazione verbalizzazione
  #obsolete bb <- loadWorkbook(excel.verbalizzazione)
  #obsolete bb.df <- readWorksheet(bb, sheet=1)
  bb.df <- read_xls(excel.verbalizzazione, sheet=1)
  #identifica dove e' la matricola
  matricola.col <- grep("Matricola", bb.df)
  matricola.row <- which(bb.df[, matricola.col]=="Matricola")
  matricola.esame <- as.numeric(unlist(bb.df[(matricola.row + 1):dim(bb.df)[1],matricola.col]))
  
  #carica risultati di aggiungiRisultatiModulo che include i tre moduli
  voti <- read.table(voti.aggregati, sep="\t", header=T, stringsAsFactors = F, quote = "\"")
  voti.selezionati <- voti[which(as.numeric(voti$MATRICOLA)%in%matricola.esame),]
  missing <- setdiff(matricola.esame, as.numeric(voti$MATRICOLA))
  cat("\nStudenti da verbalizzare ma non nel file generato dalla funzione aggregaVotiModuli", length(missing),"\n")
  if(length(missing) > 0){
      bb1.df <- bb.df[(matricola.row + 1):dim(bb.df)[1],]
      names(bb1.df) <- bb.df[matricola.row,]
      bb1.df <- bb1.df[which(as.numeric(bb1.df$Matricola) %in% missing),]
      write.table(bb1.df, studenti.missing, sep="\t", col.names = T, row.names=F)
  }
  #scrivi risultati del voto aggregato
  write.table(voti.selezionati, voti.verbalizzabili, sep="\t", col.names = T, row.names=F)
}

