#' @title Una funzione che aggrega output di aggiornaStudentiVoti.
#' @description Una funzione che aggrega output aggiornaStudentiVoti per i tre moduli. La matrice dei voti e' vuota ma sono inseriti tutti gli studenti indipendentemente dall'anno di corso.
#' @param basiBiologiche, output di aggiornaStudentiVoti per Basi Biologiche, update studenti.
#' @param biologiaMolecolare, output di aggiornaStudentiVoti per Biologia Molecolare, update studenti.
#' @param geneticaUmana,  output di aggiornaStudentiVoti per Genetica Umana, update studenti.
#' @author Raffaele Calogero, raffaele.calogero [at] unito [dot] it, University of Torino, Italy
#' @return  Il file tab delimited derivato da studenti_voti.txt,  riempito con i risultati di uno dei tre moduli.
#'
#' @examples
#' \dontrun{
#
#' aggiornaStudentiVotiModuli(basiBiologiche=bb.set, 
#'                           biologiaMolecolare=bm.set, 
#'                           geneticaUmana=gu.set)
#' 
#' }
#'
#' @export

aggiornaStudentiVotiModuli<- function(basiBiologiche, biologiaMolecolare, geneticaUmana){
  all <- rbind(basiBiologiche,  biologiaMolecolare, geneticaUmana)
  
  all <- all[!duplicated(all$MATRICOLA),]
  cat("\nIl file studenti_voti.txt e' stato creato\n")
  write.table(all, "studenti_voti.txt", sep="\t", col.names = T, row.names=F)
}
