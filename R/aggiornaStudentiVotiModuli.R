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
  
  #defining function not equal
  '%!in%' <- function(x,y)!('%in%'(x,y))
  
  all <- rbind(basiBiologiche,  biologiaMolecolare, geneticaUmana)
  #senza duplicazioni
  all.1 <- all[!duplicated(all$MATRICOLA),]
  #i duplicati
  all.2 <- all[duplicated(all$MATRICOLA),]
  #controllo quali elementi hanno valori gia' assegnati
  some.nona <- apply(all.2, 1,function(x){length(!is.na(x[7:13]))})
  all.2 <- all.2[which(some.nona > 0),]
  #identifico i duplicati con i voti bel file senza duplicati
  all.3 <- all.1[which(all.1$MATRICOLA %in%all.2$MATRICOLA),]
  #controllo quali elementi hanno valori assegnati
  some.nona <- apply(all.3, 1,function(x){length(!is.na(x[7:13]))})
  #identifico i non duplicati con valori assegnati
  all.3 <- all.3[which(some.nona > 0),]
  #non dovrebbero esserci sovrapposizioni tra all.2. ed all.3
  critical <- intersect(all.2$MATRICOLA, all.3$MATRICOLA)
  
  if(length(critical) == 0){
    all.0 <- all.1[which(all.$MATRICOLA %!in% c(all.2$MATRICOLA, all.3$MATRICOLA)),]
    all.final <- rbindi(all.0, all.2, all.3)
    
    cat("\nIl file studenti_voti.txt e' stato creato\n")
    write.table(all.final, "studenti_voti.txt", sep="\t", col.names = T, row.names=F)
    return(0)
  }else{
    cat("\nATTENZIONE: ci sono dei duplicati con dei voti! Non dovrebbe succedere.
        \nControllare i files all2duplicati.txt ed all3noduplicati.txt\n")
    write.table(all.2, "all2duplicati.txt", sep="\t", col.names = T, row.names=F)
    write.table(all.3, "all3noduplicati.txt", sep="\t", col.names = T, row.names=F)
    
    all.0 <- all.1[which(all.$MATRICOLA %!in% c(all.2$MATRICOLA, all.3$MATRICOLA)),]
    all.final <- rbindi(all.0, all.2, all.3)
    cat("\nIl file studenti_voti.txt e' stato creato ma ci sono problemi nei duplicati: controllare!\n")
    write.table(all.final, "studenti_voti.txt", sep="\t", col.names = T, row.names=F)
    
    
    return(1)
  }
}
