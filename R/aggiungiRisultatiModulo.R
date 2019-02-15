#' @title Una funzione che aggrega output di aggiornaStudentiVoti.
#' @description Una funzione che aggrega output aggiornaStudentiVoti per i tre moduli. 
#' @param excel.esame, e' l'excel scaricato per un modulo dalle prove parziali su SS3.
#' @param input.voti, output di aggiornaStudentiVotiModuli.
#' @param output.voti,  output di aggiornaStudentiVoti per Genetica Umana, update studenti.
#' @param modulo, selezione del modulo per il quale i voto vanno aggiunti
#' @author Raffaele Calogero, raffaele.calogero [at] unito [dot] it, University of Torino, Italy
#' @return  Il file tab delimited vuoto, studenti_voti.txt,  ma con l'inserimento di tutti gli studenti indipendentemente dall'anno di corso.
#'
#' @examples
#' \dontrun{
#'
#' aggiungiRisultatiModulo(excel.esame="basi_biologiche.xls", 
#'                         input.voti="studenti_voti.txt",
#'                         output.voti="studenti_votiBB.txt", modulo="BB")
#' aggiungiRisultatiModulo(excel.esame="biologia_molecolare.xls", 
#'                         input.voti="studenti_votiBB.txt",
#'                         output.voti="studenti_votiBBBM.txt", modulo="BM")
#' aggiungiRisultatiModulo(excel.esame="genetica_umana.xls", 
#'                         input.voti="studenti_votiBBBM.txt",
#'                         output.voti="studenti_votiBBBMGU.txt", modulo="GU")
#' 
#' }
#'
#' @export

aggiungiRisultatiModulo <- function(excel.esame, input.voti, output.voti, modulo=c("BB","BM", "GU")){
  crediti <- read.table(paste(path.package(package="BBBMGU"),"/crediti.txt",sep="/"), sep="\t", header=T, stringsAsFactors = F)
  studenti.voti <- read.table(input.voti, sep="\t", header=T, stringsAsFactors = F, quote = "\"")
  
  bb <- loadWorkbook(excel.esame)
  bb.df <- readWorksheet(bb, sheet=1)
  
  #identifica dove e' la matricola
  matricola.col <- grep("Matricola", bb.df)
  matricola.row <- which(bb.df[, matricola.col]=="Matricola")
  matricola.esame <- as.numeric(bb.df[(matricola.row + 1):dim(bb.df)[1],matricola.col])
  
  #identifica il numero di CFU
  cfu.col <- grep("CFU", bb.df[matricola.row,])
  cfu.esame <- as.numeric(bb.df[(matricola.row + 1):dim(bb.df)[1],cfu.col])
  
  #aggiungi cfu
  studenti.voti$CFU[which(studenti.voti$MATRICOLA%in%matricola.esame)] <- cfu.esame[which(matricola.esame%in%studenti.voti$MATRICOLA)]
  
  #identifica anno di corso
  anno.col <- grep("^Anno", bb.df[matricola.row,])
  anno.corso <- bb.df[(matricola.row + 1):dim(bb.df)[1],anno.col]
  
  #aggiungi anno di corso
  studenti.voti$AnnoCorso[which(studenti.voti$MATRICOLA%in%matricola.esame)] <- anno.corso[which(matricola.esame%in%studenti.voti$MATRICOLA)]
  
  
  #identifica dove e' l'esito
  esito.col <- grep("Esito", bb.df[matricola.row,])
  esito.esame <- as.numeric(bb.df[(matricola.row + 1):dim(bb.df)[1],esito.col])
  
  if(modulo=="BB"){
    #studenti.voti$MatricolaVoti[which(studenti.voti$MATRICOLA%in%matricola.esame)] <- matricola.esame[which(matricola.esame%in%studenti.voti$MATRICOLA)]
    #aggiungi l'esito al file di aggregazione
    studenti.voti$BasiBiologiche[which(studenti.voti$MATRICOLA%in%matricola.esame)] <- esito.esame[which(matricola.esame%in%studenti.voti$MATRICOLA)]
    
    #converti esito esame in dato normalizzato
    #esito.esame:100=x:crediti$percentuale[crediti$modulo == "Basi Biologiche"]
    
    esame.norm <- sapply(esito.esame, function(x,y){
      esame.norm <- x*y/100
    }, y=crediti$percentuale[crediti$modulo == "Basi Biologiche"])
    
    #aggiungi l'esito al file di aggregazione
    studenti.voti$BasiBiologicheNorm[which(studenti.voti$MATRICOLA%in%matricola.esame)] <- esame.norm[which(matricola.esame%in%studenti.voti$MATRICOLA)]
  }else if(modulo=="BM"){
    #studenti.voti$MatricolaVoti[which(studenti.voti$MATRICOLA%in%matricola.esame)] <- matricola.esame[which(matricola.esame%in%studenti.voti$MATRICOLA)]
    studenti.voti$BiologiaMolecolare[which(studenti.voti$MATRICOLA%in%matricola.esame)] <- esito.esame[which(matricola.esame%in%studenti.voti$MATRICOLA)]
    
    #converti esito esame in dato normalizzato
    #esito.esame:100=x:crediti$percentuale[crediti$modulo == "Biologia Molecolare"]
    
    esame.norm <- sapply(esito.esame, function(x,y){
      esame.norm <- x*y/100
    }, y=crediti$percentuale[crediti$modulo == "Biologia Molecolare"])
    
    #aggiungi l'esito al file di aggregazione
    studenti.voti$BiologiaMolecolareNorm[which(studenti.voti$MATRICOLA%in%matricola.esame)] <- esame.norm[which(matricola.esame%in%studenti.voti$MATRICOLA)]
  }else if(modulo=="GU"){
    studenti.voti$GeneticaUmana[which(studenti.voti$MATRICOLA%in%matricola.esame)] <- esito.esame[which(matricola.esame%in%studenti.voti$MATRICOLA)]
    
    #converti esito esame in dato normalizzato
    #esito.esame:100=x:crediti$percentuale[crediti$modulo == "Genetica Umana"]
    esame.norm <- sapply(esito.esame, function(x,y){
      esame.norm <- x*y/100
    }, y=crediti$percentuale[crediti$modulo == "Genetica Umana"])
    
    #aggiungi l'esito al file di aggregazione
    studenti.voti$GeneticaUmanaNorm[which(studenti.voti$MATRICOLA%in%matricola.esame)] <- esame.norm[which(matricola.esame%in%studenti.voti$MATRICOLA)]
  }
  write.table(studenti.voti, output.voti, sep="\t", col.names = T, row.names=F)
  
}
