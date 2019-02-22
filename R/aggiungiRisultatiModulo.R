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
  
  #defining function not equal
  '%!in%' <- function(x,y)!('%in%'(x,y))
  
  crediti <- read.table(paste(path.package(package="BBBMGU"),"/crediti.txt",sep="/"), sep="\t", header=T, stringsAsFactors = F)
  studenti.voti <- read.table(input.voti, sep="\t", header=T, stringsAsFactors = F, quote = "\"")

  bb <- loadWorkbook(excel.esame)
  bb.df <- readWorksheet(bb, sheet=1)
  
  #identifica dove e' la matricola
  matricola.col <- grep("Matricola", bb.df)
  matricola.row <- which(bb.df[, matricola.col]=="Matricola")
  matricola.esame <- as.numeric(bb.df[(matricola.row + 1):dim(bb.df)[1],matricola.col])

  #######################################################################
  
  #identifica il numero di CFU
  cfu.col <- grep("CFU", bb.df[matricola.row,])
  cfu.esame <- as.numeric(bb.df[(matricola.row + 1):dim(bb.df)[1],cfu.col])
  cfu.esame.df <- data.frame(matricola.esame, cfu.esame, stringsAsFactors=F)

  #aggiungi cfu
  #separa gli studenti non considerati
  studenti.voti.left <- studenti.voti[which(studenti.voti$MATRICOLA%!in%matricola.esame),]
  #studenti a cui aggiungere CFU
  studenti.voti.cfu.df <- studenti.voti[which(studenti.voti$MATRICOLA%in%matricola.esame),]
  common <- intersect(studenti.voti.cfu.df$MATRICOLA, cfu.esame.df[,1])
  #seleziona comuni
  cfu.esame.df <- cfu.esame.df[which(cfu.esame.df[,1]%in%common),]
  cfu.esame.df <- cfu.esame.df[order(cfu.esame.df[,1]),]
  studenti.voti.cfu.df <- studenti.voti.cfu.df[which(studenti.voti.cfu.df$MATRICOLA%in%common),]
  studenti.voti.cfu.df <- studenti.voti.cfu.df[order(studenti.voti.cfu.df$MATRICOLA),]
  
  if(identical(as.numeric(cfu.esame.df[,1]), as.numeric(studenti.voti.cfu.df$MATRICOLA))){
    studenti.voti.cfu.df$CFU <- cfu.esame.df[,2]
  }else{
    cat("\nProblema nell'allineamento delle matricole per la copia dei CFU\n")
    return(1)
  }
  studenti.voti <- rbind(studenti.voti.left, studenti.voti.cfu.df)
  ##########################################################################
  
  #identifica anno di corso
  anno.col <- grep("^Anno", bb.df[matricola.row,])
  anno.corso <- bb.df[(matricola.row + 1):dim(bb.df)[1],anno.col]
  
  #aggiungi anno di corso
  anno.corso.df <- data.frame(matricola.esame, anno.corso, stringsAsFactors=F)

  #separa gli studenti non considerati
  studenti.voti.left <- studenti.voti[which(studenti.voti$MATRICOLA%!in%matricola.esame),]
  #studenti a cui aggiungere AnnoCorso
  studenti.voti.anno.corso.df <- studenti.voti[which(studenti.voti$MATRICOLA%in%matricola.esame),]
  common <- intersect(studenti.voti.anno.corso.df$MATRICOLA, anno.corso.df[,1])
  #seleziona comuni
  anno.corso.df <- anno.corso.df[which(anno.corso.df[,1]%in%common),]
  anno.corso.df <- anno.corso.df[order(anno.corso.df[,1]),]
  studenti.voti.anno.corso.df <- studenti.voti.anno.corso.df[which(studenti.voti.anno.corso.df$MATRICOLA%in%common),]
  studenti.voti.anno.corso.df <- studenti.voti.anno.corso.df[order(studenti.voti.anno.corso.df$MATRICOLA),]
  
  if(identical(as.numeric(anno.corso.df[,1]), as.numeric(studenti.voti.anno.corso.df$MATRICOLA))){
    studenti.voti.anno.corso.df$AnnoCorso <- anno.corso.df[,2]
  }else{
    cat("\nProblema nell'allineamento delle matricole per la copia del AnnoCorso\n")
    return(1)
  }
  studenti.voti <- rbind(studenti.voti.left, studenti.voti.anno.corso.df)
  ##########################################################################
  
  #identifica dove e' l'esito
  esito.col <- grep("Esito", bb.df[matricola.row,])
  esito.esame <- as.numeric(bb.df[(matricola.row + 1):dim(bb.df)[1],esito.col])
  
  #aggiungi esito
  esito.esame.df <- data.frame(matricola.esame, esito.esame, stringsAsFactors=F)
  
  #separa gli studenti non considerati
  studenti.voti.left <- studenti.voti[which(studenti.voti$MATRICOLA%!in%matricola.esame),]
  #studenti a cui aggiungere esito
  studenti.voti.esito.esame.df <- studenti.voti[which(studenti.voti$MATRICOLA%in%matricola.esame),]
  common <- intersect(studenti.voti.esito.esame.df$MATRICOLA, esito.esame.df[,1])
  #seleziona comuni
  esito.esame.df <- esito.esame.df[which(esito.esame.df[,1]%in%common),]
  esito.esame.df <- esito.esame.df[order(esito.esame.df[,1]),]
  studenti.voti.esito.esame.df <- studenti.voti.esito.esame.df[which(studenti.voti.esito.esame.df$MATRICOLA%in%common),]
  studenti.voti.esito.esame.df <- studenti.voti.esito.esame.df[order(studenti.voti.esito.esame.df$MATRICOLA),]
   
  
  if(modulo=="BB"){
    #studenti.voti$MatricolaVoti[which(studenti.voti$MATRICOLA%in%matricola.esame)] <- matricola.esame[which(matricola.esame%in%studenti.voti$MATRICOLA)]
    #aggiungi l'esito al file di aggregazione
    
    if(identical(as.numeric(esito.esame.df[,1]), as.numeric(studenti.voti.esito.esame.df$MATRICOLA))){
      studenti.voti.esito.esame.df$BasiBiologiche <- esito.esame.df[,2]
      #converti esito esame in dato normalizzato
      #esito.esame:100=x:crediti$percentuale[crediti$modulo == "Basi Biologiche"]
      
      esame.norm <- sapply(esito.esame.df[,2], function(x,y){
        esame.norm <- x*y/100
      }, y=crediti$percentuale[crediti$modulo == "Basi Biologiche"])
      
      #aggiungi l'esito al file di aggregazione
      studenti.voti.esito.esame.df$BasiBiologicheNorm <- esame.norm
      
    }else{
      cat("\nProblema nell'allineamento delle matricole per esito esame BB\n")
      return(1)
    }
    studenti.voti <- rbind(studenti.voti.left, studenti.voti.esito.esame.df)

  }else if(modulo=="BM"){
    
    if(identical(as.numeric(esito.esame.df[,1]), as.numeric(studenti.voti.esito.esame.df$MATRICOLA))){
      studenti.voti.esito.esame.df$BasiBiologiche <- esito.esame.df[,2]
      #converti esito esame in dato normalizzato
      #esito.esame:100=x:crediti$percentuale[crediti$modulo == "Basi Biologiche"]
      
      esame.norm <- sapply(esito.esame.df[,2], function(x,y){
        esame.norm <- x*y/100
      }, y=crediti$percentuale[crediti$modulo == "Biologia Molecolare"])
      
      #aggiungi l'esito al file di aggregazione
      studenti.voti.esito.esame.df$BiologiaMolecolareNorm <- esame.norm
      
    }else{
      cat("\nProblema nell'allineamento delle matricole per esito esame BM\n")
      return(1)
    }
    studenti.voti <- rbind(studenti.voti.left, studenti.voti.esito.esame.df)
    
  }else if(modulo=="GU"){
    if(identical(as.numeric(esito.esame.df[,1]), as.numeric(studenti.voti.esito.esame.df$MATRICOLA))){
      studenti.voti.esito.esame.df$GeneticaUmana <- esito.esame.df[,2]
      #converti esito esame in dato normalizzato
      esame.norm <- sapply(esito.esame.df[,2], function(x,y){
        esame.norm <- x*y/100
      }, y=crediti$percentuale[crediti$modulo == "Genetica Umana"])
      
      #aggiungi l'esito al file di aggregazione
      studenti.voti.esito.esame.df$GeneticaUmanaNorm <- esame.norm
      
    }else{
      cat("\nProblema nell'allineamento delle matricole per esito esame GU\n")
      return(1)
    }
    studenti.voti <- rbind(studenti.voti.left, studenti.voti.esito.esame.df)
    
  }
  write.table(studenti.voti, output.voti, sep="\t", col.names = T, row.names=F)
  
}
