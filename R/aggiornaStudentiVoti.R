#' @title Una funzione che crea una lista voti.
#' @description Una funzione che crea una lista voti vuota e la aggiorna per la presenza di studenti non presenti nella lista dell'anno in corso.
#' @param excel.esame, e' l'excel scaricato per un modulo dalle prove parziali su SS3.
#' @param elenco.studenti, e' l'elenco degli studenti dell'anno in corso per un certo canale.
#' @author Raffaele Calogero, raffaele.calogero [at] unito [dot] it, University of Torino, Italy
#' @return  Un data.frame aggiornato con i nuovi studenti. Importante questo data.frame e' completamente vuoto 
#'
#' @examples
#' \dontrun{
#' 
#' bb.set <- aggiornaStudentiVoti(excel.esame="basi_biologiche.xls", 
#'                                 elenco.studenti="../canaleA_studenti.txt")
#' bm.set <- aggiornaStudentiVoti(excel.esame="biologia_molecolare.xls", 
#'                                 elenco.studenti="../canaleA_studenti.txt")
#' gu.set <- aggiornaStudentiVoti(excel.esame="genetica_umana.xls", 
#'                                 elenco.studenti="../canaleA_studenti.txt")
#' }
#'
#' @export

aggiornaStudentiVoti <- function(excel.esame, elenco.studenti=NULL){
    #creare un dataframe che possa aggregare tutti i voti degli studenti
    studenti <- read.table(elenco.studenti , sep="\t", header=T, stringsAsFactors = F, quote = "\"")
    studenti.voti <- data.frame(studenti, MatricolaVoti=rep(NA,dim(studenti)[1]), AnnoCorso=rep(NA,dim(studenti)[1]), CFU=rep(NA,dim(studenti)[1]), BasiBiologiche=rep(NA,dim(studenti)[1]), BasiBiologicheNorm=rep(NA,dim(studenti)[1]), 
                              BiologiaMolecolare=rep(NA,dim(studenti)[1]), BiologiaMolecolareNorm=rep(NA,dim(studenti)[1]), 
                              GeneticaUmana=rep(NA,dim(studenti)[1]), GeneticaUmanaNorm=rep(NA,dim(studenti)[1]),
                              VotoAggregato=rep(NA,dim(studenti)[1]), stringsAsFactors = F
    )
    #install.packages("readxl")
    #library("readxl")
    # per xlsx install.packages("xlsx")
    #obsolete install.packages("XLConnect")
    # obsolete library(XLConnect)
    #integrare tabella iniziale studenti
    #carica basi biologiche dal output della prova preliminare scaricata da SS3
    bb.df <- read_xls(excel.esame, sheet=1)
    #bb <- loadWorkbook(excel.esame)
    #bb.df <- readWorksheet(bb, sheet=1)
    #identifica dove e' la matricola
    matricola.col <- grep("Matricola", bb.df)
    matricola.row <- which(bb.df[, matricola.col]=="Matricola")
    matricola.esame <- as.numeric(unlist(bb.df[(matricola.row + 1):dim(bb.df)[1],matricola.col]))
    cognome.col <- grep("^Cognome", bb.df[matricola.row,])
    cognome <- as.character(unlist(bb.df[(matricola.row + 1):dim(bb.df)[1],cognome.col]))
    nome.col <- grep("^Nome", bb.df[matricola.row,])
    nome <- as.character(unlist(bb.df[(matricola.row + 1):dim(bb.df)[1],nome.col]))
    extra.matricola <- setdiff(matricola.esame, studenti$MATRICOLA)
    if(length(extra.matricola) > 0){
      #creare tabella studenti voti con studenti non inseriti nella normale tabella canale
      extra.df <- data.frame(COGNOME=cognome[which(matricola.esame%in%extra.matricola)], NOME=nome[which(matricola.esame%in%extra.matricola)], MATRICOLA=extra.matricola,
                           MatricolaVoti=rep(NA,length(extra.matricola)), AnnoCorso=rep(NA,length(extra.matricola)), CFU=rep(NA,length(extra.matricola)), 
                           BasiBiologiche=rep(NA,length(extra.matricola)), BasiBiologicheNorm=rep(NA,length(extra.matricola)), 
                           BiologiaMolecolare=rep(NA,length(extra.matricola)), BiologiaMolecolareNorm=rep(NA,length(extra.matricola)), 
                           GeneticaUmana=rep(NA,length(extra.matricola)), GeneticaUmanaNorm=rep(NA,length(extra.matricola)),
                           VotoAggregato=rep(NA,length(extra.matricola)), stringsAsFactors = F
      )
      cat("\n N. studenti extra:",length(extra.matricola),"\n")
      studenti.voti <- rbind(studenti.voti, extra.df)
    }
  return(studenti.voti)
  
}

  
  
