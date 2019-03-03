#' @title Una funzione che crea una lista voti.
#' @description Una funzione che crea una lista voti vuota e la aggiorna per la presenza di studenti non presenti nella lista dell'anno in corso.
#' @param excel.esame, e' l'excel scaricato per un modulo dalle prove parziali su SS3.
#' @param elenco.studenti, e' l'elenco degli studenti dell'anno in corso per un certo canale.
#' @param voti.studenti, voti aggreati output di aggregaVotiModuli
#' @author Raffaele Calogero, raffaele.calogero [at] unito [dot] it, University of Torino, Italy
#' @return  Un data.frame aggiornato con i nuovi studenti. Importante questo data.frame e' completamente vuoto se elenco.studenti non e' diverso da null. Invece se voti.studenti diverso da null e' il file aggregato outpout di aggregaVotiModuli aggiornato
#'
#' @examples
#' \dontrun{
#' #questo formato di query  solo per il primo esame dell'anno poi elenco.studenti  null e si usano i voti aggregati voti.studenti
#' bb.set <- aggiornaStudentiVoti(excel.esame="basi_biologiche.xls", 
#'                                 elenco.studenti="../canaleA_studenti.txt", voti.studenti=NULL)
#' bm.set <- aggiornaStudentiVoti(excel.esame="biologia_molecolare.xls", 
#'                                 elenco.studenti="../canaleA_studenti.txt", voti.studenti=NULL)
#' gu.set <- aggiornaStudentiVoti(excel.esame="genetica_umana.xls", 
#'                                 elenco.studenti="../canaleA_studenti.txt", voti.studenti=NULL)
#' #questo formato e' quello che usa i risultati aggregati
#' bb.set <- aggiornaStudentiVoti(excel.esame="basi_biologiche.xls", 
#'                                 elenco.studenti=NULL, voti.studenti="canaleA_studenti_votiBBBMGU_aggregati_190214_conDic2018.txt")
#' }
#'
#' @export

aggiornaStudentiVoti <- function(excel.esame, elenco.studenti=NULL, voti.studenti=NULL){
  if(!is.null(elenco.studenti)){
    #creare un dataframe che possa aggregare tutti i voti degli studenti
    studenti <- read.table(elenco.studenti , sep="\t", header=T, stringsAsFactors = F, quote = "\"")
    studenti.voti <- data.frame(studenti, MatricolaVoti=rep(NA,dim(studenti)[1]), AnnoCorso=rep(NA,dim(studenti)[1]), CFU=rep(NA,dim(studenti)[1]), BasiBiologiche=rep(NA,dim(studenti)[1]), BasiBiologicheNorm=rep(NA,dim(studenti)[1]), 
                              BiologiaMolecolare=rep(NA,dim(studenti)[1]), BiologiaMolecolareNorm=rep(NA,dim(studenti)[1]), 
                              GeneticaUmana=rep(NA,dim(studenti)[1]), GeneticaUmanaNorm=rep(NA,dim(studenti)[1]),
                              VotoAggregato=rep(NA,dim(studenti)[1]), stringsAsFactors = F
    )
    #install.packages("XLConnect")
  #  library(XLConnect)
    #integrare tabella iniziale studenti
    #carica basi biologiche dal output della prova preliminare scaricata da SS3
    bb <- loadWorkbook(excel.esame)
    bb.df <- readWorksheet(bb, sheet=1)
    #identifica dove e' la matricola
    matricola.col <- grep("Matricola", bb.df)
    matricola.row <- which(bb.df[, matricola.col]=="Matricola")
    matricola.esame <- as.numeric(bb.df[(matricola.row + 1):dim(bb.df)[1],matricola.col])
    cognome.col <- grep("^Cognome", bb.df[matricola.row,])
    cognome <- bb.df[(matricola.row + 1):dim(bb.df)[1],cognome.col]
    nome.col <- grep("^Nome", bb.df[matricola.row,])
    nome <- bb.df[(matricola.row + 1):dim(bb.df)[1],nome.col]
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
    }else{
      cat("\n N. studenti extra:",length(extra.matricola),"\n")
    }
  }else if(!is.null(voti.studenti)){
    studenti.voti <- read.table(voti.studenti , sep="\t", header=T, stringsAsFactors = F)
    #integrare tabella iniziale studenti
    #carica basi biologiche dal output della prova preliminare scaricata da SS3
    bb <- loadWorkbook(excel.esame)
    bb.df <- readWorksheet(bb, sheet=1)
    #identifica dove e' la matricola
    matricola.col <- grep("Matricola", bb.df)
    matricola.row <- which(bb.df[, matricola.col]=="Matricola")
    matricola.esame <- as.numeric(bb.df[(matricola.row + 1):dim(bb.df)[1],matricola.col])
    cognome.col <- grep("^Cognome", bb.df[matricola.row,])
    cognome <- bb.df[(matricola.row + 1):dim(bb.df)[1],cognome.col]
    nome.col <- grep("^Nome", bb.df[matricola.row,])
    nome <- bb.df[(matricola.row + 1):dim(bb.df)[1],nome.col]
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
    }
    studenti.voti <- rbind(studenti.voti, extra.df)
  }
         
  return(studenti.voti)
  
}

  
  
