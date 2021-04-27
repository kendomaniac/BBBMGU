#' @title Una funzione che aggrega i voti dei moduli generati con aggiungiRisultatiModulo.
#' @description Una funzione che aggrega i voti dei moduli generati con aggiungiRisultatiModulo. Il numero di crediti e' usato per definire quali risultati parziali vanno aggregati.
#' @param aggregati.precedente, output di aggregaVotiModuli dell'ultimo appello precedente a quello odierno.
#' @param aggregati.odierno, il nome del file dell'appello odierno.
#' @param output.voti, il nome del file in cui salvare i risultati.
#' @author Raffaele Calogero, raffaele.calogero [at] unito [dot] it, University of Torino, Italy
#' @return  Il file tab delimited  riempito con i risultati integrati dei due appelli. I voti dell'ultimo appello riscrivono quelli del precedente
#'
#' @examples
#' \dontrun{
#
#' aggregaAppelli(aggregati.precedente= "../canaleA_studenti_votiBBBMGU_aggregati_190214_conDic2018_oralidicunto.txt",
#'                aggregati.odierno="studenti_votiBBBMGU_aggregati.txt", output.voti="../canaleA_studenti_votiBBBMGU_aggregati_2702_conDic2018_oralidicunto.txt")
#' 
#' }
#'
#' @export

aggregaAppelli <- function(aggregati.precedente, aggregati.odierno, output.voti){
  
  #carica risultati di aggiungiRisultatiModulo che include i tre moduli
  precedente <- read.table(aggregati.precedente, sep="\t", header=T, stringsAsFactors = F, quote = "\"")
  #controllare per duplicati
  doppi <- precedente$MATRICOLA[duplicated(precedente$MATRICOLA)]
  if(length(doppi) > 0){
    cat("\nCi sono dei duplicati!\n")
    cat("\n",paste(doppi, collapse = ","),"\n")
    return(1)
  }
  odierno <- read.table(aggregati.odierno, sep="\t", header=T, stringsAsFactors = F, quote = "\"")
  precedente.only <- precedente[which(precedente$MATRICOLA %in% setdiff(precedente$MATRICOLA, odierno$MATRICOLA)),]
  odierno.only <- odierno[which(odierno$MATRICOLA %in% setdiff(odierno$MATRICOLA, precedente$MATRICOLA)),]
  precedente.comune <- precedente[which(precedente$MATRICOLA %in% intersect(precedente$MATRICOLA, odierno$MATRICOLA)),]
  precedente.comune <- precedente.comune[order(precedente.comune$MATRICOLA),]
  odierno.comune <- odierno[which(odierno$MATRICOLA %in% intersect(odierno$MATRICOLA, precedente$MATRICOLA)),]
  odierno.comune <- odierno.comune[order(odierno.comune$MATRICOLA),]
  if(!identical(odierno.comune$MATRICOLA, precedente.comune$MATRICOLA)){
    cat("\nI files:precedente.comune e odierno.comune non hanno le stesse matricole!\nControllare!\n")
    return(2)
  }
  
  #integra i voti nuovi con i vecchi
  .aggiornamento <- function(precedente.comune, odierno.comune){
    precedente.comune.aggiornato <- data.frame(precedente.comune[,1:6], BasiBiologiche=rep(NA,dim(precedente.comune)[1]), BasiBiologicheNorm=rep(NA,dim(precedente.comune)[1]), 
                                             BiologiaMolecolare=rep(NA,dim(precedente.comune)[1]), BiologiaMolecolareNorm=rep(NA,dim(precedente.comune)[1]), 
                                             GeneticaUmana=rep(NA,dim(precedente.comune)[1]), GeneticaUmanaNorm=rep(NA,dim(precedente.comune)[1]),
                                             VotoAggregato=rep(NA,dim(precedente.comune)[1]), stringsAsFactors = F)
    for(i in 1:dim(precedente.comune)[1]){
      precedente.comune.tmp <- precedente.comune[i,]
      odierno.comune.tmp <- odierno.comune[i,]
      #assegno a tutti gli NA in precedente i risultati presenti nelle corrispondenti posizioni in odierno
      precedente.comune.tmp[is.na(precedente.comune.tmp)] <- odierno.comune.tmp[is.na(precedente.comune.tmp)]
      #assegno a tutti i non NA in odierno i risultati di odierno in precedente nelle stesse posizioni
      precedente.comune.tmp[!is.na(odierno.comune.tmp)] <- odierno.comune.tmp[!is.na(odierno.comune.tmp)]
      precedente.comune.aggiornato[i,] <- precedente.comune.tmp
    }
    return(precedente.comune.aggiornato)
  }
  precedente.comune.aggiornato <- .aggiornamento(precedente.comune=precedente.comune, odierno.comune=odierno.comune)
  
  studenti.voti <- rbind(precedente.comune.aggiornato, precedente.only, odierno.only)
  
  #aggiungi l'esito al file di aggregazione
  for(i in 1:dim(studenti.voti)[1]){
    cat("\n studente in aggiornamento:",i,"\n")
    if(sum(!is.na(studenti.voti[i,7:13]))!=7){
    if(!is.na(studenti.voti$CFU[i]) && studenti.voti$CFU[i] == 15){
      if(!is.na(studenti.voti[i,7]) && !is.na(studenti.voti[i,9]) && !is.na(studenti.voti[i,11])){
        if(min(as.numeric(studenti.voti[i,c(7,9,11)])) >= 18){
          studenti.voti$VotoAggregato[i] <- as.character(round(as.numeric(studenti.voti$BasiBiologicheNorm[i]) + as.numeric(studenti.voti$BiologiaMolecolareNorm[i]) + as.numeric(studenti.voti$GeneticaUmanaNorm[i])))
        }
      }
    }else if(!is.na(studenti.voti$CFU[i]) && studenti.voti$CFU[i] == 13){
      #BB e BM da fare
      if(!is.na(studenti.voti[i,7]) && !is.na(studenti.voti[i,9]) && is.na(studenti.voti[i,11])){
        if(min(as.numeric(studenti.voti[i,c(7,9)])) >= 18){
          bbp <- 8/13*100
          BasiBiologicheNorm <- as.numeric(studenti.voti$BasiBiologiche[i])*bbp/100
          studenti.voti$BasiBiologicheNorm[i] <- BasiBiologicheNorm
          bmp <- 5/13*100
          BiologiaMolecolareNorm <- as.numeric(studenti.voti$BiologiaMolecolare[i])*bmp/100
          studenti.voti$BiologiaMolecolareNorm[i] <- BiologiaMolecolareNorm
          studenti.voti$VotoAggregato[i] <- as.character(round(BasiBiologicheNorm + BiologiaMolecolareNorm))
        }
      }
    }else if(!is.na(studenti.voti$CFU[i]) && studenti.voti$CFU[i] == 10){
      #BB e GU da fare
      if(!is.na(studenti.voti[i,7]) && is.na(studenti.voti[i,9]) && !is.na(studenti.voti[i,11])){
        if(min(as.numeric(studenti.voti[i,c(7,11)])) >= 18){
          bbp <- 8/10*100
          BasiBiologicheNorm <- as.numeric(studenti.voti$BasiBiologiche[i])*bbp/100
          studenti.voti$BasiBiologicheNorm[i] <- BasiBiologicheNorm
          gup <- 2/10*100
          GeneticaUmanaNorm <- as.numeric(studenti.voti$GeneticaUmana[i])*gup/100
          studenti.voti$GeneticaUmanaNorm[i] <- GeneticaUmanaNorm
          studenti.voti$VotoAggregato[i] <- as.character(round(BasiBiologicheNorm + GeneticaUmanaNorm))
        }
      }
    }else if(!is.na(studenti.voti$CFU[i]) && studenti.voti$CFU[i] == 7){
      #BM e GU da fare
      if(is.na(studenti.voti[i,7]) && !is.na(studenti.voti[i,9]) && !is.na(studenti.voti[i,11])){
        if(min(as.numeric(studenti.voti[i,c(9,11)])) >= 18){
          bmp <- 5/7*100
          BiologiaMolecolareNorm <- as.numeric(studenti.voti$BiologiaMolecolare[i])*bmp/100
          studenti.voti$BiologiaMolecolareNorm[i] <- BiologiaMolecolareNorm
          gup <- 2/7*100
          GeneticaUmanaNorm <- as.numeric(studenti.voti$GeneticaUmana[i])*gup/100
          studenti.voti$GeneticaUmanaNorm[i] <- GeneticaUmanaNorm
          studenti.voti$VotoAggregato[i] <- as.character(round(BiologiaMolecolareNorm + GeneticaUmanaNorm))
        }
      }
    }else if(!is.na(studenti.voti$CFU[i]) && studenti.voti$CFU[i] == 11){
      #BB e GU da fare
      if(!is.na(studenti.voti[i,7]) && is.na(studenti.voti[i,9]) && !is.na(studenti.voti[i,11])){
        if(min(as.numeric(studenti.voti[i,c(7,11)])) >= 18){
          bbp <- 8/11*100
          BasiBiologicheNorm <- as.numeric(studenti.voti$BasiBiologiche[i])*bbp/100
          studenti.voti$BasiBiologicheNorm[i] <- BasiBiologicheNorm
          gup <- 3/11*100
          GeneticaUmanaNorm <- as.numeric(studenti.voti$GeneticaUmana[i])*gup/100
          studenti.voti$GeneticaUmanaNorm[i] <- GeneticaUmanaNorm
          studenti.voti$VotoAggregato[i] <- as.character(round(BasiBiologicheNorm + GeneticaUmanaNorm))
        }
      }
    }else if(!is.na(studenti.voti$CFU[i]) && studenti.voti$CFU[i] == 3){
      #GU da fare 
      if(is.na(studenti.voti[i,7]) && is.na(studenti.voti[i,9]) && !is.na(studenti.voti[i,11])){
        if(as.numeric(studenti.voti[i,11]) >= 18){
          studenti.voti$VotoAggregato[i] <- as.character(round(as.numeric(studenti.voti$GeneticaUmana[i])))
          studenti.voti$GeneticaUmanaNorm[i] <- as.character(round(as.numeric(studenti.voti$GeneticaUmana[i])))
        }
      }
    }else if(!is.na(studenti.voti$CFU[i]) && studenti.voti$CFU[i] == 2){
      #GU da fare 
      if(is.na(studenti.voti[i,7]) && is.na(studenti.voti[i,9]) && !is.na(studenti.voti[i,11])){
        if(as.numeric(studenti.voti[i,11]) >= 18){
          studenti.voti$VotoAggregato[i] <- as.character(round(as.numeric(studenti.voti$GeneticaUmana[i])))
          studenti.voti$GeneticaUmanaNorm[i] <- as.character(round(as.numeric(studenti.voti$GeneticaUmana[i])))
        }
      }
    }else if(!is.na(studenti.voti$CFU[i]) && studenti.voti$CFU[i] == 5){
      #BM da fare
      if(is.na(studenti.voti[i,7]) && !is.na(studenti.voti[i,9]) && is.na(studenti.voti[i,11])){
        if(as.numeric(studenti.voti[i,9]) >= 18){
          studenti.voti$VotoAggregato[i] <- as.character(round(as.numeric(studenti.voti$BiologiaMolecolare[i])))
          studenti.voti$BiologiaMolecolareNorm[i] <- as.character(round(as.numeric(studenti.voti$BiologiaMolecolare[i])))
        }
      }
    }else if(!is.na(studenti.voti$CFU[i]) && studenti.voti$CFU[i] == 8){
      #BB da fare
      if(!is.na(studenti.voti[i,7]) && is.na(studenti.voti[i,9]) && is.na(studenti.voti[i,11])){
        if(min(as.numeric(studenti.voti[i,7])) >= 18){
          studenti.voti$VotoAggregato[i] <- as.character(round(as.numeric(studenti.voti$BasiBiologiche[i])))
          studenti.voti$BasiBiologicheNorm[i] <- as.character(round(as.numeric(studenti.voti$BasiBiologiche[i])))
        }
      }else if(is.na(studenti.voti[i,7]) && !is.na(studenti.voti[i,9]) && !is.na(studenti.voti[i,11])){
        if(min(as.numeric(studenti.voti[i,c(9,11)])) >= 18){
          #situazione strana in cuisommo BM che e' 5 crediti BM e 3 per GU invece degli 8 dati a BB
          bmp <- 5/8*100
          BiologiaMolecolareNorm <- as.numeric(studenti.voti$BiologiaMolecolare[i])*bmp/100
          studenti.voti$BiologiaMolecolareNorm[i] <- BiologiaMolecolareNorm
          gup <- 3/8*100
          GeneticaUmanaNorm <- as.numeric(studenti.voti$GeneticaUmana[i])*gup/100
          studenti.voti$GeneticaUmanaNorm[i] <- GeneticaUmanaNorm
          studenti.voti$VotoAggregato[i] <- as.character(round(BiologiaMolecolareNorm + GeneticaUmanaNorm))
        }
      }
    }else if(!is.na(studenti.voti$CFU[i]) && studenti.voti$CFU[i] != 15){
      studenti.voti$VotoAggregato[i] <- "da controllare!"
    }
   }
  }
  
  # ricalcola voto aggregato
  cat("\nRicalcola voto integrato\n")
  for(i in 1:dim(studenti.voti)[1]){
    if(!is.na(studenti.voti$VotoAggregato[i])){
      studenti.voti[i,dim(studenti.voti)[2]] <- sum(as.numeric(studenti.voti$BasiBiologicheNorm[i]), as.numeric(studenti.voti$BiologiaMolecolareNorm[i]), as.numeric(studenti.voti$GeneticaUmanaNorm[i]), na.rm = T)
    }
  }
  
  #scrivi risultati del voto aggregato
  write.table(studenti.voti, output.voti, sep="\t", col.names = T, row.names=F)
}


