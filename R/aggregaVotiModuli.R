#' @title Una funzione che aggrega i voti dei moduli generati con aggiungiRisultatiModulo.
#' @description Una funzione che aggrega i voti dei moduli generati con aggiungiRisultatiModulo. Il numero di crediti e' usato per definire quali risultati parziali vanno aggregati.
#' @param input.voti, output di aggiungiRisultatiModulo con i risultati dei tre moduli.
#' @param output.voti, il nome del file in cui salvar ei risultati.
#' @author Raffaele Calogero, raffaele.calogero [at] unito [dot] it, University of Torino, Italy
#' @return  Il file tab delimited  riempito con i risultati di uno dei tre moduli e con il voto finale aggregato.
#'
#' @examples
#' \dontrun{
#
#' aggregaVotiModuli(input.voti="studenti_votiBBBMGU.txt", 
#'                   output.voti="studenti_votiBBBMGU_aggregati.txt")
#' 
#' }
#'
#' @export

aggregaVotiModuli <- function(input.voti="studenti_votiBBBMGU.txt", output.voti="studenti_votiBBBMGU_aggregati.txt"){
  
  #carica risultati di aggiungiRisultatiModulo che include i tre moduli
  studenti.voti <- read.table(input.voti, sep="\t", header=T, stringsAsFactors = F, quote = "\"")
  
  #aggiungi l'esito al file di aggregazione
  for(i in 1:dim(studenti.voti)[1]){
    if(!is.na(studenti.voti$CFU[i]) && studenti.voti$CFU[i] == 15){
      if(!is.na(studenti.voti[i,7]) && !is.na(studenti.voti[i,9]) && !is.na(studenti.voti[i,11])){
        if(min(studenti.voti[i,c(7,9,11)]) >= 18){
              studenti.voti$VotoAggregato[i] <- as.character(round(studenti.voti$BasiBiologicheNorm[i] + studenti.voti$BiologiaMolecolareNorm[i] + studenti.voti$GeneticaUmanaNorm[i]))
        }
       }
    }else if(!is.na(studenti.voti$CFU[i]) && studenti.voti$CFU[i] == 13){
      #BB e BM da fare
      if(!is.na(studenti.voti[i,7]) && !is.na(studenti.voti[i,9]) && is.na(studenti.voti[i,11])){
        if(min(studenti.voti[i,c(7,9)]) >= 18){
          bbp <- 8/13*100
          BasiBiologicheNorm <- studenti.voti$BasiBiologiche[i]*bbp/100
          bmp <- 5/13*100
          BiologiaMolecolareNorm <- studenti.voti$BiologiaMolecolare[i]*bmp/100
          studenti.voti$VotoAggregato[i] <- as.character(round(BasiBiologicheNorm + BiologiaMolecolareNorm))
        }
      }
    }else if(!is.na(studenti.voti$CFU[i]) && studenti.voti$CFU[i] == 10){
      #BB e GU da fare
      if(!is.na(studenti.voti[i,7]) && is.na(studenti.voti[i,9]) && !is.na(studenti.voti[i,11])){
        if(min(studenti.voti[i,c(7,11)]) >= 18){
          bbp <- 8/10*100
          BasiBiologicheNorm <- studenti.voti$BasiBiologiche[i]*bbp/100
          gup <- 2/10*100
          GeneticaUmanaNorm <- studenti.voti$GeneticaUmana[i]*gup/100
          studenti.voti$VotoAggregato[i] <- as.character(round(BasiBiologicheNorm + GeneticaUmanaNorm))
        }
      }
    }else if(!is.na(studenti.voti$CFU[i]) && studenti.voti$CFU[i] == 7){
      #BM e GU da fare
      if(is.na(studenti.voti[i,7]) && !is.na(studenti.voti[i,9]) && !is.na(studenti.voti[i,11])){
        if(min(studenti.voti[i,c(9,11)]) >= 18){
            bmp <- 5/7*100
            BiologiaMolecolareNorm <- studenti.voti$BiologiaMolecolare[i]*bmp/100
            gup <- 2/7*100
            GeneticaUmanaNorm <- studenti.voti$GeneticaUmana[i]*gup/100
            studenti.voti$VotoAggregato[i] <- as.character(round(BiologiaMolecolareNorm + GeneticaUmanaNorm))
        }
      }
    }else if(!is.na(studenti.voti$CFU[i]) && studenti.voti$CFU[i] == 11){
      #BB e GU da fare
      if(!is.na(studenti.voti[i,7]) && is.na(studenti.voti[i,9]) && !is.na(studenti.voti[i,11])){
        if(min(studenti.voti[i,c(7,11)]) >= 18){
            bbp <- 8/11*100
            BasiBiologicheNorm <- studenti.voti$BasiBiologiche[i]*bbp/100
            gup <- 3/11*100
            GeneticaUmanaNorm <- studenti.voti$GeneticaUmana[i]*gup/100
            studenti.voti$VotoAggregato[i] <- as.character(round(BasiBiologicheNorm + GeneticaUmanaNorm))
        }
      }
    }else if(!is.na(studenti.voti$CFU[i]) && studenti.voti$CFU[i] == 3){
      #GU da fare 
      if(is.na(studenti.voti[i,7]) && is.na(studenti.voti[i,9]) && !is.na(studenti.voti[i,11])){
        if(studenti.voti[i,11] >= 18){
            studenti.voti$VotoAggregato[i] <- as.character(round(studenti.voti$GeneticaUmana[i]))
        }
      }
    }else if(!is.na(studenti.voti$CFU[i]) && studenti.voti$CFU[i] == 2){
      #GU da fare 
      if(is.na(studenti.voti[i,7]) && is.na(studenti.voti[i,9]) && !is.na(studenti.voti[i,11])){
        if(studenti.voti[i,11] >= 18){
            studenti.voti$VotoAggregato[i] <- as.character(round(studenti.voti$GeneticaUmana[i]))
        }
      }
    }else if(!is.na(studenti.voti$CFU[i]) && studenti.voti$CFU[i] == 5){
      #BM da fare
      if(is.na(studenti.voti[i,7]) && !is.na(studenti.voti[i,9]) && is.na(studenti.voti[i,11])){
        if(studenti.voti[i,9] >= 18){
            studenti.voti$VotoAggregato[i] <- as.character(round(studenti.voti$BiologiaMolecolare[i]))
        }
      }
    }else if(!is.na(studenti.voti$CFU[i]) && studenti.voti$CFU[i] == 8){
      #BB da fare
      if(!is.na(studenti.voti[i,7]) && is.na(studenti.voti[i,9]) && is.na(studenti.voti[i,11])){
        if(min(studenti.voti[i,7]) >= 18){
            studenti.voti$VotoAggregato[i] <- as.character(round(studenti.voti$BasiBiologiche[i]))
        }
      }else if(is.na(studenti.voti[i,7]) && !is.na(studenti.voti[i,9]) && !is.na(studenti.voti[i,11])){
        if(min(studenti.voti[i,c(9,11)]) >= 18){
          #situazione strana in cuisommo BM che e' 5 crediti BM e 3 per GU invece degli 8 dati a BB
          bmp <- 5/8*100
          BiologiaMolecolareNorm <- studenti.voti$BiologiaMolecolare[i]*bmp/100
          gup <- 3/8*100
          GeneticaUmanaNorm <- studenti.voti$GeneticaUmana[i]*gup/100
          studenti.voti$VotoAggregato[i] <- as.character(round(BiologiaMolecolareNorm + GeneticaUmanaNorm))
        }
      }
    }else if(!is.na(studenti.voti$CFU[i]) && studenti.voti$CFU[i] != 15){
      studenti.voti$VotoAggregato[i] <- "da controllare!"
    }
  }
  #scrivi risultati del voto aggregato
  write.table(studenti.voti, output.voti, sep="\t", col.names = T, row.names=F)
}

