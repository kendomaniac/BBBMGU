% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/aggregaAppelli.R
\name{aggregaAppelli}
\alias{aggregaAppelli}
\title{Una funzione che aggrega i voti dei moduli generati con aggiungiRisultatiModulo.}
\usage{
aggregaAppelli(aggregati.precedente, aggregati.odierno, output.voti)
}
\arguments{
\item{aggregati.precedente, }{output di aggregaVotiModuli dell'ultimo appello precedente a quello odierno.}

\item{aggregati.odierno, }{il nome del file dell'appello odierno.}

\item{output.voti, }{il nome del file in cui salvare i risultati.}
}
\value{
Il file tab delimited  riempito con i risultati integrati dei due appelli. I voti dell'ultimo appello riscrivono quelli del precedente
}
\description{
Una funzione che aggrega i voti dei moduli generati con aggiungiRisultatiModulo. Il numero di crediti e' usato per definire quali risultati parziali vanno aggregati.
}
\examples{
\dontrun{
aggregaAppelli(aggregati.precedente= "../canaleA_studenti_votiBBBMGU_aggregati_190214_conDic2018_oralidicunto.txt",
               aggregati.odierno="studenti_votiBBBMGU_aggregati.txt", output.voti="../canaleA_studenti_votiBBBMGU_aggregati_2702_conDic2018_oralidicunto.txt")

}

}
\author{
Raffaele Calogero, raffaele.calogero [at] unito [dot] it, University of Torino, Italy
}
