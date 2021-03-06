% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/aggregaVotiModuli.R
\name{aggregaVotiModuli}
\alias{aggregaVotiModuli}
\title{Una funzione che aggrega i voti dei moduli generati con aggiungiRisultatiModulo.}
\usage{
aggregaVotiModuli(
  input.voti = "studenti_votiBBBMGU.txt",
  output.voti = "studenti_votiBBBMGU_aggregati.txt"
)
}
\arguments{
\item{input.voti, }{output di aggiungiRisultatiModulo con i risultati dei tre moduli.}

\item{output.voti, }{il nome del file in cui salvare i risultati.}
}
\value{
Il file tab delimited  riempito con i risultati di uno dei tre moduli e con il voto finale aggregato.
}
\description{
Una funzione che aggrega i voti dei moduli generati con aggiungiRisultatiModulo. Il numero di crediti e' usato per definire quali risultati parziali vanno aggregati.
}
\examples{
\dontrun{
aggregaVotiModuli(input.voti="studenti_votiBBBMGU.txt", 
                  output.voti="studenti_votiBBBMGU_aggregati.txt")

}

}
\author{
Raffaele Calogero, raffaele.calogero [at] unito [dot] it, University of Torino, Italy
}
