\name{champ.import}
\alias{champ.import}

\title{
    Read data from IDAT file.
}
\description{
    A Start Function MD
}
\usage{
champ.import(directory = getwd(),
             offset=100,
             arraytype="450K")
}

\arguments{
	
	\item{directory}{Location of IDAT files, default is current working directory.(default = getwd())}
	\item{offset}{offset is set to make sure no inf value would be returned.(default = 100)}
	\item{arraytype}{Choose microarray type is "450K" or "EPIC".(default = "450K")}
}

\value{
	\item{beta}{A matrix of beta methylation scores for all probes and all samples (No filtering has been don).}
	\item{M}{A matrix of M methylation scores for all probes and all samples (No filtering has been done).}
	\item{pd}{pd file of all sample information from Sample Sheet, which would be very frequently by following functions as DEFAULT input, thus it's not very necessarily, please don't modify it.}
	\item{intensity}{A matrix of intensity values for all probes and all samples, the information would be used in champ.CNA() function. It has not been filtered. Actually, intensity are the sum of Meth Matrix and UnMeth Matrix.}
	\item{detP}{A matrix of detection p-values for all probes and all samples.}
	\item{beadcount}{A matrix beads for each probe on each sample. Value less then 3 has been set NA.}
	\item{Meth}{Methylated Matrix for all probe and all samples.}
	\item{UnMeth}{UnMethylated Matrix for all probe and all samples.}
}
\examples{
    \dontrun{
        myimport <- champ.import(directory=system.file("extdata",package="ChAMPdata"))
    }
}

\author{
Yuan Tian
}
