\name{ArcheaData}
\alias{archeaVirus}
\docType{data}
\title{
Archea virus genome data.

}
\description{

  Point process data on CRISPR (cluster of regularly interspaced
  palindromic repeats - in archea, crenarchaeal acidothermophiles)
  matches in 7 archea virus
  genomes. Matches have marks according to whether they are on the plus
  or minus strand of the virus genome, and whether the match is a
  protein only or also a nucleotide sequence similarity match. Gene start and end positions are included.

}
\usage{data(archeaVirus)}
\format{

  An object of class \code{\linkS4class{MarkedPointProcess}}
  
}

\details{

  The data set has 2 continuous process variables and 8 marks for 7
  different virus genomes. The
  continuous process variables are mere indicators of the genes on the
  plus and minus strand, respectively. Of the 8 marks the first 4 give
  the matches of the CRISPRs from archea on the virus genome and the
  latter 4 give the gene start and end positions. The information in the
  2 continuous processes is derivable from the latter 4 marks. 

  The motivation for constructing this data set was to investigate whether
  the CRISPRs incorporated into the archea genome are random
  segments from the archea virus genome or if the segments are
  associated with the genes in the virus genome.

  Analyzing the total set of matches, including those matches found at
  the protein sequence level, suggests that
  such an association is present. However, this can potentially be
  explained by a difference in mutation rates for the gene coding versus
  non-coding regions of the virus genome at the protein sequence level.
  Analyzing only the nucleotide sequence similarity matches suggests that
  such an association is not present. At least, not detectable with the
  current data set.
  
}

\references{

  Shiraz Ali Shah, Niels R. Hansen and Roger A. Garrett
  \emph{Distribution of CRISPR spacer matches in viruses 
    and plasmids of crenarchaeal acidothermophiles 
    and implications for their inhibitory mechanism}.
  Biochem. Soc. Trans. (2009) 37, 23-28; doi:10.1042/BST0370023

}

\examples{
data(archeaVirus)

\dontrun{
## Plot of the point process data only
plot(archeaVirus[,-c(1,2)])}
}

\keyword{datasets}
