# --------------------------------------------------------------------------
# LHrD.R
# 
# Procedure to create a latin hyper-rectangle design (LHrD) of experiments.
# Default function arguments will produce an example design if outputLHrD()
# is run from R prompt. This repository was created to provide these 
# functions as a reference for Johnson and Cox, 2017, given below.
# 
# List of functions:
# 
# makeLHRfromList():  Creates a latin hyper-rectangle corresponding to a
#                     named list of factor levels, and samples a design
#                     from that latin hyper-rectangle
# 
# outputLHrD():       Takes the output from makeLHRfromList() and produces
#                     a tabulated experimental design as a data.frame and
#                     as ./LHrD.csv
# 
# 
# Authors: Samuel D. N. Johnson and Sean P. Cox
# Date: 30 August, 2017
# 
# References:
# 
# S. D. N. Johnson and S. P. Cox, Simulation testing the Robin Hood approach
#     to stock assessment: Should we steal from the data rich and give to
#     the data poor? In submission, 2017.
#
# J. P. Kleijnen. Design and analysis of simulation experiments, volume 20. 
#     Springer, 2008.
# 
# --------------------------------------------------------------------------


outputLHrD <- function( LHR = makeLHRfromList() )
{
  # Takes a latin hyper rectangular design built from a list of
  # factor levels and produces a data.frame of the treatments.
  # inputs:     LHR = list of outputs from makeLHRfromList
  # ouptuts:    treatments = a data.frame of treatments with levels explicitly
  #                           recorded
  # side-effs:  treatments written to ./LHrD.csv
  treatments  <- LHR$treatments
  levels      <- LHR$levels
  row.names(treatments) <- NULL

  treatments <- as.data.frame(treatments)
  for( k in 1:nrow(treatments) )
  {
    for( l in 1:length(names(levels)))
    {
      nom       <- names(treatments)[l]
      levelIdx  <- as.numeric(treatments[k, nom])
      treatments[ k, nom ] <- levels[nom][[1]][levelIdx]
    }
  }

  write.csv( x = treatments, file = "LHrD.csv" )

  treatments
}


makeLHRfromList <- function(  levels = list(  Uhist = c("c(0.2,2,1)","c(1,1,1)"),
                                              initYear = c(1984,2003),
                                              nS = c(4,7,10),
                                              initDep = c(0.4,0.7,1.0),
                                              nDiff = c(0,1,2,3) ),
                              nPoints = 1
                            )
{
  # Creates a Latin Hyper Rectangle experimental design from a list of factor levels
  # inputs:     levels = named list of factor levels
  #             nPoints = number of points to sample from the LHR design
  # outputs:    levels = same as input
  #             designArray = base latin-hyper rectangle for design
  #             treatments = matrix of factor level indices for sampled treatments

  # First, create an array to hold the design
  # First, we need the number of factors and their levels to make dimensions
  nLevels     <- lapply ( X = levels, FUN = length )
  nLevels     <- unlist( nLevels )
  nFactors    <- length( nLevels )

  # Create the array, using the factor levels as dimnames
  LHRdesign   <- array( NA, dim = nLevels, dimnames = levels )

  # Choose the entries in the array as the size of the largest dimension
  maxEntry  <- max(nLevels)

  # I think adding the entry dimension indices mod maxEntry will
  # populate the matrix and preserve the latin property. To do this
  # we gotta expand.grid for all possible entries
  dimIndices <- vector(mode = "list", length = nFactors )
  for( lIdx in 1:nFactors )
  {
    dimIndices[[lIdx]] <- 1:nLevels[lIdx]
  }
  entryIndices <- expand.grid( dimIndices )

  # Loop over the entryIndices data.frame and pull the rows
  for( rIdx in 1:nrow(entryIndices) )
  {
    entryIdx <- as.numeric(entryIndices[rIdx,])
    LHRdesign[matrix(entryIdx,nrow=1)] <- (sum(entryIdx) %% maxEntry)
  }

  # Now randomly permute the dimensions
  idx <- lapply(dim(LHRdesign), sample)
  LHRdesign[] <- do.call(`[`, c( list(LHRdesign), idx) )

  # Now sample design space for treatments
  points      <- sample( x = 0:(maxEntry-1), size = nPoints )
  treatments  <- which( LHRdesign %in% points, arr.ind = F )
  treatments  <- arrayInd(  ind = treatments, .dim = dim(LHRdesign), 
                            .dimnames = dimnames(LHRdesign), useNames = T)

  out <- list(  levels = levels,
                designArray = LHRdesign,
                treatments =  treatments
              )

  out

}

