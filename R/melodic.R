##MEan DIssimilarity Components##
#samp:  community matrix; sites in lines, species in columns
#dis:   dissimilarity matrix
#type:  "both" for results with abundances weighted and non-weighted
#       "abundance" for results with abundances weighted
#       "presence" for results with abundances non-weighted
  
melodic <- function(samp, dis, type = "both") {
  if (!inherits(samp, "matrix")) samp <- as.matrix(samp)
  if (!inherits(dis, "matrix")) dis <- as.matrix(dis)
  
  N <- dim(samp)[1]
  melodic <- list()
  
  if (type == "both") {
    melodic$abundance <- list(mpd = numeric(N), rao = numeric(N), simpson = numeric(N))
    melodic$presence <- list(mpd = numeric(N), rao = numeric(N), simpson = numeric(N))
  }
  
  if (type == "abundance") { 
    melodic$abundance <- list(mpd = numeric(N), rao = numeric(N), simpson = numeric(N))
  }
  
  if (type == "presence") { 
    melodic$presence <- list(mpd = numeric(N), rao = numeric(N), simpson = numeric(N))
  }
  
  for (i in 1:N) {
    sppInSample <- names(samp[i, samp[i, ] > 0])
    melodic$richness[i] <- length(sppInSample)
    
    if (length(sppInSample) > 1) {
      sample.dis <- dis[sppInSample, sppInSample]
      
      if (type %in% c("both", "abundance")) {
        abund.w <- samp[i, sppInSample] / sum(samp[i, sppInSample])
        sample.weights <- outer(abund.w, abund.w)
        
        melodic$abundance$mpd[i] <- weighted.mean(sample.dis[lower.tri(sample.dis)], sample.weights[lower.tri(sample.weights)])
        melodic$abundance$rao[i] <- sum(sample.weights * sample.dis)
        melodic$abundance$simpson[i] <- sum(2 * sample.weights[lower.tri(sample.weights)])
      }
      
      if (type %in% c("both", "presence")) {
        abund.nw <- rep(1, length(sppInSample)) / length(sppInSample)
        sample.weights <- outer(abund.nw, abund.nw)
        
        melodic$presence$mpd[i] <- weighted.mean(sample.dis[lower.tri(sample.dis)], sample.weights[lower.tri(sample.weights)])
        melodic$presence$rao[i] <- sum(sample.weights * sample.dis)
        melodic$presence$simpson[i] <- sum(2 * sample.weights[lower.tri(sample.weights)])
      }
    } else {
      if (type %in% c("both", "abundance")) {
        melodic$abundance$mpd[i] <- melodic$abundance$rao[i] <- melodic$abundance$simpson[i] <- NA
      }
      if (type %in% c("both", "presence")) {
        melodic$presence$mpd[i] <- melodic$presence$rao[i] <- melodic$presence$simpson[i] <- NA
      }
    }
  }
  
  return(melodic)
}
