# set up

library(tidyverse)
library(quanteda)
library(quanteda.dictionaries)
library(quanteda.textmodels)
library(quanteda.textplots)
library(quanteda.textstats)
library(readtext)
# library(easystats)
# require(marginaleffects)
library(betareg)
library(emmeans)
library(data.table)

## Dictionaries ----

restorative_nostalgia_dict <- dictionary(list(
  restorative_nostalgia = c(
    # Historical framing
    "earlier", "once was", "ancestor", "heritage",
    "legacy", "roots", "traditional", "foundation", "forefather",
    "before", "tradition",  "proud", "values",
    "pride", "past", "now", "changes", "used to be", "was",
    "young", "youth", "legacy", "ancestral", "yesterday",
    
    # Idealization/Positive Sentiment About the Past
    "golden", "glorious", "idyllic", "rose-tinted", "home",
    "prosperity", "cherished", "storied", "lost glory", "once proud",
    "better", "authentic", "peaceful", "pleasant", "real country",
    "greatness", "impressive", "lost paradise",
    
    # Negativity About the Present or Future
    "decline", "decay", "losing", "lost", "eroding", "erosion", "moral",
    "falling apart", "unravel", "broken", "astray",
    "worse", "tomorrow", "future", "bittersweet",
    
    # Superiority / group membership
    "our people", "our", "us", "them", "their", "patriots", 
    "group", "we", "real", "outsiders", "majority", "minority", "belong",
    "native", "superior", "inferior",
    
    # Calls to Restore or Revive the Past
    "bring back", "restore", "reclaim", "revive", "take back",
    "great again", "return", "renaissance", "reset", "security", "fix",
    "recapture",
    
    # Terms associated with identity fusion and extremism
    "homeland", "motherland", "fatherland", "family", "loyalty",
    "blood", "brother", "sister", "daughters", "sons", "conspirators",
    "stealing", "stolen", "resistance", "glory", "admire", "bloodline",
    "kin", "traitor", "demolish", "pollute", "scum", "impure", 
    "brainwashed", "subjugate", "overwhelmed", "under siege", "running out of time",
    "betray", "betrayal", "sell", "sold", "conspire", "collude",
    # Martyrdom
    "defend", "protect", "fight", "self-defence", "self-defense", "preserve"
  )
))

restorative_nostalgia_dict_updated <- dictionary(list(
  restorative_nostalgia = c(
    # Historical framing
    "earlier", "once was", "ancestor", "heritage",
    "legacy", "roots", "traditional", "foundation", "forefather",
    "before", "tradition",  "proud", "values",
    "pride", "past", "now", "changes", "used to be", "was",
    "young", "youth", "legacy", "ancestral", "yesterday",
    
    # Idealization/Positive Sentiment About the Past
    "golden", "glorious", "idyllic", "rose-tinted", "home",
    "prosperity", "cherished", "storied", "lost glory", "once proud",
    "better", "authentic", "peaceful", "pleasant", "real country",
    "greatness", "impressive", "lost paradise",
    
    # Negativity About the Present or Future
    "decline", "decay", "losing", "lost", "eroding", "erosion", "moral",
    "falling apart", "unravel", "broken", "astray",
    "worse", "tomorrow", "future", "bittersweet",
    
    # Superiority / group membership
    "our people", "our", "us", "them", "their", "patriots", 
    "group", "we", "real", "outsiders", "majority", "minority", "belong",
    "native", "superior", "inferior",
    
    # Calls to Restore or Revive the Past
    "bring back", "restore", "reclaim", "revive", "take back",
    "great again", "return", "renaissance", "reset", "security", "fix",
    "recapture", "again",
    
    # Terms associated with identity fusion and extremism
    "homeland", "motherland", "fatherland", "family", "loyalty",
    "blood", "brother", "sister", "daughters", "sons", "conspirators",
    "stealing", "stolen", "resistance", "glory", "admire", "bloodline",
    "kin", "traitor", "demolish", "pollute", "scum", "impure", 
    "brainwashed", "subjugate", "overwhelmed", "under siege", "running out of time",
    "betray", "betrayal", "sell", "sold", "conspire", "collude",
    
    # Martyrdom
    "defend", "protect", "fight", "self-defence", "self-defense", "preserve",
    
    # Reference to specific groups
    "white", "black", "africa", "asia", "europe", "western", "eastern", "middle east",
    
    
    # Terms from Bonacchi, Acerbi & Gab Hate Corpus
    "historic", "homeland"
  )
))





## Correlation matrices

# Square correlation matrix
square_correlation_matrix <- function(x, # Input is a data table
                                      method=c("pearson", "spearman"), 
                                      result=c("none", "html","latex")){
  # Compute the correlation matrix
  require(Hmisc) # Package used to calculate correlations
  x <- as.matrix(x) # Converts data table to matrix
  correlation_matrix<-rcorr(x, # Calculate correlation matrix 
                            type=method[1]) # Pearson by default
  R <- correlation_matrix$r # Matrix of correlation coefficients
  p <- correlation_matrix$P # Matrix of p-values
  
  ## Define significance levels, spacing is important
  mystars <- ifelse(p < .001, "*** ", 
                    ifelse(p < .01, "**  ", 
                           ifelse(p < .05, "*   ", "    ")))
  
  ## Truncate the correlation matrix to two decimals
  R <- format(round(cbind(rep(-1.11, ncol(x)), R), 2))[,-1]
  
  ## build a new matrix that maps correlations to stars
  Rnew <- matrix(paste(R, mystars, sep=""), ncol=ncol(x))
  diag(Rnew) <- paste(diag(R)," ", sep="")
  rownames(Rnew) <- colnames(x)
  colnames(Rnew) <- paste(colnames(x), "", sep="")
  Rnew <- as.data.frame(Rnew)
  
  ## Return the correlation matrix
  Rnew <- cbind(Rnew[1:length(Rnew)])
  if (result[1]=="none") return(Rnew)
  else{
    if(result[1]=="html") print(xtable(Rnew), type="html")
    else print(xtable(Rnew), type="latex")
  }
}

# Triangle correlation matrix
triangle_correlation_matrix <-function(x, method=c("pearson", "spearman"), removeTriangle=c("upper", "lower"),
                                       result=c("none", "html", "latex")){
  #Compute correlation matrix
  require(Hmisc)
  x <- as.matrix(x)
  correlation_matrix<-rcorr(x, type=method[1])
  R <- correlation_matrix$r # Matrix of correlation coeficients
  p <- correlation_matrix$P # Matrix of p-value 
  
  ## Define notions for significance levels; spacing is important.
  mystars <- ifelse(p < .001, "*** ", 
                    ifelse(p < .01, "**  ", 
                           ifelse(p < .05, "*   ", "    ")))
  
  ## trunctuate the correlation matrix to two decimal
  R <- format(round(cbind(rep(-1.11, ncol(x)), R), 2))[,-1]
  
  ## build a new matrix that includes the correlations with their appropriate stars
  Rnew <- matrix(paste(R, mystars, sep=""), ncol=ncol(x))
  diag(Rnew) <- paste(diag(R), " ", sep="")
  rownames(Rnew) <- colnames(x)
  colnames(Rnew) <- paste(colnames(x), "", sep="")
  
  ## remove upper triangle of correlation matrix
  if(removeTriangle[1]=="upper"){
    Rnew <- as.matrix(Rnew)
    Rnew[upper.tri(Rnew, diag = TRUE)] <- ""
    Rnew <- as.data.frame(Rnew)
  }
  
  ## remove lower triangle of correlation matrix
  else if(removeTriangle[1]=="lower"){
    Rnew <- as.matrix(Rnew)
    Rnew[lower.tri(Rnew, diag = TRUE)] <- ""
    Rnew <- as.data.frame(Rnew)
  }
  
  ## remove last column and return the correlation matrix
  Rnew <- cbind(Rnew[1:length(Rnew)-1])
  if (result[1]=="none") return(Rnew)
  else{
    if(result[1]=="html") print(xtable(Rnew), type="html")
    else print(xtable(Rnew), type="latex") 
  }
}

## No star correlation matrix 
no_stars_correlation <- function(x,
                                 method=c("pearson","spearman")){
  require(Hmisc)
  x <- as.matrix(x)
  correlation_matrix<-rcorr(x,type=method[1])
  R <- correlation_matrix$r
  R <- format(round(cbind(rep(-1.11, ncol(x)), R), 2))[,-1]
  Rnew <- matrix(paste(R, sep=""), ncol=ncol(x))
  rownames(Rnew) <- colnames(x)
  colnames(Rnew) <- paste(colnames(x), "", sep="")
  Rnew <- as.data.frame(Rnew)
  return(Rnew)
}