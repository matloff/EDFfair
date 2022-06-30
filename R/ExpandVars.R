# 
# ExpandVars() arguments: 
#   data: dataframe, raw datasets
#   Cvar: variable C in X that could be correlated with sensitive variable S
#   Weight: weight set to the cordinates corresponding to variable C, ranging from  0~1, 1 represent no modification to variable C
# 
#   
# values:
#   expand$val: expanded level names of C variable
#   expand$var: corresponding explansion values(weights)
  
  


ExpandVars <- function( data,Cvar,Weight)
{
  expandVal <- NULL
  expand <- NULL
  expandvar<- NULL
  lvls <- NULL
  for (C in 1:length(Cvar)){
    if (is.factor(data[,Cvar[C]])){
      lvls <- levels(data[,Cvar[C]])
      for (i in 1:length(lvls)){
        expandvar <- c (expandvar,paste0(Cvar[C],'.',lvls[i]))
        }
      if (length(lvls) > 1){
        expandVal <- c(expandVal,rep(Weight[C],length(lvls)))
      } else{
        expandVal <- c(expandVal,Weight[C])
            }
    } else{
        expandvar <- c(expandvar,Cvar[C])
        expandVal <- c(expandVal,Weight[C])
      }
    
  }
  expand$val <- expandVal
  expand$var <- expandvar
  expand
}
