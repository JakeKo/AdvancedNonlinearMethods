
# Make formula for machine learning methods  
  
MakeFormula = function(X,dep) {
  Xname = names(X)
  Xname = Xname[Xname != dep]
  ind = paste(Xname, collapse= "+")
  y = paste(dep,' ~ ')
  f = as.formula(paste(y, ind))
  return(f)
}
 
# Scale and unscale data   
  
ScaleUnif = function(x) { 
  return ( (x - min(x)) / (max(x) - min(x)) )
}  

ScaleNorm = function(x) { 
  return ( (x - mean(x)) / sd(x) ) 
}  
  
UnScaleUnif = function(x,s) {
  return ( (max(x) - min(x)) * s + min(x) )
}

UnScaleNorm = function(x,s) {
  mu = mean(x) ; sig = sd(x)
  return ( s * sig + mu )
}
 
# Print  
  
psr = function(s,r,d) {
  rr = format(round(r, d), nsmall=d, big.mark=",", scientific=FALSE)
  cat (paste(s,as.character(rr),sep=' '),"\n")
}

psi = function(s,i) {
  cat (paste(s,as.character(i),sep=' '),"\n")
}

printDim = function(X) {
  print ( c( paste('nrow= ',dim(X)[1]) , paste('  ncol= ',dim(X)[2])))
}

printCor = function(x,y) {  
  psr('correlation: ', cor(x,y)[1], 4 )
}
   
# Numeric error: err vector,  actual - predicted error, print error   
  
sse = function(e) { 
  return ( sum(e*e) )
} 

mse = function(e) { 
  return ( sum(e*e) / length(e) ) 
}  

rmse = function(e) {  
  return ( sqrt(sum(e*e) / length(e) ) ) 
}

ssePrint = function(e) { 
  psr('rms error', sum(e*e)  ,5)
} 

msePrint = function(e) { 
  psr('mse error',  sum(e*e) / length(e), 5)
}  

rmsePrint = function(e) { 
  psr('rms error', sqrt(sum(e*e) / length(e)), 5)
}  
  
sse2 = function(y,t) { 
  return (sum((y-t)^2)) 
} 

mse2 = function(y,t) { 
  return ( sum((y-t)^2) / length(y) ) 
}

rmse2 = function(y,t) { 
  return ( sqrt( sum((y-t)^2) / length(y) ) ) 
}  
  
sse2Print = function(y,t) { 
  psr('sse error',  sum((y-t)^2)   ,5)
} 
  
mse2Print = function(y,t) { 
  psr('mse error', sum((y-t)^2) / length(y)   ,5)
}
  
rmse2Print = function(y,t) { 
  psr('rmse error', sqrt(sum((y-t)^2) / length(y))   ,5)
}  

lineFeed = function(s) { 
  t = paste('\n',s,'\n')
  cat(t)
}
  
# Categorical error:  accuracy table  

acc = function(t,full) {
  rownames(t) = c('act false','act true')
  colnames(t) = c('pred false','pred true')
  print (t)
  TN = t[1] ; FN = t[2]; FP = t[3]; TP = t[4]
  N = t[1] + t[2] + t[3] + t[4]
  ActualFalse = TN + FP ; ActualTrue = FN + TP
  PredictedFalse = TN + FN ; PredictedTrue = FP + TP
  Correct = TN + TP ; InCorrect = FN + FP
  
  print (c("Num of Observ",N))
  print (c("Sensitivity or Recall", round(TP / ActualTrue, 4)))
  print (c("Specificity", round(TN / ActualFalse, 4)))
  print (c("Overall Accuracy",round(Correct / N,4)))
  
  if (full == TRUE)  {
    
    print (c("True Negative",TN))
    print (c("False Negative",FN))
    print (c("False Positive",FP))
    print (c("True Positive",TP))
    
    print (c("Precision", round(TP / PredictedTrue, 4)))
    
    print (c("Overall Error Rate",round(InCorrect / N,4)))
    print (c("False Negative Error Rate", round(FN / ActualTrue, 4)))
    print (c("False Positive Error Rate", round(FP / ActualFalse, 4)))
  }
}
  
# Information error: AIC, BIC, SSE with number of parameters  
  
#  Finding and counting  
  
count = function(x, n){ length((which(x == n))) }

whichIndexMax = function(v) {
  return(which(v == max(v)))
}

whichIndexMin = function(v) {
  return(which(v == min(v)))
}

whichIndex = function(v,e) {
  return(which(v == e ) )
}

findMax = function(v) { 
  ind = whichIndexMax(v)
  return (v[ind])
  }
  
findMin = function(v) { 
  ind = whichIndexMin(v)
  return (v[ind])
}
  
# Colors  

histCol = function() {
  x = c('cornsilk','lemonchiffon','wheat','bisque', 'yellow', 'lightyellow', 
        'ghostwhite','khaki1','gainsboro','seashell','linen','ivory','azure','antiquewhite',
        'beige','moccasin','oldlace','aliceblue','blanchedalmond','floralwhite','gray99',
        'lightgoldenrodyellow','mintcream', "lightblue", "lavenderblush", "lightcyan", "papayawhip") 
  return (sample(x,1))
}

lineCol = function() {
  return (c('navy', 'red', 'green4', 'darkmagenta', 'mediumvioletred', 'darkgoldenrod4', 'purple3'))
}

abCol = function() {
  return (c('aquamarine','chartreuse','darkseagreen1'))
}