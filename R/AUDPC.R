AUDPC = function(time, y, y_proportion, type = "absolute"){
  if (missing(y)) {
    stop(gettextf("Missing 'y' vector"))
  }

  {
    if (missing(time)) {
      stop(gettextf("Missing 'time' vector"))
    }
  }
  if (missing(y_proportion)) {
    stop(gettextf("Missing 'proportion' argument.  If 'y' is provided as proportion set 'proportion == TRUE', if is provided as percentage set 'proportion == FALSE' "))
  }


  if(length(time)!= length(y)){
    stop(gettextf("Number of elements in 'time' and 'y' must agree"))}

  if(y_proportion == T){
    ymax = 1
  }else{
    ymax = 100
  }
  audpc1= auc= max_potential=NULL
  auc = as.numeric(length(y))
  for(i in 1:(length(y)-1)){
    auc[i] = ((y[i]+y[i+1])/2)*(time[i+1]-time[i])
    audpc1 = sum(auc)
  }
  max_potential = ymax * (time[length(time)] - time[1])

  if(type =="absolute"){
    audpc1=audpc1
  }
  if(type =="relative"){
    audpc1 = audpc1/max_potential
  }

  return(audpc1)

}
