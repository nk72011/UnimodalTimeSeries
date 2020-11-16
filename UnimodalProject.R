###Project unimodalTimeSeries
###Definiowanie konstruktora
unimodalTimeSeries<-function(p_dates, p_values, p_unit, p_name){
  value<-list(dates=p_dates, values=p_values, unit=p_unit, name=p_name)
  attr(value, "class")<-"unimodalTimeSeries"
  value
}
###Tworzenie funkcji print
print.unimodalTimeSeries<-function(object){
  
  cat("Object of class", " ")
  cat(class(object), "\n")
  cat("Start date", " ")
  cat(as.character(object$dates[1]), "\n")
  cat("End date", " ")
  cat(as.character(object$dates[length(object$dates)]), "\n")
  cat("   ", "dates", "  ", "values", "\n")
  for (var in 1:length(object$dates))
  {
    cat(var, " ")
    cat(as.character(object$dates[var]), " ")
    cat(object$values[var], "\n")
    
  }
  
}
### Tworzenie przykładowych obiektów
n <- 100
a <- unimodalTimeSeries(
  p_dates = as.Date( Sys.Date()) + cumsum( rbinom( n, 10, .5)),
  p_values = cumsum( rnorm( n)),
  p_unit = "USD",
  p_name = "Price of good"
)

a

b <- unimodalTimeSeries(
  p_dates = as.Date( Sys.Date()) + cumsum( rbinom( 9, 10, .5)) + 30,
  p_values = cumsum( rnorm( 9)) + 10,
  p_unit = "USD",
  p_name = "Price of some other good")

b

###Definiowanie funkcji  gettera dates i values
get_dates<-function(object){
  for (var in 1:length(object$dates)){
    z<-var%%5
    if(z==1){
      cat("[", "")
      cat(var, "")
      cat("]", "")
    }
    cat(as.character(object$dates[var]), " ")
    if (z==0){
      cat("","\n")
    }
    
  }
  
}
get_dates(a)

get_values<-function(object){
  for (var in 1:length(object$values)){
    z<-var%%5
    
    if (z==1){
      cat("[", "")
      cat(var, "")
      cat("]", "")
    }
    cat(as.character(object$values[var]), " ")
    if (z==0){
      cat("","\n")
    }
    
  }
  
}
get_values(a)

###Tworzenie funkcji timeSpan
timeSpan<-function(object, mode){
  cat("[1]", " ")
  cat(as.character(object$dates[1]), " ")
  cat(as.character(object$dates[length(object$dates)]), "\n")
  cat("Time difference of", " ")
  if ( mode == 'days'){
    
    cat(as.character(floor((object$dates[length(object$dates)]-object$dates[1])))," ")
    cat(mode)
  }
  else if(mode == 'years'){
    
    days<-as.numeric(object$dates[length(object$dates)]-object$dates[1])
    years<-floor(days/365)
    
    reszta<- days%%365
    
    cat(years, " ")
    cat(mode, " ")
    cat(reszta, " ")
    cat("days", " ")
  }
}
timeSpan(a, mode="years")
timeSpan(a, mode="days")

###Tworzenie funkcji range
range <- function(object, object2) {
  UseMethod("range")
}

range.unimodalTimeSeries<-function(object, object2){
  if (missing(object2)){
    cat(min(object$values), " ")
    cat(max(object$values), "\n")
    result<-as.vector(list(min(object$values), max(object$values)), mode="numeric")
    
  }else{
    cat(min(min(object$values), min(object2$values)), " ")
    cat(max(max(object$values), max(object2$values)), "\n")
    min_val<-min(min(object$values), min(object2$values))
    max_val<-max(max(object$values), max(object2$values))
    result<-as.vector(list(min_val,max_val),mode = "numeric")
    
   
  }
  return(result)
}
range(a)
range(b)
range(a, b)

###Tworzenie funkcji [
'[.unimodalTimeSeries' <- function(object, param, ...) {
  UseMethod('[')
}

'[.unimodalTimeSeries'<- function(object, param, ...){
  d<-unlist(strsplit(x=gsub('"', '', deparse(substitute(param))), split = ':', fixed=TRUE))
  isDate1<-as.numeric(grep("[1-2][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]",d[1],value = F))
  isDate2<-as.numeric(grep("[1-2][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]",d[2],value = F))
  isLetter1<-grepl("[A-Za-z]", d[1])
  isLetter2<-grepl("[A-Za-z]", d[2])
  
  ###Pierwszy przypadek obie są datą  
  if (length(isDate1)==1 & length(isDate2)==1){
    days <- seq(from=as.Date(d[1]), to=as.Date(d[2]),by='days' )
    days_rev <- rev(seq(from=as.Date(d[1]), to=as.Date(d[2]),by='days' ))
    first_indx <-0
    last_indx <-0
    for ( i in seq_along(days) ){
      first_indx<-as.numeric(grep(days[i], object$dates, value = F))
      if (length(first_indx) > 0){
        break;
      }
    }
    for ( i in seq_along(days_rev) ){
      last_indx<-as.numeric(grep(days_rev[i], object$dates, value = F))
      if (length(last_indx) > 0){
        break;
      }
    }
    cat("Object of class", " ")
    cat(class(object), "\n")
    cat("Start date", " ")
    cat(as.character(object$dates[first_indx]), "\n")
    cat("End date", " ")
    cat(as.character(object$dates[last_indx]), "\n")
    cat("   ", "dates", "  ", "values", "\n")
    
    for (i in first_indx:last_indx){
      cat(as.character(object$dates[i]), " ")
      cat(as.character(object$values[i]), "\n")
    }
    result<- unimodalTimeSeries(
      p_dates = as.Date(c(a$dates[first_indx:last_indx]),origin="1970-01-01"),
      p_values = c(a$values[first_indx:last_indx]),
      p_unit = "USD",
      p_name = "Price of some good")
  }
  ###Drugi przypadek piersza data druga zmienna/cyfra 
  else if (length(isDate1)==1 & length(isDate2)==0){
    if (isLetter2){
      variable_name2 <- d[2]
      variable_value2 <- as.character(eval(parse(text=variable_name2)))
      cat(variable_value2, " ")
      isDateLetter2 <- as.numeric(grep("[1-2][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]",variable_value2,value = F))
      
      if(length(isDateLetter2)>0){
        cat("Drugi parametr to zmienna datowa", "\n")
        days_rev <- rev(seq(from=as.Date(d[1]), to=as.Date(variable_value2),by='days' ))
        for ( i in seq_along(days_rev) ){
          to_indx<-as.numeric(grep(days_rev[i], object$dates, value = F))
          if (length(to_indx) > 0){
            break;
          }
        }
      }else{
        to_indx<- eval(parse(text=variable_name2))
      }
    }
    else{
      to_indx <- as.numeric(d[2])
    }
    second_date<-object$dates[as.numeric(to_indx)]
    sec_date_char <- as.character(second_date)
    days <- seq(from=as.Date(as.character(d[1])), to=as.Date(sec_date_char),by='days' )
    for ( i in seq_along(days) ){
      first_indx<-as.numeric(grep(days[i], object$dates, value = F))
      if (length(first_indx) > 0){
        break;
      }
    }
    cat("Object of class", " ")
    cat(class(object), "\n")
    cat("Start date", " ")
    cat(as.character(object$dates[first_indx]), "\n")
    cat("End date", " ")
    cat(as.character(object$dates[as.numeric(to_indx)]), "\n")
    cat("   ", "dates", "  ", "values", "\n")
    
    for (i in first_indx:as.numeric(to_indx)){
      cat(as.character(object$dates[i]), " ")
      cat(as.character(object$values[i]), "\n")
    }
    result<- unimodalTimeSeries(
      p_dates = as.Date(c(a$dates[first_indx:to_indx]),origin="1970-01-01"),
      p_values = c(a$values[first_indx:to_indx]),
      p_unit = "USD",
      p_name = "Price of some good")
  }
  ###Trzeci przypadek pierwsza zmienna/cyfra druga data
  else if (length(isDate1)==0 & length(isDate2) ==1){
    if (isLetter1) {
      variable_name1<-d[1]
      variable_value1<- as.character(eval(parse(text = variable_name1)))
      isDateLetter1<-as.numeric(grep("[1-2][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]", variable_value1, value = F))
      
      if(length(isDateLetter1)>0){
        days <- seq(from=as.Date(variable_value1), to=as.Date((d[2])),by='days')
        for ( i in seq_along(days) ){
          from_indx<-as.numeric(grep(days[i], object$dates, value = F))
          if (length(from_indx) > 0){
            break;
          }
        }
      }else{
        from_indx<-eval(parse(text=variable_name1))
      }
    }
    else{
      from_indx <- as.numeric(d[1])
    }
    first_date<-object$dates[as.numeric(from_indx)]
    fir_date_char <- as.character(first_date)
    
    days_rev <- rev(seq(from=as.Date(fir_date_char), to=as.Date(as.character(d[2])),by='days' ))
    
    for ( i in seq_along(days_rev) ){
      last_indx<-as.numeric(grep(days_rev[i], object$dates, value = F))
      if (length(last_indx) > 0){
        break;
      }
    }
    cat("Object of class", " ")
    cat(class(object), "\n")
    cat("Start date", " ")
    cat(as.character(object$dates[as.numeric(from_indx)]), "\n")
    cat("End date", " ")
    cat(as.character(object$dates[last_indx]), "\n")
    cat("   ", "dates", "  ", "values", "\n")
    
    for (i in as.numeric(from_indx):last_indx){
      cat(as.character(object$dates[i]), " ")
      cat(as.character(object$values[i]), "\n")
    }
    result<- unimodalTimeSeries(
      p_dates = as.Date(c(a$dates[from_indx:last_indx]),origin="1970-01-01"),
      p_values = c(a$values[from_indx:last_indx]),
      p_unit = "USD",
      p_name = "Price of some good")
  }
  
  
  ###Czwarty przypadek obie nie są datami zmienna/cyfra i zmienna/cyfra 
  else if (length(isDate1)==0 && length(isDate2)==0){
    if ((isLetter1)&&(isLetter2)){
      variable_name1  <- d[1]
      variable_value1 <- as.character(eval(parse(text=variable_name1)))
      variable_name2 <- d[2]
      variable_value2 <- as.character(eval(parse(text=variable_name2)))
      isDateLetter1  <- as.numeric(grep("[1-2][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]",variable_value1,value = F))
      isDateLetter2  <- as.numeric(grep("[1-2][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]",variable_value2,value = F))
      
      if ((length(isDateLetter1)) && (length(isDateLetter2))>0){
        days <- seq(from=as.Date(variable_value1), to=as.Date(variable_value2),by='days' )
        for ( i in seq_along(days) ){
          from_indx<-as.numeric(grep(days[i], object$dates, value = F))
          if (length(from_indx) > 0){
            break;
          }
        }
        days_rev <- rev(seq(from=as.Date(variable_value1), to=as.Date(variable_value2),by='days' ))
        for ( i in seq_along(days_rev) ){
          to_indx<-as.numeric(grep(days_rev[i], object$dates, value = F))
          if (length(to_indx) > 0){
            break;
          }
        }
        
      }else{
        from_indx<- eval(parse(text=variable_name1))
        to_indx<- eval(parse(text=variable_name2))
      }
    }else{
      from_indx <- as.numeric(d[1])
      to_indx <- as.numeric(d[2])
    }
    cat("Object of class", " ")
    cat(class(object), "\n")
    cat("Start date", " ")
    cat(as.character(object$dates[as.numeric(from_indx)]), "\n")
    cat("End date", " ")
    cat(as.character(object$dates[as.numeric(to_indx)]), "\n")
    cat("   ", "dates", "  ", "values", "\n")
    
    for (i in as.numeric(from_indx):as.numeric(to_indx)){
      cat(as.character(object$dates[i]), " ")
      cat(as.character(object$values[i]), "\n")

      
    }
    result<- unimodalTimeSeries(
      p_dates = as.Date(c(a$dates[from_indx:to_indx]),origin="1970-01-01"),
      p_values = c(a$values[from_indx:to_indx]),
      p_unit = "USD",
      p_name = "Price of some good")
  }
 return(result)
}
a[1:5]
x <- 1
a[x:7]
x<-"2020-01-01"
a[x:"2020-10-10"]
x<-"2020-10-10"
a["2020-01-01":x]
wq<-"2021-05-16"
x<-"2020-09-06"
a[x:wq]

###Tworzenie metody plot, points, lines
plot <- function(o, ...) {
  UseMethod("plot")
}
points <- function(o, ...) {
  UseMethod("points")
}
lines <- function(o, ...) {
  UseMethod("lines")
}

plot.unimodalTimeSeries <- function( o,xlab="time",ylab = "USD", n=1, main = "Price of goods", 
                                     col="black", pch=21, type="p",axes=FALSE,...){
   
  days<-as.numeric(o$dates[length(o$dates)]-o$dates[1])
  observable_count <-length(o$dates)
  
  interval<- days/n
  
  interval_chr<-paste(as.character(interval),"days",sep=" ")
  
  plot( x = o$dates, y = o$values, xlab= xlab, ylab = ylab, main = main,
        pch=pch, type=type, col=col, axes=axes,...)
  axis.Date(1, at = seq(o$dates[1], o$dates[length(o$dates)], by = interval_chr),
            format = "%d-%m-%Y",las=2)
  axis(side=2, at=seq(floor(min(o$values)),round(max(o$values)+1),by = 2))
}
points.unimodalTimeSeries<-function(o, xlab="time",ylab = "USD", n=1, main = "Price of goods", pch=5, col="magenta",type="p", ...){
  points(x = o$dates, y = o$values, xlab= xlab, ylab = ylab, main = main, type=type, pch=pch, col=col, ...)
}

lines.unimodalTimeSeries<-function(o, xlab="time",ylab = "USD", n=1, main = "Price of goods",pch=20, col="red", type="p", ...){
  lines(x = o$dates, y = o$values, xlab= xlab, ylab = ylab, main = main, pch=pch, col = col,...)
  
}
###Przykłady
png( filename = "Wykres1.png")
plot(a)
dev.off()
png( filename = "Wykres2.png")
plot( a,
     n = 10, # liczba etykiet dla dat ok. 10% wszystkich możliwych
      xlab = "daty obserwacji",
      ylab = paste( "wartości obserwacji w", a$unit),
     pch = 20,
      type = "o",
    col = "pink",
      axes = FALSE)
dev.off()
png( filename = "Wykres3.png")
plot(a, n = 1, type = "o", col = "yellow", pch = 20, ylim=range(a,b))

points( a['2021-03-26':50], pch = 5, cex = 1.2, col = "blue")
points( a[40:'2021-05-16'], pch = 20, cex = 1.2, col = "black")

lines( a['2021-03-26':'2021-05-16'], col = "red", lwd = 2)
points( b, pch = 19, cex = 1.2, col = "magenta")
lines( b, col = "green", lwd = 2)
dev.off()
