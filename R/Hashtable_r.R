Hashtable_r <- function(){
  FunctionList = list()
  ValuesEnvironment = new.env()

  #Unsafe get & set
  FunctionList$get_u <- function(key){
    return(get(x = key, envir = ValuesEnvironment))
  }
  FunctionList$set_u <- function(key, val){
    assign(x = key, value = val, envir = ValuesEnvironment)
  }
  FunctionList$exists_u <- function(key){
    return(exists(x = key, envir = ValuesEnvironment))
  }

  #Safe get & set
  FunctionList$get_s <- function(key){
    if(is.null(key)) return(NULL)
    if(is.na(key)) return(NULL)
    if(length(key) != 1) return(NULL)
    return(get(x = as.character(key), envir = ValuesEnvironment))
  }
  FunctionList$set_s <- function(key, val){
    if(is.null(key)) return(FALSE)
    if(is.na(key)) return(FALSE)
    if(length(key) != 1) return(FALSE)
    assign(x = as.character(key), value = val, envir = ValuesEnvironment)
    return(TRUE)
  }
  FunctionList$exists_s <- function(key){
    if(is.null(key)) return(FALSE)
    if(is.na(key)) return(FALSE)
    if(length(key) != 1) return(FALSE)
    return(exists(x = as.character(key), envir = ValuesEnvironment))
  }



  FunctionList$Keys <- function(){
    return(unlist(ls(envir = ValuesEnvironment)))
  }

  FunctionList$Clear <- function(){
    rm(list = ls(envir = ValuesEnvironment), envir = ValuesEnvironment)
  }
  class(FunctionList) = "Hashtable_r"
  return(FunctionList)
}


"[[.Hashtable_r" <- function(x, k){
  return(x$get_u(k))
}

"[[<-.Hashtable_r" <- function(x, k, value){
  x$set_u(k, value)
  return(x)
}


"[.Hashtable_r" <- function(x, ...){
  Result = list()
  Keys = unlist(list(...))
  i = 1
  l = length(Keys)
  while(i <= l){
    Result[[i]] = x$get_u(Keys[i])
    i = i + 1
  }
  names(Result) = Keys
  return(Result)
}

"[<-.Hashtable_r" <- function(x, ..., value){
  i = 1
  Keys = unlist(list(...))
  l = length(Keys)
  while(i <= l){
    x$set_u(Keys[i], value[[i]])
    i = i + 1
  }
  return(x)
}

#.S3method

