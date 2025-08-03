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

  FunctionList$Count <- function(){
    return(length(ls(envir = ValuesEnvironment)))
  }

  FunctionList$Keys <- function(){
    return(unlist(ls(envir = ValuesEnvironment)))
  }

  FunctionList$ToList <- function(){
    KeyNames = unlist(ls(envir = ValuesEnvironment))
    Result = list()
    i = 1
    l = length(KeyNames)
    while(i <= l){
      Result[[i]] = get(x = KeyNames[i], envir = ValuesEnvironment)
      i = i + 1
    }
    names(Result) = KeyNames
    return(Result)
  }

  FunctionList$ToDataFrame <- function(){
    KeyNames = unlist(ls(envir = ValuesEnvironment))
    Result = list()
    i = 1
    l = length(KeyNames)
    while(i <= l){
      Result[[i]] = get(x = KeyNames[i], envir = ValuesEnvironment)
      i = i + 1
    }
    names(Result) = KeyNames
    return(data.frame(Result, check.rows = FALSE, check.names = FALSE, fix.empty.names = FALSE, stringsAsFactors = FALSE))
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

"length.Hashtable_r" <- function(x){
  return(x$Count())
}

"as.list.Hashtable_r" <- function(x, ...){
  return(x$ToList())
}

"as.data.frame.Hashtable_r" <- function(x, ...){
  return(x$ToDataFrame())
}

"print.Hashtable_r" <- function(x, MaxItems = 6, ...){
  if(MaxItems < 0){
    MaxItems = 0
  }
  TheCount = x$Count()
  NumLines = min(TheCount, MaxItems)
  if(NumLines <= 0){
    cat("An empty Hashtable_r\n")
    return(invisible())
  }
  cat("A Hashtable_r containing", TheCount, "key - value pairs\n")
  TheNames = x$Keys()
  i = 1
  while(i <= NumLines){
    cat("Item[[", TheNames[i],"]] is ")
    print(x$get_u(TheNames[i]))
    i = i + 1
  }
  return(invisible())
}
