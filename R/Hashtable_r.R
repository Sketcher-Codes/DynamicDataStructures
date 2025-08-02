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
  return(FunctionList)
}
