#Todo - would be nice to have a deep-copy clone feature
#Todo - would be nice to have push-and-pop options

#Fast vector pass data by reference
DynamicVector_r <- function(ChunkSize = 1024){
  #In values environment



  FunctionResult = list()

  ValuesEnvironment = new.env(hash = T)
  Chunk_Size = ChunkSize
  Vector_Length = 0
  Chunk_Count = 1
  Capacity = Chunk_Size


  ValuesEnvironment$d = list()
  ValuesEnvironment$d[[1]] = rep(NA,Chunk_Size)

  FunctionResult$get <- function(i){
    imo = i - 1
    return(ValuesEnvironment$d[[(floor(imo / Chunk_Size) + 1)]][((imo %% Chunk_Size) + 1)])
  }

  FunctionResult$set <- function(i, val){
    if(i > Vector_Length){
      Vector_Length <<- i
    }
    while(i > Capacity){
      s = Chunk_Count + 1
      ValuesEnvironment$d[[s]] = rep(NA,Chunk_Size)
      Chunk_Count <<- s
      Capacity <<- Capacity + Chunk_Size
    }
    #Parse index and value into environment
    imo = i - 1
    ValuesEnvironment$d[[(floor(imo / Chunk_Size) + 1)]][(imo %% Chunk_Size + 1)] = val

  }

  FunctionResult$clear <- function(){
    eval("rm(list = ls())", envir = ValuesEnvironment)
    ValuesEnvironment$d = list()
    ValuesEnvironment$d[[1]] = rep(NA,Chunk_Size)
    Vector_Length <<- 0
    Chunk_Count <<- 1
    Capacity <<- Chunk_Size
  }

  FunctionResult$getFlatVector <- function(){
    # Temp workaround for now
    # This is a bit memory hungry and innefficient as it requires a bit more copying than necessary
    # Ideally we should swap out the last chunk with a reduced-length version something similar
    return(unlist(get(x = "d", envir = ValuesEnvironment, inherits = F))[1:Vector_Length])
  }

  FunctionResult$ChunkCount <- function(){
    return(Chunk_Count)
  }

  FunctionResult$ChunkSize <- function(){
    return(Chunk_Size)
  }

  FunctionResult$VectorLength <- function(){
    return(Vector_Length)
  }

  FunctionResult$Capacity <- function(){
    return(Capacity)
  }


  class(FunctionResult) = "DynamicVector_r"
  return(FunctionResult)
}


"[[.DynamicVector_r" <- function(x, k){
  return(x$get(k))
}

"[[<-.DynamicVector_r" <- function(x, k, value){
  x$set(k, value)
  return(x)
}


"[.DynamicVector_r" <- function(x, ...){
  Keys = unlist(list(...))
  i = 1
  l = length(Keys)
  Result = rep(NA,l)
  while(i <= l){
    Result[i] = x$get(Keys[i])
    i = i + 1
  }
  return(Result)
}

"[<-.DynamicVector_r" <- function(x, ..., value){
  i = 1
  Keys = unlist(list(...))
  l = length(Keys)
  while(i <= l){
    x$set(Keys[i], value[[i]])
    i = i + 1
  }
  return(x)
}



