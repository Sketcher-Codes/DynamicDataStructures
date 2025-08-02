#Todo - would be nice to have a deep-copy clone feature
#Todo - would be nice to have push-and-pop options

#Fast vector pass data by reference
DynamicVector_r <- function(ChunkSize = 1024){
  #In values environment
  #v = value register
  #s = segment index register
  #i = subchunk index register
  #d = data (a list of vectors)
  #l = chunk size for allocations


  FunctionResult = list()

  ValuesEnvironment = new.env(hash = T)
  Chunk_Size = ChunkSize
  Vector_Length = 0
  Chunk_Count = 1
  Capacity = Chunk_Size

  assign(envir = ValuesEnvironment, x = "d", value = list())
  assign(envir = ValuesEnvironment, x = "s", value = 1)
  assign(envir = ValuesEnvironment, x = "i", value = 1)
  assign(envir = ValuesEnvironment, x = "v", value = NA)
  assign(envir = ValuesEnvironment, x = "l", value = Chunk_Size)
  eval(parse(text = "d[[1]] = rep(NA,l)"), envir = ValuesEnvironment)

  FunctionResult$get <- function(i){
    imo = i - 1
    assign(envir = ValuesEnvironment, x = "s", value = (floor(imo / Chunk_Size) + 1), inherits = F)#Segment starting at 1
    assign(envir = ValuesEnvironment, x = "i", value = ((imo %% Chunk_Size) + 1), inherits = F)#Subchunk starting at 1
    eval(parse(text = "v = d[[s]][i]"), envir = ValuesEnvironment)#Value to extract out
    return(get(x = "v", envir = ValuesEnvironment, inherits = F))#Extract value
  }

  FunctionResult$set <- function(i, val){
    if(i > Vector_Length){
      Vector_Length <<- i
    }
    while(i > Capacity){
      s = Chunk_Count + 1
      assign(envir = ValuesEnvironment, x = "s", value = s, inherits = FALSE)
      eval(parse(text = "d[[s]] = rep(NA,l)"), envir = ValuesEnvironment)
      Chunk_Count <<- s
      Capacity <<- Capacity + Chunk_Size
    }
    #Parse index and value into environment
    imo = i - 1
    assign(envir = ValuesEnvironment, x = "s", value = (floor(imo / Chunk_Size) + 1), inherits = FALSE)#Segment starting at 1
    assign(x = "i", value = (imo %% Chunk_Size + 1), envir = ValuesEnvironment)#Subchunk starting at 1
    assign(x = "v", value = val, envir = ValuesEnvironment)#Value to parse in
    eval(parse(text = "d[[s]][i] = v"), envir = ValuesEnvironment)
  }

  FunctionResult$clear <- function(){
    eval("rm(list = ls())", envir = ValuesEnvironment)
    assign(envir = ValuesEnvironment, x = "d", value = rep(NA, Chunk_Size))
    assign(envir = ValuesEnvironment, x = "s", value = 1)
    assign(envir = ValuesEnvironment, x = "i", value = 1)
    assign(envir = ValuesEnvironment, x = "v", value = NA)
    assign(envir = ValuesEnvironment, x = "l", value = Chunk_Size)
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



  return(FunctionResult)
}
