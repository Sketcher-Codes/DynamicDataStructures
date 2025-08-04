ListStream_r <- function(ChunkSize = 1024, OperatingDirectory = NULL){
  Operating_Directory = OperatingDirectory
  if(dir.exists(Operating_Directory)){
    stop("ListStream_r will shred through files in the operating directory.\nFor safe operation please specify a valid name for a directory that doesn't exist.")
    return(NULL)
  }
  dir.create(Operating_Directory, recursive = TRUE)
  if(!dir.exists(Operating_Directory)){
    stop("Failed to create operating directory.")
    return(NULL)
  }
  FunctionsList = list()
  BottomValuesEnvironment = new.env()
  TopValuesEnvironment = new.env()
  Chunk_Size = ChunkSize
  Vector_Length = 0#Highest position written to
  Segment_Count = 2

  BottomChunkSegment = 1
  TopChunkSegment = 2


  BottomValuesEnvironment$d = as.list(rep(NA,Chunk_Size))
  TopValuesEnvironment$d = as.list(rep(NA,Chunk_Size))


  LoadSegment <- function(FileChunkNumber){
    ThePath = paste0(Operating_Directory,"/LSR",as.character(FileChunkNumber),".rds")
    if(!file.exists(ThePath)){
      BlankEnv = new.env()
      BlankEnv$d = as.list(rep(NA,Chunk_Size))
      return(BlankEnv)
    }

    return(readRDS(ThePath))
  }

  UnloadSegment <- function(Env, FileChunkNumber){
    saveRDS(object = Env,file =  paste0(Operating_Directory,"/LSR",as.character(FileChunkNumber),".rds"))
  }



  FunctionsList$get <- function(i){
    imo = i - 1
    FileChunkNumber = floor(imo / ChunkSize) + 1
    if(FileChunkNumber > TopChunkSegment){
      UnloadSegment(BottomValuesEnvironment, BottomChunkSegment)
      BottomValuesEnvironment <<- TopValuesEnvironment
      BottomChunkSegment <<- TopChunkSegment
      TopChunkSegment <<- FileChunkNumber

      if(FileChunkNumber > Segment_Count){
        return(NA)
      }
      else{
        TopValuesEnvironment <<- LoadSegment(FileChunkNumber)
      }
    } else if(FileChunkNumber < BottomChunkSegment){
      UnloadSegment(TopValuesEnvironment, TopChunkSegment)
      TopValuesEnvironment <<- BottomValuesEnvironment
      BottomValuesEnvironment <<- LoadSegment(FileChunkNumber)

      TopChunkSegment <<- BottomChunkSegment
      BottomChunkSegment <<- FileChunkNumber

    }


    if(FileChunkNumber == TopChunkSegment){
      return(TopValuesEnvironment$d[[(imo %% Chunk_Size + 1)]])
    } else if (FileChunkNumber == BottomChunkSegment) {
      return(BottomValuesEnvironment$d[[(imo %% Chunk_Size + 1)]])
    }

    #ok we have a split segment condition
    #try to load a pair if we can (bottom adjacent to top), otherwise just go with the top (hoping bottom adjacent)
    if(FileChunkNumber == TopChunkSegment - 1){
      UnloadSegment(BottomValuesEnvironment, BottomChunkSegment)
      BottomValuesEnvironment <<- LoadSegment(FileChunkNumber)
      BottomChunkSegment <<- FileChunkNumber
      return(BottomValuesEnvironment$d[[(imo %% Chunk_Size + 1)]])
    } else {
      UnloadSegment(TopValuesEnvironment, TopChunkSegment)
      TopValuesEnvironment <<- LoadSegment(FileChunkNumber)
      TopChunkSegment <<- FileChunkNumber
      return(TopValuesEnvironment$d[[(imo %% Chunk_Size + 1)]])
    }


  }

  FunctionsList$set <- function(i, val){
    if(i > Vector_Length){
      Vector_Length <<- i
    }
    imo = i - 1
    FileChunkNumber = floor(imo / ChunkSize) + 1



    if(FileChunkNumber > TopChunkSegment){
      UnloadSegment(BottomValuesEnvironment, BottomChunkSegment)
      BottomValuesEnvironment <<- TopValuesEnvironment
      BottomChunkSegment <<- TopChunkSegment
      TopChunkSegment <<- FileChunkNumber

      if(FileChunkNumber > Segment_Count){
        TopValuesEnvironment <<- new.env()
        TopValuesEnvironment$d = as.list(rep(NA,Chunk_Size))
      }
      else{
        TopValuesEnvironment <<- LoadSegment(FileChunkNumber)
      }
    } else if(FileChunkNumber < BottomChunkSegment){
      UnloadSegment(TopValuesEnvironment, TopChunkSegment)
      TopValuesEnvironment <<- BottomValuesEnvironment
      BottomValuesEnvironment <<- LoadSegment(FileChunkNumber)

      TopChunkSegment <<- BottomChunkSegment
      BottomChunkSegment <<- FileChunkNumber
    }

    if(FileChunkNumber > Segment_Count){
      Segment_Count <<- FileChunkNumber
    }


    if(FileChunkNumber == TopChunkSegment){
      TopValuesEnvironment$d[[(imo %% Chunk_Size + 1)]] = val
      return()
    } else if (FileChunkNumber == BottomChunkSegment) {
      BottomValuesEnvironment$d[[(imo %% Chunk_Size + 1)]] = val
      return()
    }

    #ok we have a split segment condition
    #try to load a pair if we can (bottom adjacent to top), otherwise just go with the top (hoping bottom adjacent)
    if(FileChunkNumber == TopChunkSegment - 1){
      UnloadSegment(BottomValuesEnvironment, BottomChunkSegment)
      BottomValuesEnvironment <<- LoadSegment(FileChunkNumber)
      BottomValuesEnvironment$d[[(imo %% Chunk_Size + 1)]] = val
      BottomChunkSegment <<- FileChunkNumber
      return()
    } else {
      UnloadSegment(TopValuesEnvironment, TopChunkSegment)
      TopValuesEnvironment <<- LoadSegment(FileChunkNumber)
      TopValuesEnvironment$d[[(imo %% Chunk_Size + 1)]] = val
      TopChunkSegment <<- FileChunkNumber
      return()
    }
  }

  FunctionsList$Count <- function(){
    return(Vector_Length)
  }



  FunctionsList$DestroyOperatingDirectoryAndAllFilesTherein <-function(){
    unlink(Operating_Directory, recursive = TRUE, force = FALSE, expand = FALSE)
  }


  class(FunctionsList) = "ListStream_r"
  return(FunctionsList)
}


"[[.ListStream_r" <- function(x, k){
  return(x$get(k))
}

"[[<-.ListStream_r" <- function(x, k, value){
  x$set(k, value)
  return(x)
}


"[.ListStream_r" <- function(x, ...){
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

"[<-.ListStream_r" <- function(x, ..., value){
  i = 1
  Keys = unlist(list(...))
  l = length(Keys)
  while(i <= l){
    x$set(Keys[i], value[[i]])
    i = i + 1
  }
  return(x)
}



