VectorStream_r <- function(ChunkSize = 1024, OperatingDirectory = NULL){
  Operating_Directory = OperatingDirectory
  if(dir.exists(Operating_Directory)){
    stop("VectorStream_r will shred through files in the operating directory.\nFor safe operation please specify a valid name for a directory that doesn't exist.")
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


  InitEnvironment <- function(EnvToInit){
    #i = list subchunk register - this is aligned with the read head subsegment
    assign(x = "i", value = 1, envir = EnvToInit)
    #v = register for injecting and extracting values from the environment
    assign(x = "v", value = NA, envir = EnvToInit)
    #l = chunk size constant
    assign(x = "l", value = Chunk_Size, envir = EnvToInit)
    #Init environment to NAs
    #assign(x = "d", value = rep(NA, Chunk_Size), envir = EnvToInit)
    eval(parse(text = "d = rep(NA,l)"), envir = EnvToInit)
  }


  InitEnvironment(BottomValuesEnvironment)
  InitEnvironment(TopValuesEnvironment)


  LoadSegment <- function(FileChunkNumber){
    ThePath = paste0(Operating_Directory,"/VSR",as.character(FileChunkNumber),".rds")
    if(!file.exists(ThePath)){
      BlankEnv = new.env()
      InitEnvironment(BlankEnv)
      return(BlankEnv)
    }

    return(readRDS(ThePath))
  }

  UnloadSegment <- function(Env, FileChunkNumber){
    saveRDS(object = Env,file =  paste0(Operating_Directory,"/VSR",as.character(FileChunkNumber),".rds"))
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
        print(FileChunkNumber)
        print(Segment_Count)
        stop(paste0("index ", i, " outside bounds of the data structure"))
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
      assign(x = "i", value = (imo %% Chunk_Size + 1), envir = TopValuesEnvironment)
      eval(parse(text = "v = d[i]"), envir = TopValuesEnvironment)
      return(get(x = "v", envir = TopValuesEnvironment))
    } else if (FileChunkNumber == BottomChunkSegment) {
      assign(x = "i", value = (imo %% Chunk_Size + 1), envir = BottomValuesEnvironment)
      eval(parse(text = "v = d[i]"), envir = BottomValuesEnvironment)
      return(get(x = "v", envir = BottomValuesEnvironment))
    }

    #ok we have a split segment condition
    #try to load a pair if we can (bottom adjacent to top), otherwise just go with the top (hoping bottom adjacent)
    if(FileChunkNumber == TopChunkSegment - 1){
      UnloadSegment(BottomValuesEnvironment, BottomChunkSegment)
      BottomValuesEnvironment <<- LoadSegment(FileChunkNumber)
      assign(x = "i", value = (imo %% Chunk_Size + 1), envir = BottomValuesEnvironment)
      eval(parse(text = "v = d[i]"), envir = BottomValuesEnvironment)
      return(get(x = "v", envir = BottomValuesEnvironment))
    } else {
      UnloadSegment(TopValuesEnvironment, TopChunkSegment)
      TopValuesEnvironment <<- LoadSegment(FileChunkNumber)
      assign(x = "i", value = (imo %% Chunk_Size + 1), envir = TopValuesEnvironment)
      eval(parse(text = "v = d[i]"), envir = TopValuesEnvironment)
      return(get(x = "v", envir = TopValuesEnvironment))
    }


  }

  FunctionsList$set <- function(i, val){
    if(i > Vector_Length){
      Vector_Length <<- i
    }
    imo = i - 1
    FileChunkNumber = floor(imo / ChunkSize) + 1
    if(FileChunkNumber > Segment_Count){
      Segment_Count <<- FileChunkNumber
    }
    if(FileChunkNumber > TopChunkSegment){
      UnloadSegment(BottomValuesEnvironment, BottomChunkSegment)
      BottomValuesEnvironment <<- TopValuesEnvironment
      BottomChunkSegment <<- TopChunkSegment
      TopChunkSegment <<- FileChunkNumber

      if(FileChunkNumber > Segment_Count){
        TopValuesEnvironment <<- new.env()
        InitEnvironment(TopValuesEnvironment)
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
      assign(x = "i", value = (imo %% Chunk_Size + 1), envir = TopValuesEnvironment)
      assign(x = "v", value = val, envir = TopValuesEnvironment)
      eval(parse(text = "d[i] = v"), envir = TopValuesEnvironment)
      return()
    } else if (FileChunkNumber == BottomChunkSegment) {
      assign(x = "i", value = (imo %% Chunk_Size + 1), envir = BottomValuesEnvironment)
      assign(x = "v", value = val, envir = BottomValuesEnvironment)
      eval(parse(text = "d[i] = v"), envir = BottomValuesEnvironment)
      return()
    }

    #ok we have a split segment condition
    #try to load a pair if we can (bottom adjacent to top), otherwise just go with the top (hoping bottom adjacent)
    if(FileChunkNumber == TopChunkSegment - 1){
      UnloadSegment(BottomValuesEnvironment, BottomChunkSegment)
      BottomValuesEnvironment <<- LoadSegment(FileChunkNumber)
      assign(x = "i", value = (imo %% Chunk_Size + 1), envir = BottomValuesEnvironment)
      assign(x = "v", value = val, envir = BottomValuesEnvironment)
      eval(parse(text = "d[i] = v"), envir = BottomValuesEnvironment)
      return()
    } else {
      UnloadSegment(TopValuesEnvironment, TopChunkSegment)
      TopValuesEnvironment <<- LoadSegment(FileChunkNumber)
      assign(x = "i", value = (imo %% Chunk_Size + 1), envir = TopValuesEnvironment)
      assign(x = "v", value = val, envir = TopValuesEnvironment)
      eval(parse(text = "d[i] = v"), envir = TopValuesEnvironment)
      return()
    }
  }

  FunctionsList$Count <- function(){
    return(Vector_Length)
  }



  FunctionsList$DestroyOperatingDirectoryAndAllFilesTherein <-function(){
    unlink(Operating_Directory, recursive = TRUE, force = FALSE, expand = FALSE)
  }



  return(FunctionsList)
}


DestructableDir = paste0(getwd(),"/VectorStreamTest")
unlink(DestructableDir, recursive = TRUE)

MyVectorStream = VectorStream_r(100, DestructableDir)

for(i in 1:20){
  MyVectorStream$set(i,i)
}

for(i in 1:20){
  cat(MyVectorStream$get(i),"")
}

for(i in 21:601){
  MyVectorStream$set(i,i)
}

for(i in 550:601){
  cat(MyVectorStream$get(i),"")
}


for(i in 1:601){
  cat(MyVectorStream$get(i),"")
}


print(MyVectorStream$Count())

MyVectorStream$DestroyOperatingDirectoryAndAllFilesTherein()
