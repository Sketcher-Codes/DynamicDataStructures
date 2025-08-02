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


  BottomValuesEnvironment$d = rep(NA,Chunk_Size)
  TopValuesEnvironment$d = rep(NA,Chunk_Size)


  LoadSegment <- function(FileChunkNumber){
    ThePath = paste0(Operating_Directory,"/VSR",as.character(FileChunkNumber),".rds")
    if(!file.exists(ThePath)){
      BlankEnv = new.env()
      BlankEnv$d = rep(NA,Chunk_Size)
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
      return(TopValuesEnvironment$d[(imo %% Chunk_Size + 1)])
    } else if (FileChunkNumber == BottomChunkSegment) {
      return(BottomValuesEnvironment$d[(imo %% Chunk_Size + 1)])
    }

    #ok we have a split segment condition
    #try to load a pair if we can (bottom adjacent to top), otherwise just go with the top (hoping bottom adjacent)
    if(FileChunkNumber == TopChunkSegment - 1){
      UnloadSegment(BottomValuesEnvironment, BottomChunkSegment)
      BottomValuesEnvironment <<- LoadSegment(FileChunkNumber)
      BottomChunkSegment <<- FileChunkNumber
      return(BottomValuesEnvironment$d[(imo %% Chunk_Size + 1)])
    } else {
      UnloadSegment(TopValuesEnvironment, TopChunkSegment)
      TopValuesEnvironment <<- LoadSegment(FileChunkNumber)
      TopChunkSegment <<- FileChunkNumber
      return(TopValuesEnvironment$d[(imo %% Chunk_Size + 1)])
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
        TopValuesEnvironment$d = rep(NA,Chunk_Size)
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
      TopValuesEnvironment$d[(imo %% Chunk_Size + 1)] = val
      return()
    } else if (FileChunkNumber == BottomChunkSegment) {
      BottomValuesEnvironment$d[(imo %% Chunk_Size + 1)] = val
      return()
    }

    #ok we have a split segment condition
    #try to load a pair if we can (bottom adjacent to top), otherwise just go with the top (hoping bottom adjacent)
    if(FileChunkNumber == TopChunkSegment - 1){
      UnloadSegment(BottomValuesEnvironment, BottomChunkSegment)
      BottomValuesEnvironment <<- LoadSegment(FileChunkNumber)
      BottomValuesEnvironment$d[(imo %% Chunk_Size + 1)] = val
      return()
    } else {
      UnloadSegment(TopValuesEnvironment, TopChunkSegment)
      TopValuesEnvironment <<- LoadSegment(FileChunkNumber)
      TopValuesEnvironment$d[(imo %% Chunk_Size + 1)] = val
      return()
    }
  }

  FunctionsList$Count <- function(){
    return(Vector_Length)
  }



  FunctionsList$DestroyOperatingDirectoryAndAllFilesTherein <-function(){
    unlink(Operating_Directory, recursive = TRUE, force = FALSE, expand = FALSE)
  }


  class(FunctionsList) = "VectorStream_r"
  return(FunctionsList)
}


