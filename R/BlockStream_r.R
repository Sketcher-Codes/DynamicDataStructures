BlockStream_r <- function(OperatingDirectory){
  OperationalEnvironment = new.env()
  OperationalEnvironment$NamesTable = Hashtable_r()
  OperationalEnvironment$Operating_Directory = OperatingDirectory
  FunctionList = list()
  OperationalEnvironment$Child_Directory = 0
  OperationalEnvironment$KeysList = list()

  if(dir.exists(OperationalEnvironment$Operating_Directory)){
    stop("BlockStream_r will shred through files in the operating directory.\nFor safe operation please specify a valid name for a directory that doesn't exist.")
    return(NULL)
  }
  dir.create(OperationalEnvironment$Operating_Directory, recursive = TRUE)
  if(!dir.exists(OperationalEnvironment$Operating_Directory)){
    stop("Failed to create operating directory.")
    return(NULL)
  }


  FunctionList$AddVectorColumn <- function(ColName = NULL, ChunkSize = 1024){
    if(is.null(ColName)){
      stop("Column Name can not be NULL.")
    }
    if(is.na(ColName)){
      stop("Column Name can not be NA.")
    }
    if(OperationalEnvironment$NamesTable$exists_s(key = ColName)){
      stop(paste0("Column name ", ColName, " already exists."))
    }
    OperationalEnvironment$NamesTable$set_s(key = ColName, val = VectorStream_r(ChunkSize, OperatingDirectory = paste0(OperationalEnvironment$Operating_Directory, "/VS",as.character(OperationalEnvironment$Child_Directory))))
    OperationalEnvironment$Child_Directory = OperationalEnvironment$Child_Directory + 1
    OperationalEnvironment$KeysList[[OperationalEnvironment$Child_Directory]] = as.character(ColName)
  }


  FunctionList$AddListColumn <- function(ColName = NULL, ChunkSize = 1024){
    if(is.null(ColName)){
      stop("Column Name can not be NULL.")
    }
    if(is.na(ColName)){
      stop("Column Name can not be NA.")
    }
    if(OperationalEnvironment$NamesTable$exists_s(key = ColName)){
      stop(paste0("Column name ", ColName, " already exists."))
    }
    OperationalEnvironment$NamesTable$set_s(key = ColName, val = ListStream_r(ChunkSize, OperatingDirectory = paste0(OperationalEnvironment$Operating_Directory, "/VS",as.character(OperationalEnvironment$Child_Directory))))
    OperationalEnvironment$Child_Directory = OperationalEnvironment$Child_Directory + 1
    OperationalEnvironment$KeysList[[OperationalEnvironment$Child_Directory]] = as.character(ColName)
  }

  FunctionList$get_u <- function(ColName, i){
    return(OperationalEnvironment$NamesTable$get_u(ColName)$get(i))
  }
  FunctionList$set_u <- function(ColName, i, val){
    return(OperationalEnvironment$NamesTable$get_u(ColName)$set(i, val))
  }

  FunctionList$get_row <- function(RowNumber){
    Result = list()
    Counter = 1
    while(Counter <= OperationalEnvironment$Child_Directory){
      Temp = OperationalEnvironment$NamesTable$get_u(OperationalEnvironment$KeysList[[Counter]])$get(RowNumber)
      Result[[Counter]] = Temp
      Counter = Counter + 1
    }
    return(Result)
  }

  FunctionList$DestroyOperatingDirectoryAndAllFilesTherein <-function(){
    unlink(OperationalEnvironment$Operating_Directory, recursive = TRUE, force = FALSE, expand = FALSE)
  }

  return(FunctionList)
}


