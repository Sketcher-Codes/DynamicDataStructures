\name{ListStream_r}
\alias{ListStream_r}

\title{
ListStream_r
}
\description{
ListStream_r is a a way to stream list data from the disk without having to do all the hard work yourself.
*WARNING* ListStream_r will overwrite files in the specified operating directory.
As a saftey measure ListStream_r will fail to initialise if the operating directory exists at construction time.
Typical use involves firstly ensuring the directory does not exist, than providing the no
Please only use one ListStream_r per operating directory. ListStream_r will create sequentially numbered files to store data in the operating directory, having two operate from the one directory will cause a collision, a error at best, and data corruption at worst.
Unlike base R 'lists', ListStream_r only uses numbered indicies, you cannot index with a character string.
The key value of this is that the objects held in the stream can be arbitary r objects, unlike with VectorStream_r where they are simple base types.
}
\examples{
#Not run
#if(dir.exists(paste0(getwd(),"/ListStream_r_example"))) {
# unlink(paste0(getwd(),"/ListStream_r_example"), recursive = F, force = F)
#}
#MyListStream_r = ListStream_r(
#                        ChunkSize = 1024
#                        , OperatingDirectory = paste0(getwd(),"/ListStream_r_example")
#                   )
}
