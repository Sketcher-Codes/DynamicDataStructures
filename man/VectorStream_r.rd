\name{VectorStream_r}
\alias{VectorStream_r}

\title{
VectorStream_r
}
\description{
VectorStream_r is a a way to stream vector data from the disk without having to do all the hard work yourself.
*WARNING* VectorStream_r will overwrite files in the specified operating directory.
As a saftey measure VectorStream_r will fail to initialise if the operating directory exists at construction time.
Typical use involves firstly ensuring the directory does not exist, than providing the no
Please only use one VectorStream_r per operating directory. VectorStream_r will create sequentially numbered files to store data in the operating directory, having two operate from the one directory will cause a collision, a error at best, and data corruption at worst.

}
\examples{
#Not run
#if(dir.exists(paste0(getwd(),"/VectorStream_r_example"))) { unlink(paste0(getwd(),"/VectorStream_r_example"), recursive = F, force = F) }
#MyVectorStream_r = VectorStream_r(ChunkSize = 1024, OperatingDirectory = paste0(getwd(),"/VectorStream_r_example"))
}
