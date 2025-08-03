\name{Cpp style for loop}
\alias{\%for\%}
\alias{Cpp style for loop}
\alias{C++ style for loop}
\alias{ForCppStyle}
\title{
CPP style for loop
}
\description{
A CPP style of for loop
}
\details{
This code is a wrapper to create a while loop, it is comparable in speed to a while loop for long execution times, but will be slower due to creating the while loop at run time.
Operation is similar to a C++ for loop, with a space for initilisation of a variable, execution of a condition check, and an action performed at the end of each iteration.
}

\examples{

#Example usage of cpp style '\%for\%'
Test = 0
silent = capture.output(gc())
st = Sys.time()
{i = 1; i <= 1000000; i = i + 1} \%for\% {
  Test = i
}
et = Sys.time()
print(et - st)

#Speed comparision to a 'while' loop
Test = 0
silent = capture.output(gc())
st = Sys.time()
i = 1
while(i < 1000000) {
  Test = i
  i = i + 1
}
et = Sys.time()
print(et - st)

#Speed comparision to a R style for loop
Test = 0
gc()
silent = capture.output(gc())
for(i in 1:1000000) {
  Test = i
}
et = Sys.time()
print(et - st)






}
