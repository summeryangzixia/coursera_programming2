# coursera_programming2

This programming is an R function being able to cache potentially time-consuming computations, which will take advantage of the scoping rules of the R language and how they can be manipulated to preserve state inside of an R object.
For example, taking the mean of a numeric vector is typically a fast operation. However, for a very long vector, it may take too long to compute the mean, especially if it has to be computed repeatedly (e.g.in a loop). If the contents of a vector are not changing, it may make sense to cache the value of the mean so that when we need it again, it can be looked up in the cache rather than recomputed. 
