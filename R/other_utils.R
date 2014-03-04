
#' get files
#' 
#' returns a list of all files in the \code{folder}
#' 
#' @export
#' @param folder to return the files for
#' @return list of file names
get_files <- function(folder) {
  
  files_unfiltered <- dir(folder)
  files_unfiltered <- files_unfiltered[file.info(paste0(folder,files_unfiltered))$isdir == FALSE]
  return(files_unfiltered)
}



#' Print size of workspace objects nicely
#' 
#' Prints a table of name and size of all objects larger than \code{min_size}.
#' 
#' @export
#' @param sorted (bool) should the objects be sorted by size (otherwise by name)
#' @param min_size (int) minimal byte size for an objects to be printed individualiy
#' @examples
#' x <- rep(0,10^7)
#' ram_objects_summary()
#' 
ram_objects_summary <- function(sorted = TRUE, min_size = 1000000) {
  sizes <- sapply(objects(name=1), function (x) object.size(eval(as.symbol(x))))
  modes <-  sapply(objects(name=1), function (x) mode(eval(as.symbol(x))))
  sizes <- sizes[modes != "function"]
  if (sorted) {
    sizes <- rev(sort(sizes))
  }
  sizes_big <- sizes[sizes > min_size]
  
  sapply(names(sizes_big), function (x) {cat(x,"\t"); print(object.size(eval(as.symbol(x))),quote=FALSE, units="a")})
  
  size_other <- sum(sizes[sizes <= min_size])
  class(size_other) <- "object_size"
  cat("\n--------------\nOther\t")
  print(size_other,quote=FALSE, units="a")
  size_all <- sum(sizes)
  class(size_all) <- "object_size"
  cat("--------------\nTotal\t")
  print(size_all,quote=FALSE, units="a")
}
