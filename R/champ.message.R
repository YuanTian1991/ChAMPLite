champ.message <- function(tabs = 1, msg, type = "message") {
  output <- ""
  for (i in 1:tabs) output <- paste(output, "  ")
  output <- paste(output, msg)

  if (type == "stop") stop(output) else message(output)
}
