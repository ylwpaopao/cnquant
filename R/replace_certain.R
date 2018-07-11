replace_certain <- function(x, pattern, replacement) {
  if (is.nan(pattern)) x[is.nan(x)] <- replacement
  else if (is.na(pattern)) x[is.na(x)] <- replacement
  else x[x == pattern] <- replacement
  return(x)
}
