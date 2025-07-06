# Combinación de todas las series temporales separadas en una sola gran serie temporal
TSMerge <- function(db) {
  merged_df <- Reduce(
    function(x,y,...) {
      merge(x,y, by = "Fecha", all = TRUE)
      },
    db
  )
  return(merged_df)
}