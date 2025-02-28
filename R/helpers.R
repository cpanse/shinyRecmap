#R

#' transforms a sf object into a recmap input data.frame
#' @return data.frame 
.as.recmap <- function(x){
  lapply(x$geometry, function(o){
    region <- o[[1]][[1]]
    xx <- region[,1]
    yy <- region[,2]
    dx <- max(xx) - min(xx)
    dy <- max(yy) - min(yy)
    x <- min(xx) + 0.5 * (dx)
    y <- min(yy) + 0.5 * (dy)
    
    data.frame(x, y, dx, dy)
    }) |> Reduce(f = rbind) -> df
  
  df$name = x$NAME
  df$z = x$value
  
  df
}

#' Get census data
#'
#' @return data.frame
#' @importFrom tidycensus get_decennial
#' @import recmap
#' 
#' @examples
#' .get_census() |> shinyRecmap:::.as.recmap()
.get_census <- function(geography = "county", variables = "P1_001N", ...){
  tidycensus::get_decennial(geography = geography, 
                            variables = variables, 
                            year = 2020,
                            geometry = TRUE,
                            sumfile = "dhc",
                            cache_table = TRUE, ...)
}


#' Get U.S. county minimal bounding boxes 
#' 
#' @importFrom maps map
#' @return recmap input
#' @examples 
#' .get_county_mb()
.get_county_mbb <-
  function(state = 'Colorado',
           scaleX = 0.5,
           scaleY = 0.5) {
    
    
    # sanity check
    if (isFALSE(state %in%  row.names(state.x77))) {
      warning("not a valid U.S. state")
      return(NULL)
    }
    
    message("Extracting and scale MBBs for state ", state, " ...")
    
    maps::map('county', state, plot = FALSE)$names |>
      lapply(FUN = function(x) {
        r <- maps::map('county', x, plot = FALSE)
        dx <- scaleX * (r$range[2] - r$range[1])
        dy <- scaleY * (r$range[4] - r$range[3])
        x <- r$range[1] + dx
        y <- r$range[3] + dy
        data.frame(
          polyname = r$name,
          x = x,
          y = y,
          dx = dx,
          dy = dy
        )
      }) |>
      Reduce(f = rbind) -> MBB 
    
    .get_census_county_population() -> POP
    
    merge(MBB, POP, by.x="polyname", by.y="NAME") -> rv
    
    M <- data.frame(name = rv$polyname, x = rv$x, y = rv$y, dx = rv$dx, dy = rv$dy, z = rv$value)
    
    
    attr(M, 'Map.name') <- paste("U.S.", state)
    attr(M, 'Map.stat') <- 'population'
    class(M) <- c('recmap', 'data.frame')
    M
  }
