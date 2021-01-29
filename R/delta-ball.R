#' Find delta for covering
#'
#' @description Find the minimum distance (delta) such that all points are
#' within delta of at least one other point
#'
#' @details
#' This function is shared with \pkg{TCpredictionbands} on github:
#' \href{https://github.com/Mr8ND/TC-prediction-bands/tree/master/TCpredictionbands}{TCpredictionbands}.
#'
#' @param data continuous data frame of individual points (in each row)
#' @param dist_mat distance matrix, calculated otherwise via euclidean distance
#'
#' @return
#' \describe{
#' \item{dist_mat}{distance matrix between points}
#' \item{mm_delta}{the minimum distance (delta)}
#' }
#' @export
get_delta <- function(data = NULL, dist_mat = NULL){
  if (is.null(dist_mat) & is.null(data)) {
    stop("need to provide either data or dist_mat")
  }
  
  if (!is.null(data)){
    if (is.null(dist_mat)) {
      dist_mat <- as.matrix(stats::dist(data))
    }
    return(list(dist_mat = dist_mat, mm_delta = get_delta_nn(data = data)))
  }

  diag(dist_mat) <- max(dist_mat)
  mm_delta <- apply(dist_mat, MARGIN = 1, min) %>% max
  diag(dist_mat) <- 0
  return(list(dist_mat = dist_mat, mm_delta = mm_delta))
}

#' calculate maxmin distance between points
#'
#' Fast calculation of maxmin distance using kd trees and nearest neighbors from
#' the \code{RANN} package.
#'
#' @param data data.frame (with only columns that are needed)
#'
#' @return minimum radius for all points to be covered
#' @export
get_delta_nn <- function(data){
  check <- RANN::nn2(data, data, k = 2, treetype = "kd", eps = 0)
  mm_delta <- check$nn.dists[,2] %>% max()
  return(mm_delta)
}

#' Performs delta ball approach (2d approach)
#'
#' @param data_deep_points data deep points from depth function
#' @param xy_columns columns of data.frame that relate to
#'   the points's coordinates in euclidean space. The input should look like
#' something like \code{c(x,y)} or \code{c("x","y")}.
#'
#' @return
#' \describe{
#' \item{structure}{data frame of non-ordered lines of contour}
#' \item{delta}{optimal delta for covering}
#' }
#' @export
delta_structure <- function(data_deep_points, xy_columns = c("x", "y")){

  #quos
  xy_columns_q <- dplyr::enquos(xy_columns)
  xy_columns <- unname(tidyselect::vars_select(dplyr::tbl_vars(data_deep_points),
                                               !!!xy_columns_q))

  data_deep_points <- data_deep_points %>%
    dplyr::select(dplyr::one_of(xy_columns)) %>%
    dplyr::rename(x = xy_columns[1], y = xy_columns[2])

  d_out <- get_delta_nn(data = data_deep_points)
  delta = d_out
  structure_df <- inner_delta_ball_wrapper(data_deep_points,
                                           remove_duplicates = TRUE)
  names(structure_df)[names(structure_df) == "x"] <- xy_columns[1]
  names(structure_df)[names(structure_df) == "y"] <- xy_columns[2]

  out <- list()
  out[["structure"]] <- structure_df
  out[["delta"]] <- delta
  return(out)
}

#' Run delta ball analysis
#'
#' @description Run delta-ball analysis to obtain an outline points for
#'   delta-ball covering where points are in 2d space. We first find the minimum
#'   delta such that all balls centered at each point in the data set is
#'   touching at least 1 other ball (see \code{\link{get_delta}} for more
#'   information). This function then creates a geometric objects that trying to
#'   represent the covering of all these delta balls. Specifically we use
#'   geometric properties to find the points on the "outside" of this covering
#'   and return a set of lines that create a "boundary" of shorts. Intuition
#'   from "Computing Polygonal Surfaces from Unions of Balls" by Tam and
#'   Heidrich was used in this function.
#'
#' @details
#' This function (renamed as \code{delta_ball_wrapper}) is shared with \pkg{TCpredictionbands} on github:
#' \href{https://github.com/Mr8ND/TC-prediction-bands/tree/master/TCpredictionbands}{TCpredictionbands}.
#'
#' @param data_raw data frame with center points of balls
#' @param n_steps number of equidistance points along the line, past delta on
#'   both sides, that will be checked to approximate all points along the line
#' @param xy_columns columns of data.frame that relate to
#'   the points's coordinates in euclidean space. The input should look like
#' something like \code{c(x,y)} or \code{c("x","y")}.
#' @param remove_duplicates boolean if need to remove duplicates in data_raw
#'
#' @return data frame of exterior lines (not ordered)
#' @export
inner_delta_ball_wrapper <- function(data_raw, xy_columns = c("x", "y"),
                                     n_steps = 100,
                                     remove_duplicates = F){

  #quos
  xy_columns_q <- dplyr::enquos(xy_columns)
  xy_columns <- unname(tidyselect::vars_select(dplyr::tbl_vars(data_raw),
                                               !!!xy_columns_q))

  data <- data_raw %>% dplyr::select(dplyr::one_of(xy_columns))

  if (remove_duplicates) {
    data <- data %>% dplyr::distinct()
  }

  sp::coordinates(data) <- xy_columns

  # get delta value --------------------
  d <- get_delta_nn(data = data_raw)
  delta <- d/2

  # create correct edges --------------------
  dtri_data_edges <- rgeos::gDelaunayTriangulation(data, onlyEdges = T,
                                                   tolerance = 0)

  lines_info <- get_lines(dtri_data_edges,
                          data_raw,
                          delta,
                          n_steps = n_steps)
  desired_lines <- lines_info$lines_mat
  keep <- desired_lines %>% apply(MARGIN = 1,
                                  function(row) sum(is.na(row)) == 0)
  desired_lines <- desired_lines[keep,]

  removed_mat <- lines_info$removed_mat

  # string representation of nodes and edges --------------------
  nodes <- paste0("(",desired_lines$x, ",", desired_lines$y, ")")
  edge_mat <- matrix(c(nodes[seq(from = 1,to = length(nodes),by = 2)],
                       nodes[seq(from = 2,to = length(nodes),by = 2)]),
                     ncol = 2) %>%
    data.frame() %>%
    dplyr::mutate(X1 = as.character(.data$X1),
                  X2 = as.character(.data$X2),
                  id = desired_lines$idx[seq(from = 1,to = length(nodes),by = 2)])

  # get DT triangles --------------------
  dtri_data_tri <- rgeos::gDelaunayTriangulation(data,tolerance = 0)
  tri_matrix <- get_tri_matrix(dtri_data_tri)

  tuples_of_tri <- data.frame(rbind(tri_matrix[,c(1,2)],
                                    tri_matrix[,c(1,3)],
                                    tri_matrix[,c(2,3)],
                                    # both directions
                                    tri_matrix[,c(2,1)],
                                    tri_matrix[,c(3,1)],
                                    tri_matrix[,c(3,2)]),
                              stringsAsFactors = F
  ) %>%
    dplyr::mutate(idx_tri = rep(1:nrow(tri_matrix),times = 6))

  tuples_of_tri <- remove_incomplete_tri(tuples_of_tri = tuples_of_tri,
                                         removed_mat = removed_mat)
  # what type of edge are you? --------------------

  num_tri <- edge_mat %>% dplyr::left_join(tuples_of_tri,
                                           by = c("X1" = "X1", "X2" = "X2")) %>%
    dplyr::group_by(.data$id) %>%
    dplyr::summarise(idx_tri = paste0(.data$idx_tri, collapse = ","),
                     X1 = unique(.data$X1),
                     X2 = unique(.data$X2),
                     count = dplyr::n())

  # merging & getting regular lines --------------------

  index_mapping <- data.frame(dl = sort(unique(desired_lines$idx)),
                              nt = sort(unique(num_tri$id)))

  select_lines <- (num_tri[num_tri$count == 1, c("id")] %>%
                     dplyr::left_join(index_mapping, by = c("id" = "nt")))$dl

  output_lines <- desired_lines %>% dplyr::filter(.data$idx %in% select_lines)

  names(output_lines)[names(output_lines) %in% c("x", "y")] <- xy_columns

  return(output_lines)
}

#' Remove triangles with edges that need removal
#'
#' @description Remove triangles from tuple matrix that have 1 or more edge that
#' needs to be removed
#'
#' @details
#' This function is shared with \pkg{TCpredictionbands} on github:
#' \href{https://github.com/Mr8ND/TC-prediction-bands/tree/master/TCpredictionbands}{TCpredictionbands}.
#'
#' @param tuples_of_tri data frame with tuples of triangle edges and
#' triangle index
#' @param removed_mat edges to be removed
#'
#' @return data frame with tuples of triangle not removed
remove_incomplete_tri <- function(tuples_of_tri, removed_mat){

  # removes triangles for tuples data frame that have an edge removed
  removed_mat <- removed_mat[apply(removed_mat, 1,
                                   function(row) sum(is.na(row)) == 0), ]

  tuples_of_tri$combo <- apply(tuples_of_tri,1,
                               function(row) paste0(row[1],"~",row[2]))

  # if you nothing to remove:
  if (dim(removed_mat)[1] == 0){
    return(tuples_of_tri)
  }

  removed_values_dat <- removed_mat %>%
    dplyr::group_by(.data$idx) %>%
    dplyr::summarize(first = paste0("(",.data$x[1],",", .data$y[1],")"),
                     second = paste0("(",.data$x[2],",", .data$y[2],")"),
                     combo = paste0(.data$first,"~",.data$second),
                     combo2 = paste0(.data$second,"~",.data$first))

  removed_values_single <- c(removed_values_dat$combo,
                             removed_values_dat$combo2)

  remove_tri <- tuples_of_tri$idx_tri[(
    tuples_of_tri$combo %in% removed_values_single
  )]

  out_tuples <- tuples_of_tri[!(tuples_of_tri$idx_tri %in% remove_tri),]

  return(out_tuples)
}

#' Make triangle matrix
#'
#' @description Makes triangle matrix for points in matrix
#'
#' @details
#' This function is shared with \pkg{TCpredictionbands} on github:
#' \href{https://github.com/Mr8ND/TC-prediction-bands/tree/master/TCpredictionbands}{TCpredictionbands}.
#'
#' @param dtri_data_tri sp object with triangles (sp::SpatialPolygons) -
#' n triangles
#'
#' @return a matrix (n x 3) with strings of locations of 3 points in triangle
get_tri_matrix <- function(dtri_data_tri){
  # makes a matrix with string points of triangle (n x 3)
  num_tri <- length(dtri_data_tri@polygons)
  all_tri <- matrix("", nrow = num_tri, ncol = 3)

  for (idx_tri in 1:num_tri) {
    tri <- data.frame(
      dtri_data_tri@polygons[[idx_tri]]@Polygons[[1]]@coords)[1:3,]
    all_tri[idx_tri,] <- tri %>%
      apply(1, function(x) paste0("(",x[1],",",x[2],")"))
  }

  return(all_tri)
}

#' Get edges within union of balls
#'
#' @description Figure out which edges in the delaunay diagram are within the
#' union of balls
#'
#' @details
#' This function is shared with \pkg{TCpredictionbands} on github:
#' \href{https://github.com/Mr8ND/TC-prediction-bands/tree/master/TCpredictionbands}{TCpredictionbands}.
#'
#' @param delaunay_tri_data sp data of delaunay triangles lines (sp::SpatialLines)
#' @param data_raw data frame with center points of balls
#' @param delta fixed radius of all the balls
#' @param n_steps number of equidistance points along the line, past delta
#' on both sides, that will be checked to approximate all points along the line
#'
#' @return
#' \describe{
#' \item{lines_mat}{lines of edges that are kept (each edge has 2 rows and
#' share an index). These edges are within the union of the balls.}
#' \item{removed_mat}{lines of edges that should be removed (i.e. are not
#' with the union of balls.)}
#' }
get_lines <- function(delaunay_tri_data, data_raw, delta, n_steps = 100){
  ## this function gets lines that are included within the balls

  idv_lines <- delaunay_tri_data@lines[[1]]@Lines
  n <- length(idv_lines)

  lines_mat <- matrix(NA, nrow = 2*n, ncol = 2)

  removed_mat <- matrix(NA, nrow = 2*n, ncol = 2)

  for (idx in 1:n) {
    l <- idv_lines[[idx]]@coords

    if (stats::dist(l) > delta * 2) {
      l_inner <- remove_delta_off_line(l, delta)
      points_along <- steps_along_2d_line(l_inner,n_steps)
      neighbor <- RANN::nn2(data = as.data.frame(data_raw),
                            query = points_along,
                            k = 1, treetype = "kd")
      if (!all(neighbor$nn.dists < delta)) {
        removed_mat[(2*idx - 1):(2*idx),] <- l
        next
      }
    }

    lines_mat[(2*idx - 1):(2*idx),] <- l
  }

  # cleaning up return

  colnames(lines_mat) <- c("x", "y")
  lines_mat <- data.frame(lines_mat)
  lines_mat$idx <- rep(1:(nrow(lines_mat)/2), each = 2)

  colnames(removed_mat) <- c("x", "y")
  removed_mat <- data.frame(removed_mat)
  removed_mat$idx <- rep(1:(nrow(removed_mat)/2), each = 2)

  return(list(lines_mat = lines_mat, removed_mat = removed_mat ))
}

#' Create n equidistance points
#'
#' @description Inner Function to create n equidistant points along a line
#'
#' @param line 2 x 2 matrix of edge points of line
#' @param n_steps integer number of steps (n)
#'
#' @return (n_steps + 1) x 2 matrix with points on path
steps_along_2d_line <- function(line, n_steps = 100){
  # (inner function) finds equidistance points along a line
  len   <- stats::dist(line)
  diffs <- diff(line)

  if (diffs[1] == 0) {
    x_shift <- 0
    y_shift <- len
  } else {
    slope <- diffs[2]/diffs[1]

    x_shift <- sqrt(len^2/(slope^2 + 1))
    y_shift <- slope * x_shift
  }


  points <-  matrix(rep(line[1,], each = n_steps + 1), ncol = 2) + t(
    matrix(rep((1:(n_steps + 1) - 1) / n_steps , each = 2),
           nrow = 2, byrow = F) * c(x_shift,y_shift) )
  return(points)
}

#' Shorten line by delta on both sides
#'
#' @description inner function to remove delta length from both sides of a line
#'
#' @details
#' This function is shared with \pkg{TCpredictionbands} on github:
#' \href{https://github.com/Mr8ND/TC-prediction-bands/tree/master/TCpredictionbands}{TCpredictionbands}.

#'
#' @param line 2 x 2 matrix of edge points of line
#' @param delta numeric delta to be subtracted
#'
#' @return 2 x 2 matrix of edges that are shrunk
remove_delta_off_line <- function(line, delta){

    diffs <- diff(line)
  if (diffs[1] == 0) {
    x_shift <- 0
    y_shift <- delta
  } else {
    slope <- diffs[2]/diffs[1]

    x_shift <- sqrt(delta^2 / (slope^2 + 1))
    y_shift <- slope * x_shift
  }


  out_line <- line + matrix(c(x_shift,y_shift,
                              -x_shift,-y_shift), nrow = 2, byrow = T)
  return(out_line)
}
