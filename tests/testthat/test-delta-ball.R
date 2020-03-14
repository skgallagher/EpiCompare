context("delta ball internal functions")

test_that("delta_structure tests, strings", {
  data_deep_points <- data.frame(x = c(0,1,1),
                                 y = c(0,0,1))

  out_list <- delta_structure(data_deep_points,xy_columns = c("x","y"))
  structure_out_list <- out_list$structure
  line_numbers <- structure_out_list %>% dplyr::pull(idx) %>% unique

  expected_min_delta <- 1
  expected_structure <- list(first = data.frame(x = c(0,1),
                                                y = c(0,0),
                                                extra = 1:2),
                             second = data.frame(x = c(1, 1),
                                                 y = c(0, 1),
                                                 extra = 1:2))

  line_info <- c()
  for (line in expected_structure) {
    in_the_match <- FALSE
    for (line_num in line_numbers) {
      d_structure <- structure_out_list %>%
        dplyr::filter(idx == line_num)

      combined_info <- d_structure %>%
        dplyr::left_join(line, by = c("x", "y"))

      if (sum(is.na(combined_info)) == 0){
        in_the_match  <- TRUE
      }
    }
    line_info <- c(line_info, in_the_match)

  }
  testthat::expect_true(all(line_info))
})

test_that("delta_structure tests, tidyify", {
  data_deep_points <- data.frame(x = c(0,1,1),
                                 y = c(0,0,1))

  out_list <- delta_structure(data_deep_points,xy_columns = c(x,y))
  structure_out_list <- out_list$structure
  line_numbers <- structure_out_list %>% dplyr::pull(idx) %>% unique

  expected_min_delta <- 1
  expected_structure <- list(first = data.frame(x = c(0,1),
                                                y = c(0,0),
                                                extra = 1:2),
                             second = data.frame(x = c(1, 1),
                                                 y = c(0, 1),
                                                 extra = 1:2))

  line_info <- c()
  for (line in expected_structure) {
    in_the_match <- FALSE
    for (line_num in line_numbers) {
      d_structure <- structure_out_list %>%
        dplyr::filter(idx == line_num)

      combined_info <- d_structure %>%
        dplyr::left_join(line, by = c("x", "y"))

      if (sum(is.na(combined_info)) == 0){
        in_the_match  <- TRUE
      }
    }
    line_info <- c(line_info, in_the_match)

  }
  testthat::expect_true(all(line_info))
})

test_that("get_delta basic tests", {
  # static
  data <- data.frame(x = c(0,1), y = c(0,1))
  dist_mat <- matrix(c(0,sqrt(2),
                       sqrt(2),0), byrow = T, nrow = 2)
  gd_out1 <- get_delta(data)
  gd_out2 <- get_delta(dist_mat = dist_mat)

  testthat::expect_equivalent(gd_out1,gd_out2)
  testthat::expect_equivalent(gd_out1, list("dist_mat" = dist_mat,
                                            "mm_delta" = sqrt(2)))
  # static 2
  data2 <- data.frame(x = c(0, 1, 1, 0), y = c(0, 0, 1, 1))
  dist_mat2 <- matrix(c(0,1,sqrt(2),1,
                        1,0,1,sqrt(2),
                        sqrt(2),1,0,1,
                        1,sqrt(2),1,0), byrow = T, nrow = 4)
  gd_out3 <- get_delta(data2)
  gd_out4 <- get_delta(dist_mat = dist_mat2)

  testthat::expect_equivalent(gd_out3, gd_out4)
  testthat::expect_equivalent(gd_out3, list("dist_mat" = dist_mat2,
                                            "mm_delta" = 1))

  # error
  testthat::expect_error(get_delta())
})

test_that("test inner_delta_ball_wrapper - basic", {
  #square - no center ----------------
  data <- data.frame(x = c(0,0,1,1),
                     y = c(0,1,0,1))

  out_delta_ball <- inner_delta_ball_wrapper(data)
  # just looking for outer square (no inner lines)

  testthat::expect_equal(dim(out_delta_ball), c(8,3)) #4 lines

  # by construction the sums of pairs = 1 or 3, not 2
  sums_pairs <- out_delta_ball[,c("x","y")] %>% split(out_delta_ball$idx) %>%
    sapply(sum)

  testthat::expect_true(all(sums_pairs != 2)) #no lines across the center

  # single triangle - no diag -------------
  data <- data.frame(x = c(0,0,1),
                     y = c(0,1,1))

  out_delta_ball <- inner_delta_ball_wrapper(data)
  # just looking for outer square (no inner lines)

  testthat::expect_equal(dim(out_delta_ball), c(4,3)) #2 lines

  # by construction the sums of pairs = 1 or 3, not 2
  sums_pairs <- out_delta_ball[,c("x","y")] %>% split(out_delta_ball$idx) %>%
    sapply(sum)

  testthat::expect_true(all(sums_pairs != 2)) #no lines across the center

})

test_that("test inner_delta_ball_wrapper - basic, string", {
  #square - no center ----------------
  data <- data.frame(x = c(0,0,1,1),
                     z = c(0,1,0,1))

  out_delta_ball <- inner_delta_ball_wrapper(data,xy_columns = c("x","z"))
  # just looking for outer square (no inner lines)

  testthat::expect_equal(dim(out_delta_ball), c(8,3)) #4 lines

  # by construction the sums of pairs = 1 or 3, not 2
  sums_pairs <- out_delta_ball[,c("x","z")] %>% split(out_delta_ball$idx) %>%
    sapply(sum)

  testthat::expect_true(all(sums_pairs != 2)) #no lines across the center

  # single triangle - no diag -------------
  data <- data.frame(x = c(0,0,1),
                     z = c(0,1,1))

  out_delta_ball <- inner_delta_ball_wrapper(data, xy_columns = c("x","z"))
  # just looking for outer square (no inner lines)

  testthat::expect_equal(dim(out_delta_ball), c(4,3)) #2 lines

  # by construction the sums of pairs = 1 or 3, not 2
  sums_pairs <- out_delta_ball[,c("x","z")] %>% split(out_delta_ball$idx) %>%
    sapply(sum)

  testthat::expect_true(all(sums_pairs != 2)) #no lines across the center

})


test_that("test inner_delta_ball_wrapper - basic, tidyify", {
  #square - no center ----------------
  data <- data.frame(x = c(0,0,1,1),
                     z = c(0,1,0,1))

  out_delta_ball <- inner_delta_ball_wrapper(data,xy_columns = c(x,z))
  # just looking for outer square (no inner lines)

  testthat::expect_equal(dim(out_delta_ball), c(8,3)) #4 lines

  # by construction the sums of pairs = 1 or 3, not 2
  sums_pairs <- out_delta_ball[,c("x","z")] %>% split(out_delta_ball$idx) %>%
    sapply(sum)

  testthat::expect_true(all(sums_pairs != 2)) #no lines across the center

  # single triangle - no diag -------------
  data <- data.frame(x = c(0,0,1),
                     z = c(0,1,1))

  out_delta_ball <- inner_delta_ball_wrapper(data, xy_columns = c(x,z))
  # just looking for outer square (no inner lines)

  testthat::expect_equal(dim(out_delta_ball), c(4,3)) #2 lines

  # by construction the sums of pairs = 1 or 3, not 2
  sums_pairs <- out_delta_ball[,c("x","z")] %>% split(out_delta_ball$idx) %>%
    sapply(sum)

  testthat::expect_true(all(sums_pairs != 2)) #no lines across the center

})


test_that("test remove_incomplete_tri", {
  n_steps = 100
  ### ----------------------
  # basic example, 2 triangles - all triangles lost
  data <- data.frame(x = c(0,0,1,1),
                     y = c(0,1,0,1))
  sp::coordinates(data) <- names(data)[1:2]
  # interior lines lost (aka between (0,1)-(1,0))
  # -> all triangles are lost
  delta <- 1/2

  dtri_data_edges <- rgeos::gDelaunayTriangulation(data, onlyEdges = T,
                                                   tolerance = 0)

  lines_info <- get_lines(dtri_data_edges,
                          data,
                          delta,
                          n_steps = n_steps)
  desired_lines <- lines_info$lines_mat
  keep <- desired_lines %>% apply(MARGIN = 1,
                                  function(row) sum(is.na(row)) == 0)
  desired_lines <- desired_lines[keep,]

  removed_mat <- lines_info$removed_mat

  # string representation of nodes and edges
  nodes <- paste0("(",desired_lines$x, ",", desired_lines$y, ")")
  edge_mat <- matrix(c(nodes[seq(from = 1,to = length(nodes),by = 2)],
                       nodes[seq(from = 2,to = length(nodes),by = 2)]),
                     ncol = 2) %>%
    data.frame() %>%
    dplyr::mutate(X1 = as.character(X1),
                  X2 = as.character(X2),
                  id = desired_lines$idx[seq(from = 1,to = length(nodes),by = 2)])

  # get DT triangles
  dtri_data_tri <- rgeos::gDelaunayTriangulation(data, tolerance = 0)
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

  # tests
  testthat::expect_equal(length(unique(tuples_of_tri$X1)), 0)
  testthat::expect_equal(dim(tuples_of_tri), c(0, 4))
  testthat::expect_equal(length(unique(tuples_of_tri$idx_tri)), 0)

  ### ----------------------
  # basic example, 2 triangles - 0 triangles lost
  data <- data.frame(x = c(0,0,1,1),
                     y = c(0,1,0,1))
  sp::coordinates(data) <- names(data)[1:2]
  # interior lines lost (aka between (0,1)-(1,0))
  # -> all triangles are lost
  delta <- sqrt(2)/2

  dtri_data_edges <- rgeos::gDelaunayTriangulation(data, onlyEdges = T,
                                                   tolerance = 0)

  lines_info <- get_lines(dtri_data_edges,
                          data,
                          delta,
                          n_steps = n_steps)
  desired_lines <- lines_info$lines_mat
  keep <- desired_lines %>% apply(MARGIN = 1,
                                  function(row) sum(is.na(row)) == 0)
  desired_lines <- desired_lines[keep,]

  removed_mat <- lines_info$removed_mat

  # string representation of nodes and edges
  nodes <- paste0("(",desired_lines$x, ",", desired_lines$y, ")")
  edge_mat <- matrix(c(nodes[seq(from = 1,to = length(nodes),by = 2)],
                       nodes[seq(from = 2,to = length(nodes),by = 2)]),
                     ncol = 2) %>%
    data.frame() %>%
    dplyr::mutate(X1 = as.character(X1),
                  X2 = as.character(X2),
                  id = desired_lines$idx[seq(from = 1,to = length(nodes),by = 2)])

  # get DT triangles
  dtri_data_tri <- rgeos::gDelaunayTriangulation(data, tolerance = 0)
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

  testthat::expect_equal(length(unique(tuples_of_tri$X1)), 4)
  testthat::expect_equal(dim(tuples_of_tri),c(12, 4))
  testthat::expect_equal(length(unique(tuples_of_tri$idx_tri)), 2)



  ### ----------------------
  # basic example, 2 triangles - 1 triangles lost
  data <- data.frame(x = c(0,0,1,2),
                     y = c(0,1,0,2))
  sp::coordinates(data) <- names(data)[1:2]
  # interior lines lost (aka between (0,1)-(1,0))
  # -> all triangles are lost
  delta <- sqrt(2)/2

  dtri_data_edges <- rgeos::gDelaunayTriangulation(data, onlyEdges = T,
                                                   tolerance = 0)

  lines_info <- get_lines(dtri_data_edges,
                          data,
                          delta,
                          n_steps = n_steps)
  desired_lines <- lines_info$lines_mat
  keep <- desired_lines %>% apply(MARGIN = 1,
                                  function(row) sum(is.na(row)) == 0)
  desired_lines <- desired_lines[keep,]

  removed_mat <- lines_info$removed_mat

  # string representation of nodes and edges
  nodes <- paste0("(",desired_lines$x, ",", desired_lines$y, ")")
  edge_mat <- matrix(c(nodes[seq(from = 1,to = length(nodes),by = 2)],
                       nodes[seq(from = 2,to = length(nodes),by = 2)]),
                     ncol = 2) %>%
    data.frame() %>%
    dplyr::mutate(X1 = as.character(X1),
                  X2 = as.character(X2),
                  id = desired_lines$idx[seq(from = 1,to = length(nodes),by = 2)])

  # get DT triangles
  dtri_data_tri <- rgeos::gDelaunayTriangulation(data, tolerance = 0)
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

  testthat::expect_equal(length(unique(tuples_of_tri$X1)), 3)
  testthat::expect_equal(dim(tuples_of_tri), c(6, 4))
  testthat::expect_equal(length(unique(tuples_of_tri$idx_tri)), 1)


})

test_that("test get_tri_matrix - basic examples", {
  # basic example, single triangle
  data <- data.frame(x = c(0,1,1),
                     y = c(1,0,1))
  sp::coordinates(data) <- names(data)[1:2]
  dtri_data_tri <- rgeos::gDelaunayTriangulation(data, onlyEdges = F,
                                                 tolerance = 0)

  tri_strings <- get_tri_matrix(dtri_data_tri)

  testthat::expect_true(setequal(tri_strings[1,],
                                 c("(0,1)", "(1,0)", "(1,1)")))

  # basic example, 2 triangles
  data <- data.frame(x = c(0,0,1,1),
                     y = c(0,1,0,1))
  sp::coordinates(data) <- names(data)[1:2]
  dtri_data_tri <- rgeos::gDelaunayTriangulation(data, onlyEdges = F,
                                                 tolerance = 0)

  tri_strings <- get_tri_matrix(dtri_data_tri)

  one_order <- rep(F,2)
  other_order <- rep(F,2)

  tri_list <- list(c("(0,1)", "(1,0)", "(1,1)"),
                   c("(0,1)", "(1,0)", "(0,0)"))

  for (i in 1:2){
    one_order[i] <- setequal(tri_strings[i,], tri_list[[i]])
    other_order[i] <- setequal(tri_strings[i,], tri_list[[3-i]])
  }
  testthat::expect_true(all(one_order) | all(other_order))
})

test_that("get_lines test", {
  # basic example
  data <- data.frame(x = c(0,1,1),
                     y = c(1,0,1))
  sp::coordinates(data) <- names(data)[1:2]
  dtri_data_edges <- rgeos::gDelaunayTriangulation(data, onlyEdges = T,
                                                   tolerance = 0)

  expected_full_df_template <- data.frame(x = c(0,1,0,1,1,1),
                                          y = c(1,1,1,0,0,1),
                                          idx = rep(1:3, each = 2))

  ## distance between = .25 (remove all lines)
  which_lines_none <- get_lines(dtri_data_edges, data, delta = .25)

  ### lines_mat should be all NAs
  testthat::expect_true(which_lines_none$lines_mat %>%
                          dplyr::select(- .data$idx) %>% is.na %>%
                          all)
  ### removed_mat should be all of data
  testthat::expect_true(!(which_lines_none$removed_mat %>%
                            dplyr::select(- .data$idx) %>% is.na %>%
                            all))
  testthat::expect_equal(which_lines_none$removed_mat,
                         expected_full_df_template)


  ## distance between = 1/2 (removes 1 line)
  which_lines_part <- get_lines(dtri_data_edges, data, delta = .5)

  ### lines_mat have 1 set NA
  expected_lines_mat <- expected_full_df_template
  expected_lines_mat[3:4, c("x", "y")] <- NA
  testthat::expect_equal(which_lines_part$lines_mat,
                         expected_lines_mat)

  ### removed_mat should only contain the (0,1)-(1,0) line
  expected_lines_mat <- expected_full_df_template
  expected_lines_mat[c(1:2, 5:6), c("x", "y")] <- NA

  testthat::expect_equal(which_lines_part$removed_mat,
                         expected_lines_mat)

  # distance between = sqrt(2)/2 (remove 0 lines)
  which_lines_all <- get_lines(dtri_data_edges, data, delta = sqrt(2)/2)

  ### lines_mat should be all of data
  testthat::expect_true(!(which_lines_all$lines_mat %>%
                            dplyr::select(- .data$idx) %>% is.na %>%
                            all))
  testthat::expect_equal(which_lines_all$lines_mat,
                         expected_full_df_template)
  ### removed_mat should be NAs
  testthat::expect_true(which_lines_all$removed_mat %>%
                          dplyr::select(- .data$idx) %>% is.na %>%
                          all)

})

test_that("basic tests for steps_along_2d_line",{
  #straight line
  my_mat <- data.frame(x = c(1,20), y = c(1,20)) %>%
    as.matrix()

  for (num_splits in sample(5:25,size = 5)){
    my_df_compression <- steps_along_2d_line(my_mat, n_steps = num_splits)

    testthat::expect_equal(nrow(my_df_compression), num_splits + 1)

    testthat::expect_equal(diff(my_df_compression[,1]),
                           rep(19/(num_splits), num_splits))

    testthat::expect_equal(my_df_compression[,1], my_df_compression[,2])
  }

  # default n_steps
  testthat::expect_equal(dim(steps_along_2d_line(my_mat)), c(101,2))
})

test_that("remove_delta_off_line tests - basic", {
  # basic tests
  # in x
  line <- data.frame(x = c(0,1),
                     y = c(0,0)) %>%
    as.matrix()

  delta = 1/4

  out <- remove_delta_off_line(line, delta)
  expected_out <- data.frame(x = c(.25,.75),
                             y = c(0,0)) %>%
    as.matrix()

  testthat::expect_equal(out, expected_out)

  # in both:
  line <- data.frame(x = c(0,1),
                     y = c(0,1)) %>%
    as.matrix()

  delta = sqrt(2)/4

  out <- remove_delta_off_line(line, delta)
  expected_out <- data.frame(x = c(.25,.75),
                             y = c(.25,.75)) %>%
    as.matrix()

  testthat::expect_equal(out, expected_out)
})

