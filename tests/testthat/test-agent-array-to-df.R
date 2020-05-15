## SKG
## Tests for agent array to df
## May 14, 2020

test_that("agent_array_to_mat", {

   arr <- array(-1, dim = c(2, 3, 4))
   arr[1, 1,] <- c(0, 0, 1, 1)
   arr[1, 2,] <- c(-1, -1, 1, 1)
   arr[1, 3,] <- c(0, 1, 1, 1)
   arr[2, 1,] <- c(0, 1, 1, 1)
   arr[2, 2,] <- c(-1, -1, 1, 1)
   arr[2, 3,] <- c(0, 0, 0, 0)


   out <- agent_array_to_mat(arr)

   exp_out <- matrix(c(1, 1, 1, 0,
                       1, 1, 3, 1,
                       1, 2, 3, 1,
                       1, 3, 1, 0,
                       1, 3, 2, 1,
                       2, 1, 1, 0,
                       2, 1, 2, 1,
                       2, 2, 3, 1,
                       2, 3, 1, 0), ncol = 4, byrow = TRUE)
   expect_equal(out, exp_out)

   ## ##############################################
   ## person 2 should never be born and not show up in df
   arr <- array(-1, dim = c(2, 3, 4))
   arr[1, 1,] <- c(0, 0, 1, 1)
   arr[1, 2,] <- c(-1, -1, -1, -1)
   arr[1, 3,] <- c(0, 1, 1, 1)
   arr[2, 1,] <- c(0, 1, 1, 1)
   arr[2, 2,] <- c(-1, -1, -1, -1)
   arr[2, 3,] <- c(0, 0, 0, 0)


   out <- agent_array_to_mat(arr)

   exp_out <- matrix(c(1, 1, 1, 0,
                       1, 1, 3, 1,
                       1, 3, 1, 0,
                       1, 3, 2, 1,
                       2, 1, 1, 0,
                       2, 1, 2, 1,
                       2, 3, 1, 0), ncol = 4, byrow = TRUE)
   expect_equal(out, exp_out)
})


