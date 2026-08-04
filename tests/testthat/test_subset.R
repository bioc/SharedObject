context("Subset shared vectors")

test_that("normalized and out-of-range indices match base R", {
    value <- as.integer(101:110)
    indices <- list(
        negative_and_zero = -3:0,
        upper_out_of_range = 5:(length(value) + 1L),
        nonconsecutive_duplicates = c(8L, 2L, 8L, 1L),
        zeros_mixed_positive = c(0L, 5L, 0L, 2L),
        missing_and_out_of_range = c(NA_integer_, 1L, 11L)
    )

    for (sharedSubset in c(FALSE, TRUE)) {
        shared <- share(value, sharedSubset = sharedSubset)
        for (index in indices) {
            result <- shared[index]
            expect_identical(result, value[index])
            expect_identical(is.shared(result), sharedSubset)
        }
    }

    gc()
})


