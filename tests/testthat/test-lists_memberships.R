test_that("lists_memberships returns data frame with nrow > 1", {
    skip_on_cran()
    skip_if_offline()
    
    df <- lists_memberships("kearneymw", filter_to_owned_lists = TRUE)
    
    expect_s3_class(df, "tbl_df")
    expect_equal(df$name, "test-memberships")
})
