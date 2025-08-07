# This test file checks the contents of the configuration.rda data file
# to ensure it contains all expected columns and values.
# It also verifies that the values in certain columns match expected sets.
# If you add new columns or change the configuration, update these tests accordingly.
test_that("configuration.rda contains what we expect", {
    # Note: configuration.rda is automatically loaded when package is installed

    # Check if the configuration file contains all the expected stuff
    expect_true(
        all(
            c(
                "species",
                "fxn",
                "source",
                "family",
                "formula",
                "min_depth",
                "max_depth",
                "min_latitude",
                "max_latitude",
                "min_year",
                "max_year",
                "anisotropy",
                "knots",
                "spatiotemporal1",
                "spatiotemporal2",
                "share_range",
                "used"
            ) %in%
                names(indexwc::configuration)
        )
    )
    # Check that all values in the 'source' column are from the expected set
    expect_true(
        all(
            unique(indexwc::configuration[["source"]]) %in%
                c("NWFSC.Combo", "Triennial", "AFSC.Slope", "NWFSC.Slope")
        )
    )

    # Check that all values in the 'spatiotemporal1' column are either "iid" or "off"
    expect_true(
        all(
            unique(indexwc::configuration[["spatiotemporal1"]]) %in%
                c("iid", "off")
        )
    )

    # Check that all values in the 'spatiotemporal2' column are either "iid" or "off"
    expect_true(
        all(
            unique(indexwc::configuration[["spatiotemporal2"]]) %in%
                c("iid", "off")
        )
    )

    # Check that the 'used' column contains only logical values or NA
    expect_true(
        all(
            is.na(indexwc::configuration[["used"]]) |
                is.logical(indexwc::configuration[["used"]])
        )
    )
})
