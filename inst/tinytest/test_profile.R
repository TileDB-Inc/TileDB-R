library(tinytest)
library(tiledb)

# Use base directory to prevent over-writing user profiles.
base_dir <- tempdir()
dir1 <- file.path(base_dir, "tiledb_profile/")


# 1. Test creating profiles.
profile1 <- tiledb_profile()
profile2 <- tiledb_profile("profile2")
profile3 <- tiledb_profile(dir=dir1)
profile4 <- tiledb_profile("profile4", dir1)

expect_equal(tiledb_profile_name(profile1), "default")
expect_equal(tiledb_profile_name(profile2), "profile2")
expect_equal(tiledb_profile_name(profile3), "default")
expect_equal(tiledb_profile_name(profile4), "profile4")

expect_true(dir.exists(tiledb_profile_dir(profile1)))
expect_true(dir.exists(tiledb_profile_dir(profile2)))
expect_equal(tiledb_profile_dir(profile3), dir1)
expect_equal(tiledb_profile_dir(profile4), dir1)


# 2. Test setting/getting profile parameters.
key1 <- "username"
key2 <- "server_address"
key3 <- "rest.token"
expected_value1 <- "my_username"
expected_value2 <- "https://my.address"
expected_value3 <- "123456"

tiledb_profile_set_param(profile4, key1, expected_value1)
tiledb_profile_set_param(profile4, key2, expected_value2)
tiledb_profile_set_param(profile4, key3, expected_value3)

actual_value1 <- tiledb_profile_get_param(profile4, key1)
actual_value2 <- tiledb_profile_get_param(profile4, key2)
actual_value3 <- tiledb_profile_get_param(profile4, key3)
actual_non_value <- tiledb_profile_get_param(profile4, "not_a_parameter")

expect_equal(actual_value1, expected_value1)
expect_equal(actual_value2, expected_value2)
expect_equal(actual_value3, expected_value3)
expect_true(is.null(actual_non_value))

# 3. Test save, load, and remove.
# IMPORTANT: Do not save a profile to a location that might over-write an actual profile.
tiledb_profile_save(profile3)
tiledb_profile_save(profile4)

# -- Check can load profile3
profile_loaded <- tiledb_profile_load(dir=dir1)

# -- Remove profile2. Check can no longer load.
tiledb_profile_remove(dir=dir1)
expect_error(tiledb_profile_load(dir=dir1))

# -- Check loaded profile4
profile_loaded <- tiledb_profile_load("profile4", dir1)
expect_equal(tiledb_profile_name(profile_loaded), "profile4")
expect_equal(tiledb_profile_name(profile_loaded), "profile4")
loaded_value1 <- tiledb_profile_get_param(profile_loaded, key1)
loaded_value2 <- tiledb_profile_get_param(profile_loaded, key2)
loaded_value3 <- tiledb_profile_get_param(profile_loaded, key3)
loaded_non_value <- tiledb_profile_get_param(profile_loaded, "not_a_parameter")
expect_equal(loaded_value1, expected_value1)
expect_equal(loaded_value2, expected_value2)
expect_equal(loaded_value3, expected_value3)
expect_true(is.null(loaded_non_value))

# -- Remove profile4
tiledb_profile_remove("profile4", dir1)

# -- Check cannot load profile2 or profile4.
expect_error(tiledb_profile_load(dir=dir1))
expect_error(tiledb_profile_load("profile4", dir1))
