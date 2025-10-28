library(tinytest)
library(tiledb)

# Use base directory to prevent over-writing user profiles.
name <- "test_profile"
dir <- file.path(tempdir(), "tiledb_config_profile/")

# Create profile
profile <- tiledb_profile(name, dir)

# Set  some parameters
token <- "12345"
server_address <- "https://profile_address.com"
tiledb_profile_set_param(profile, "rest.token", token)
tiledb_profile_set_param(profile, "rest.server_address", server_address)

# Save the profile
tiledb_profile_save(profile)


# Create a config and set the profile directory
config <- tiledb_config(c(profile_name = name, profile_dir = dir))
config_profile_name <- config["profile_name"]
config_profile_dir <- config["profile_dir"]
config_rest_token <- config["rest.token"]
config_rest_server_address <- config["rest.server_address"]

# Test that the config parameters are set correctly
expect_equal(config["profile_name"], c(profile_name = name))
expect_equal(config["profile_dir"], c(profile_dir = dir))
expect_equal(config["rest.token"], c(rest.token = token))
expect_equal(config["rest.server_address"], c(rest.server_address = server_address))


# Remove the profile
tiledb_profile_remove(name, dir)
