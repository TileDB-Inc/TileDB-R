# Save a TileDB Config object to a local text file

Save a TileDB Config object to a local text file

## Usage

``` r
tiledb_config_save(config, path)
```

## Arguments

- config:

  A `tiledb_config` object

- path:

  The path to config file to be created

## Value

path to created config file

## Examples

``` r
tmp <- tempfile()
cfg <- tiledb_config(c("sm.tile_cache_size" = "10"))
pth <- tiledb_config_save(cfg, tmp)

cat(readLines(pth), sep = "\n")
#> config.env_var_prefix TILEDB_
#> config.logging_format DEFAULT
#> config.logging_level 0
#> filestore.buffer_size 104857600
#> rest.capnp_traversal_limit 2147483648
#> rest.curl.buffer_size 524288
#> rest.curl.retry_errors true
#> rest.curl.tcp_keepalive true
#> rest.curl.verbose false
#> rest.http_compressor any
#> rest.load_enumerations_on_array_open false
#> rest.load_enumerations_on_array_open_all_schemas false
#> rest.load_metadata_on_array_open true
#> rest.load_non_empty_domain_on_array_open true
#> rest.retry_count 25
#> rest.retry_delay_factor 1.25
#> rest.retry_http_codes 503
#> rest.retry_initial_delay_ms 500
#> rest.server_address https://api.tiledb.com
#> rest.server_serialization_format CAPNP
#> rest.use_refactored_array_open true
#> rest.use_refactored_array_open_and_query_submit true
#> sm.allow_separate_attribute_writes false
#> sm.allow_updates_experimental false
#> sm.check_coord_dups true
#> sm.check_coord_oob true
#> sm.check_global_order true
#> sm.compute_concurrency_level 4
#> sm.consolidation.amplification 1.0
#> sm.consolidation.buffer_size 50000000
#> sm.consolidation.max_fragment_size 18446744073709551615
#> sm.consolidation.mode fragments
#> sm.consolidation.purge_deleted_cells false
#> sm.consolidation.step_max_frags 4294967295
#> sm.consolidation.step_min_frags 4294967295
#> sm.consolidation.step_size_ratio 0.0
#> sm.consolidation.steps 4294967295
#> sm.consolidation.timestamp_end 18446744073709551615
#> sm.consolidation.timestamp_start 0
#> sm.dedup_coords false
#> sm.enable_signal_handlers true
#> sm.encryption_type NO_ENCRYPTION
#> sm.enumerations_max_size 10485760
#> sm.enumerations_max_total_size 52428800
#> sm.fragment_info.preload_mbrs false
#> sm.group.timestamp_end 18446744073709551615
#> sm.group.timestamp_start 0
#> sm.io_concurrency_level 4
#> sm.max_tile_overlap_size 314572800
#> sm.mem.consolidation.buffers_weight 1
#> sm.mem.consolidation.reader_weight 3
#> sm.mem.consolidation.writer_weight 2
#> sm.mem.malloc_trim true
#> sm.mem.reader.sparse_global_order.ratio_array_data 0.1
#> sm.mem.reader.sparse_global_order.ratio_coords 0.5
#> sm.mem.reader.sparse_global_order.ratio_tile_ranges 0.1
#> sm.mem.reader.sparse_unordered_with_dups.ratio_array_data 0.1
#> sm.mem.reader.sparse_unordered_with_dups.ratio_coords 0.5
#> sm.mem.reader.sparse_unordered_with_dups.ratio_tile_ranges 0.1
#> sm.mem.tile_upper_memory_limit 1073741824
#> sm.mem.total_budget 10737418240
#> sm.memory_budget 5368709120
#> sm.memory_budget_var 10737418240
#> sm.merge_overlapping_ranges_experimental true
#> sm.partial_tile_offsets_loading false
#> sm.query.condition_evaluator ast
#> sm.query.dense.qc_coords_mode false
#> sm.query.dense.reader refactored
#> sm.query.sparse_global_order.preprocess_tile_merge 32768
#> sm.query.sparse_global_order.reader refactored
#> sm.query.sparse_unordered_with_dups.reader refactored
#> sm.read_range_oob warn
#> sm.skip_checksum_validation false
#> sm.skip_est_size_partitioning false
#> sm.skip_unary_partitioning_budget_check false
#> sm.tile_cache_size 10
#> sm.vacuum.mode fragments
#> sm.var_offsets.bitsize 64
#> sm.var_offsets.extra_element false
#> sm.var_offsets.mode bytes
#> ssl.verify true
#> vfs.azure.block_list_block_size 5242880
#> vfs.azure.max_parallel_ops 4
#> vfs.azure.max_retries 5
#> vfs.azure.max_retry_delay_ms 60000
#> vfs.azure.retry_delay_ms 800
#> vfs.azure.use_block_list_upload true
#> vfs.file.posix_directory_permissions 755
#> vfs.file.posix_file_permissions 644
#> vfs.gcs.max_direct_upload_size 10737418240
#> vfs.gcs.max_parallel_ops 4
#> vfs.gcs.multi_part_size 5242880
#> vfs.gcs.request_timeout_ms 3000
#> vfs.gcs.use_multi_part_upload true
#> vfs.log_operations false
#> vfs.max_batch_size 104857600
#> vfs.min_batch_gap 512000
#> vfs.min_batch_size 20971520
#> vfs.min_parallel_size 10485760
#> vfs.read_ahead_cache_size 10485760
#> vfs.read_ahead_size 102400
#> vfs.s3.bucket_canned_acl NOT_SET
#> vfs.s3.config_source auto
#> vfs.s3.connect_max_tries 5
#> vfs.s3.connect_scale_factor 25
#> vfs.s3.connect_timeout_ms 10800
#> vfs.s3.install_sigpipe_handler true
#> vfs.s3.logging_level Off
#> vfs.s3.max_parallel_ops 4
#> vfs.s3.multipart_part_size 5242880
#> vfs.s3.no_sign_request false
#> vfs.s3.object_canned_acl NOT_SET
#> vfs.s3.proxy_port 0
#> vfs.s3.proxy_scheme http
#> vfs.s3.request_timeout_ms 3000
#> vfs.s3.requester_pays false
#> vfs.s3.scheme https
#> vfs.s3.skip_init false
#> vfs.s3.storage_class NOT_SET
#> vfs.s3.use_multipart_upload true
#> vfs.s3.use_virtual_addressing true
#> vfs.s3.verify_ssl true
```
