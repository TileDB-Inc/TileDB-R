# Convert a TileDB Config object to `data.frame`

Convert a TileDB Config object to `data.frame`

## Usage

``` r
# S3 method for class 'tiledb_config'
as.data.frame(x, ...)
```

## Arguments

- x:

  A `tiledb_config` object

- ...:

  Extra parameter for method signature, currently unused.

## Value

a data.frame wth parameter, value columns

## Examples

``` r
cfg <- tiledb_config()
as.data.frame(cfg)
#>                                                      parameter
#> 1                                        config.env_var_prefix
#> 2                                        config.logging_format
#> 3                                         config.logging_level
#> 4                                        filestore.buffer_size
#> 5                                                  profile_dir
#> 6                                                 profile_name
#> 7                                   rest.capnp_traversal_limit
#> 8                                        rest.curl.buffer_size
#> 9                                       rest.curl.retry_errors
#> 10                                     rest.curl.tcp_keepalive
#> 11                                           rest.curl.verbose
#> 12                                        rest.http_compressor
#> 13                        rest.load_enumerations_on_array_open
#> 14            rest.load_enumerations_on_array_open_all_schemas
#> 15                            rest.load_metadata_on_array_open
#> 16                    rest.load_non_empty_domain_on_array_open
#> 17                                        rest.payer_namespace
#> 18                                            rest.retry_count
#> 19                                     rest.retry_delay_factor
#> 20                                       rest.retry_http_codes
#> 21                                 rest.retry_initial_delay_ms
#> 22                                         rest.server_address
#> 23                            rest.server_serialization_format
#> 24                              rest.use_refactored_array_open
#> 25             rest.use_refactored_array_open_and_query_submit
#> 26                          sm.allow_separate_attribute_writes
#> 27                               sm.allow_updates_experimental
#> 28                                         sm.check_coord_dups
#> 29                                          sm.check_coord_oob
#> 30                                       sm.check_global_order
#> 31                                sm.compute_concurrency_level
#> 32                              sm.consolidation.amplification
#> 33                                sm.consolidation.buffer_size
#> 34                          sm.consolidation.max_fragment_size
#> 35                                       sm.consolidation.mode
#> 36                        sm.consolidation.purge_deleted_cells
#> 37                             sm.consolidation.step_max_frags
#> 38                             sm.consolidation.step_min_frags
#> 39                            sm.consolidation.step_size_ratio
#> 40                                      sm.consolidation.steps
#> 41                              sm.consolidation.timestamp_end
#> 42                            sm.consolidation.timestamp_start
#> 43                                             sm.dedup_coords
#> 44                                   sm.enable_signal_handlers
#> 45                                           sm.encryption_key
#> 46                                          sm.encryption_type
#> 47                                    sm.enumerations_max_size
#> 48                              sm.enumerations_max_total_size
#> 49                               sm.fragment_info.preload_mbrs
#> 50                                      sm.group.timestamp_end
#> 51                                    sm.group.timestamp_start
#> 52                                     sm.io_concurrency_level
#> 53                                    sm.max_tile_overlap_size
#> 54                         sm.mem.consolidation.buffers_weight
#> 55                          sm.mem.consolidation.reader_weight
#> 56                          sm.mem.consolidation.writer_weight
#> 57                                          sm.mem.malloc_trim
#> 58          sm.mem.reader.sparse_global_order.ratio_array_data
#> 59              sm.mem.reader.sparse_global_order.ratio_coords
#> 60         sm.mem.reader.sparse_global_order.ratio_tile_ranges
#> 61   sm.mem.reader.sparse_unordered_with_dups.ratio_array_data
#> 62       sm.mem.reader.sparse_unordered_with_dups.ratio_coords
#> 63  sm.mem.reader.sparse_unordered_with_dups.ratio_tile_ranges
#> 64                              sm.mem.tile_upper_memory_limit
#> 65                                         sm.mem.total_budget
#> 66                                            sm.memory_budget
#> 67                                        sm.memory_budget_var
#> 68                    sm.merge_overlapping_ranges_experimental
#> 69                             sm.partial_tile_offsets_loading
#> 70                                sm.query.condition_evaluator
#> 71                               sm.query.dense.qc_coords_mode
#> 72                                       sm.query.dense.reader
#> 73          sm.query.sparse_global_order.preprocess_tile_merge
#> 74                         sm.query.sparse_global_order.reader
#> 75                  sm.query.sparse_unordered_with_dups.reader
#> 76                                           sm.read_range_oob
#> 77                                 sm.skip_checksum_validation
#> 78                               sm.skip_est_size_partitioning
#> 79                     sm.skip_unary_partitioning_budget_check
#> 80                                              sm.vacuum.mode
#> 81                                      sm.var_offsets.bitsize
#> 82                                sm.var_offsets.extra_element
#> 83                                         sm.var_offsets.mode
#> 84                                                 ssl.ca_file
#> 85                                                 ssl.ca_path
#> 86                                                  ssl.verify
#> 87                                     vfs.azure.blob_endpoint
#> 88                             vfs.azure.block_list_block_size
#> 89                             vfs.azure.is_data_lake_endpoint
#> 90                                  vfs.azure.max_parallel_ops
#> 91                                       vfs.azure.max_retries
#> 92                                vfs.azure.max_retry_delay_ms
#> 93                                    vfs.azure.retry_delay_ms
#> 94                               vfs.azure.storage_account_key
#> 95                              vfs.azure.storage_account_name
#> 96                                 vfs.azure.storage_sas_token
#> 97                             vfs.azure.use_block_list_upload
#> 98                        vfs.file.posix_directory_permissions
#> 99                             vfs.file.posix_file_permissions
#> 100                                           vfs.gcs.endpoint
#> 101                        vfs.gcs.impersonate_service_account
#> 102                             vfs.gcs.max_direct_upload_size
#> 103                                   vfs.gcs.max_parallel_ops
#> 104                                    vfs.gcs.multi_part_size
#> 105                                         vfs.gcs.project_id
#> 106                                 vfs.gcs.request_timeout_ms
#> 107                                vfs.gcs.service_account_key
#> 108                              vfs.gcs.use_multi_part_upload
#> 109                    vfs.gcs.workload_identity_configuration
#> 110                                         vfs.log_operations
#> 111                                         vfs.max_batch_size
#> 112                                          vfs.min_batch_gap
#> 113                                         vfs.min_batch_size
#> 114                                      vfs.min_parallel_size
#> 115                                  vfs.read_ahead_cache_size
#> 116                                        vfs.read_ahead_size
#> 117                                      vfs.read_logging_mode
#> 118                                   vfs.s3.aws_access_key_id
#> 119                                     vfs.s3.aws_external_id
#> 120                                  vfs.s3.aws_load_frequency
#> 121                                        vfs.s3.aws_role_arn
#> 122                               vfs.s3.aws_secret_access_key
#> 123                                    vfs.s3.aws_session_name
#> 124                                   vfs.s3.aws_session_token
#> 125                                   vfs.s3.bucket_canned_acl
#> 126                                             vfs.s3.ca_file
#> 127                                             vfs.s3.ca_path
#> 128                                       vfs.s3.config_source
#> 129                                   vfs.s3.connect_max_tries
#> 130                                vfs.s3.connect_scale_factor
#> 131                                  vfs.s3.connect_timeout_ms
#> 132                                   vfs.s3.endpoint_override
#> 133                             vfs.s3.install_sigpipe_handler
#> 134                                       vfs.s3.logging_level
#> 135                                    vfs.s3.max_parallel_ops
#> 136                                 vfs.s3.multipart_part_size
#> 137                                     vfs.s3.no_sign_request
#> 138                                   vfs.s3.object_canned_acl
#> 139                                          vfs.s3.proxy_host
#> 140                                      vfs.s3.proxy_password
#> 141                                          vfs.s3.proxy_port
#> 142                                        vfs.s3.proxy_scheme
#> 143                                      vfs.s3.proxy_username
#> 144                                              vfs.s3.region
#> 145                                  vfs.s3.request_timeout_ms
#> 146                                      vfs.s3.requester_pays
#> 147                                              vfs.s3.scheme
#> 148                                           vfs.s3.skip_init
#> 149                                                 vfs.s3.sse
#> 150                                      vfs.s3.sse_kms_key_id
#> 151                                       vfs.s3.storage_class
#> 152                                vfs.s3.use_multipart_upload
#> 153                              vfs.s3.use_virtual_addressing
#> 154                                          vfs.s3.verify_ssl
#>                      value
#> 1                  TILEDB_
#> 2                  DEFAULT
#> 3                        0
#> 4                104857600
#> 5                         
#> 6                         
#> 7               2147483648
#> 8                   524288
#> 9                     true
#> 10                    true
#> 11                   false
#> 12                     any
#> 13                   false
#> 14                   false
#> 15                    true
#> 16                    true
#> 17                        
#> 18                      25
#> 19                    1.25
#> 20                     503
#> 21                     500
#> 22  https://api.tiledb.com
#> 23                   CAPNP
#> 24                    true
#> 25                    true
#> 26                   false
#> 27                   false
#> 28                    true
#> 29                    true
#> 30                    true
#> 31                       4
#> 32                     1.0
#> 33                50000000
#> 34    18446744073709551615
#> 35               fragments
#> 36                   false
#> 37              4294967295
#> 38              4294967295
#> 39                     0.0
#> 40              4294967295
#> 41    18446744073709551615
#> 42                       0
#> 43                   false
#> 44                    true
#> 45                        
#> 46           NO_ENCRYPTION
#> 47                10485760
#> 48                52428800
#> 49                   false
#> 50    18446744073709551615
#> 51                       0
#> 52                       4
#> 53               314572800
#> 54                       1
#> 55                       3
#> 56                       2
#> 57                    true
#> 58                     0.1
#> 59                     0.5
#> 60                     0.1
#> 61                     0.1
#> 62                     0.5
#> 63                     0.1
#> 64              1073741824
#> 65             10737418240
#> 66              5368709120
#> 67             10737418240
#> 68                    true
#> 69                   false
#> 70                     ast
#> 71                   false
#> 72              refactored
#> 73                   32768
#> 74              refactored
#> 75              refactored
#> 76                    warn
#> 77                   false
#> 78                   false
#> 79                   false
#> 80               fragments
#> 81                      64
#> 82                   false
#> 83                   bytes
#> 84                        
#> 85                        
#> 86                    true
#> 87                        
#> 88                 5242880
#> 89                        
#> 90                       4
#> 91                       5
#> 92                   60000
#> 93                     800
#> 94                        
#> 95                        
#> 96                        
#> 97                    true
#> 98                     755
#> 99                     644
#> 100                       
#> 101                       
#> 102            10737418240
#> 103                      4
#> 104                5242880
#> 105                       
#> 106                   3000
#> 107                       
#> 108                   true
#> 109                       
#> 110                  false
#> 111              104857600
#> 112                 512000
#> 113               20971520
#> 114               10485760
#> 115               10485760
#> 116                 102400
#> 117                       
#> 118                       
#> 119                       
#> 120                       
#> 121                       
#> 122                       
#> 123                       
#> 124                       
#> 125                NOT_SET
#> 126                       
#> 127                       
#> 128                   auto
#> 129                      5
#> 130                     25
#> 131                  10800
#> 132                       
#> 133                   true
#> 134                    Off
#> 135                      4
#> 136                5242880
#> 137                  false
#> 138                NOT_SET
#> 139                       
#> 140                       
#> 141                      0
#> 142                   http
#> 143                       
#> 144                       
#> 145                   3000
#> 146                  false
#> 147                  https
#> 148                  false
#> 149                       
#> 150                       
#> 151                NOT_SET
#> 152                   true
#> 153                   true
#> 154                   true
```
