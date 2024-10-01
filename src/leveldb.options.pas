unit LevelDB.Options;

{$mode objfpc}{$H+}

interface

uses
  ctypes
, LevelDB.Common
;

const
  LEVELDB_DEFAULT_WRITE_BUFFER_SIZE = 4 * 1024 * 1024;
  LEVELDB_DEFAULT_MAX_OPEN_FILES = 1000;
  LEVELDB_DEFAULT_BLOCK_SIZE = 4 * 1024;
  LEVELDB_DEFAULT_BLOCK_RESTART_INTERVAL = 16;
  LEVELDB_DEFAULT_MAX_FILE_SIZE = 2 * 1024 * 1024;

function leveldb_options_create(): Pleveldb_options_t; cdecl; external  libname;
procedure leveldb_options_destroy(options: Pleveldb_options_t); cdecl; external  libname;
procedure leveldb_options_set_comparator(options: Pleveldb_options_t; cmp: Pleveldb_comparator_t); cdecl; external  libname;
procedure leveldb_options_set_filter_policy(options: Pleveldb_options_t; policy: Pleveldb_filterpolicy_t); cdecl; external  libname;
procedure leveldb_options_set_create_if_missing(options: Pleveldb_options_t; value: cbool); cdecl; external  libname;
procedure leveldb_options_set_error_if_exists(options: Pleveldb_options_t; value: cbool); cdecl; external  libname;
procedure leveldb_options_set_paranoid_checks(options: Pleveldb_options_t; value: cbool); cdecl; external  libname;
procedure leveldb_options_set_env(options: Pleveldb_options_t; env: Pleveldb_env_t); cdecl; external  libname;
procedure leveldb_options_set_info_log(options: Pleveldb_options_t; logger: Pleveldb_logger_t); cdecl; external  libname;
procedure leveldb_options_set_write_buffer_size(options: Pleveldb_options_t; size: cint); cdecl; external  libname;
procedure leveldb_options_set_max_open_files(options: Pleveldb_options_t; num: cint); cdecl; external  libname;
procedure leveldb_options_set_cache(options: Pleveldb_options_t; cache: Pleveldb_cache_t); cdecl; external  libname;
procedure leveldb_options_set_block_size(options: Pleveldb_options_t; size: cint); cdecl; external  libname;
procedure leveldb_options_set_block_restart_interval(options: Pleveldb_options_t; interval: cint); cdecl; external  libname;
procedure leveldb_options_set_max_file_size(options: Pleveldb_options_t; size: cint); cdecl; external  libname;
procedure leveldb_options_set_compression(options: Pleveldb_options_t; level: cint); cdecl; external  libname;

// Read options
function leveldb_readoptions_create(): Pleveldb_readoptions_t; cdecl; external  libname;
procedure leveldb_readoptions_destroy(options: Pleveldb_readoptions_t); cdecl; external  libname;
procedure leveldb_readoptions_set_verify_checksums(options: Pleveldb_readoptions_t; value: cbool); cdecl; external  libname;
procedure leveldb_readoptions_set_fill_cache(options: Pleveldb_readoptions_t; value: cbool); cdecl; external  libname;
procedure leveldb_readoptions_set_snapshot(options: Pleveldb_readoptions_t; snapshot: Pleveldb_snapshot_t); cdecl; external  libname;

// Write options
function leveldb_writeoptions_create(): Pleveldb_writeoptions_t; cdecl; external  libname;
procedure leveldb_writeoptions_destroy(options: Pleveldb_writeoptions_t); cdecl; external  libname;
procedure leveldb_writeoptions_set_sync(options: Pleveldb_writeoptions_t; value: cbool); cdecl; external  libname;

implementation

end.
