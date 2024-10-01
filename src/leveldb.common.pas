unit LevelDB.Common;

{$mode objfpc}{$H+}

interface

{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}

type
  Pleveldb_t = ^leveldb_t;
  Pleveldb_cache_t = ^leveldb_cache_t;
  Pleveldb_comparator_t = ^leveldb_comparator_t;
  Pleveldb_env_t = ^leveldb_env_t;
  Pleveldb_filelock_t = ^leveldb_filelock_t;
  Pleveldb_filterpolicy_t = ^leveldb_filterpolicy_t;
  Pleveldb_iterator_t = ^leveldb_iterator_t;
  Pleveldb_logger_t = ^leveldb_logger_t;
  Pleveldb_options_t = ^leveldb_options_t;
  Pleveldb_randomfile_t = ^leveldb_randomfile_t;
  Pleveldb_readoptions_t = ^leveldb_readoptions_t;
  Pleveldb_seqfile_t = ^leveldb_seqfile_t;
  Pleveldb_snapshot_t = ^leveldb_snapshot_t;
  Pleveldb_state_t = ^leveldb_state_t;
  Pleveldb_status_t = ^leveldb_status_t;
  Pleveldb_writablefile_t = ^leveldb_writablefile_t;
  Pleveldb_writebatch_t = ^leveldb_writebatch_t;
  Pleveldb_writeoptions_t = ^leveldb_writeoptions_t;
  //PPChar = ^PChar;

  leveldb_t = record end;
  leveldb_cache_t = record end;
  leveldb_comparator_t = record end;
  leveldb_env_t = record end;
  leveldb_filelock_t = record end;
  leveldb_filterpolicy_t = record end;
  leveldb_iterator_t = record end;
  leveldb_logger_t = record end;
  leveldb_options_t = record end;
  leveldb_randomfile_t = record end;
  leveldb_readoptions_t = record end;
  leveldb_seqfile_t = record end;
  leveldb_snapshot_t = record end;
  leveldb_state_t = record end;
  leveldb_status_t = record end;
  leveldb_writablefile_t = record end;
  leveldb_writebatch_t = record end;
  leveldb_writeoptions_t = record end;

const
{$IFDEF UNIX}
  libname = 'libleveldb.so';
{$ENDIF}
{$IFDEF WINDOWS}
  libname = 'libleveldb.dll'; // May have a different name
{$ENDIF}
{$IFDEF DARWIN}
  libname = 'libleveldb.???'; // May have a different name
{$ENDIF}
  // Status codes
  LEVELDB_OK = 0;
  LEVELDB_NOT_FOUND = 1;
  LEVELDB_CORRUPTION = 2;
  LEVELDB_NOT_SUPPORTED = 3;
  LEVELDB_INVALID_ARGUMENT = 4;
  LEVELDB_IO_ERROR = 5;

  // Compression Types
  LEVELDB_NO_COMPRESSION = 0;
  LEVELDB_SNAPPY_COMPRESSION = 1;

implementation

end.
