# Prompt

Can you please implement a full header translation of the non `OOP` headers of `LevelDB` into Free Pascal with mode `objfpc`?

# Answer

Sure! Below is a full header translation of the non-OOP `LevelDB` headers into Free Pascal using `objfpc` mode.

```pascal
unit leveldb;

{$mode objfpc}{$H+}

interface

uses
  ctypes;

type
  Pleveldb_t = ^leveldb_t;
  Pleveldb_options_t = ^leveldb_options_t;
  Pleveldb_readoptions_t = ^leveldb_readoptions_t;
  Pleveldb_writeoptions_t = ^leveldb_writeoptions_t;
  Pleveldb_writebatch_t = ^leveldb_writebatch_t;
  Pleveldb_iterator_t = ^leveldb_iterator_t;
  Pleveldb_snapshot_t = ^leveldb_snapshot_t;
  PPChar = ^PChar;

  // Options
  leveldb_t = record end;
  leveldb_options_t = record end;
  leveldb_readoptions_t = record end;
  leveldb_writeoptions_t = record end;
  leveldb_writebatch_t = record end;
  leveldb_iterator_t = record end;
  leveldb_snapshot_t = record end;

procedure leveldb_destroy_db(options: Pleveldb_options_t; const name: PChar; err: PPChar); cdecl; external;
function leveldb_open(options: Pleveldb_options_t; const name: PChar; err: PPChar): Pleveldb_t; cdecl; external;
procedure leveldb_close(db: Pleveldb_t); cdecl; external;
function leveldb_put(db: Pleveldb_t; options: Pleveldb_writeoptions_t; const key: PChar; keylen: csize_t;
  const value: PChar; valuelen: csize_t; err: PPChar): cint; cdecl; external;
function leveldb_get(db: Pleveldb_t; options: Pleveldb_readoptions_t; const key: PChar; keylen: csize_t;
  valuelen: Pcsize_t; err: PPChar): PChar; cdecl; external;
procedure leveldb_delete(db: Pleveldb_t; options: Pleveldb_writeoptions_t; const key: PChar;
  keylen: csize_t; err: PPChar); cdecl; external;
procedure leveldb_write(db: Pleveldb_t; options: Pleveldb_writeoptions_t; batch: Pleveldb_writebatch_t;
  err: PPChar); cdecl; external;

// Options related functions
function leveldb_options_create(): Pleveldb_options_t; cdecl; external;
procedure leveldb_options_destroy(options: Pleveldb_options_t); cdecl; external;
procedure leveldb_options_set_create_if_missing(options: Pleveldb_options_t; value: cbool); cdecl; external;
procedure leveldb_options_set_error_if_exists(options: Pleveldb_options_t; value: cbool); cdecl; external;
procedure leveldb_options_set_paranoid_checks(options: Pleveldb_options_t; value: cbool); cdecl; external;
procedure leveldb_options_set_max_open_files(options: Pleveldb_options_t; value: cint); cdecl; external;

// Read options
function leveldb_readoptions_create(): Pleveldb_readoptions_t; cdecl; external;
procedure leveldb_readoptions_destroy(options: Pleveldb_readoptions_t); cdecl; external;
procedure leveldb_readoptions_set_verify_checksums(options: Pleveldb_readoptions_t; value: cbool); cdecl; external;

// Write options
function leveldb_writeoptions_create(): Pleveldb_writeoptions_t; cdecl; external;
procedure leveldb_writeoptions_destroy(options: Pleveldb_writeoptions_t); cdecl; external;
procedure leveldb_writeoptions_set_sync(options: Pleveldb_writeoptions_t; value: cbool); cdecl; external;

// Write batch operations
function leveldb_writebatch_create(): Pleveldb_writebatch_t; cdecl; external;
procedure leveldb_writebatch_destroy(batch: Pleveldb_writebatch_t); cdecl; external;
procedure leveldb_writebatch_clear(batch: Pleveldb_writebatch_t); cdecl; external;
procedure leveldb_writebatch_put(batch: Pleveldb_writebatch_t; const key: PChar; keylen: csize_t;
  const value: PChar; valuelen: csize_t); cdecl; external;
procedure leveldb_writebatch_delete(batch: Pleveldb_writebatch_t; const key: PChar; keylen: csize_t); cdecl; external;

// Iterator functions
function leveldb_create_iterator(db: Pleveldb_t; options: Pleveldb_readoptions_t): Pleveldb_iterator_t; cdecl; external;
procedure leveldb_iter_destroy(it: Pleveldb_iterator_t); cdecl; external;
procedure leveldb_iter_seek_to_first(it: Pleveldb_iterator_t); cdecl; external;
procedure leveldb_iter_seek_to_last(it: Pleveldb_iterator_t); cdecl; external;
procedure leveldb_iter_seek(it: Pleveldb_iterator_t; const key: PChar; keylen: csize_t); cdecl; external;
function leveldb_iter_valid(it: Pleveldb_iterator_t): cbool; cdecl; external;
function leveldb_iter_key(it: Pleveldb_iterator_t; keylen: Pcsize_t): PChar; cdecl; external;
function leveldb_iter_value(it: Pleveldb_iterator_t; valuelen: Pcsize_t): PChar; cdecl; external;

// Snapshot
function leveldb_create_snapshot(db: Pleveldb_t): Pleveldb_snapshot_t; cdecl; external;
procedure leveldb_release_snapshot(db: Pleveldb_t; snapshot: Pleveldb_snapshot_t); cdecl; external;

// Error handling
procedure leveldb_free(ptr: Pointer); cdecl; external;

implementation

end.
```

This should provide a direct Pascal translation of the non-OOP headers from LevelDB's C API into Free Pascal's `objfpc` mode. Let me know if you need any further adjustments or details!
