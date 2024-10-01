unit LevelDB.Main;

{$mode objfpc}{$H+}

interface

uses
  ctypes
, LevelDB.Common
;

procedure leveldb_destroy_db(options: Pleveldb_options_t; const name: PChar; err: PChar); cdecl; external libname;
// Tested
function leveldb_open(options: Pleveldb_options_t; const name: PChar; err: PChar): Pleveldb_t; cdecl; external libname;
// Tested
procedure leveldb_close(db: Pleveldb_t); cdecl; external libname;
// Tested
function leveldb_put(db: Pleveldb_t; options: Pleveldb_writeoptions_t; const key: PChar; keylen: csize_t;
  const value: PChar; valuelen: csize_t; err: PChar): cint; cdecl; external libname;
// Tested
function leveldb_get(db: Pleveldb_t; options: Pleveldb_readoptions_t; const key: PChar; keylen: csize_t;
  valuelen: Pcsize_t; err: PChar): PChar; cdecl; external libname;
// Tested
procedure leveldb_delete(db: Pleveldb_t; options: Pleveldb_writeoptions_t; const key: PChar;
  keylen: csize_t; err: PChar); cdecl; external libname;
// Tested
procedure leveldb_write(db: Pleveldb_t; options: Pleveldb_writeoptions_t; batch: Pleveldb_writebatch_t;
  err: PChar); cdecl; external libname;
// Tested
procedure leveldb_free(ptr: Pointer); cdecl; external libname;

implementation

end.
