unit LevelDB.Batch;

{$mode objfpc}{$H+}

interface

uses
  ctypes
, LevelDB.Common
;

// Tested
function leveldb_writebatch_create(): Pleveldb_writebatch_t; cdecl; external libname;
// Tested
procedure leveldb_writebatch_destroy(batch: Pleveldb_writebatch_t); cdecl; external libname;
// Tested
procedure leveldb_writebatch_clear(batch: Pleveldb_writebatch_t); cdecl; external libname;
// Tested
procedure leveldb_writebatch_put(batch: Pleveldb_writebatch_t; const key: PChar; keylen: csize_t; const val: PChar; vallen: csize_t); cdecl; external libname;
// Tested
procedure leveldb_writebatch_delete(batch: Pleveldb_writebatch_t; const key: PChar; keylen: csize_t); cdecl; external libname;
//procedure leveldb_writebatch_iterate(batch: Pleveldb_writebatch_t; state: Pleveldb_state_t; put: procedure(state: Pleveldb_state_t; const key: PChar; keylen: csize_t; const val: PChar; vallen: csize_t); cdecl; deleted: procedure(state: Pleveldb_state_t; const key: PChar; keylen: csize_t); cdecl); cdecl; external libname;
function leveldb_writebatch_append(destination: Pleveldb_writebatch_t; source: Pleveldb_writebatch_t): leveldb_status_t; cdecl; external libname;

implementation

end.
