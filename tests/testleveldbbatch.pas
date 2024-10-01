unit TestLevelDBBatch;

{$mode objfpc}{$H+}

interface

uses
  Classes
, SysUtils
, fpcunit
//, testutils
, testregistry
, LevelDB.Common
, LevelDB.Main
, LevelDB.Options
, LevelDB.Batch
;

type

  { TTestLevelDBBatch }

  TTestLevelDBBatch= class(TTestCase)
  private
    FDB: Pleveldb_t;
    FOptions: Pleveldb_options_t;
    FWriteOptions: Pleveldb_writeoptions_t;
    FBatch: Pleveldb_writebatch_t;
    FError: PChar;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestLevelDBBatchPut;
    procedure TestLevelDBBatchDelete;
    procedure TestLevelDBBatchCommit;
  end;

implementation

const
  cDBPath = 'test.db';
  cTestKey = 'LevelDB';
  cTestValue = 'Rocks';

procedure TTestLevelDBBatch.SetUp;
begin
  FError:= nil;
  FBatch:= nil;
  FOptions:= leveldb_options_create();
  leveldb_options_set_create_if_missing(FOptions, True);
  FDB:= leveldb_open(FOptions, PChar(cDBPath), @FError);
end;

procedure TTestLevelDBBatch.TearDown;
begin
  // Close down things
  if FBatch <> nil then
    leveldb_writebatch_destroy(FBatch);
  leveldb_options_destroy(FOptions);
  leveldb_close(FDB);


  if FError <> nil then
    leveldb_free(FError); // Free any remaining error strings
end;

procedure TTestLevelDBBatch.TestLevelDBBatchPut;
begin
  if FBatch = nil then
    FBatch := leveldb_writebatch_create();
  leveldb_writebatch_put(
    FBatch,
    PChar(cTestKey),
    Length(cTestKey),
    PChar(cTestValue),
    Length(cTestValue)
  );
  // Need to assert something here
end;

procedure TTestLevelDBBatch.TestLevelDBBatchDelete;
begin
  if FBatch = nil then
    FBatch := leveldb_writebatch_create();
  leveldb_writebatch_delete(
    FBatch,
    PChar(cTestKey),
    Length(cTestKey)
  );
  // Need to assert something here
end;

procedure TTestLevelDBBatch.TestLevelDBBatchCommit;
begin
  FWriteOptions := leveldb_writeoptions_create();
  if FBatch <> nil then
  begin
    leveldb_write(
      FDB,
      FWriteOptions,
      FBatch,
      @FError
    );
    AssertNull('After batch commit error is null', FError);
    leveldb_writebatch_clear(FBatch); // Clear the batch after committing
  end;
  leveldb_writeoptions_destroy(FWriteOptions);
end;

initialization
  RegisterTest(TTestLevelDBBatch);
end.

