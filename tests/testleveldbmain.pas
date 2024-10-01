unit TestLevelDBMain;

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
;

type

  { TTestLevelDBMain }

  TTestLevelDBMain= class(TTestCase)
  private
    FDB: Pleveldb_t;
    FOptions: Pleveldb_options_t;
    FWriteOptions: Pleveldb_writeoptions_t;
    FReadOptions: Pleveldb_readoptions_t;
    FError: PChar;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestLevelDBMainOpen;
    procedure TestLevelDBMainPut;
    procedure TestLevelDBMainGet;
    procedure TestLevelDBMainDelete;
  end;

implementation

uses
  ctypes
;

const
  cDBPath = 'test.db';
  cTestKey = 'LevelDB';
  cTestValue = 'Rocks';

procedure TTestLevelDBMain.SetUp;
begin
  FError:= nil;
  FOptions:= leveldb_options_create();
  leveldb_options_set_create_if_missing(FOptions, True);
  FDB:= leveldb_open(FOptions, PChar(cDBPath), @FError);
end;

procedure TTestLevelDBMain.TearDown;
begin
  // Close down things
  leveldb_options_destroy(FOptions);
  leveldb_close(FDB);

  if FError <> nil then
    leveldb_free(FError); // Free any remaining error strings
end;

procedure TTestLevelDBMain.TestLevelDBMainOpen;
begin
  AssertNull('After open error is null', FError);
end;

procedure TTestLevelDBMain.TestLevelDBMainPut;
begin
  FWriteOptions := leveldb_writeoptions_create();

  leveldb_put(
    FDB,
    FWriteOptions,
    PChar(cTestKey),
    Length(cTestKey),
    PChar(cTestValue),
    Length(cTestValue),
    @FError
  );
  AssertNull('After put error is null', FError);

  leveldb_writeoptions_destroy(FWriteOptions);
end;

procedure TTestLevelDBMain.TestLevelDBMainGet;
var
  value: PChar;
  valueLen: csize_t;
begin
  FReadOptions := leveldb_readoptions_create();
  value := leveldb_get(
    FDB,
    FReadOptions,
    PChar(cTestKey),
    Length(cTestKey),
    @valueLen,
    @FError
  );
  AssertNull('After get error is null', FError);
  AssertEquals('Value returned is "'+cTestValue+'"', cTestValue, StrPas(value));

  leveldb_free(value);
  leveldb_readoptions_destroy(FReadOptions);
end;

procedure TTestLevelDBMain.TestLevelDBMainDelete;
begin
  FWriteOptions := leveldb_writeoptions_create();

  leveldb_delete(
    FDB,
    FWriteOptions,
    PChar(cTestKey),
    Length(cTestKey),
    @FError
  );
  AssertNull('After delete error is null', FError);

  leveldb_writeoptions_destroy(FWriteOptions);
end;

initialization
  RegisterTest(TTestLevelDBMain);
end.

