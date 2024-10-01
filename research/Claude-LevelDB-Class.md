# Prompt

Still using the header wrapper you provided, can you please implement a wrapper class around it, still in mode `objfpc`?

# Answer

Certainly! I'll create a wrapper class around the LevelDB functions we've translated, using the `objfpc` mode in Free Pascal. This wrapper class will encapsulate the LevelDB operations and provide a more object-oriented interface for working with LevelDB.

```pascal
unit LevelDB.Wrapper;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes,
  LevelDB.C, LevelDB.Status, LevelDB.Options;

type
  ELevelDBException = class(Exception);

  { TLevelDB }

  TLevelDB = class
  private
    FDB: leveldb_t;
    FOptions: leveldb_options_t;
    FReadOptions: leveldb_readoptions_t;
    FWriteOptions: leveldb_writeoptions_t;
    FPath: string;

    procedure CheckError(error: PChar);
  public
    constructor Create(const APath: string);
    destructor Destroy; override;

    procedure Put(const AKey, AValue: string);
    function Get(const AKey: string): string;
    procedure Delete(const AKey: string);

    procedure BatchBegin;
    procedure BatchPut(const AKey, AValue: string);
    procedure BatchDelete(const AKey: string);
    procedure BatchCommit;

    property Path: string read FPath;
  end;

implementation

{ TLevelDB }

procedure TLevelDB.CheckError(error: PChar);
begin
  if error <> nil then
  begin
    raise ELevelDBException.Create(string(error));
    leveldb_free(error);
  end;
end;

constructor TLevelDB.Create(const APath: string);
var
  err: PChar;
begin
  FPath := APath;

  FOptions := leveldb_options_create();
  leveldb_options_set_create_if_missing(FOptions, 1);

  FReadOptions := leveldb_readoptions_create();
  FWriteOptions := leveldb_writeoptions_create();

  FDB := leveldb_open(FOptions, PChar(FPath), @err);
  CheckError(err);
end;

destructor TLevelDB.Destroy;
begin
  leveldb_close(FDB);
  leveldb_options_destroy(FOptions);
  leveldb_readoptions_destroy(FReadOptions);
  leveldb_writeoptions_destroy(FWriteOptions);
  inherited Destroy;
end;

procedure TLevelDB.Put(const AKey, AValue: string);
var
  err: PChar;
begin
  leveldb_put(FDB, FWriteOptions, PChar(AKey), Length(AKey), PChar(AValue), Length(AValue), @err);
  CheckError(err);
end;

function TLevelDB.Get(const AKey: string): string;
var
  err: PChar;
  len: NativeUInt;
  value: PChar;
begin
  value := leveldb_get(FDB, FReadOptions, PChar(AKey), Length(AKey), @len, @err);
  CheckError(err);
  if value <> nil then
  begin
    SetString(Result, value, len);
    leveldb_free(value);
  end
  else
    Result := '';
end;

procedure TLevelDB.Delete(const AKey: string);
var
  err: PChar;
begin
  leveldb_delete(FDB, FWriteOptions, PChar(AKey), Length(AKey), @err);
  CheckError(err);
end;

var
  FBatch: leveldb_writebatch_t;

procedure TLevelDB.BatchBegin;
begin
  if FBatch <> nil then
    raise ELevelDBException.Create('Batch operation already in progress');
  FBatch := leveldb_writebatch_create();
end;

procedure TLevelDB.BatchPut(const AKey, AValue: string);
begin
  if FBatch = nil then
    raise ELevelDBException.Create('No batch operation in progress');
  leveldb_writebatch_put(FBatch, PChar(AKey), Length(AKey), PChar(AValue), Length(AValue));
end;

procedure TLevelDB.BatchDelete(const AKey: string);
begin
  if FBatch = nil then
    raise ELevelDBException.Create('No batch operation in progress');
  leveldb_writebatch_delete(FBatch, PChar(AKey), Length(AKey));
end;

procedure TLevelDB.BatchCommit;
var
  err: PChar;
begin
  if FBatch = nil then
    raise ELevelDBException.Create('No batch operation in progress');
  try
    leveldb_write(FDB, FWriteOptions, FBatch, @err);
    CheckError(err);
  finally
    leveldb_writebatch_destroy(FBatch);
    FBatch := nil;
  end;
end;

end.

```

This wrapper class, `TLevelDB`, encapsulates the LevelDB functionality and provides a more Pascal-friendly interface. Here's an overview of the class:

1. It handles the creation and destruction of LevelDB resources in the constructor and destructor.
2. It provides methods for basic CRUD operations: `Put`, `Get`, and `Delete`.
3. It includes methods for batch operations: `BatchBegin`, `BatchPut`, `BatchDelete`, and `BatchCommit`.
4. It uses exceptions (`ELevelDBException`) for error handling.

To demonstrate how to use this wrapper class, here's a simple example:

```pascal
program LevelDBWrapperExample;

{$mode objfpc}{$H+}

uses
  SysUtils, LevelDB.Wrapper;

var
  DB: TLevelDB;
  Value: string;

begin
  try
    DB := TLevelDB.Create('./testdb');
    try
      // Simple Put and Get
      DB.Put('key1', 'value1');
      Value := DB.Get('key1');
      WriteLn('key1 = ', Value);

      // Update
      DB.Put('key1', 'new_value1');
      Value := DB.Get('key1');
      WriteLn('Updated key1 = ', Value);

      // Delete
      DB.Delete('key1');
      Value := DB.Get('key1');
      if Value = '' then
        WriteLn('key1 deleted successfully')
      else
        WriteLn('Unexpected: key1 still exists with value: ', Value);

      // Batch operations
      DB.BatchBegin;
      try
        DB.BatchPut('batch_key1', 'batch_value1');
        DB.BatchPut('batch_key2', 'batch_value2');
        DB.BatchDelete('batch_key1');
        DB.BatchCommit;
      except
        on E: Exception do
          WriteLn('Batch operation failed: ', E.Message);
      end;

      // Verify batch operations
      Value := DB.Get('batch_key1');
      if Value = '' then
        WriteLn('batch_key1 not found (as expected after batch delete)')
      else
        WriteLn('Unexpected: batch_key1 still exists with value: ', Value);

      Value := DB.Get('batch_key2');
      WriteLn('batch_key2 = ', Value);

    finally
      DB.Free;
    end;
  except
    on E: Exception do
      WriteLn('Error: ', E.Message);
  end;
end.

```

This example demonstrates how to use the `TLevelDB` wrapper class to perform CRUD operations and batch operations. The wrapper class provides a more intuitive and object-oriented interface for working with LevelDB in Free Pascal.

To use this wrapper:

1. Make sure you have the LevelDB library installed and properly linked to your Free Pascal project.
2. Include both the `LevelDB.Wrapper` unit and the previously created units (`LevelDB.C`, `LevelDB.Status`, and `LevelDB.Options`) in your project.
3. Compile and run the example program.

This wrapper class simplifies the use of LevelDB in Free Pascal while still providing access to all the core functionality. You can extend this class further if you need additional features or want to expose more LevelDB options.

Is there anything specific about this wrapper class that you'd like me to explain further or any additional functionality you'd like to see added?
