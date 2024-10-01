# Prompt

Still using the header wrapper you provided, can you please implement a wrapper class around it, still in mode `objfpc`?

# Answer

Sure! Below is an implementation of a wrapper class around the `LevelDB` header translation that was previously provided, using Free Pascal's `objfpc` mode. This class will encapsulate common `LevelDB` operations (CRUD and batch operations), providing a more idiomatic Pascal interface for working with the database.

### LevelDB Pascal Wrapper Class

```pascal
unit leveldbwrapper;

{$mode objfpc}{$H+}

interface

uses
  leveldb, ctypes, sysutils;

type
  { TLevelDB }
  TLevelDB = class
  private
    FDB: Pleveldb_t;
    FOptions: Pleveldb_options_t;
    FWriteOptions: Pleveldb_writeoptions_t;
    FReadOptions: Pleveldb_readoptions_t;
    FBatch: Pleveldb_writebatch_t;
    FError: PChar;

    procedure CheckError;
  public
    constructor Create(const DBPath: string);
    destructor Destroy; override;

    procedure Put(const Key, Value: string);
    function Get(const Key: string): string;
    procedure Delete(const Key: string);
    procedure BatchPut(const Key, Value: string);
    procedure BatchDelete(const Key: string);
    procedure CommitBatch;
  end;

implementation

{ TLevelDB }

constructor TLevelDB.Create(const DBPath: string);
begin
  inherited Create;
  FError := nil;

  // Initialize options
  FOptions := leveldb_options_create();
  leveldb_options_set_create_if_missing(FOptions, 1);

  // Open the database
  FDB := leveldb_open(FOptions, PChar(DBPath), @FError);
  CheckError;

  // Initialize write and read options
  FWriteOptions := leveldb_writeoptions_create();
  FReadOptions := leveldb_readoptions_create();
  FBatch := nil; // Batch will be created only when needed
end;

destructor TLevelDB.Destroy;
begin
  // Clean up resources
  if FBatch <> nil then
    leveldb_writebatch_destroy(FBatch);
  leveldb_readoptions_destroy(FReadOptions);
  leveldb_writeoptions_destroy(FWriteOptions);
  leveldb_options_destroy(FOptions);
  leveldb_close(FDB);

  if FError <> nil then
    leveldb_free(FError); // Free any remaining error strings

  inherited Destroy;
end;

procedure TLevelDB.CheckError;
begin
  if FError <> nil then
  begin
    raise Exception.CreateFmt('LevelDB Error: %s', [StrPas(FError)]);
    leveldb_free(FError); // Free the error string allocated by LevelDB
    FError := nil;
  end;
end;

procedure TLevelDB.Put(const Key, Value: string);
begin
  leveldb_put(FDB, FWriteOptions, PChar(Key), Length(Key), PChar(Value), Length(Value), @FError);
  CheckError;
end;

function TLevelDB.Get(const Key: string): string;
var
  Value: PChar;
  ValueLen: csize_t;
begin
  Value := leveldb_get(FDB, FReadOptions, PChar(Key), Length(Key), @ValueLen, @FError);
  CheckError;
  Result := StrPas(Value);
  leveldb_free(Value); // Free the value string returned by LevelDB
end;

procedure TLevelDB.Delete(const Key: string);
begin
  leveldb_delete(FDB, FWriteOptions, PChar(Key), Length(Key), @FError);
  CheckError;
end;

procedure TLevelDB.BatchPut(const Key, Value: string);
begin
  if FBatch = nil then
    FBatch := leveldb_writebatch_create();
  leveldb_writebatch_put(FBatch, PChar(Key), Length(Key), PChar(Value), Length(Value));
end;

procedure TLevelDB.BatchDelete(const Key: string);
begin
  if FBatch = nil then
    FBatch := leveldb_writebatch_create();
  leveldb_writebatch_delete(FBatch, PChar(Key), Length(Key));
end;

procedure TLevelDB.CommitBatch;
begin
  if FBatch <> nil then
  begin
    leveldb_write(FDB, FWriteOptions, FBatch, @FError);
    CheckError;
    leveldb_writebatch_clear(FBatch); // Clear the batch after committing
  end;
end;

end.
```

### Explanation:

- **Constructor (`Create`)**:
  - Initializes `LevelDB` with options to create the database if it does not already exist.
  - Opens the database and initializes write/read options.

- **Destructor (`Destroy`)**:
  - Cleans up all resources, including options, batch (if created), and any potential error strings.

- **`CheckError` method**:
  - Checks if an error has occurred. If an error is present, it raises an exception with the error message and frees the allocated memory for the error string.

- **CRUD Methods**:
  - **`Put`**: Inserts or updates a key-value pair.
  - **`Get`**: Retrieves the value associated with a key.
  - **`Delete`**: Deletes a key-value pair from the database.

- **Batch Operations**:
  - **`BatchPut`**: Adds a `PUT` operation to the batch.
  - **`BatchDelete`**: Adds a `DELETE` operation to the batch.
  - **`CommitBatch`**: Commits the batch to the database and clears the batch.

### Example of Using the `TLevelDB` Wrapper:

```pascal
program LevelDBWrapperExample;

{$mode objfpc}{$H+}

uses
  leveldbwrapper, sysutils;

var
  DB: TLevelDB;
  value: string;
begin
  try
    // Create/open the LevelDB database
    DB := TLevelDB.Create('testdb');

    // *** Simple CRUD operations ***
    // Put a key-value pair
    DB.Put('username', 'Gus');
    Writeln('Put: username -> Gus');

    // Get the value for a key
    value := DB.Get('username');
    Writeln('Get: username -> ', value);

    // Update the value for a key
    DB.Put('username', 'GusUpdated');
    value := DB.Get('username');
    Writeln('Updated: username -> ', value);

    // Delete the key
    DB.Delete('username');
    Writeln('Deleted: username');

    // *** Batch operations ***
    DB.BatchPut('key1', 'value1');
    DB.BatchPut('key2', 'value2');
    DB.BatchDelete('key1'); // Delete one key in the batch
    DB.CommitBatch; // Commit the batch operations
    Writeln('Batch operations committed.');

    // Check the result of batch operations
    value := DB.Get('key2');
    Writeln('Get after batch: key2 -> ', value);

    // Clean up
    DB.Free;
  except
    on E: Exception do
      Writeln('Error: ', E.Message);
  end;
end.
```

### Explanation of the Example:

1. **Opening the Database**: The `TLevelDB` class is instantiated, which opens or creates the database.
2. **CRUD Operations**:
   - The program performs `Put`, `Get`, `Update`, and `Delete` operations on the `username` key.
3. **Batch Operations**:
   - It adds two `Put` operations and one `Delete` operation to a batch.
   - The batch is committed, and the changes are reflected in the database.
4. **Error Handling**:
   - If any error occurs (e.g., during a database operation), an exception is raised and caught in the `try..except` block.

This Pascal wrapper encapsulates the `LevelDB` C API to make it easier to work with in Free Pascal. You can now perform CRUD and batch operations using a clean OOP-style interface.

Let me know if you need further improvements or adjustments!
