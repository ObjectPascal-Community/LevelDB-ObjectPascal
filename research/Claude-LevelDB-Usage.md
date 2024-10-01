# Prompt

Using the header wrapper you just provided, can you please give me an example of using it with full `CRUD` including all operations that a batch supports?

# Answer

Certainly! I'll create an example that demonstrates CRUD (Create, Read, Update, Delete) operations using LevelDB with the Pascal header wrappers we've created. This example will also include batch operations.

Let's create a new unit that will use our LevelDB wrappers and demonstrate these operations:



```pascal
program LevelDBCRUDExample;

{$mode objfpc}{$H+}

uses
  SysUtils,
  LevelDB.C,
  LevelDB.Status,
  LevelDB.Options;

var
  db: leveldb_t;
  options: leveldb_options_t;
  roptions: leveldb_readoptions_t;
  woptions: leveldb_writeoptions_t;
  err: PChar;
  read_len: NativeUInt;
  read_value: PChar;
  batch: leveldb_writebatch_t;

procedure CheckError(error: PChar);
begin
  if error <> nil then
  begin
    WriteLn('Error: ', error);
    leveldb_free(error);
    Halt(1);
  end;
end;

begin
  // Open database
  options := leveldb_options_create();
  leveldb_options_set_create_if_missing(options, 1);

  db := leveldb_open(options, PChar('./testdb'), @err);
  CheckError(err);

  WriteLn('Database opened successfully.');

  // Create read and write options
  roptions := leveldb_readoptions_create();
  woptions := leveldb_writeoptions_create();

  // Create (Put) operation
  leveldb_put(db, woptions, PChar('key1'), 4, PChar('value1'), 6, @err);
  CheckError(err);
  WriteLn('Created key1 = value1');

  // Read (Get) operation
  read_value := leveldb_get(db, roptions, PChar('key1'), 4, @read_len, @err);
  CheckError(err);
  WriteLn('Read key1 = ', read_value);
  leveldb_free(read_value);

  // Update operation
  leveldb_put(db, woptions, PChar('key1'), 4, PChar('new_value1'), 10, @err);
  CheckError(err);
  WriteLn('Updated key1 = new_value1');

  // Read again to verify update
  read_value := leveldb_get(db, roptions, PChar('key1'), 4, @read_len, @err);
  CheckError(err);
  WriteLn('Read updated key1 = ', read_value);
  leveldb_free(read_value);

  // Delete operation
  leveldb_delete(db, woptions, PChar('key1'), 4, @err);
  CheckError(err);
  WriteLn('Deleted key1');

  // Try to read deleted key
  read_value := leveldb_get(db, roptions, PChar('key1'), 4, @read_len, @err);
  CheckError(err);
  if read_value = nil then
    WriteLn('key1 not found (as expected after deletion)')
  else
  begin
    WriteLn('Unexpected: key1 still exists with value: ', read_value);
    leveldb_free(read_value);
  end;

  // Batch operations
  WriteLn('Performing batch operations...');
  batch := leveldb_writebatch_create();

  leveldb_writebatch_put(batch, PChar('batch_key1'), 10, PChar('batch_value1'), 12);
  leveldb_writebatch_put(batch, PChar('batch_key2'), 10, PChar('batch_value2'), 12);
  leveldb_writebatch_delete(batch, PChar('batch_key1'), 10);

  leveldb_write(db, woptions, batch, @err);
  CheckError(err);

  // Verify batch operations
  read_value := leveldb_get(db, roptions, PChar('batch_key1'), 10, @read_len, @err);
  CheckError(err);
  if read_value = nil then
    WriteLn('batch_key1 not found (as expected after batch delete)')
  else
  begin
    WriteLn('Unexpected: batch_key1 still exists with value: ', read_value);
    leveldb_free(read_value);
  end;

  read_value := leveldb_get(db, roptions, PChar('batch_key2'), 10, @read_len, @err);
  CheckError(err);
  WriteLn('Read batch_key2 = ', read_value);
  leveldb_free(read_value);

  // Clean up
  leveldb_writebatch_destroy(batch);
  leveldb_readoptions_destroy(roptions);
  leveldb_writeoptions_destroy(woptions);
  leveldb_options_destroy(options);
  leveldb_close(db);

  WriteLn('LevelDB operations completed successfully.');
end.

```

This example demonstrates the following operations:

1. Opening a LevelDB database
2. Create (Put) operation
3. Read (Get) operation
4. Update operation
5. Delete operation
6. Batch operations (multiple puts and a delete in a single atomic operation)

Here's a breakdown of the CRUD operations and batch support:

1. Create: `leveldb_put` is used to create new key-value pairs.
2. Read: `leveldb_get` is used to retrieve values for given keys.
3. Update: `leveldb_put` is also used for updates (overwrites existing values).
4. Delete: `leveldb_delete` is used to remove key-value pairs.

Batch operations are demonstrated using `leveldb_writebatch_create`, `leveldb_writebatch_put`, `leveldb_writebatch_delete`, and `leveldb_write`. This allows multiple operations to be performed atomically.

To use this example:

1. Make sure you have the LevelDB library installed and properly linked to your Free Pascal project.
2. Compile and run the program.
3. It will create a new database in the `./testdb` directory and perform the operations.

This example provides a comprehensive demonstration of CRUD operations and batch support in LevelDB using the Free Pascal wrappers we created earlier. You can modify and expand upon this example to suit your specific needs.

Is there anything specific about these operations you'd like me to explain further or any additional functionality you'd like to see demonstrated?
