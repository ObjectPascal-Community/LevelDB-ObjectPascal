# Prompt

Using the header wrapper you just provided, can you please give me an example of using it with full `CRUD` including all operations that a batch supports?

# Answer

Sure! Below is a full example demonstrating how to use the `LevelDB` Pascal wrapper for `CRUD` operations (Create, Read, Update, Delete) and utilizing batch operations. The example assumes you have already linked the LevelDB library to your Pascal project.

### Example of CRUD operations with batch support:

```pascal
program LevelDBExample;

{$mode objfpc}{$H+}

uses
  leveldb, ctypes, sysutils;

var
  db: Pleveldb_t;
  options: Pleveldb_options_t;
  writeOptions: Pleveldb_writeoptions_t;
  readOptions: Pleveldb_readoptions_t;
  batch: Pleveldb_writebatch_t;
  err: PChar;
  key, value: PChar;
  keylen, valuelen: csize_t;
  val: PChar;

procedure CheckError;
begin
  if err <> nil then
  begin
    Writeln('Error: ', StrPas(err));
    leveldb_free(err); // Free the error string allocated by LevelDB
    Halt(1);
  end;
end;

begin
  err := nil;

  // Initialize options
  options := leveldb_options_create();
  leveldb_options_set_create_if_missing(options, 1);

  // Open or create the database
  db := leveldb_open(options, 'testdb', @err);
  CheckError;

  // Initialize write options
  writeOptions := leveldb_writeoptions_create();
  leveldb_writeoptions_set_sync(writeOptions, 1); // Use synchronous writes

  // Initialize read options
  readOptions := leveldb_readoptions_create();

  // *** CREATE (PUT) OPERATION ***
  key := 'name';
  value := 'Gus';
  keylen := Length(key);
  valuelen := Length(value);
  leveldb_put(db, writeOptions, key, keylen, value, valuelen, @err);
  CheckError;
  Writeln('PUT: ', key, ' -> ', value);

  // *** READ OPERATION ***
  key := 'name';
  keylen := Length(key);
  val := leveldb_get(db, readOptions, key, keylen, @valuelen, @err);
  CheckError;
  Writeln('GET: ', key, ' -> ', StrPas(val));
  leveldb_free(val); // Free the value returned by LevelDB

  // *** UPDATE OPERATION ***
  key := 'name';
  value := 'GusUpdated';
  keylen := Length(key);
  valuelen := Length(value);
  leveldb_put(db, writeOptions, key, keylen, value, valuelen, @err);
  CheckError;
  Writeln('UPDATE: ', key, ' -> ', value);

  // *** READ UPDATED VALUE ***
  val := leveldb_get(db, readOptions, key, keylen, @valuelen, @err);
  CheckError;
  Writeln('GET Updated: ', key, ' -> ', StrPas(val));
  leveldb_free(val);

  // *** DELETE OPERATION ***
  key := 'name';
  keylen := Length(key);
  leveldb_delete(db, writeOptions, key, keylen, @err);
  CheckError;
  Writeln('DELETE: ', key);

  // *** BATCH OPERATIONS ***
  Writeln('Performing batch operations...');
  batch := leveldb_writebatch_create();

  // Add key-value pairs in the batch
  key := 'key1';
  value := 'value1';
  keylen := Length(key);
  valuelen := Length(value);
  leveldb_writebatch_put(batch, key, keylen, value, valuelen);

  key := 'key2';
  value := 'value2';
  keylen := Length(key);
  valuelen := Length(value);
  leveldb_writebatch_put(batch, key, keylen, value, valuelen);

  // Delete a key in the batch
  key := 'key1';
  keylen := Length(key);
  leveldb_writebatch_delete(batch, key, keylen);

  // Commit the batch
  leveldb_write(db, writeOptions, batch, @err);
  CheckError;
  Writeln('Batch operations completed.');

  // Read the values after batch commit
  key := 'key2';
  keylen := Length(key);
  val := leveldb_get(db, readOptions, key, keylen, @valuelen, @err);
  CheckError;
  Writeln('GET after batch: ', key, ' -> ', StrPas(val));
  leveldb_free(val);

  // Clean up resources
  leveldb_writebatch_destroy(batch);
  leveldb_readoptions_destroy(readOptions);
  leveldb_writeoptions_destroy(writeOptions);
  leveldb_options_destroy(options);
  leveldb_close(db);

  Writeln('Database operations completed successfully.');
end.
```

### Explanation of the example:

1. **Creating and Opening the Database**:
   - We create the LevelDB options using `leveldb_options_create()` and set the `create_if_missing` option to create the database if it does not already exist.
   - The database is then opened using `leveldb_open()`.

2. **PUT Operation (Create)**:
   - We use `leveldb_put()` to insert a key-value pair into the database. In this case, the key is `'name'`, and the value is `'Gus'`.

3. **GET Operation (Read)**:
   - The value for the key `'name'` is read using `leveldb_get()`.

4. **Update Operation**:
   - We update the value for the key `'name'` to `'GusUpdated'`.

5. **DELETE Operation**:
   - We delete the key `'name'` using `leveldb_delete()`.

6. **Batch Operations**:
   - We create a write batch using `leveldb_writebatch_create()`.
   - We add two key-value pairs to the batch and delete one key.
   - We use `leveldb_write()` to commit the batch to the database.

7. **Resource Management**:
   - We destroy all the created options, write batches, and free memory returned by `leveldb_get()` and error strings.

### Compilation:
Ensure that you have the LevelDB C library properly linked in your Free Pascal environment.

Let me know if you need further assistance!
