
type dbraw
type header = string
type headers = header array
type row = string option array
type row_not_null = string array
type sql = string
type conn =
         string option  (*   - an optional user *)
       * string option  (*   - an optional pass *)
       * string option  (*   - an optional host *)
       * string option  (*   - an optional port *)
       * string         (*   - an optional dbname *)

type db = {
  open_db : conn -> dbraw ;

  exec_iter : dbraw -> (row -> headers -> unit) -> sql -> unit ;
  exec_iter_no_headers : dbraw -> (row -> unit) -> sql -> unit ;

  exec_map : 'a . dbraw -> (row -> headers -> 'a) -> sql -> 'a list ;
  exec_map_no_headers : 'a . dbraw -> (row -> 'a) -> sql -> 'a list ;

  exec_no_headers : dbraw -> sql -> row list ;
  exec_no_result : dbraw -> sql -> unit ;
  exec : dbraw -> sql -> (row * headers) list ;
}

val dbobj : int ref

val database : db ref
