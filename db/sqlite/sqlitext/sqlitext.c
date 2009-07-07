
#include <Python.h>
#include "sqlitext.h"

int enable_extension(Connection *conn, int onoff) {
    return sqlite3_enable_load_extension(conn->db, onoff);
}

