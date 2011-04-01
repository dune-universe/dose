/* connection.h - definitions for the connection type
 *
 * Copyright (C) 2004-2006 Gerhard Häring <gh@ghaering.de>
 *
 * This file is part of pysqlite.
 *
 * This software is provided 'as-is', without any express or implied
 * warranty.  In no event will the authors be held liable for any damages
 * arising from the use of this software.
 *
 * Permission is granted to anyone to use this software for any purpose,
 * including commercial applications, and to alter it and redistribute it
 * freely, subject to the following restrictions:
 *
 * 1. The origin of this software must not be misrepresented; you must not
 *    claim that you wrote the original software. If you use this software
 *    in a product, an acknowledgment in the product documentation would be
 *    appreciated but is not required.
 * 2. Altered source versions must be plainly marked as such, and must not be
 *    misrepresented as being the original software.
 * 3. This notice may not be removed or altered from any source distribution.
 */

#include "Python.h"
#include "pythread.h"
#include "structmember.h"
#include "sqlite3.h"

typedef struct
{
    PyObject_HEAD
    sqlite3* db;
    int inTransaction;
    int detect_types;
    double timeout;
    double timeout_started;
    PyObject* isolation_level;
    char* begin_statement;
    int check_same_thread;
    long thread_ident;
    PyObject* statements;
    int created_statements;
    PyObject* row_factory;
    PyObject* text_factory;
    PyObject* function_pinboard;
    PyObject* collations;
    PyObject* Warning;
    PyObject* Error;
    PyObject* InterfaceError;
    PyObject* DatabaseError;
    PyObject* DataError;
    PyObject* OperationalError;
    PyObject* IntegrityError;
    PyObject* InternalError;
    PyObject* ProgrammingError;
    PyObject* NotSupportedError;
} Connection;
