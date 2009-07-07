#include <sqlite3ext.h>
#include <apt-pkg/init.h>
#include <apt-pkg/debversion.h>

using namespace std;

extern "C" {
SQLITE_EXTENSION_INIT1

static int debianCollatingFunc(
  void *NotUsed,
  int nKey1, const void *pKey1,
  int nKey2, const void *pKey2
){
	const char *cKey1 = (char *) pKey1;
	const char *cKey2 = (char *) pKey2;
	pkgVersioningSystem *pvs = _system->VS;
  pvs->DoCmpVersion(cKey1,cKey1+nKey1,cKey2,cKey2+nKey2);
}

/* SQLite invokes this routine once when it loads the extension.
** Create new functions, collating sequences, and virtual table
** modules here.  This is usually the only exported symbol in
** the shared library.
*/
int sqlite3_extension_init(
  sqlite3 *db,
  char **pzErrMsg,
  const sqlite3_api_routines *pApi
){
  SQLITE_EXTENSION_INIT2(pApi)
  pkgInitConfig(*_config);
  pkgInitSystem(*_config,_system);
  sqlite3_create_collation(db, "DEBIAN", SQLITE_UTF8, 0, debianCollatingFunc);
  return 0;
}

}
