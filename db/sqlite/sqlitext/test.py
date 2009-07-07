
import os.path
import storm.locals as stm
import storm.exceptions as stme

import storm.database

import sqlitext

_db_name = 'packages.db'

class Store(object):
    def __init__(self, dbdir):
        self.dbdir = os.path.abspath(dbdir)
        if not os.path.isdir(self.dbdir):
            os.makedirs(self.dbdir)
        self.db_path = os.path.join(self.dbdir, _db_name)
        self.has_db = os.path.exists(self.db_path)
        self.db = self.__open_db(self.db_path)
        sqlitext.enable_extension(self.db._connection._raw_connection,1)

    def __open_db(self,pathname):
	#db_url = 'sqlite:/:memory:'
	db_url = 'sqlite:' + pathname
	db_url += '?debug=1'
	storm.database.DEBUG = True
	database = stm.create_database(db_url)
	store = stm.Store(database)
	return store

    def initialise(self):
	self.db.execute('''SELECT load_extension('./libcollate_debian.so')''')
	self.db.commit()

    def close(self):
	self.db.commit()
	self.db.close()


s = Store('./')
s.initialise()
s.db.execute('''
    CREATE TABLE version (
	id INTEGER PRIMARY KEY,
	number VARCHAR (255) COLLATE DEBIAN,
	package_id INT,
	info_id INT,
	replaces VARCHAR,
	provides VARCHAR,
	pre_depends VARCHAR,
	depends VARCHAR,
	suggests VARCHAR,
	enhances VARCHAR,
	recommends VARCHAR,
	conflicts VARCHAR,
	creationdate DATE,
	removaldate DATE)''')
s.close()
