import sqlite3

db = sqlite3.connect(":memory:")
db.execute("create table foo (bar text)")

db.execute("vacuum main into '/tmp/saved.db'")

