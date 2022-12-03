from flask import Flask, request
import json
import psycopg2
import psycopg2.extras
import os
# Estructura del uri:
# "motor://user:password@host:port/database"
database_uri = f'postgresql://{os.environ["PGUSR"]}:{os.environ["PGPASS"]}@{os.environ["PGHOST"]}:5432/{os.environ["PGDB"]}'

app = Flask(__name__)
conn = psycopg2.connect(database_uri)

#@app.route('/')
#def home():
 #   cur = conn.cursor(cursor_factory=psycopg2.extras.NamedTupleCursor)
  #  cur.execute("select * from users")
   # results = cur.fetchall()
    #cur.close()
    #return json.dumps([x._asdict() for x in results], default=str)


@app.route('/MNIST', methods=['GET'])
def MNIST():
    cur = conn.cursor(cursor_factory=psycopg2.extras.NamedTupleCursor)
    user_id = request.args.get("id")
    cur.execute(f"select * from MNIST")
    results = cur.fetchall()
    cur.close()
    return json.dumps([x._asdict() for x in results], default=str)


if __name__ == "__main__":
    app.run(host="0.0.0.0", debug=True, port=8080)
