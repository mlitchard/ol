[![BuildStatus](https://travis-ci.org/mlitchard/ol.svg?branch=master)](https://travis-ci.org/mlitchard/ol)
ol project
==========

#To Use
git clone git@github.com:mlitchard/ol.git
cd ol
git checkout master
cd dockerfiles/postgres

Note: You may have to use sudo to execute docker commands

docker build -t postgresql_ol_db .

docker create -v /etc/postgresql/9.3/main --name etc-postgres postgresql_ol_db

docker run --rm -p 3000:3000 -P --volumes-from etc-postgres --name local_db postgresql_ol_db

In another terminal:

cd ol/dockerfiles/yesod

docker build -t yesod_ol .

sudo docker run -ti --net=container:local_db --volumes-from etc-postgres --name ol_app yesod_ol

In yet another term, you can go ahead and play, for example:

curl -G 'http://localhost:3000/businesses/1'
{"next":2,"first":0,"self":1,"last":49999,"id":1,"prev":0}

You can see Travis CI is passing all tests. If you want to run them yourself, do this:

sudo docker run -t --net=container:local_db --volumes-from etc-postgres --name ol_app yesod_ol bash

at the prompt type
'stack test'



