#!/bin/bash
cat ./bin/.env | sed "s/HEAD_SERVER=1/HEAD_SERVER=$HEAD_SERVER/" > ./bin/.env.tmp
rm ./bin/.env
mv ./bin/.env.tmp ./bin/.env
