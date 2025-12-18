#!/bin/bash

cd app/

chown -R $(id -u):$(id -g) .

stack build --allow-different-user

stack exec exact-square-root-rest-api-server-exe