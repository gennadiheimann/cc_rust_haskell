#!/bin/bash

cd app/

stack build

stack exec exact-square-root-rest-api-server-exe