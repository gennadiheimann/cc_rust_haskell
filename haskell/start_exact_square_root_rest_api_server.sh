#!/bin/bash

cd app/exact-square-root-rest-api-server

stack setup

stack build

stack exec exact-square-root-rest-api-server-exe