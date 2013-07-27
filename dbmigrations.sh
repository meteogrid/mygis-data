#!/bin/bash

MOO=`which moo`

export DBM_DATABASE_TYPE=postgresql
export DBM_DATABASE=dbname=sigym4-dev
export DBM_MIGRATION_STORE=dbmigrations

exec $MOO $@
