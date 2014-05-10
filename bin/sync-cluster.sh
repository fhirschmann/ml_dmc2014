#!/bin/bash

TO=/home/dmc/models/$USER/
echo "Syncing to $TO"

rsync --update -avz -e ssh models/* dmc@0x0b.de:$TO
