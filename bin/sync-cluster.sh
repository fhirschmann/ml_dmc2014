#!/bin/bash

rsync --update -avz -e ssh models/* dmc@0x0b.de:/home/dmc/models/$USERNAME/
