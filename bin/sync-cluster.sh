#!/bin/bash

rsync --update -avz -e ssh models/*.RData dmc@0x0b.de:/home/dmc/models/$USERNAME/
