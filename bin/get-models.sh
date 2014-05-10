#!/bin/bash

rsync --update -avz --exclude old dmc@srv:~/models/* models.0x0b
