#!/bin/bash

rsync --update -avz --exclude old dmc@srv:~/models/* models/
