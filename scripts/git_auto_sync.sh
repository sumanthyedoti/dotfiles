#!/bin/bash

git add .
git commit -m "auto-commit @ $(date '+%d-%m-%Y %H:%M:%S')"
git push
