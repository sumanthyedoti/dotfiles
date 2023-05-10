#!/bin/bash

git pull
git add .
git commit -m "auto-commit @ $(date '+%d-%m-%Y %H:%M')"
git push
