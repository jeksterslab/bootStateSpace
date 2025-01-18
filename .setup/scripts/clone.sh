#!/bin/bash

git clone git@github.com:jeksterslab/bootStateSpace.git
rm -rf "$PWD.git"
mv bootStateSpace/.git "$PWD"
rm -rf bootStateSpace
