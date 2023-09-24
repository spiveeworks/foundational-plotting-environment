#!/bin/sh
gcc `pkg-config x11 xext --cflags --libs` main.c -o grapher
