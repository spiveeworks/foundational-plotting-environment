#!/bin/sh
gcc `pkg-config x11 xext --cflags --libs` main.c -O2 -o fpe
