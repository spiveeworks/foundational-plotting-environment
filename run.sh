#!/bin/sh
tcc `pkg-config x11 xext freetype2 --cflags --libs` -run main.c $@
