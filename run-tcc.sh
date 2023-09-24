#!/bin/sh
tcc `pkg-config x11 xext --cflags --libs` -run main.c $@
