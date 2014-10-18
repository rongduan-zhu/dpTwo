#!/usr/bin/env python
f = open("puzzle6", "r")
lines = f.readlines()
lines = map(lambda x: x.strip('\n'), lines)
lines = map(lambda x: x[::-1] + "\n", lines)
f.close()
f = open("puzzle8", "w")
map(lambda x: f.write(x), lines)
f.close()
