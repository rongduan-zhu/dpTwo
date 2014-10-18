#!/usr/bin/env python
f = open("words6", "r")
lines = f.readlines()
# lines = map(lambda x: x.strip('\n'), lines)
# lines = map(lambda x: x[::-1] + "\n", lines)
lines = lines[::-1]
print lines
f.close()
f = open("words8", "w")
map(lambda x: f.write(x), lines)
f.close()
