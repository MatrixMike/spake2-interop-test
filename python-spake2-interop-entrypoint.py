#!/usr/bin/env python

from __future__ import print_function, unicode_literals

from sys import argv, stdout

if argv[1] == b"A":
    from spake2 import SPAKE2_A as SPAKE2_SIDE
elif argv[1] == b"B":
    from spake2 import SPAKE2_B as SPAKE2_SIDE
elif argv[1] == b"Symmetric":
    from spake2 import SPAKE2_Symmetric as SPAKE2_SIDE
else:
    raise ValueError("Specify side A or B")

password = argv[2]

s = SPAKE2_SIDE(password)
msg_out = s.start()
print(msg_out.encode("hex"))
stdout.flush()
msg_in = raw_input().decode("hex")
key = s.finish(msg_in)
print(key.encode("hex"))
stdout.flush()
