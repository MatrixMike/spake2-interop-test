#!/usr/bin/env python

from __future__ import print_function, unicode_literals

from sys import argv, stdout, stderr

from spake2 import params

if argv[1] == b"A":
    from spake2 import SPAKE2_A as SPAKE2_SIDE
elif argv[1] == b"B":
    from spake2 import SPAKE2_B as SPAKE2_SIDE
elif argv[1] == b"Symmetric":
    from spake2 import SPAKE2_Symmetric as SPAKE2_SIDE
else:
    raise ValueError("Specify side A or B")

password = argv[2]

PARAMS = {
    'I1024': params.Params1024,
    'I2048': params.Params2048,
    'I3072': params.Params3072,
    'Ed25519': params.ParamsEd25519,
}

param = params.ParamsEd25519
if len(argv) > 3:
    try:
        param = PARAMS[argv[3]]
    except ValueError:
        raise ValueError(
            'Choose a valid group to use (one of %r), got %s'
            % (list(PARAMS.keys()), param))


s = SPAKE2_SIDE(password, params=param)
msg_out = s.start()
print(msg_out.encode("hex"))
stdout.flush()
line = raw_input()
try:
    msg_in = line.decode("hex")
except TypeError as e:
    stderr.write("ERROR: Could not decode line (%s): %r\n" % (e, line))
    stderr.flush()
    raise e
key = s.finish(msg_in)
print(key.encode("hex"))
stdout.flush()
