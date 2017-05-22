# spake2-interop-test
A harness for interoperability testing between SPAKE2 implementations.

## python-spake2 example

```console
$ runhaskell TestInterop.hs ./python-spake2-interop-entrypoint.py A abc -- ./python-spake2-interop-entrypoint.py B abc["./python-spake2-interop-entrypoint.py","A","abc"]
["./python-spake2-interop-entrypoint.py","B","abc"]
A's key: 8a2e19664f0a2bc6e446d2c44900c67604fe42f6d7e0a1328a5253b21f4131a5
B's key: 8a2e19664f0a2bc6e446d2c44900c67604fe42f6d7e0a1328a5253b21f4131a5
Session keys match.
```
