# blockparty

**WARNING:**  This is extremely a work in progress, and should not be trusted.

## developing and testing

Dependencies are managed by [Qi](https://github.com/CodyReichert/qi):

```bash
qi --install-deps blockparty.asd
```

Then run `make server` to start the application within the Hunchentoot
server.  Set `LISP_ENV=development` if you want REPL access to the
running program.

Run `make tests` to run the tests.

Set the `LISP` environment variable or append it to the `make`
commands to change the implementation used (defaults to
[SBCL](http://www.sbcl.org/)).

## compatibility

(**NB:** Qi only works with SBCL.)

- SBCL 1.3.x
- Clozure CL 1.11 (64-bit or whatever matches how you built libyaml)

It almost works in ABCL except for like
[one thing](https://github.com/edicl/hunchentoot/blob/24f638f8d01fc5f15d1169be1de944392b38d1a2/set-timeouts.lisp#L82).
It starts OK in ECL, then errors when you try to authenticate with
Twitter:

```
[2016-07-11 21:03:21 [ERROR]] An I/O error occurred: undocumented reason (return code: 5).
SSL error queue is empty.
backtrace output unavailable.
```
and/or:
```
[2016-07-11 21:08:32 [ERROR]] In function FOREIGN-DATA-POINTER, the value of the only argument is
  NIL
which is not of the expected type FOREIGN-DATA
backtrace output unavailable.
```

It dies during setup in CMUCL while compiling cl+ssl with:

```
Undefined foreign symbol: "_TLSv1_1_client_method"
   [Condition of type KERNEL:SIMPLE-PROGRAM-ERROR]
```
This might be because of the ancient OpenSSL in `/usr/bin/` on OS X.
