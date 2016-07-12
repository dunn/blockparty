# blockparty

**WARNING:**  This is extremely a work in progress, and should not be trusted.

## compatibility

- SBCL 1.3.7
- Clozure CL 1.11 (64-bit)

32-bit Clozure-CL will have trouble with libyaml:
```
[package libyaml.lib]
> Error: Unable to load any of the alternatives:
>           ("libyaml.dylib" "libyaml-0.2.dylib")
```

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
