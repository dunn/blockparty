# blockparty

**WARNING:**  This is extremely a work in progress, and should not be trusted.

## compatibility

So far it’s only known to run on SBCL (1.3.7).  It almost works in
ABCL except for like
[one thing](https://github.com/edicl/hunchentoot/blob/24f638f8d01fc5f15d1169be1de944392b38d1a2/set-timeouts.lisp#L82).
It starts OK in ECL, then errors when you try to authenticate with
Twitter:
```
[2016-07-10 22:35:28 [ERROR]] In function FOREIGN-DATA-POINTER, the value of the only argument is
  NIL
which is not of the expected type FOREIGN-DATA
backtrace output unavailable.
```

It dies during setup in CMUCL while compiling cl+ssl with

```
Undefined foreign symbol: "_TLSv1_1_client_method"
   [Condition of type KERNEL:SIMPLE-PROGRAM-ERROR]
```
