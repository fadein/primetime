# It's Prime Time!

A colorful prime number clock for your terminal

by Erik Falor @fadein


## Written in CHICKEN Scheme v5
http://call-cc.org


## What is a prime number clock?

This is a clock which lets you experience time in a whole new way.  Disconnect
from the regular rigidity of minutes and hours and feel the rhythm of pure
number theory.

Each second since the Unix epoch is a 32-bit integer which, on modern computers
using clever algorithms[1], can be factorized into its prime components in mere
milliseconds.  If it turns out that the current second is a prime number, then
this moment, right now, is prime time!

If the second second from right now is also a prime number, that will be twin
prime time (a real cause for celebration).  Prime cousins, sexy primes and
other rare combos are highlighted for your enjoyment.  But enjoy these quickly,
as now won't stay prime for long!  How long will it take before you can
experience a triple prime?

Some times are just teeming with primes.  At other times primes are few and far
between.  With *Primetime* you can always know which time is the right time.


## See for yourself
`telnet unnovative.net`


## ...or just build it!
*Primetime* requires that the following eggs are first installed with Chicken
Scheme's extension manager `chicken-install(1)`:

* ansi-escape-sequences
* srfi-1
* srfi-13
* srfi-18

The following commands will install the requisite eggs and build a
statically-linked `primetime` executable:

```
$ chicken-install -s ansi-escape-sequences srfi-1 srfi-13 srfi-18
$ make
```


----------------------------------------------------------------------

[1] The very clever prime factorization algorithm used by *Primetime* was
adapted from a prior release of GNU Coreutils' factor(1) tool.
https://www.gnu.org/software/coreutils/manual/html_node/factor-invocation.html
