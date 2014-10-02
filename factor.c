/* factor -- print prime factors of n.
   Copyright (C) 1986-2014 Free Software Foundation, Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

/* Originally written by Paul Rubin <phr@ocf.berkeley.edu>.
   Adapted for GNU, fixed to factor UINT_MAX by Jim Meyering.
   Arbitrary-precision code adapted by James Youngman from Torbjörn
   Granlund's factorize.c, from GNU MP version 4.2.2.
   In 2012, the core was rewritten by Torbjörn Granlund and Niels Möller.
   Contains code from GNU MP.  */

/* Efficiently factor numbers that fit in one or two words (word = uintmax_t),
   or, with GMP, numbers of any size.

  Code organisation:

    There are several variants of many functions, for handling one word, two
    words, and GMP's mpz_t type.  If the one-word variant is called foo, the
    two-word variant will be foo2, and the one for mpz_t will be mp_foo.  In
    some cases, the plain function variants will handle both one-word and
    two-word numbers, evidenced by function arguments.

    The factoring code for two words will fall into the code for one word when
    progress allows that.

    Using GMP is optional.  Define HAVE_GMP to make this code include GMP
    factoring code.  The GMP factoring code is based on GMP's demos/factorize.c
    (last synched 2012-09-07).  The GMP-based factoring code will stay in GMP
    factoring code even if numbers get small enough for using the two-word
    code.

  Algorithm:

    (1) Perform trial division using a small primes table, but without hardware
        division since the primes table store inverses modulo the word base.
        (The GMP variant of this code doesn't make use of the precomputed
        inverses, but instead relies on GMP for fast divisibility testing.)
    (2) Check the nature of any non-factored part using Miller-Rabin for
        detecting composites, and Lucas for detecting primes.
    (3) Factor any remaining composite part using the Pollard-Brent rho
        algorithm or the SQUFOF algorithm, checking status of found factors
        again using Miller-Rabin and Lucas.

    We prefer using Hensel norm in the divisions, not the more familiar
    Euclidian norm, since the former leads to much faster code.  In the
    Pollard-Brent rho code and the prime testing code, we use Montgomery's
    trick of multiplying all n-residues by the word base, allowing cheap Hensel
    reductions mod n.

  Improvements:

    * Use modular inverses also for exact division in the Lucas code, and
      elsewhere.  A problem is to locate the inverses not from an index, but
      from a prime.  We might instead compute the inverse on-the-fly.

    * Tune trial division table size (not forgetting that this is a standalone
      program where the table will be read from disk for each invocation).

    * Implement less naive powm, using k-ary exponentiation for k = 3 or
      perhaps k = 4.

    * Try to speed trial division code for single uintmax_t numbers, i.e., the
      code using DIVBLOCK.  It currently runs at 2 cycles per prime (Intel SBR,
      IBR), 3 cycles per prime (AMD Stars) and 5 cycles per prime (AMD BD) when
      using gcc 4.6 and 4.7.  Some software pipelining should help; 1, 2, and 4
      respectively cycles ought to be possible.

    * The redcify function could be vastly improved by using (plain Euclidian)
      pre-inversion (such as GMP's invert_limb) and udiv_qrnnd_preinv (from
      GMP's gmp-impl.h).  The redcify2 function could be vastly improved using
      similar methoods.  These functions currently dominate run time when using
      the -w option.
*/

//#include <config.h>
#include <getopt.h>
#include <stdio.h>
#include <sys/time.h>

#include <assert.h>

//#include "system.h"
#include <error.h>
//#include "quote.h"
//#include "xstrtol.h"

typedef unsigned long uintmax_t;
typedef long int intmax_t;
typedef int bool;

#define false 0
#define true 1
#define UINTMAX_MAX ((unsigned long)4294967295)
#define CHAR_BIT 8

# define W_TYPE_SIZE (8 * sizeof (uintmax_t))
# define __ll_B ((uintmax_t) 1 << (W_TYPE_SIZE / 2))
# define __ll_lowpart(t)  ((uintmax_t) (t) & (__ll_B - 1))
# define __ll_highpart(t) ((uintmax_t) (t) >> (W_TYPE_SIZE / 2))

#if !defined __clz_tab && !defined UHWtype
/* Without this seemingly useless conditional, gcc -Wunused-macros
   warns that each of the two tested macros is unused on Fedora 18.
   FIXME: this is just an ugly band-aid.  Fix it properly.  */
#endif

enum alg_type { ALG_POLLARD_RHO = 1, ALG_SQUFOF = 2 };

static enum alg_type alg;

/* 2*3*5*7*11...*101 is 128 bits, and has 26 prime factors */
#define MAX_NFACTS 26

struct factors
{
  uintmax_t     plarge[2]; /* Can have a single large factor */
  uintmax_t     p[MAX_NFACTS];
  unsigned char e[MAX_NFACTS];
  unsigned char nfactors;
};

static void factor (uintmax_t, struct factors *);

#ifndef umul_ppmm
# define umul_ppmm(w1, w0, u, v)                                        \
  do {                                                                  \
    uintmax_t __x0, __x1, __x2, __x3;                                   \
    unsigned long int __ul, __vl, __uh, __vh;                           \
    uintmax_t __u = (u), __v = (v);                                     \
                                                                        \
    __ul = __ll_lowpart (__u);                                          \
    __uh = __ll_highpart (__u);                                         \
    __vl = __ll_lowpart (__v);                                          \
    __vh = __ll_highpart (__v);                                         \
                                                                        \
    __x0 = (uintmax_t) __ul * __vl;                                     \
    __x1 = (uintmax_t) __ul * __vh;                                     \
    __x2 = (uintmax_t) __uh * __vl;                                     \
    __x3 = (uintmax_t) __uh * __vh;                                     \
                                                                        \
    __x1 += __ll_highpart (__x0);/* this can't give carry */            \
    __x1 += __x2;               /* but this indeed can */               \
    if (__x1 < __x2)            /* did we get it? */                    \
      __x3 += __ll_B;           /* yes, add it in the proper pos. */    \
                                                                        \
    (w1) = __x3 + __ll_highpart (__x1);                                 \
    (w0) = (__x1 << W_TYPE_SIZE / 2) + __ll_lowpart (__x0);             \
  } while (0)
#endif

#if !defined udiv_qrnnd || defined UDIV_NEEDS_NORMALIZATION
/* Define our own, not needing normalization. This function is
   currently not performance critical, so keep it simple. Similar to
   the mod macro below. */
# undef udiv_qrnnd
# define udiv_qrnnd(q, r, n1, n0, d)                                    \
  do {                                                                  \
    uintmax_t __d1, __d0, __q, __r1, __r0;                              \
                                                                        \
    assert ((n1) < (d));                                                \
    __d1 = (d); __d0 = 0;                                               \
    __r1 = (n1); __r0 = (n0);                                           \
    __q = 0;                                                            \
    for (unsigned int __i = W_TYPE_SIZE; __i > 0; __i--)                \
      {                                                                 \
        rsh2 (__d1, __d0, __d1, __d0, 1);                               \
        __q <<= 1;                                                      \
        if (ge2 (__r1, __r0, __d1, __d0))                               \
          {                                                             \
            __q++;                                                      \
            sub_ddmmss (__r1, __r0, __r1, __r0, __d1, __d0);            \
          }                                                             \
      }                                                                 \
    (r) = __r0;                                                         \
    (q) = __q;                                                          \
  } while (0)
#endif

#if !defined add_ssaaaa
# define add_ssaaaa(sh, sl, ah, al, bh, bl)                             \
  do {                                                                  \
    uintmax_t _add_x;                                                   \
    _add_x = (al) + (bl);                                               \
    (sh) = (ah) + (bh) + (_add_x < (al));                               \
    (sl) = _add_x;                                                      \
  } while (0)
#endif

#define rsh2(rh, rl, ah, al, cnt)                                       \
  do {                                                                  \
    (rl) = ((ah) << (W_TYPE_SIZE - (cnt))) | ((al) >> (cnt));           \
    (rh) = (ah) >> (cnt);                                               \
  } while (0)

#define lsh2(rh, rl, ah, al, cnt)                                       \
  do {                                                                  \
    (rh) = ((ah) << cnt) | ((al) >> (W_TYPE_SIZE - (cnt)));             \
    (rl) = (al) << (cnt);                                               \
  } while (0)

#define ge2(ah, al, bh, bl)                                             \
  ((ah) > (bh) || ((ah) == (bh) && (al) >= (bl)))

#define gt2(ah, al, bh, bl)                                             \
  ((ah) > (bh) || ((ah) == (bh) && (al) > (bl)))

#ifndef sub_ddmmss
# define sub_ddmmss(rh, rl, ah, al, bh, bl)                             \
  do {                                                                  \
    uintmax_t _cy;                                                      \
    _cy = (al) < (bl);                                                  \
    (rl) = (al) - (bl);                                                 \
    (rh) = (ah) - (bh) - _cy;                                           \
  } while (0)
#endif

#ifndef count_leading_zeros
# define count_leading_zeros(count, x) do {                             \
    uintmax_t __clz_x = (x);                                            \
    unsigned int __clz_c;                                               \
    for (__clz_c = 0;                                                   \
         (__clz_x & ((uintmax_t) 0xff << (W_TYPE_SIZE - 8))) == 0;      \
         __clz_c += 8)                                                  \
      __clz_x <<= 8;                                                    \
    for (; (intmax_t)__clz_x >= 0; __clz_c++)                           \
      __clz_x <<= 1;                                                    \
    (count) = __clz_c;                                                  \
  } while (0)
#endif

#ifndef count_trailing_zeros
# define count_trailing_zeros(count, x) do {                            \
    uintmax_t __ctz_x = (x);                                            \
    unsigned int __ctz_c = 0;                                           \
    while ((__ctz_x & 1) == 0)                                          \
      {                                                                 \
        __ctz_x >>= 1;                                                  \
        __ctz_c++;                                                      \
      }                                                                 \
    (count) = __ctz_c;                                                  \
  } while (0)
#endif

/* Requires that a < n and b <= n */
#define submod(r,a,b,n)                                                 \
  do {                                                                  \
    uintmax_t _t = - (uintmax_t) (a < b);                               \
    (r) = ((n) & _t) + (a) - (b);                                       \
  } while (0)

#define addmod(r,a,b,n)                                                 \
  submod ((r), (a), ((n) - (b)), (n))

/* Modular two-word addition and subtraction.  For performance reasons, the
   most significant bit of n1 must be clear.  The destination variables must be
   distinct from the mod operand.  */
#define addmod2(r1, r0, a1, a0, b1, b0, n1, n0)                         \
  do {                                                                  \
    add_ssaaaa ((r1), (r0), (a1), (a0), (b1), (b0));                    \
    if (ge2 ((r1), (r0), (n1), (n0)))                                   \
      sub_ddmmss ((r1), (r0), (r1), (r0), (n1), (n0));                  \
  } while (0)
#define submod2(r1, r0, a1, a0, b1, b0, n1, n0)                         \
  do {                                                                  \
    sub_ddmmss ((r1), (r0), (a1), (a0), (b1), (b0));                    \
    if ((intmax_t) (r1) < 0)                                            \
      add_ssaaaa ((r1), (r0), (r1), (r0), (n1), (n0));                  \
  } while (0)

#define HIGHBIT_TO_MASK(x)                                              \
  (((intmax_t)-1 >> 1) < 0                                              \
   ? (uintmax_t)((intmax_t)(x) >> (W_TYPE_SIZE - 1))                    \
   : ((x) & ((uintmax_t) 1 << (W_TYPE_SIZE - 1))                        \
      ? UINTMAX_MAX : (uintmax_t) 0))

/* Compute r = a mod d, where r = <*t1,retval>, a = <a1,a0>, d = <d1,d0>.
   Requires that d1 != 0.  */
static uintmax_t
mod2 (uintmax_t *r1, uintmax_t a1, uintmax_t a0, uintmax_t d1, uintmax_t d0)
{
  int cntd, cnta;

  assert (d1 != 0);

  if (a1 == 0)
    {
      *r1 = 0;
      return a0;
    }

  count_leading_zeros (cntd, d1);
  count_leading_zeros (cnta, a1);
  int cnt = cntd - cnta;
  lsh2 (d1, d0, d1, d0, cnt);
  for (int i = 0; i < cnt; i++)
    {
      if (ge2 (a1, a0, d1, d0))
        sub_ddmmss (a1, a0, a1, a0, d1, d0);
      rsh2 (d1, d0, d1, d0, 1);
    }

  *r1 = a1;
  return a0;
}

static uintmax_t
gcd_odd (uintmax_t a, uintmax_t b)
{
  if ( (b & 1) == 0)
    {
      uintmax_t t = b;
      b = a;
      a = t;
    }
  if (a == 0)
    return b;

  /* Take out least significant one bit, to make room for sign */
  b >>= 1;

  for (;;)
    {
      uintmax_t t;
      uintmax_t bgta;

      while ((a & 1) == 0)
        a >>= 1;
      a >>= 1;

      t = a - b;
      if (t == 0)
        return (a << 1) + 1;

      bgta = HIGHBIT_TO_MASK (t);

      /* b <-- min (a, b) */
      b += (bgta & t);

      /* a <-- |a - b| */
      a = (t ^ bgta) - bgta;
    }
}

static uintmax_t
gcd2_odd (uintmax_t *r1, uintmax_t a1, uintmax_t a0, uintmax_t b1, uintmax_t b0)
{
  while ((a0 & 1) == 0)
    rsh2 (a1, a0, a1, a0, 1);
  while ((b0 & 1) == 0)
    rsh2 (b1, b0, b1, b0, 1);

  for (;;)
    {
      if ((b1 | a1) == 0)
        {
          *r1 = 0;
          return gcd_odd (b0, a0);
        }

      if (gt2 (a1, a0, b1, b0))
        {
          sub_ddmmss (a1, a0, a1, a0, b1, b0);
          do
            rsh2 (a1, a0, a1, a0, 1);
          while ((a0 & 1) == 0);
        }
      else if (gt2 (b1, b0, a1, a0))
        {
          sub_ddmmss (b1, b0, b1, b0, a1, a0);
          do
            rsh2 (b1, b0, b1, b0, 1);
          while ((b0 & 1) == 0);
        }
      else
        break;
    }

  *r1 = a1;
  return a0;
}

static void
factor_insert_multiplicity (struct factors *factors,
                            uintmax_t prime, unsigned int m)
{
  unsigned int nfactors = factors->nfactors;
  uintmax_t *p = factors->p;
  unsigned char *e = factors->e;

  /* Locate position for insert new or increment e.  */
  int i;
  for (i = nfactors - 1; i >= 0; i--)
    {
      if (p[i] <= prime)
        break;
    }

  if (i < 0 || p[i] != prime)
    {
      for (int j = nfactors - 1; j > i; j--)
        {
          p[j + 1] = p[j];
          e[j + 1] = e[j];
        }
      p[i + 1] = prime;
      e[i + 1] = m;
      factors->nfactors = nfactors + 1;
    }
  else
    {
      e[i] += m;
    }
}

#define factor_insert(f, p) factor_insert_multiplicity (f, p, 1)

static void
factor_insert_large (struct factors *factors,
                     uintmax_t p1, uintmax_t p0)
{
  if (p1 > 0)
    {
      assert (factors->plarge[1] == 0);
      factors->plarge[0] = p0;
      factors->plarge[1] = p1;
    }
  else
    factor_insert (factors, p0);
}


/* Number of bits in an uintmax_t.  */
enum { W = sizeof (uintmax_t) * CHAR_BIT };

#define P(a,b,c,d) a,
static const unsigned char primes_diff[] = {
#include "primes.h"
0,0,0,0,0,0,0                           /* 7 sentinels for 8-way loop */
};
#undef P

#define PRIMES_PTAB_ENTRIES \
  (sizeof (primes_diff) / sizeof (primes_diff[0]) - 8 + 1)

#define P(a,b,c,d) b,
static const unsigned char primes_diff8[] = {
#include "primes.h"
0,0,0,0,0,0,0                           /* 7 sentinels for 8-way loop */
};
#undef P

struct primes_dtab
{
  uintmax_t binv, lim;
};

#define P(a,b,c,d) {c,d},
static const struct primes_dtab primes_dtab[] = {
#include "primes.h"
{1,0},{1,0},{1,0},{1,0},{1,0},{1,0},{1,0} /* 7 sentinels for 8-way loop */
};
#undef P

/* debugging for developers.  Enables devmsg().
   This flag is used only in the GMP code.  */
static bool dev_debug = false;

/* Prove primality or run probabilistic tests.  */
static bool flag_prove_primality = true;

/* Number of Miller-Rabin tests to run when not proving primality. */
#define MR_REPS 25

#ifdef __GNUC__
# define LIKELY(cond)    __builtin_expect ((cond), 1)
# define UNLIKELY(cond)  __builtin_expect ((cond), 0)
#else
# define LIKELY(cond)    (cond)
# define UNLIKELY(cond)  (cond)
#endif

static void
factor_insert_refind (struct factors *factors, uintmax_t p, unsigned int i,
                      unsigned int off)
{
  for (unsigned int j = 0; j < off; j++)
    p += primes_diff[i + j];
  factor_insert (factors, p);
}

/* Trial division with odd primes uses the following trick.

   Let p be an odd prime, and B = 2^{W_TYPE_SIZE}. For simplicity,
   consider the case t < B (this is the second loop below).

   From our tables we get

     binv = p^{-1} (mod B)
     lim = floor ( (B-1) / p ).

   First assume that t is a multiple of p, t = q * p. Then 0 <= q <= lim
   (and all quotients in this range occur for some t).

   Then t = q * p is true also (mod B), and p is invertible we get

     q = t * binv (mod B).

   Next, assume that t is *not* divisible by p. Since multiplication
   by binv (mod B) is a one-to-one mapping,

     t * binv (mod B) > lim,

   because all the smaller values are already taken.

   This can be summed up by saying that the function

     q(t) = binv * t (mod B)

   is a permutation of the range 0 <= t < B, with the curious property
   that it maps the multiples of p onto the range 0 <= q <= lim, in
   order, and the non-multiples of p onto the range lim < q < B.
 */

static uintmax_t
factor_using_division (uintmax_t *t1p, uintmax_t t1, uintmax_t t0,
                       struct factors *factors)
{
  if (t0 % 2 == 0)
    {
      unsigned int cnt;

      if (t0 == 0)
        {
          count_trailing_zeros (cnt, t1);
          t0 = t1 >> cnt;
          t1 = 0;
          cnt += W_TYPE_SIZE;
        }
      else
        {
          count_trailing_zeros (cnt, t0);
          rsh2 (t1, t0, t1, t0, cnt);
        }

      factor_insert_multiplicity (factors, 2, cnt);
    }

  uintmax_t p = 3;
  unsigned int i;
  for (i = 0; t1 > 0 && i < PRIMES_PTAB_ENTRIES; i++)
    {
      for (;;)
        {
          uintmax_t q1, q0, hi, lo;

          q0 = t0 * primes_dtab[i].binv;
          umul_ppmm (hi, lo, q0, p);
          if (hi > t1)
            break;
          hi = t1 - hi;
          q1 = hi * primes_dtab[i].binv;
          if (LIKELY (q1 > primes_dtab[i].lim))
            break;
          t1 = q1; t0 = q0;
          factor_insert (factors, p);
        }
      p += primes_diff[i + 1];
    }
  if (t1p)
    *t1p = t1;

#define DIVBLOCK(I)                                                     \
  do {                                                                  \
    for (;;)                                                            \
      {                                                                 \
        q = t0 * pd[I].binv;                                            \
        if (LIKELY (q > pd[I].lim))                                     \
          break;                                                        \
        t0 = q;                                                         \
        factor_insert_refind (factors, p, i + 1, I);                    \
      }                                                                 \
  } while (0)

  for (; i < PRIMES_PTAB_ENTRIES; i += 8)
    {
      uintmax_t q;
      const struct primes_dtab *pd = &primes_dtab[i];
      DIVBLOCK (0);
      DIVBLOCK (1);
      DIVBLOCK (2);
      DIVBLOCK (3);
      DIVBLOCK (4);
      DIVBLOCK (5);
      DIVBLOCK (6);
      DIVBLOCK (7);

      p += primes_diff8[i];
      if (p * p > t0)
        break;
    }

  return t0;
}


/* Entry i contains (2i+1)^(-1) mod 2^8.  */
static const unsigned char  binvert_table[128] =
{
  0x01, 0xAB, 0xCD, 0xB7, 0x39, 0xA3, 0xC5, 0xEF,
  0xF1, 0x1B, 0x3D, 0xA7, 0x29, 0x13, 0x35, 0xDF,
  0xE1, 0x8B, 0xAD, 0x97, 0x19, 0x83, 0xA5, 0xCF,
  0xD1, 0xFB, 0x1D, 0x87, 0x09, 0xF3, 0x15, 0xBF,
  0xC1, 0x6B, 0x8D, 0x77, 0xF9, 0x63, 0x85, 0xAF,
  0xB1, 0xDB, 0xFD, 0x67, 0xE9, 0xD3, 0xF5, 0x9F,
  0xA1, 0x4B, 0x6D, 0x57, 0xD9, 0x43, 0x65, 0x8F,
  0x91, 0xBB, 0xDD, 0x47, 0xC9, 0xB3, 0xD5, 0x7F,
  0x81, 0x2B, 0x4D, 0x37, 0xB9, 0x23, 0x45, 0x6F,
  0x71, 0x9B, 0xBD, 0x27, 0xA9, 0x93, 0xB5, 0x5F,
  0x61, 0x0B, 0x2D, 0x17, 0x99, 0x03, 0x25, 0x4F,
  0x51, 0x7B, 0x9D, 0x07, 0x89, 0x73, 0x95, 0x3F,
  0x41, 0xEB, 0x0D, 0xF7, 0x79, 0xE3, 0x05, 0x2F,
  0x31, 0x5B, 0x7D, 0xE7, 0x69, 0x53, 0x75, 0x1F,
  0x21, 0xCB, 0xED, 0xD7, 0x59, 0xC3, 0xE5, 0x0F,
  0x11, 0x3B, 0x5D, 0xC7, 0x49, 0x33, 0x55, 0xFF
};

/* Compute n^(-1) mod B, using a Newton iteration.  */
#define binv(inv,n)                                                     \
  do {                                                                  \
    uintmax_t  __n = (n);                                               \
    uintmax_t  __inv;                                                   \
                                                                        \
    __inv = binvert_table[(__n / 2) & 0x7F]; /*  8 */                   \
    if (W_TYPE_SIZE > 8)   __inv = 2 * __inv - __inv * __inv * __n;     \
    if (W_TYPE_SIZE > 16)  __inv = 2 * __inv - __inv * __inv * __n;     \
    if (W_TYPE_SIZE > 32)  __inv = 2 * __inv - __inv * __inv * __n;     \
                                                                        \
    if (W_TYPE_SIZE > 64)                                               \
      {                                                                 \
        int  __invbits = 64;                                            \
        do {                                                            \
          __inv = 2 * __inv - __inv * __inv * __n;                      \
          __invbits *= 2;                                               \
        } while (__invbits < W_TYPE_SIZE);                              \
      }                                                                 \
                                                                        \
    (inv) = __inv;                                                      \
  } while (0)

/* q = u / d, assuming d|u.  */
#define divexact_21(q1, q0, u1, u0, d)                                  \
  do {                                                                  \
    uintmax_t _di, _q0;                                                 \
    binv (_di, (d));                                                    \
    _q0 = (u0) * _di;                                                   \
    if ((u1) >= (d))                                                    \
      {                                                                 \
        uintmax_t _p1, _p0;                            \
        umul_ppmm (_p1, _p0, _q0, d);                                   \
        (q1) = ((u1) - _p1) * _di;                                      \
        (q0) = _q0;                                                     \
      }                                                                 \
    else                                                                \
      {                                                                 \
        (q0) = _q0;                                                     \
        (q1) = 0;                                                       \
      }                                                                 \
  } while (0)

/* x B (mod n). */
#define redcify(r_prim, r, n)                                           \
  do {                                                                  \
    uintmax_t _redcify_q;                              \
    udiv_qrnnd (_redcify_q, r_prim, r, 0, n);                           \
  } while (0)

/* x B^2 (mod n). Requires x > 0, n1 < B/2 */
#define redcify2(r1, r0, x, n1, n0)                                     \
  do {                                                                  \
    uintmax_t _r1, _r0, _i;                                             \
    if ((x) < (n1))                                                     \
      {                                                                 \
        _r1 = (x); _r0 = 0;                                             \
        _i = W_TYPE_SIZE;                                               \
      }                                                                 \
    else                                                                \
      {                                                                 \
        _r1 = 0; _r0 = (x);                                             \
        _i = 2*W_TYPE_SIZE;                                             \
      }                                                                 \
    while (_i-- > 0)                                                    \
      {                                                                 \
        lsh2 (_r1, _r0, _r1, _r0, 1);                                   \
        if (ge2 (_r1, _r0, (n1), (n0)))                                 \
          sub_ddmmss (_r1, _r0, _r1, _r0, (n1), (n0));                  \
      }                                                                 \
    (r1) = _r1;                                                         \
    (r0) = _r0;                                                         \
  } while (0)

/* Modular two-word multiplication, r = a * b mod m, with mi = m^(-1) mod B.
   Both a and b must be in redc form, the result will be in redc form too. */
static inline uintmax_t
mulredc (uintmax_t a, uintmax_t b, uintmax_t m, uintmax_t mi)
{
  uintmax_t rh, rl, q, th, tl, xh;

  umul_ppmm (rh, rl, a, b);
  q = rl * mi;
  umul_ppmm (th, tl, q, m);
  xh = rh - th;
  if (rh < th)
    xh += m;

  return xh;
}

/* Modular two-word multiplication, r = a * b mod m, with mi = m^(-1) mod B.
   Both a and b must be in redc form, the result will be in redc form too.
   For performance reasons, the most significant bit of m must be clear. */
static uintmax_t
mulredc2 (uintmax_t *r1p,
          uintmax_t a1, uintmax_t a0, uintmax_t b1, uintmax_t b0,
          uintmax_t m1, uintmax_t m0, uintmax_t mi)
{
  uintmax_t r1, r0, q, p1, p0, t1, t0, s1, s0;
  mi = -mi;
  assert ( (a1 >> (W_TYPE_SIZE - 1)) == 0);
  assert ( (b1 >> (W_TYPE_SIZE - 1)) == 0);
  assert ( (m1 >> (W_TYPE_SIZE - 1)) == 0);

  /* First compute a0 * <b1, b0> B^{-1}
        +-----+
        |a0 b0|
     +--+--+--+
     |a0 b1|
     +--+--+--+
        |q0 m0|
     +--+--+--+
     |q0 m1|
    -+--+--+--+
     |r1|r0| 0|
     +--+--+--+
  */
  umul_ppmm (t1, t0, a0, b0);
  umul_ppmm (r1, r0, a0, b1);
  q = mi * t0;
  umul_ppmm (p1, p0, q, m0);
  umul_ppmm (s1, s0, q, m1);
  r0 += (t0 != 0); /* Carry */
  add_ssaaaa (r1, r0, r1, r0, 0, p1);
  add_ssaaaa (r1, r0, r1, r0, 0, t1);
  add_ssaaaa (r1, r0, r1, r0, s1, s0);

  /* Next, (a1 * <b1, b0> + <r1, r0> B^{-1}
        +-----+
        |a1 b0|
        +--+--+
        |r1|r0|
     +--+--+--+
     |a1 b1|
     +--+--+--+
        |q1 m0|
     +--+--+--+
     |q1 m1|
    -+--+--+--+
     |r1|r0| 0|
     +--+--+--+
  */
  umul_ppmm (t1, t0, a1, b0);
  umul_ppmm (s1, s0, a1, b1);
  add_ssaaaa (t1, t0, t1, t0, 0, r0);
  q = mi * t0;
  add_ssaaaa (r1, r0, s1, s0, 0, r1);
  umul_ppmm (p1, p0, q, m0);
  umul_ppmm (s1, s0, q, m1);
  r0 += (t0 != 0); /* Carry */
  add_ssaaaa (r1, r0, r1, r0, 0, p1);
  add_ssaaaa (r1, r0, r1, r0, 0, t1);
  add_ssaaaa (r1, r0, r1, r0, s1, s0);

  if (ge2 (r1, r0, m1, m0))
    sub_ddmmss (r1, r0, r1, r0, m1, m0);

  *r1p = r1;
  return r0;
}

static uintmax_t
powm (uintmax_t b, uintmax_t e, uintmax_t n, uintmax_t ni, uintmax_t one)
{
  uintmax_t y = one;

  if (e & 1)
    y = b;

  while (e != 0)
    {
      b = mulredc (b, b, n, ni);
      e >>= 1;

      if (e & 1)
        y = mulredc (y, b, n, ni);
    }

  return y;
}

static uintmax_t
powm2 (uintmax_t *r1m,
       const uintmax_t *bp, const uintmax_t *ep, const uintmax_t *np,
       uintmax_t ni, const uintmax_t *one)
{
  uintmax_t r1, r0, b1, b0, n1, n0;
  unsigned int i;
  uintmax_t e;

  b0 = bp[0];
  b1 = bp[1];
  n0 = np[0];
  n1 = np[1];

  r0 = one[0];
  r1 = one[1];

  for (e = ep[0], i = W_TYPE_SIZE; i > 0; i--, e >>= 1)
    {
      if (e & 1)
        {
          r0 = mulredc2 (r1m, r1, r0, b1, b0, n1, n0, ni);
          r1 = *r1m;
        }
      b0 = mulredc2 (r1m, b1, b0, b1, b0, n1, n0, ni);
      b1 = *r1m;
    }
  for (e = ep[1]; e > 0; e >>= 1)
    {
      if (e & 1)
        {
          r0 = mulredc2 (r1m, r1, r0, b1, b0, n1, n0, ni);
          r1 = *r1m;
        }
      b0 = mulredc2 (r1m, b1, b0, b1, b0, n1, n0, ni);
      b1 = *r1m;
    }
  *r1m = r1;
  return r0;
}

static bool
millerrabin (uintmax_t n, uintmax_t ni, uintmax_t b, uintmax_t q,
             unsigned int k, uintmax_t one)
{
  uintmax_t y = powm (b, q, n, ni, one);

  uintmax_t nm1 = n - one;      /* -1, but in redc representation. */

  if (y == one || y == nm1)
    return true;

  for (unsigned int i = 1; i < k; i++)
    {
      y = mulredc (y, y, n, ni);

      if (y == nm1)
        return true;
      if (y == one)
        return false;
    }
  return false;
}

static bool
millerrabin2 (const uintmax_t *np, uintmax_t ni, const uintmax_t *bp,
              const uintmax_t *qp, unsigned int k, const uintmax_t *one)
{
  uintmax_t y1, y0, nm1_1, nm1_0, r1m;

  y0 = powm2 (&r1m, bp, qp, np, ni, one);
  y1 = r1m;

  if (y0 == one[0] && y1 == one[1])
    return true;

  sub_ddmmss (nm1_1, nm1_0, np[1], np[0], one[1], one[0]);

  if (y0 == nm1_0 && y1 == nm1_1)
    return true;

  for (unsigned int i = 1; i < k; i++)
    {
      y0 = mulredc2 (&r1m, y1, y0, y1, y0, np[1], np[0], ni);
      y1 = r1m;

      if (y0 == nm1_0 && y1 == nm1_1)
        return true;
      if (y0 == one[0] && y1 == one[1])
        return false;
    }
  return false;
}

/* Lucas' prime test.  The number of iterations vary greatly, up to a few dozen
   have been observed.  The average seem to be about 2.  */
static bool
prime_p (uintmax_t n)
{
  int k;
  bool is_prime;
  uintmax_t a_prim, one, ni;
  struct factors factors;

  if (n <= 1)
    return false;

  /* We have already casted out small primes. */
  if (n < (uintmax_t) FIRST_OMITTED_PRIME * FIRST_OMITTED_PRIME)
    return true;

  /* Precomputation for Miller-Rabin.  */
  uintmax_t q = n - 1;
  for (k = 0; (q & 1) == 0; k++)
    q >>= 1;

  uintmax_t a = 2;
  binv (ni, n);                 /* ni <- 1/n mod B */
  redcify (one, 1, n);
  addmod (a_prim, one, one, n); /* i.e., redcify a = 2 */

  /* Perform a Miller-Rabin test, finds most composites quickly.  */
  if (!millerrabin (n, ni, a_prim, q, k, one))
    return false;

  if (flag_prove_primality)
    {
      /* Factor n-1 for Lucas.  */
      factor (n - 1, &factors);
    }

  /* Loop until Lucas proves our number prime, or Miller-Rabin proves our
     number composite.  */
  for (unsigned int r = 0; r < PRIMES_PTAB_ENTRIES; r++)
    {
      if (flag_prove_primality)
        {
          is_prime = true;
          for (unsigned int i = 0; i < factors.nfactors && is_prime; i++)
            {
              is_prime
                = powm (a_prim, (n - 1) / factors.p[i], n, ni, one) != one;
            }
        }
      else
        {
          /* After enough Miller-Rabin runs, be content. */
          is_prime = (r == MR_REPS - 1);
        }

      if (is_prime)
        return true;

      a += primes_diff[r];      /* Establish new base.  */

      /* The following is equivalent to redcify (a_prim, a, n).  It runs faster
         on most processors, since it avoids udiv_qrnnd.  If we go down the
         udiv_qrnnd_preinv path, this code should be replaced.  */
      {
        uintmax_t s1, s0;
        umul_ppmm (s1, s0, one, a);
        if (LIKELY (s1 == 0))
          a_prim = s0 % n;
        else
          {
            uintmax_t dummy;
            udiv_qrnnd (dummy, a_prim, s1, s0, n);
          }
      }

      if (!millerrabin (n, ni, a_prim, q, k, one))
        return false;
    }

  error (0, 0, "Lucas prime test failure.  This should not happen");
  return false;
}

static bool
prime2_p (uintmax_t n1, uintmax_t n0)
{
  uintmax_t q[2], nm1[2];
  uintmax_t a_prim[2];
  uintmax_t one[2];
  uintmax_t na[2];
  uintmax_t ni;
  unsigned int k;
  struct factors factors;

  if (n1 == 0)
    return prime_p (n0);

  nm1[1] = n1 - (n0 == 0);
  nm1[0] = n0 - 1;
  if (nm1[0] == 0)
    {
      count_trailing_zeros (k, nm1[1]);

      q[0] = nm1[1] >> k;
      q[1] = 0;
      k += W_TYPE_SIZE;
    }
  else
    {
      count_trailing_zeros (k, nm1[0]);
      rsh2 (q[1], q[0], nm1[1], nm1[0], k);
    }

  uintmax_t a = 2;
  binv (ni, n0);
  redcify2 (one[1], one[0], 1, n1, n0);
  addmod2 (a_prim[1], a_prim[0], one[1], one[0], one[1], one[0], n1, n0);

  /* FIXME: Use scalars or pointers in arguments? Some consistency needed. */
  na[0] = n0;
  na[1] = n1;

  if (!millerrabin2 (na, ni, a_prim, q, k, one))
    return false;

  if (flag_prove_primality)
    {
      /* Factor n-1 for Lucas.  */
      factor (nm1[0], &factors);
    }

  /* Loop until Lucas proves our number prime, or Miller-Rabin proves our
     number composite.  */
  for (unsigned int r = 0; r < PRIMES_PTAB_ENTRIES; r++)
    {
      bool is_prime;
      uintmax_t e[2], y[2];

      if (flag_prove_primality)
        {
          is_prime = true;
          if (factors.plarge[1])
            {
              uintmax_t pi;
              binv (pi, factors.plarge[0]);
              e[0] = pi * nm1[0];
              e[1] = 0;
              y[0] = powm2 (&y[1], a_prim, e, na, ni, one);
              is_prime = (y[0] != one[0] || y[1] != one[1]);
            }
          for (unsigned int i = 0; i < factors.nfactors && is_prime; i++)
            {
              /* FIXME: We always have the factor 2. Do we really need to
                 handle it here? We have done the same powering as part
                 of millerrabin. */
              if (factors.p[i] == 2)
                rsh2 (e[1], e[0], nm1[1], nm1[0], 1);
              else
                divexact_21 (e[1], e[0], nm1[1], nm1[0], factors.p[i]);
              y[0] = powm2 (&y[1], a_prim, e, na, ni, one);
              is_prime = (y[0] != one[0] || y[1] != one[1]);
            }
        }
      else
        {
          /* After enough Miller-Rabin runs, be content. */
          is_prime = (r == MR_REPS - 1);
        }

      if (is_prime)
        return true;

      a += primes_diff[r];      /* Establish new base.  */
      redcify2 (a_prim[1], a_prim[0], a, n1, n0);

      if (!millerrabin2 (na, ni, a_prim, q, k, one))
        return false;
    }

  error (0, 0, "Lucas prime test failure.  This should not happen");
  return false;
}

static void
factor_using_pollard_rho (uintmax_t n, unsigned long int a,
                          struct factors *factors)
{
  uintmax_t x, z, y, P, t, ni, g;

  unsigned long int k = 1;
  unsigned long int l = 1;

  redcify (P, 1, n);
  addmod (x, P, P, n);          /* i.e., redcify(2) */
  y = z = x;

  while (n != 1)
    {
      assert (a < n);

      binv (ni, n);             /* FIXME: when could we use old 'ni' value? */

      for (;;)
        {
          do
            {
              x = mulredc (x, x, n, ni);
              addmod (x, x, a, n);

              submod (t, z, x, n);
              P = mulredc (P, t, n, ni);

              if (k % 32 == 1)
                {
                  if (gcd_odd (P, n) != 1)
                    goto factor_found;
                  y = x;
                }
            }
          while (--k != 0);

          z = x;
          k = l;
          l = 2 * l;
          for (unsigned long int i = 0; i < k; i++)
            {
              x = mulredc (x, x, n, ni);
              addmod (x, x, a, n);
            }
          y = x;
        }

    factor_found:
      do
        {
          y = mulredc (y, y, n, ni);
          addmod (y, y, a, n);

          submod (t, z, y, n);
          g = gcd_odd (t, n);
        }
      while (g == 1);

      n = n / g;

      if (!prime_p (g))
        factor_using_pollard_rho (g, a + 1, factors);
      else
        factor_insert (factors, g);

      if (prime_p (n))
        {
          factor_insert (factors, n);
          break;
        }

      x = x % n;
      z = z % n;
      y = y % n;
    }
}

/* FIXME: Maybe better to use an iteration converging to 1/sqrt(n)?  If
   algorithm is replaced, consider also returning the remainder. */
static uintmax_t
isqrt (uintmax_t n)
{
  uintmax_t x;
  unsigned c;
  if (n == 0)
    return 0;

  count_leading_zeros (c, n);

  /* Make x > sqrt(n). This will be invariant through the loop. */
  x = (uintmax_t) 1 << ((W_TYPE_SIZE + 1 - c) / 2);

  for (;;)
    {
      uintmax_t y = (x + n/x) / 2;
      if (y >= x)
        return x;

      x = y;
    }
}

static uintmax_t
isqrt2 (uintmax_t nh, uintmax_t nl)
{
  unsigned int shift;
  uintmax_t x;

  /* Ensures the remainder fits in an uintmax_t. */
  assert (nh < ((uintmax_t) 1 << (W_TYPE_SIZE - 2)));

  if (nh == 0)
    return isqrt (nl);

  count_leading_zeros (shift, nh);
  shift &= ~1;

  /* Make x > sqrt(n) */
  x = isqrt ( (nh << shift) + (nl >> (W_TYPE_SIZE - shift))) + 1;
  x <<= (W_TYPE_SIZE - shift) / 2;

  /* Do we need more than one iteration? */
  for (;;)
    {
      uintmax_t r;
      uintmax_t q, y;
      udiv_qrnnd (q, r, nh, nl, x);
      y = (x + q) / 2;

      if (y >= x)
        {
          uintmax_t hi, lo;
          umul_ppmm (hi, lo, x + 1, x + 1);
          assert (gt2 (hi, lo, nh, nl));

          umul_ppmm (hi, lo, x, x);
          assert (ge2 (nh, nl, hi, lo));
          sub_ddmmss (hi, lo, nh, nl, hi, lo);
          assert (hi == 0);

          return x;
        }

      x = y;
    }
}

/* invtab[i] = floor(0x10000 / (0x100 + i) */
static const unsigned short invtab[0x81] =
  {
    0x200,
    0x1fc, 0x1f8, 0x1f4, 0x1f0, 0x1ec, 0x1e9, 0x1e5, 0x1e1,
    0x1de, 0x1da, 0x1d7, 0x1d4, 0x1d0, 0x1cd, 0x1ca, 0x1c7,
    0x1c3, 0x1c0, 0x1bd, 0x1ba, 0x1b7, 0x1b4, 0x1b2, 0x1af,
    0x1ac, 0x1a9, 0x1a6, 0x1a4, 0x1a1, 0x19e, 0x19c, 0x199,
    0x197, 0x194, 0x192, 0x18f, 0x18d, 0x18a, 0x188, 0x186,
    0x183, 0x181, 0x17f, 0x17d, 0x17a, 0x178, 0x176, 0x174,
    0x172, 0x170, 0x16e, 0x16c, 0x16a, 0x168, 0x166, 0x164,
    0x162, 0x160, 0x15e, 0x15c, 0x15a, 0x158, 0x157, 0x155,
    0x153, 0x151, 0x150, 0x14e, 0x14c, 0x14a, 0x149, 0x147,
    0x146, 0x144, 0x142, 0x141, 0x13f, 0x13e, 0x13c, 0x13b,
    0x139, 0x138, 0x136, 0x135, 0x133, 0x132, 0x130, 0x12f,
    0x12e, 0x12c, 0x12b, 0x129, 0x128, 0x127, 0x125, 0x124,
    0x123, 0x121, 0x120, 0x11f, 0x11e, 0x11c, 0x11b, 0x11a,
    0x119, 0x118, 0x116, 0x115, 0x114, 0x113, 0x112, 0x111,
    0x10f, 0x10e, 0x10d, 0x10c, 0x10b, 0x10a, 0x109, 0x108,
    0x107, 0x106, 0x105, 0x104, 0x103, 0x102, 0x101, 0x100,
  };

/* Compute q = [u/d], r = u mod d.  Avoids slow hardware division for the case
   that q < 0x40; here it instead uses a table of (Euclidian) inverses.  */
#define div_smallq(q, r, u, d)                                          \
  do {                                                                  \
    if ((u) / 0x40 < (d))                                               \
      {                                                                 \
        int _cnt;                                                       \
        uintmax_t _dinv, _mask, _q, _r;                                 \
        count_leading_zeros (_cnt, (d));                                \
        _r = (u);                                                       \
        if (UNLIKELY (_cnt > (W_TYPE_SIZE - 8)))                        \
          {                                                             \
            _dinv = invtab[((d) << (_cnt + 8 - W_TYPE_SIZE)) - 0x80];   \
            _q = _dinv * _r >> (8 + W_TYPE_SIZE - _cnt);                \
          }                                                             \
        else                                                            \
          {                                                             \
            _dinv = invtab[((d) >> (W_TYPE_SIZE - 8 - _cnt)) - 0x7f];   \
            _q = _dinv * (_r >> (W_TYPE_SIZE - 3 - _cnt)) >> 11;        \
          }                                                             \
        _r -= _q*(d);                                                   \
                                                                        \
        _mask = -(uintmax_t) (_r >= (d));                               \
        (r) = _r - (_mask & (d));                                       \
        (q) = _q - _mask;                                               \
        assert ( (q) * (d) + (r) == u);                                 \
      }                                                                 \
    else                                                                \
      {                                                                 \
        uintmax_t _q = (u) / (d);                                       \
        (r) = (u) - _q * (d);                                           \
        (q) = _q;                                                       \
      }                                                                 \
  } while (0)

/* Notes: Example N = 22117019. After first phase we find Q1 = 6314, Q
   = 3025, P = 1737, representing F_{18} = (-6314, 2* 1737, 3025),
   with 3025 = 55^2.

   Constructing the square root, we get Q1 = 55, Q = 8653, P = 4652,
   representing G_0 = (-55, 2*4652, 8653).

   In the notation of the paper:

   S_{-1} = 55, S_0 = 8653, R_0 = 4652

   Put

     t_0 = floor([q_0 + R_0] / S0) = 1
     R_1 = t_0 * S_0 - R_0 = 4001
     S_1 = S_{-1} +t_0 (R_0 - R_1) = 706
*/

/* Multipliers, in order of efficiency:
   0.7268  3*5*7*11 = 1155 = 3 (mod 4)
   0.7317  3*5*7    =  105 = 1
   0.7820  3*5*11   =  165 = 1
   0.7872  3*5      =   15 = 3
   0.8101  3*7*11   =  231 = 3
   0.8155  3*7      =   21 = 1
   0.8284  5*7*11   =  385 = 1
   0.8339  5*7      =   35 = 3
   0.8716  3*11     =   33 = 1
   0.8774  3        =    3 = 3
   0.8913  5*11     =   55 = 3
   0.8972  5        =    5 = 1
   0.9233  7*11     =   77 = 1
   0.9295  7        =    7 = 3
   0.9934  11       =   11 = 3
*/
#define QUEUE_SIZE 50


/* Compute the prime factors of the 128-bit number (T1,T0), and put the
   results in FACTORS.  Use the algorithm selected by the global ALG.  */
static void
factor (uintmax_t t0, struct factors *factors)
{
  uintmax_t t1 = 0;
  factors->nfactors = 0;
  factors->plarge[1] = 0;

  if (t0 < 2)
    return;

  t0 = factor_using_division (&t1, t1, t0, factors);

  if (t1 == 0 && t0 < 2)
    return;

  if (prime2_p (t1, t0))
    factor_insert_large (factors, t1, t0);
  else
    {
      if (t1 == 0)
        factor_using_pollard_rho (t0, 1, factors);
      else
      error(0, 0, "I got rid of factor_using_pollard_rho2!!!");
    }
}

void
factor_time(uintmax_t t) {
  struct factors factors;

  printf(" t=[%lu]", t);

  factor((unsigned long)t, &factors);

  for (unsigned int j = 0; j < factors.nfactors; j++)
    for (unsigned int k = 0; k < factors.e[j]; k++)
    {
      printf(" %lu", factors.p[j]);
    }
  putchar ('\n');
}

/* set tabstop=2 shiftwidth=2 expandtab */
