#ifndef __MATH_H__
#define __MATH_H__

#ifdef __cplusplus
extern "C" {
#endif

#include <sgidefs.h>
#include <standards.h>

/* Copyright (C) 1989 Silicon Graphics, Inc. All rights reserved.  */
/* ------------------------------------------------------------------ */
/* | Copyright Unpublished, MIPS Computer Systems, Inc.  All Rights | */
/* | Reserved.  This software contains proprietary and confidential | */
/* | information of MIPS and its suppliers.  Use, disclosure or     | */
/* | reproduction is prohibited without the prior express written   | */
/* | consent of MIPS.                                               | */
/* ------------------------------------------------------------------ */

/* #ident "$Revision: 1.3 $" */

/* ANSI C Notes:
 *
 * - THE IDENTIFIERS APPEARING OUTSIDE OF #ifdef __EXTENSIONS__ IN THIS
 *   standard header ARE SPECIFIED BY ANSI!  CONFORMANCE WILL BE ALTERED
 *   IF ANY NEW IDENTIFIERS ARE ADDED TO THIS AREA UNLESS THEY ARE IN ANSI's
 *   RESERVED NAMESPACE. (i.e., unless they are prefixed by __[a-z] or
 *   _[A-Z].  For external objects, identifiers with the prefix _[a-z] 
 *   are also reserved.)
 *
 *  - Names created by appending either the suffix 'l' or 'f'
 *    to one of the reserved function names are reserved for the corresponding
 *    single- or extended- precision version of the function.
 */

/* Power C Notes:
 *
 *  - Power C assumes that all external functions (unless otherwise marked)
 *    might modify global data and therefore calls to such functions can
 *    not be safely concurrentized.
 *
 *  - The pragma "no side effects" indicates that the named function is
 *    free from such side effects and calls to it can be concurrentized.
 *
 *  - This pragma has been added below for each function in the math library
 *    which is "safe".
 *
 *  - The pragma is properly passed on by 'cpp' & 'c++' and ignored by
 *    'ccom'.  It only has meaning to 'pca'.
 */
/*
 * XPG4/POSIX Notes:
 *	This header is also specified by POSIX/XOPEN..
 *	Although the standards say nothing - we assume that the ANSI
 *	'guideline' that all std ANSI names with an 'l' or 'f' are reserved.
 */

/*
 * ANSI definitions
 */
#ifndef HUGE_VAL	/* Also in limits.h */
#if _SGIAPI
#ifndef __TYPEDEF_H_VAL
#define __TYPEDEF_H_VAL
typedef union _h_val {
#if (_MIPS_SZLONG == 32)
        unsigned long i[2];
#endif
#if (_MIPS_SZLONG == 64)
        __uint32_t i[2];
#endif
        double d;
} _h_val;
#endif /* __TYPEDEF_H_VAL */

extern const _h_val __huge_val;
#define HUGE_VAL __huge_val.d
#else /* _SGIAPI */

/* __infinity is a double-precision variable in libc set to infinity */

extern const double __infinity;
#define HUGE_VAL __infinity	
#endif /* _SGIAPI */
#endif /* !HUGE_VAL */

/* 
 *  ANSI-standard functions.  Each is listed with 
 *  its single-precision counterpart, if it exists. 
 */
extern double	acos(double);
#ifdef __MATH_HAS_NO_SIDE_EFFECTS
#pragma no side effects (acos)
#endif /* __MATH_HAS_NO_SIDE_EFFECTS */

extern float	acosf(float);
#ifdef __MATH_HAS_NO_SIDE_EFFECTS
#pragma no side effects (acosf)
#endif /* __MATH_HAS_NO_SIDE_EFFECTS */

extern double	asin(double);
#ifdef __MATH_HAS_NO_SIDE_EFFECTS
#pragma no side effects (asin)
#endif /* __MATH_HAS_NO_SIDE_EFFECTS */

extern float	asinf(float);
#ifdef __MATH_HAS_NO_SIDE_EFFECTS
#pragma no side effects (asinf)
#endif /* __MATH_HAS_NO_SIDE_EFFECTS */

extern double	atan(double);
#ifdef __MATH_HAS_NO_SIDE_EFFECTS
#pragma no side effects (atan)
#endif /* __MATH_HAS_NO_SIDE_EFFECTS */

extern float	atanf(float);
#ifdef __MATH_HAS_NO_SIDE_EFFECTS
#pragma no side effects (atanf)
#endif /* __MATH_HAS_NO_SIDE_EFFECTS */

#if _NO_XOPEN4 && (defined(__INLINE_INTRINSICS) && \
        defined(_COMPILER_VERSION) && (_COMPILER_VERSION >= 710))
#pragma intrinsic (acos)
#pragma intrinsic (acosf)
#pragma intrinsic (asin)
#pragma intrinsic (asinf)
#pragma intrinsic (atan)
#pragma intrinsic (atanf)
#endif

extern double	atan2(double, double);
#ifdef __MATH_HAS_NO_SIDE_EFFECTS
#pragma no side effects (atan2)
#endif /* __MATH_HAS_NO_SIDE_EFFECTS */

extern float	atan2f(float, float);
#ifdef __MATH_HAS_NO_SIDE_EFFECTS
#pragma no side effects (atan2f)
#endif /* __MATH_HAS_NO_SIDE_EFFECTS */

extern double	cos(double);
#ifdef __MATH_HAS_NO_SIDE_EFFECTS
#pragma no side effects (cos)
#endif /* __MATH_HAS_NO_SIDE_EFFECTS */

extern float	cosf(float);
#ifdef __MATH_HAS_NO_SIDE_EFFECTS
#pragma no side effects (cosf)
#endif /* __MATH_HAS_NO_SIDE_EFFECTS */

extern double	sin(double);
#ifdef __MATH_HAS_NO_SIDE_EFFECTS
#pragma no side effects (sin)
#endif /* __MATH_HAS_NO_SIDE_EFFECTS */

extern float	sinf(float);
#ifdef __MATH_HAS_NO_SIDE_EFFECTS
#pragma no side effects (sinf)
#endif /* __MATH_HAS_NO_SIDE_EFFECTS */

extern double	tan(double);
#ifdef __MATH_HAS_NO_SIDE_EFFECTS
#pragma no side effects (tan)
#endif /* __MATH_HAS_NO_SIDE_EFFECTS */

extern float	tanf(float);
#ifdef __MATH_HAS_NO_SIDE_EFFECTS
#pragma no side effects (tanf)
#endif /* __MATH_HAS_NO_SIDE_EFFECTS */

#if _NO_XOPEN4 && (defined(__INLINE_INTRINSICS) && \
        defined(_COMPILER_VERSION) && (_COMPILER_VERSION >= 710))
/* #pragma intrinsic (cos) */
#pragma intrinsic (cosf)
/* #pragma intrinsic (sin) */
#pragma intrinsic (sinf)
#pragma intrinsic (tan)
#pragma intrinsic (tanf)
#endif

extern double	cosh(double);
#ifdef __MATH_HAS_NO_SIDE_EFFECTS
#pragma no side effects (cosh)
#endif /* __MATH_HAS_NO_SIDE_EFFECTS */

extern float	coshf(float);
#ifdef __MATH_HAS_NO_SIDE_EFFECTS
#pragma no side effects (coshf)
#endif /* __MATH_HAS_NO_SIDE_EFFECTS */

extern double	sinh(double);
#ifdef __MATH_HAS_NO_SIDE_EFFECTS
#pragma no side effects (sinh)
#endif /* __MATH_HAS_NO_SIDE_EFFECTS */

extern float	sinhf(float);
#ifdef __MATH_HAS_NO_SIDE_EFFECTS
#pragma no side effects (sinhf)
#endif /* __MATH_HAS_NO_SIDE_EFFECTS */

extern double	tanh(double);
#ifdef __MATH_HAS_NO_SIDE_EFFECTS
#pragma no side effects (tanh)
#endif /* __MATH_HAS_NO_SIDE_EFFECTS */

extern float	tanhf(float);
#ifdef __MATH_HAS_NO_SIDE_EFFECTS
#pragma no side effects (tanhf)
#endif /* __MATH_HAS_NO_SIDE_EFFECTS */

extern double	exp(double);
#ifdef __MATH_HAS_NO_SIDE_EFFECTS
#pragma no side effects (exp)
#endif /* __MATH_HAS_NO_SIDE_EFFECTS */

extern float	expf(float);
#ifdef __MATH_HAS_NO_SIDE_EFFECTS
#pragma no side effects (expf)
#endif /* __MATH_HAS_NO_SIDE_EFFECTS */

#if _NO_XOPEN4 && (defined(__INLINE_INTRINSICS) && \
        defined(_COMPILER_VERSION) && (_COMPILER_VERSION >= 710))
#pragma intrinsic (exp)
#pragma intrinsic (expf)
#endif

extern double	frexp(double, int *);

#if 0
/* not yet implemented */
extern float	frexpf(float, int *);
#endif

extern double	ldexp(double, int);

#if 0
/* not yet implemented */
extern float	ldexpf(float, int);
#endif

extern double	log(double);
#ifdef __MATH_HAS_NO_SIDE_EFFECTS
#pragma no side effects (log)
#endif /* __MATH_HAS_NO_SIDE_EFFECTS */

extern float	logf(float);
#ifdef __MATH_HAS_NO_SIDE_EFFECTS
#pragma no side effects (logf)
#endif /* __MATH_HAS_NO_SIDE_EFFECTS */

extern double	log10(double);
#ifdef __MATH_HAS_NO_SIDE_EFFECTS
#pragma no side effects (log10)
#endif /* __MATH_HAS_NO_SIDE_EFFECTS */

extern float	log10f(float);
#ifdef __MATH_HAS_NO_SIDE_EFFECTS
#pragma no side effects (log10f)
#endif /* __MATH_HAS_NO_SIDE_EFFECTS */

#if _NO_XOPEN4 && (defined(__INLINE_INTRINSICS) && \
        defined(_COMPILER_VERSION) && (_COMPILER_VERSION >= 710))
#pragma intrinsic (log)
#pragma intrinsic (logf)
#pragma intrinsic (log10)
#pragma intrinsic (log10f)
#endif

extern double	modf(double, double *);

/* version of modff implemented for completeness only */

extern float	modff(float, float *); 

extern double	pow(double, double);
#ifdef __MATH_HAS_NO_SIDE_EFFECTS
#pragma no side effects (pow)
#endif /* __MATH_HAS_NO_SIDE_EFFECTS */

/* version of powf implemented for completeness only */
extern float	powf(float, float);
#ifdef __MATH_HAS_NO_SIDE_EFFECTS
#pragma no side effects (powf)
#endif /* __MATH_HAS_NO_SIDE_EFFECTS */

extern double	sqrt(double);
#ifdef __MATH_HAS_NO_SIDE_EFFECTS
#pragma no side effects (sqrt)
#endif /* __MATH_HAS_NO_SIDE_EFFECTS */

extern float	sqrtf(float);
#ifdef __MATH_HAS_NO_SIDE_EFFECTS
#pragma no side effects (sqrtf)
#endif /* __MATH_HAS_NO_SIDE_EFFECTS */

extern double	ceil(double);
#ifdef __MATH_HAS_NO_SIDE_EFFECTS
#pragma no side effects (ceil)
#endif /* __MATH_HAS_NO_SIDE_EFFECTS */

extern float	ceilf(float);
#ifdef __MATH_HAS_NO_SIDE_EFFECTS
#pragma no side effects (ceilf)
#endif /* __MATH_HAS_NO_SIDE_EFFECTS */

extern double	fabs(double);
#ifdef __MATH_HAS_NO_SIDE_EFFECTS
#pragma no side effects (fabs)
#endif /* __MATH_HAS_NO_SIDE_EFFECTS */

/* version of fabsf implemented for completeness only */

extern float	fabsf(float);
#ifdef __MATH_HAS_NO_SIDE_EFFECTS
#pragma no side effects (fabsf)
#endif /* __MATH_HAS_NO_SIDE_EFFECTS */

extern double	floor(double);
#ifdef __MATH_HAS_NO_SIDE_EFFECTS
#pragma no side effects (floor)
#endif /* __MATH_HAS_NO_SIDE_EFFECTS */

extern float	floorf(float);
#ifdef __MATH_HAS_NO_SIDE_EFFECTS
#pragma no side effects (floorf)
#endif /* __MATH_HAS_NO_SIDE_EFFECTS */

extern double	fmod(double, double);
#ifdef __MATH_HAS_NO_SIDE_EFFECTS
#pragma no side effects (fmod)
#endif /* __MATH_HAS_NO_SIDE_EFFECTS */

/* version of fmodf implemented for completeness only */

extern float	fmodf(float, float);
#ifdef __MATH_HAS_NO_SIDE_EFFECTS
#pragma no side effects (fmodf)
#endif /* __MATH_HAS_NO_SIDE_EFFECTS */

#if _XOPEN4 && _NO_ANSIMODE
/*
 * XPG4 extensions
 */
/* Some useful constants */
#define M_E		2.7182818284590452354
#define M_LOG2E		1.4426950408889634074
#define M_LOG10E	0.43429448190325182765
#define M_LN2		0.69314718055994530942
#define M_LN10		2.30258509299404568402
#define M_PI		3.14159265358979323846
#define M_PI_2		1.57079632679489661923
#define M_PI_4		0.78539816339744830962
#define M_1_PI		0.31830988618379067154
#define M_2_PI		0.63661977236758134308
#define M_2_SQRTPI	1.12837916709551257390
#define M_SQRT2		1.41421356237309504880
#define M_SQRT1_2	0.70710678118654752440

extern int signgam;

extern double	gamma(double);
extern double	lgamma(double);

extern int	isnan(double);
#ifdef __MATH_HAS_NO_SIDE_EFFECTS
#pragma no side effects (isnan)
#endif /* __MATH_HAS_NO_SIDE_EFFECTS */

extern double	erf(double);
#ifdef __MATH_HAS_NO_SIDE_EFFECTS
#pragma no side effects (erf)
#endif /* __MATH_HAS_NO_SIDE_EFFECTS */

extern double	erfc(double);
#ifdef __MATH_HAS_NO_SIDE_EFFECTS
#pragma no side effects (erfc)
#endif /* __MATH_HAS_NO_SIDE_EFFECTS */

extern double	hypot(double, double);
#ifdef __MATH_HAS_NO_SIDE_EFFECTS
#pragma no side effects (hypot)
#endif /* __MATH_HAS_NO_SIDE_EFFECTS */

extern double	j0(double);
#ifdef __MATH_HAS_NO_SIDE_EFFECTS
#pragma no side effects (j0)
#endif /* __MATH_HAS_NO_SIDE_EFFECTS */

extern double	j1(double);
#ifdef __MATH_HAS_NO_SIDE_EFFECTS
#pragma no side effects (j1)
#endif /* __MATH_HAS_NO_SIDE_EFFECTS */

extern double	jn(int, double);
#ifdef __MATH_HAS_NO_SIDE_EFFECTS
#pragma no side effects (jn)
#endif /* __MATH_HAS_NO_SIDE_EFFECTS */

extern double	y0(double);
#ifdef __MATH_HAS_NO_SIDE_EFFECTS
#pragma no side effects (y0)
#endif /* __MATH_HAS_NO_SIDE_EFFECTS */

extern double	y1(double);
#ifdef __MATH_HAS_NO_SIDE_EFFECTS
#pragma no side effects (y1)
#endif /* __MATH_HAS_NO_SIDE_EFFECTS */

extern double	yn(int, double);
#ifdef __MATH_HAS_NO_SIDE_EFFECTS
#pragma no side effects (yn)
#endif /* __MATH_HAS_NO_SIDE_EFFECTS */


#ifndef _MAXFLOAT
#define _MAXFLOAT
#define MAXFLOAT	((float)3.40282346638528860e+38)
#endif  /* _MAXFLOAT */

#endif /* _XOPEN4 && _NO_ANSIMODE */

#if _XOPEN4UX && _NO_ANSIMODE
/*
 * XPG4 Unix Extensions
 */
extern double	rint(double);
#ifdef __MATH_HAS_NO_SIDE_EFFECTS
#pragma no side effects (rint)
#endif /* __MATH_HAS_NO_SIDE_EFFECTS */

extern double	asinh(double);
#ifdef __MATH_HAS_NO_SIDE_EFFECTS
#pragma no side effects (asinh)
#endif /* __MATH_HAS_NO_SIDE_EFFECTS */

extern double	acosh(double);
#ifdef __MATH_HAS_NO_SIDE_EFFECTS
#pragma no side effects (acosh)
#endif /* __MATH_HAS_NO_SIDE_EFFECTS */

extern double	atanh(double);
#ifdef __MATH_HAS_NO_SIDE_EFFECTS
#pragma no side effects (atanh)
#endif /* __MATH_HAS_NO_SIDE_EFFECTS */

extern double	cbrt(double);
#ifdef __MATH_HAS_NO_SIDE_EFFECTS
#pragma no side effects (cbrt)
#endif /* __MATH_HAS_NO_SIDE_EFFECTS */

extern double	log1p(double);
#ifdef __MATH_HAS_NO_SIDE_EFFECTS
#pragma no side effects (log1p)
#endif /* __MATH_HAS_NO_SIDE_EFFECTS */

extern double	expm1(double);
#ifdef __MATH_HAS_NO_SIDE_EFFECTS
#pragma no side effects (expm1)
#endif /* __MATH_HAS_NO_SIDE_EFFECTS */

extern double	logb(double);
#ifdef __MATH_HAS_NO_SIDE_EFFECTS
#pragma no side effects (logb)
#endif /* __MATH_HAS_NO_SIDE_EFFECTS */

#if (defined(_COMPILER_VERSION) && (_COMPILER_VERSION >= 400))

extern double	nextafter(double, double);
#ifdef __MATH_HAS_NO_SIDE_EFFECTS
#pragma no side effects (nextafter)
#endif /* __MATH_HAS_NO_SIDE_EFFECTS */

extern double	remainder(double, double);
#ifdef __MATH_HAS_NO_SIDE_EFFECTS
#pragma no side effects (remainder)
#endif /* __MATH_HAS_NO_SIDE_EFFECTS */

#endif /* _COMPILER */

extern double	scalb(double, double);
#ifdef __MATH_HAS_NO_SIDE_EFFECTS
#pragma no side effects (scalb)
#endif /* __MATH_HAS_NO_SIDE_EFFECTS */

#endif /* _XOPEN4UX && _NO_ANSIMODE */

#if defined(_COMPILER_VERSION) && (_COMPILER_VERSION >= 400) && _SGIAPI

/* long double precision routines */

extern long double absl( long double );

extern long double acosl( long double );

extern long double asinl( long double );

extern long double atanl( long double );

#if _NO_XOPEN4 && (defined(__INLINE_INTRINSICS) && \
        defined(_COMPILER_VERSION) && (_COMPILER_VERSION >= 710))
#pragma intrinsic (acosl)
#pragma intrinsic (asinl)
#pragma intrinsic (atanl)
#endif

extern long double atan2l( long double, long double );

struct __cabsl_s { long double a,b; };

extern long double cabsl( struct __cabsl_s );

extern long double ceill( long double );

extern long double copysignl( long double, long double );

extern long double cosl( long double );

#if _NO_XOPEN4 && (defined(__INLINE_INTRINSICS) && \
        defined(_COMPILER_VERSION) && (_COMPILER_VERSION >= 710))
#pragma intrinsic (cosl)
#endif

extern long double coshl( long double );

extern long double expl( long double );

#if _NO_XOPEN4 && (defined(__INLINE_INTRINSICS) && \
        defined(_COMPILER_VERSION) && (_COMPILER_VERSION >= 710))
#pragma intrinsic (expl)
#endif

extern int finitel( long double );

extern long double floorl( long double );

extern long double fmodl( long double, long double );

extern long double hypotl( long double );

extern long double logl( long double );

extern long double log10l( long double );

#if _NO_XOPEN4 && (defined(__INLINE_INTRINSICS) && \
        defined(_COMPILER_VERSION) && (_COMPILER_VERSION >= 710))
#pragma intrinsic (logl)
#pragma intrinsic (log10l)
#endif

extern long double logbl( long double );

extern long double powl( long double, long double );

extern long double rintl( long double );

extern long double sinl( long double );

#if _NO_XOPEN4 && (defined(__INLINE_INTRINSICS) && \
        defined(_COMPILER_VERSION) && (_COMPILER_VERSION >= 710))
#pragma intrinsic (sinl)
#endif

extern long double sinhl( long double );

extern long double sqrtl( long double );

extern long double tanl( long double );

#if _NO_XOPEN4 && (defined(__INLINE_INTRINSICS) && \
        defined(_COMPILER_VERSION) && (_COMPILER_VERSION >= 710))
#pragma intrinsic (tanl)
#endif

extern long double tanhl( long double );

extern long double truncl( long double );

#ifdef __MATH_HAS_NO_SIDE_EFFECTS

#pragma no side effects (absl)
#pragma no side effects (acosl)
#pragma no side effects (asinl)
#pragma no side effects (atanl)
#pragma no side effects (atan2l)
#pragma no side effects (cabsl)
#pragma no side effects (ceill)
#pragma no side effects (copysignl)
#pragma no side effects (cosl)
#pragma no side effects (coshl)
#pragma no side effects (expl)
#pragma no side effects (finitel)
#pragma no side effects (floorl)
#pragma no side effects (fmodl)
#pragma no side effects (hypotl)
#pragma no side effects (logl)
#pragma no side effects (log10l)
#pragma no side effects (logbl)
#pragma no side effects (powl)
#pragma no side effects (rintl)
#pragma no side effects (sinl)
#pragma no side effects (sinhl)
#pragma no side effects (sqrtl)
#pragma no side effects (tanl)
#pragma no side effects (tanhl)
#pragma no side effects (truncl)

#endif /* __MATH_HAS_NO_SIDE_EFFECTS */

#endif

#if _SGIAPI
/*
 * SGI/SVR4 Additions
 */

enum version { c_issue_4, ansi_1, strict_ansi };
extern const enum version _lib_version;

struct __cabs_s { double a,b; };

extern double	cabs(struct __cabs_s);
#ifdef __MATH_HAS_NO_SIDE_EFFECTS
#pragma no side effects (cabs)
#endif /* __MATH_HAS_NO_SIDE_EFFECTS */

extern double	copysign(double, double);
#ifdef __MATH_HAS_NO_SIDE_EFFECTS
#pragma no side effects (copysign)
#endif /* __MATH_HAS_NO_SIDE_EFFECTS */

extern double	drem(double, double);
#ifdef __MATH_HAS_NO_SIDE_EFFECTS
#pragma no side effects (drem)
#endif /* __MATH_HAS_NO_SIDE_EFFECTS */

extern int	finite(double);
#ifdef __MATH_HAS_NO_SIDE_EFFECTS
#pragma no side effects (finite)
#endif /* __MATH_HAS_NO_SIDE_EFFECTS */


#if (defined(_COMPILER_VERSION) && (_COMPILER_VERSION >= 400))

/* long double precision routines */

/* libc routines */

extern long double qfrexp( long double, int *);

extern long double qldexp( long double, int );

extern long double qmodf( long double, long double *);

extern long double qnextafter( long double, long double );

extern long double qscalb( long double, long double );

#ifdef __MATH_HAS_NO_SIDE_EFFECTS

#pragma no side effects (qldexp)
#pragma no side effects (qnextafter)
#pragma no side effects (qscalb)

#endif /* __MATH_HAS_NO_SIDE_EFFECTS */

#endif

extern double	atof(const char *);

extern double   strtod(const char *, char **);

extern double   trunc(double);
#ifdef __MATH_HAS_NO_SIDE_EFFECTS
#pragma no side effects (trunc)
#endif /* __MATH_HAS_NO_SIDE_EFFECTS */

extern int	rand(void);
extern void	srand(unsigned);

extern long	random(void);
extern int	srandom(unsigned);/* doesn't return a value -- should be void */
extern char *	initstate(unsigned int, char *, int);
extern char *	setstate(char *);

extern double	drand48(void);
extern double	erand48(unsigned short [3]);
extern long	lrand48(void);
extern long	nrand48(unsigned short [3]);
extern long	mrand48(void);
extern long	jrand48(unsigned short [3]);
extern void	srand48(long);
extern unsigned short * seed48(unsigned short int [3]);
extern void	lcong48(unsigned short int [7]);

/* Map old MIPS names of single-precision forms to ANSI names.*/

#define facos	acosf
#define fasin	asinf
#define fatan	atanf
#define fatan2	atan2f
#define fcos	cosf
#define fsin	sinf
#define ftan	tanf
#define fcosh	coshf
#define fsinh	sinhf
#define ftanh	tanhf
#define fexp	expf
#define flog	logf
#define flog10	log10f
#define fsqrt	sqrtf
#define fceil	ceilf
#define ffloor	floorf


#if (defined(_COMPILER_VERSION) && (_COMPILER_VERSION >= 400))

/* map long double precision forms to the ANSI names */

#define	qabs	fabsl
#define	qacos	acosl
#define	qasin	asinl
#define	qatan	atanl
#define	qatan2	atan2l

#define __qcabs_s __cabsl_s

#define	qcabs	cabsl
#define	qceil	ceill
#define	qcopysign copysignl
#define	qcos	cosl
#define	qcosh	coshl
#define	qexp	expl
#define	qfinite	finitel
#define	qfloor	floorl
#define	qhypot	hypotl
#define	qlog	logl
#define	qlog10	log10l
#define	qlogb	logbl
#define	qmod	fmodl
#define	qpow	powl
#define	qrint	rintl
#define	qsin	sinl
#define	qsinh	sinhl
#define	qsqrt	sqrtl
#define	qtan	tanl
#define	qtanh	tanhl
#define	qtrunc	truncl

#endif

/* similar mapping of old MIPS names to ANSI-like names */

#undef flog1p
#define flog1p 	log1pf
#define ftrunc	truncf

#ifdef __cplusplus
#ifndef _ABS_
#define _ABS_
inline int abs(int x) {return x > 0 ? x : -x;}
#endif
#endif

/* additional single-percision forms */
extern float	fhypot(float, float);
#ifdef __MATH_HAS_NO_SIDE_EFFECTS
#pragma no side effects (fhypot)
#endif /* __MATH_HAS_NO_SIDE_EFFECTS */

extern float	hypotf(float, float);
#ifdef __MATH_HAS_NO_SIDE_EFFECTS
#pragma no side effects (hypotf)
#endif /* __MATH_HAS_NO_SIDE_EFFECTS */

struct __fcabs_s { float a,b; };

extern float	fcabs(struct __fcabs_s);
#ifdef __MATH_HAS_NO_SIDE_EFFECTS
#pragma no side effects (fcabs)
#endif /* __MATH_HAS_NO_SIDE_EFFECTS */

extern float	fexpm1(float);
#ifdef __MATH_HAS_NO_SIDE_EFFECTS
#pragma no side effects (fexpm1)
#endif /* __MATH_HAS_NO_SIDE_EFFECTS */

extern float	expm1f(float);
#ifdef __MATH_HAS_NO_SIDE_EFFECTS
#pragma no side effects (expm1f)
#endif /* __MATH_HAS_NO_SIDE_EFFECTS */

extern float	log1pf(float);
#ifdef __MATH_HAS_NO_SIDE_EFFECTS
#pragma no side effects (log1pf)
#endif /* __MATH_HAS_NO_SIDE_EFFECTS */

extern float	truncf(float);
#ifdef __MATH_HAS_NO_SIDE_EFFECTS
#pragma no side effects (truncf)
#endif /* __MATH_HAS_NO_SIDE_EFFECTS */

#if (defined(__INLINE_INTRINSICS) && defined(_COMPILER_VERSION) && (_COMPILER_VERSION >= 400))
#if _MIPS_ISA != _MIPS_ISA_MIPS1
#pragma intrinsic (hypotf)
#pragma intrinsic (fhypot)
#endif
#endif

#if 0 /* These are not implemented yet. */

extern float	ferf(float);
extern float	ferfc(float);
extern float	flgamma(float);
extern float	fcopysign(float, float);
extern float	fdrem(float, float);
extern float	flogb(float);
extern float	fscalb(float, int);
extern int	ffinite(float);
extern float	fj0(float);
extern float	fj1(float);
extern float	fjn(int, float);
extern float	fy0(float);
extern float	fy1(float);
extern float	fyn(int, float);
extern float	fatof(char *);
extern float	frint(float);
extern float	facosh(float);
extern float	fatanh(float);
extern float	fcbrt(float);
#endif /* 0 */

#ifndef HUGE
#ifdef  MAXFLOAT
#define HUGE		MAXFLOAT
#else
#define HUGE	((float)3.40282346638528860e+38) /* value of MAXFLOAT */
#endif /* MAXFLOAT */
#endif /* !HUGE */

#define _ABS(x)	((x) < 0 ? -(x) : (x))
#define _REDUCE(TYPE, X, XN, C1, C2)	{ \
	double x1 = (double)(TYPE)X, x2 = X - x1; \
	X = x1 - (XN) * (C1); X += x2; X -= (XN) * (C2); }
#define _POLY1(x, c)	((c)[0] * (x) + (c)[1])
#define _POLY2(x, c)	(_POLY1((x), (c)) * (x) + (c)[2])
#define _POLY3(x, c)	(_POLY2((x), (c)) * (x) + (c)[3])
#define _POLY4(x, c)	(_POLY3((x), (c)) * (x) + (c)[4])
#define _POLY5(x, c)	(_POLY4((x), (c)) * (x) + (c)[5])
#define _POLY6(x, c)	(_POLY5((x), (c)) * (x) + (c)[6])
#define _POLY7(x, c)	(_POLY6((x), (c)) * (x) + (c)[7])
#define _POLY8(x, c)	(_POLY7((x), (c)) * (x) + (c)[8])
#define _POLY9(x, c)	(_POLY8((x), (c)) * (x) + (c)[9])


struct exception {
	int type;
	char *name;
	double arg1;
	double arg2;
	double retval;
};

extern int matherr(struct exception *p);


#include <svr4_math.h>

#endif /* _SGIAPI */

#ifdef __INLINE_INTRINSICS

/* The functions made intrinsic here can be activated by the driver
** passing -D__INLINE_INTRINSICS to cfe, but cfe should ensure that
** this has no effect unless the hardware architecture directly
** supports these basic operations.
*/

#if _MIPS_ISA != _MIPS_ISA_MIPS1
#pragma intrinsic (sqrt)
#pragma intrinsic (sqrtf)
#if (defined(_COMPILER_VERSION) && (_COMPILER_VERSION >= 400))
#pragma intrinsic (pow)
#pragma intrinsic (powf)
#endif
#endif
#pragma intrinsic (fabs)
#pragma intrinsic (fabsf)
#endif

#ifdef __cplusplus
}
#endif
#endif /* !__MATH_H__ */

