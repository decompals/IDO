#ifndef	__FP_CLASS_H__
#define	__FP_CLASS_H__
#ifdef __cplusplus
extern "C" {
#endif
/* | Copyright (c) 1986 MIPS Computer Systems, Inc.  | */
/* | All Rights Reserved.                            | */
/* --------------------------------------------------- */
/* $Revision: 1.9 $ */



#if (defined(_LANGUAGE_C) || defined(_LANGUAGE_C_PLUS_PLUS))
extern int fp_class_d(double x);
extern int fp_class_f(float x);
#endif /* LANGUAGE_C */

/*
 * Constants returned by the floating point fp_class_[fd]() functions.
 */
#define	FP_SNAN		0
#define	FP_QNAN		1
#define	FP_POS_INF	2
#define	FP_NEG_INF	3
#define	FP_POS_NORM	4
#define	FP_NEG_NORM	5
#define	FP_POS_DENORM	6
#define	FP_NEG_DENORM	7
#define	FP_POS_ZERO	8
#define	FP_NEG_ZERO	9

#ifdef __cplusplus
}
#endif
#endif /* __FP_CLASS_H__ */
