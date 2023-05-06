/*
 * This file is similar to Library.tmpl, and it needs to be explicitly
 * included by anything that cares.
 *
 * It is only needed by libraries that have both C anc C++ sources.
 * (The X11R6 macros do not deal with this case.)  If a directory has
 * just one or the other, then the standard rules work as is.
 *
 * We choose to follow X11R6, not Motif 2.0, because Motif 2.0 has not yet
 * gotten synched up with X11R6.
 */


/*
 * The following definitions are an abbreviated version, analagous to
 * that which is in Library.tmpl:
 *	* This version assume CplusplusSource is on.
 *	* This version assumes that LibraryObjectRule() is also called,
 *	  and so does not duplicate it.
 */
#if DoDebugLib
#define _SgCpp_DebuggedObjCompile(opt) DebuggedLibObjCplusplusCompile(opt)
#else
#define _SgCpp_DebuggedObjCompile(opt) $(_NULLCMD_)
#endif

#if DoProfileLib
#define _SgCpp_ProfiledObjCompile(opt) ProfiledLibObjCplusplusCompile(opt)
#else
#define _SgCpp_ProfiledObjCompile(opt) $(_NULLCMD_)
#endif

#if !DoNormalLib
#define _SgCpp_NormalObjCompile(opt) $(_NULLCMD_)
#else
#if DoSharedLib && SeparateSharedCompile
#define _SgCpp_NormalObjCompile(opt) UnsharedLibObjCplusplusCompile(opt)
#else
#define _SgCpp_NormalObjCompile(opt) NormalLibObjCplusplusCompile(opt)
#endif
#endif

#if !DoSharedLib || (DoNormalLib && !SeparateSharedCompile)
#define _SgCpp_SharedObjCompile(opt) $(_NULLCMD_)
#else
#if SeparateSharedCompile
#define _SgCpp_SharedObjCompile(opt) NormalSharedLibObjCplusplusCompile(opt)
#else
#define _SgCpp_SharedObjCompile(opt) NormalLibObjCplusplusCompile(opt)
#endif
#endif


#ifndef _Sg_CPlusPlusObjectRule
#define _Sg_CPlusPlusObjectRule()					@@\
.c++.o:									@@\
	_SgCpp_DebuggedObjCompile($(_NOOP_))				@@\
	_SgCpp_ProfiledObjCompile($(_NOOP_))				@@\
	_SgCpp_NormalObjCompile($(_NOOP_))				@@\
	_SgCpp_SharedObjCompile($(_NOOP_))
#endif /* _Sg_CPlusPlusObjectRule */
