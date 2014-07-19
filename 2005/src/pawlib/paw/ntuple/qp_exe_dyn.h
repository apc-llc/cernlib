/*
 *  qp_exe_dyn.h  --
 *	Instantiate the code for ntuple variable slices.
 *
 *  Original:  4-Apr-1995 15:48
 *
 *  Author:   Maarten Ballintijn <Maarten.Ballintijn@cern.ch>
 *
 *  $Id: qp_exe_dyn.h,v 1.5 1996/05/06 09:18:02 maartenb Exp $
 *
 *  $Log: qp_exe_dyn.h,v $
 *  Revision 1.5  1996/05/06 09:18:02  maartenb
 *  - Introduce a consistent reporting of array bound errors
 *
 *  - Cleanup dynamic non scalar access.
 *
 *  Revision 1.4  1996/04/23 18:38:26  maartenb
 *  - Add RCS keywords
 *
 *
 */

#define	OP_TYPE		bool
#define	OP_DTYPE	D_BOOL
#define	OP_BASE		D_BOOL

#include	"qp_exe_dyn_templ.h"

#undef	OP_TYPE
#undef	OP_DTYPE
#undef	OP_BASE

#define	OP_TYPE		UInt32
#define	OP_DTYPE	D_UINT
#define	OP_BASE		D_UINT

#include	"qp_exe_dyn_templ.h"

#undef	OP_TYPE
#undef	OP_DTYPE
#undef	OP_BASE

#define	OP_TYPE		UInt64
#define	OP_DTYPE	D_ULONG
#define	OP_BASE		D_ULONG

#include	"qp_exe_dyn_templ.h"

#undef	OP_TYPE
#undef	OP_DTYPE
#undef	OP_BASE

#define	OP_TYPE		Int32
#define	OP_DTYPE	D_INT
#define	OP_BASE		D_INT

#include	"qp_exe_dyn_templ.h"

#undef	OP_TYPE
#undef	OP_DTYPE
#undef	OP_BASE

#define	OP_TYPE		Int64
#define	OP_DTYPE	D_LONG
#define	OP_BASE		D_LONG

#include	"qp_exe_dyn_templ.h"

#undef	OP_TYPE
#undef	OP_DTYPE
#undef	OP_BASE

#define	OP_TYPE		Float32
#define	OP_DTYPE	D_FLOAT
#define	OP_BASE		D_FLOAT

#include	"qp_exe_dyn_templ.h"

#undef	OP_TYPE
#undef	OP_DTYPE
#undef	OP_BASE

#define	OP_TYPE		Float64
#define	OP_DTYPE	D_DOUBLE
#define	OP_BASE		D_DOUBLE

#include	"qp_exe_dyn_templ.h"

#undef	OP_TYPE
#undef	OP_DTYPE
#undef	OP_BASE

#define	OP_TYPE		String32
#define	OP_DTYPE	D_STR
#define	OP_BASE		D_STR

#include	"qp_exe_dyn_templ.h"

#undef	OP_TYPE
#undef	OP_DTYPE
#undef	OP_BASE
