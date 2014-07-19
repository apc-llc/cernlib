/*
 * $Id: mkutda.h,v 1.1.1.1 1996/03/08 15:33:12 mclareni Exp $
 *
 * $Log: mkutda.h,v $
 * Revision 1.1.1.1  1996/03/08 15:33:12  mclareni
 * Kuip
 *
 */
/*** Global data with NO reference to any Motif/X11 include files ***/
#ifndef _mkutda_
#define _mkutda_

#include "mkdims.h"

/* #define Extern must be in one routine to allocate space */
#ifndef Extern
#  define Extern extern
#endif

/* Global data (general datas not connected to Motif include files) */
extern int UxScreenWidth, UxScreenHeight;
extern char main_prompt[MAX_string];
extern char class_name[MAX_string];
extern int km_all_cycles; /* flag for "View" with "all cycles" for VMS */

#define KM_MAIN_CURSOR 0
#define KM_WAIT_CURSOR 1
#define KM_CROSS_CURSOR 2
#define KM_INPUT_CURSOR 3
#define KM_HELP_CURSOR 1
#define KM_BUSY_CURSOR 1
#define KM_BLCK_CURSOR 1
#define KM_SELC_CURSOR 1

#define NO_ICON 0
#define ICON 1

#define by_NAME     0
#define by_ICON     1
#define by_BOTH     2
#define by_COMMAND  3
#define by_COMMAND1 4

#endif /* _mkutda_ */

