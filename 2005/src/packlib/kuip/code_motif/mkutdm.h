/*
 * $Id: mkutdm.h,v 1.1.1.1 1996/03/08 15:33:12 mclareni Exp $
 *
 * $Log: mkutdm.h,v $
 * Revision 1.1.1.1  1996/03/08 15:33:12  mclareni
 * Kuip
 *
 */
/*** Global data which refer to some Motif/X11 include files ***/
#ifndef _mkutdm_
#define _mkutdm_

/* #define Extern must be in one routine to allocate space */
#ifndef Extern
#  define Extern extern
#endif

#if XtSpecificationRelease <= 4   /* X11R4 */
typedef Cardinal IntC;
#else                             /* X11R5 */
typedef int IntC;
#endif

/* global data (general data but connected to Motif include files) */
#include "mkutda.h"
/* Instance Specific Variables and Context Routines */
#include "mkuxxt.h"
Extern Display *km_Display;
Extern Position km_xcur_pos, km_ycur_pos; /* current cursor position */
Extern Widget km_toplevel;  /* shadow kxterm used to position popups */
Extern Widget km_main_browser; /* KUIP Main Browser */
Extern Widget km_scrOutput;  /* scrollable text output (created by KUMOUT) */
Extern Widget km_HigzOutput; /* scrollable text output for HIGZ (Igxmess) */
Extern Cursor km_main_cursor, km_input_cursor, km_wait_cursor, km_cross_cursor,
              km_help_cursor, km_busy_cursor, km_blck_cursor, km_selc_cursor;
Extern Pixmap km_pixmap; /* general pixmap for KUIP/Motif */
Extern XmStringCharSet km_charset;  /* KUIP/Motif character set used */
Extern Pixel km_foreground, km_background;
Extern Boolean km_panel_context; /* flag for context help inside panels */

#endif /* _mkutdm_ */

