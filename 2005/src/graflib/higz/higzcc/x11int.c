/*
 * $Id: x11int.c,v 1.20 2000/07/08 06:19:30 couet Exp $
 *
 * $Log: x11int.c,v $
 * Revision 1.20  2000/07/08 06:19:30  couet
 * - with an external window as parent, the position of the window created by
 *   ixopnwi was not correct.
 *
 * Revision 1.19  1999/04/01 09:20:24  couet
 * - MapNotify is not supported by all the X11 emulators (problem on Mac !!!)
 *
 * Revision 1.18  1999/01/08 14:07:10  couet
 * - imgscaling prototype only in GIF is selected
 *
 * Revision 1.17  1999/01/08 13:56:56  couet
 * - prototype for imgscaling
 *
 * Revision 1.16  1998/11/16 11:11:17  couet
 * - ixdogif can scale pictures
 *
 * Revision 1.15  1998/11/13 14:13:23  couet
 * - uses image size instead of window size in ixdogif
 *
 * Revision 1.14  1998/11/13 10:49:07  couet
 * - New version of the routine imgpickpalette coming from a C++ version
 *   from Fons Rademakers. This version allows to work on TrueColor
 *   devices. ixdogif in x11int.c has been modified in order to use this
 *   new version of imgpickpalette. Protections have been added in
 *   ixdogif to prevent some crashes when one try to create a gif after
 *   a window resizing or when the program (for example PAW) started
 *   with a workstation type equal to 0. All the static arrays in ixdogif
 *   have been converted into dynamic one.
 *
 * Revision 1.13  1998/11/11 16:23:39  couet
 * - Protection added in ixdogif in case the Visual is TrueColor
 *
 * Revision 1.12  1998/10/28 10:05:37  couet
 * - a printf (debug) has been removed
 *
 * Revision 1.10  1998/01/30 15:22:06  couet
 * - APOLLO version removed
 *
 * Revision 1.9  1997/06/20 15:07:19  couet
 * - protection added in ixdogif in case the output file (gif) cannot be open.
 *
 * Revision 1.8  1997/04/22 08:42:57  couet
 * - The delete option in the WM top left button is disabled because it stopped
 * the application.
 *
 * Revision 1.7  1996/10/16 13:50:50  couet
 * - Remaining defined colors are freed in ixclsds
 *
 * Revision 1.6  1996/04/23 08:36:54  couet
 * - The test on "display" in ixopnds was not correct. In the case of Paw++
 *   the display is set outside the ixopnds routine and in that case the
 *   graphics contexts was not initialized. Now the variable isdisp is tested.
 *
 * Revision 1.5  1996/04/19 16:23:52  maartenb
 * - Use XFreeFontNames() to free fontlist
 *
 * Revision 1.4  1996/04/19 13:23:00  couet
 * - free(fontlist) was misplaced
 *
 * Revision 1.3  1996/04/19 12:10:31  couet
 * - Free fontlist to avoid memory leak
 *
 * Revision 1.2  1996/04/18 14:36:12  couet
 * - ixopnds exit with return code 0 if the display is already open.
 *
 * - Modify the way the fonts are managed to avoid memory leak.
 *
 * Revision 1.1.1.1  1996/02/14 13:10:26  mclareni
 * Higz
 *
 */
#include "higz/pilot.h"
#if (defined(CERNLIB_X11))&&(!defined(CERNLIB_WINNT))

/******************************************************************************
 *                                                                            *
 *-- Author : O.Couet                                                         *
 *                                                                            *
 * Fortran interface to X11 window routines for HIGZ                          *
 *                                                                            *
 ******************************************************************************/

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>
#include <math.h>
#if defined(CERNLIB_CRAY)
#include <fortran.h>
#endif
#if defined(CERNLIB_VAX)
#include <descrip.h>
#include <lnmdef.h>
#include <types.h>
#include <socket.h>  /* for gethostname()                                     */
#define CADDR_T      /* defined in types.h would be redefined in Xlib.h       */
#include <decw$include/Xlib.h>
#include <decw$include/Xutil.h>
#include <decw$include/Xatom.h>
#include <decw$include/cursorfont.h>
#include <decw$include/keysym.h>
#endif
#if defined(CERNLIB_IBM)
#include <types.h>
#endif
#if !defined(CERNLIB_VAX)
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xatom.h>
#include <X11/cursorfont.h>
#include <X11/keysym.h>
#endif
#if defined(CERNLIB_CRAY)
#define ixclip  IXCLIP
#define ixclrwi IXCLRWI
#define ixclsds IXCLSDS
#define ixclswi IXCLSWI
#define ixflare IXFLARE
#define ixgetge IXGETGE
#define ixgetpl IXGETPL
#define ixgetht IXGETHT
#define ixgetwi IXGETWI
#define ixline  IXLINE
#define ixmarke IXMARKE
#define ixnocli IXNOCLI
#define ixopnds IXOPNDS
#define ixopnwi IXOPNWI
#define ixreqlo IXREQLO
#define ixqptr  IXQPTR
#define ixwarp  IXWARP
#define ixreqst IXREQST
#define ixselwi IXSELWI
#define ixsetco IXSETCO
#define ixsetfc IXSETFC
#define ixsetfs IXSETFS
#define ixsetlc IXSETLC
#define ixsetln IXSETLN
#define ixsetls IXSETLS
#define ixsetmc IXSETMC
#define ixsetms IXSETMS
#define ixsetta IXSETTA
#define ixsettc IXSETTC
#define ixsettf IXSETTF
#define ixtext  IXTEXT
#define ixupdwi IXUPDWI
#define ixbox   IXBOX
#define ixtxtl  IXTXTL
#define ixdrmde IXDRMDE
#define ixsync  IXSYNC
#define ixsethn IXSETHN
#define ixopnpx IXOPNPX
#define ixclpx  IXCLPX
#define ixcppx  IXCPPX
#define ixrmpx  IXRMPX
#define ixwipx  IXWIPX
#define ixclrpx IXCLRPX
#define ixs2buf IXS2BUF
#define ixq2buf IXQ2BUF
#define ixwrpx  IXWRPX
#define ixrsiz  IXRSIZ
#define ixsavwi IXSAVWI
#define ixsetwi IXSETWI
#define ixrscwi IXRSCWI
#define ixmovwi IXMOVWI
#define ixminit IXMINIT
#define ixca    IXCA
#endif
#if (defined(CERNLIB_CRAY))&&(defined(CERNLIB_GIF))
#define ixdogif IXDOGIF
#define ixldgif IXLDGIF
#endif
#if defined(CERNLIB_IBM)
#pragma linkage (ixclip ,FORTRAN)
#pragma linkage (ixclrwi,FORTRAN)
#pragma linkage (ixclsds,FORTRAN)
#pragma linkage (ixclswi,FORTRAN)
#pragma linkage (ixflare,FORTRAN)
#pragma linkage (ixgetge,FORTRAN)
#pragma linkage (ixgetpl,FORTRAN)
#pragma linkage (ixgetht,FORTRAN)
#pragma linkage (ixgetwi,FORTRAN)
#pragma linkage (ixline ,FORTRAN)
#pragma linkage (ixmarke,FORTRAN)
#pragma linkage (ixnocli,FORTRAN)
#pragma linkage (ixopnds,FORTRAN)
#pragma linkage (ixopnwi,FORTRAN)
#pragma linkage (ixreqlo,FORTRAN)
#pragma linkage (ixqptr ,FORTRAN)
#pragma linkage (ixwarp ,FORTRAN)
#pragma linkage (ixreqst,FORTRAN)
#pragma linkage (ixselwi,FORTRAN)
#pragma linkage (ixsetco,FORTRAN)
#pragma linkage (ixsetfc,FORTRAN)
#pragma linkage (ixsetfs,FORTRAN)
#pragma linkage (ixsetlc,FORTRAN)
#pragma linkage (ixsetln,FORTRAN)
#pragma linkage (ixsetls,FORTRAN)
#pragma linkage (ixsetmc,FORTRAN)
#pragma linkage (ixsetms,FORTRAN)
#pragma linkage (ixsetta,FORTRAN)
#pragma linkage (ixsettc,FORTRAN)
#pragma linkage (ixsettf,FORTRAN)
#pragma linkage (ixtext ,FORTRAN)
#pragma linkage (ixupdwi,FORTRAN)
#pragma linkage (ixbox  ,FORTRAN)
#pragma linkage (ixtxtl ,FORTRAN)
#pragma linkage (ixdrmde,FORTRAN)
#pragma linkage (ixsync ,FORTRAN)
#pragma linkage (ixsethn,FORTRAN)
#pragma linkage (ixopnpx,FORTRAN)
#pragma linkage (ixclpx ,FORTRAN)
#pragma linkage (ixcppx ,FORTRAN)
#pragma linkage (ixrmpx ,FORTRAN)
#pragma linkage (ixwipx ,FORTRAN)
#pragma linkage (ixclrpx,FORTRAN)
#pragma linkage (ixs2buf,FORTRAN)
#pragma linkage (ixq2buf,FORTRAN)
#pragma linkage (ixwrpx ,FORTRAN)
#pragma linkage (ixrsiz ,FORTRAN)
#pragma linkage (ixsavwi,FORTRAN)
#pragma linkage (ixsetwi,FORTRAN)
#pragma linkage (ixrscwi,FORTRAN)
#pragma linkage (ixmovwi,FORTRAN)
#pragma linkage (ixminit,FORTRAN)
#pragma linkage (ixca   ,FORTRAN)
#define XRotDrawString XRDS
#define XRotDrawImageString XRDIS
#define XRotDrawAlignedString XRDAS
#define XRotDrawAlignedImageString XRDAIS
#endif
#if (defined(CERNLIB_IBM))&&(defined(CERNLIB_GIF))
#pragma linkage (ixdogif,FORTRAN)
#pragma linkage (ixldgif,FORTRAN)
#endif
#if defined(CERNLIB_QX_SC)
#define ixclip  ixclip_
#define ixclrwi ixclrwi_
#define ixclsds ixclsds_
#define ixclswi ixclswi_
#define ixflare ixflare_
#define ixgetge ixgetge_
#define ixgetpl ixgetpl_
#define ixgetht ixgetht_
#define ixgetwi ixgetwi_
#define ixline  ixline_
#define ixmarke ixmarke_
#define ixnocli ixnocli_
#define ixopnds ixopnds_
#define ixopnwi ixopnwi_
#define ixreqlo ixreqlo_
#define ixqptr  ixqptr_
#define ixwarp  ixwarp_
#define ixreqst ixreqst_
#define ixselwi ixselwi_
#define ixsetco ixsetco_
#define ixsetfc ixsetfc_
#define ixsetfs ixsetfs_
#define ixsetlc ixsetlc_
#define ixsetln ixsetln_
#define ixsetls ixsetls_
#define ixsetmc ixsetmc_
#define ixsetms ixsetms_
#define ixsetta ixsetta_
#define ixsettc ixsettc_
#define ixsettf ixsettf_
#define ixtext  ixtext_
#define ixupdwi ixupdwi_
#define ixbox   ixbox_
#define ixtxtl  ixtxtl_
#define ixdrmde ixdrmde_
#define ixsync  ixsync_
#define ixsethn ixsethn_
#define ixopnpx ixopnpx_
#define ixclpx  ixclpx_
#define ixcppx  ixcppx_
#define ixrmpx  ixrmpx_
#define ixwipx  ixwipx_
#define ixclrpx ixclrpx_
#define ixs2buf ixs2buf_
#define ixq2buf ixq2buf_
#define ixwrpx  ixwrpx_
#define ixrsiz  ixrsiz_
#define ixsavwi ixsavwi_
#define ixsetwi ixsetwi_
#define ixrscwi ixrscwi_
#define ixmovwi ixmovwi_
#define ixminit ixminit_
#define ixca    ixca_
#endif
#if (defined(CERNLIB_QX_SC))&&(defined(CERNLIB_GIF))
#define ixdogif ixdogif_
#define ixldgif ixldgif_
#endif
#if (defined(CERNLIB_TKTCL))&&(defined(CERNLIB_WINNT))
#define ixclip  __stdcall IXCLIP
#define ixclrwi __stdcall IXCLRWI
#define ixclsds __stdcall IXCLSDS
#define ixclswi __stdcall IXCLSWI
#define ixflare __stdcall IXFLARE
#define ixgetge __stdcall IXGETGE
#define ixgetpl __stdcall IXGETPL
#define ixgetht __stdcall IXGETHT
#define ixgetwi __stdcall IXGETWI
#define ixline  __stdcall IXLINE
#define ixmarke __stdcall IXMARKE
#define ixnocli __stdcall IXNOCLI
#define ixopnds __stdcall IXOPNDS
#define ixopnwi __stdcall IXOPNWI
#define ixreqlo __stdcall IXREQLO
#define ixqptr  __stdcall IXQPTR
#define ixwarp  __stdcall IXWARP
#define ixreqst __stdcall IXREQST
#define ixselwi __stdcall IXSELWI
#define ixsetco __stdcall IXSETCO
#define ixsetfc __stdcall IXSETFC
#define ixsetfs __stdcall IXSETFS
#define ixsetlc __stdcall IXSETLC
#define ixsetln __stdcall IXSETLN
#define ixsetls __stdcall IXSETLS
#define ixsetmc __stdcall IXSETMC
#define ixsetms __stdcall IXSETMS
#define ixsetta __stdcall IXSETTA
#define ixsettc __stdcall IXSETTC
#define ixsettf __stdcall IXSETTF
#define ixtext  __stdcall IXTEXT
#define ixupdwi __stdcall IXUPDWI
#define ixbox   __stdcall IXBOX
#define ixtxtl  __stdcall IXTXTL
#define ixdrmde __stdcall IXDRMDE
#define ixsync  __stdcall IXSYNC
#define ixsethn __stdcall IXSETHN
#define ixopnpx __stdcall IXOPNPX
#define ixclpx  __stdcall IXCLPX
#define ixcppx  __stdcall IXCPPX
#define ixrmpx  __stdcall IXRMPX
#define ixwipx  __stdcall IXWIPX
#define ixclrpx __stdcall IXCLRPX
#define ixs2buf __stdcall IXS2BUF
#define ixq2buf __stdcall IXQ2BUF
#define ixwrpx  __stdcall IXWRPX
#define ixrsiz  __stdcall IXRSIZ
#define ixsavwi __stdcall IXSAVWI
#define ixsetwi __stdcall IXSETWI
#define ixrscwi __stdcall IXRSCWI
#define ixmovwi __stdcall IXMOVWI
#define ixminit __stdcall IXMINIT
#define ixca    __stdcall IXCA
#endif
#if (defined(CERNLIB_TKTCL))&&(defined(CERNLIB_WINNT))&&(defined(CERNLIB_GIF))
#define ixdogif __stdcall IXDOGIF
#define ixldgif __stdcall IXLDGIF
#endif

#define max(a,b) ((a) > (b) ? (a) : (b))
#define min(a,b) ((a) < (b) ? (a) : (b))

#define forever while(1)
#define LNULL 0L

#define MAXWN 20                 /* maximum number of windows                 */
static struct {
  int      open;                 /* 1 if the window is open, 0 if not         */
  int      motif;                /* 1 if the window is a motif window         */
  Drawable motif_window;         /* adress of the Motif window                */
  int      double_buffer;        /* 1 if the double buffer is on, 0 if not    */
  Drawable drawing;              /* drawing area, equal to window or buffer   */
  Drawable window;               /* X11 window                                */
  Drawable buffer;               /* pixmap used for double buffer             */
  int width;                     /* width of the window                       */
  int height;                    /* height of the window                      */
  int clip;                      /* 1 if the clipping is on                   */
  int xclip;                     /* x coordinate of the clipping rectangle    */
  int yclip;                     /* y coordinate of the clipping rectangle    */
  int wclip;                     /* width of the clipping rectangle           */
  int hclip;                     /* height of the clipping rectangle          */
} windows[MAXWN],                /* windows : list of the opened windows      */
  *cws, *tws, *sws;              /* cws : pointer to the current window       */
                                 /* tws : temporary pointer                   */
                                 /* sws : saved window (ixsavwi ixsetwi)      */

Drawable oldwin;                 /* allows to save the current opened window  */
static int ispix = 0;            /* if ispix=1 a pixmap has been opened       */

static XClassHint class_hints = {"higz", "HIGZ"}; /* Class name is HIGZ       */

static Display *display;
static int isdisp = 0;           /* if isdisp=1 the display has been opened   */
static Screen *screen;
static int screen_number;
static char hname[132];          /* host name                                 */
static char vendor[132];         /* output of XServerVendor                   */

static Window (*motif_open) ();  /* external routine to open a motif window   */
static void (*motif_init) ();    /* external routine to init a motif window   */
static void (*motif_close) ();   /* external routine to close a motif window  */

static int external_int = 0;
static Drawable external_window;

static int do_raise = 0;         /* if do_raise =1 cws is raised in ixupdwi   */

/*
 * colors[1]           : foreground also used for b/w screen
 * colors[0]           : background also used for b/w screen
 * colors[2..MAXCOL-1] : colors which can be set by IXSETCO
 */
static int colored;              /* Flag if color screen                      */
static Colormap colormap;        /* Maba's Colormap                           */

#define MAXCOL 256
static struct {
  int defined;
  unsigned long pixel;
  float red;
  float green;
  float blue;
} colors[MAXCOL] = {0, 0, 1., 1., 1.};

/*
 * Primitives Graphic Contexts global for all windows
 */
#define MAXGC 7
static GC gclist[MAXGC];
static GC *gcline = &gclist[0];  /* Polylines                                 */
static GC *gcmark = &gclist[1];  /* Polymarker                                */
static GC *gcfill = &gclist[2];  /* Fill areas                                */
static GC *gctext = &gclist[3];  /* Text                                      */
static GC *gcinvt = &gclist[4];  /* Inverse text                              */
static GC *gcdash = &gclist[5];  /* Dashed lines                              */
static GC *gcpxmp = &gclist[6];  /* Pixmap management                         */

static unsigned long gc_mask = GCForeground | GCBackground;
static XGCValues gc_return;

/*
 * Input echo Graphic Context global for all windows
 */
static GC gcecho;                           /* input echo                     */
static XGCValues gcechov;

static int fill_hollow;                     /* Flag if fill style is hollow   */
static Pixmap fill_pattern = (Pixmap)LNULL; /* Fill pattern                   */
static int current_fasi = 0;                /* Current fill area style index  */
static int text_align = 0;                  /* Align text left, center, right */

/*
 * Text management
 */
#define MAXFONT 4
static struct {
  XFontStruct *id;
  char name[80];                            /* font name                      */
} font[MAXFONT];                            /* list of fonts loaded           */
static XFontStruct *text_font;              /* current font                   */
static int current_font_number = 0;         /* current font number in font[]  */

/*
 * keep style values for line GC
 */
static int line_width = 0;
static int line_style = LineSolid;
static int cap_style  = CapButt;
static int join_style = JoinMiter;
static char dash_list[4];
static int dash_length = 0;
static int dash_offset = 0;

/*
 * event masks
 */
static unsigned long mouse_mask = ButtonPressMask   | ButtonReleaseMask |
                                  EnterWindowMask   | LeaveWindowMask   |
                                  PointerMotionMask | KeyPressMask      |
                                  KeyReleaseMask;
static unsigned long keybd_mask = ButtonPressMask | KeyPressMask |
                                  EnterWindowMask | LeaveWindowMask;

/*
 * last cursor positions
 */
static int xloc  = 0;
static int yloc  = 0;
static int xlocp = 0;
static int ylocp = 0;

/*
 * data to create an invisible cursor
 */
static char null_cursor_bits[] = {
0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00};
static Cursor null_cursor = (Cursor)LNULL;

/*
 * data to create fill area interior style
 */
#if defined(CERNLIB_TK_WINDOWS)
#define Hatches_type static unsigned char
#endif
#if !defined(CERNLIB_TK_WINDOWS)
#define Hatches_type static char
#endif

Hatches_type         p1_bits[] = {
   0xaa, 0xaa, 0x55, 0x55, 0xaa, 0xaa, 0x55, 0x55, 0xaa, 0xaa, 0x55, 0x55,
   0xaa, 0xaa, 0x55, 0x55, 0xaa, 0xaa, 0x55, 0x55, 0xaa, 0xaa, 0x55, 0x55,
   0xaa, 0xaa, 0x55, 0x55, 0xaa, 0xaa, 0x55, 0x55};
Hatches_type         p2_bits[] = {
   0x44, 0x44, 0x11, 0x11, 0x44, 0x44, 0x11, 0x11, 0x44, 0x44, 0x11, 0x11,
   0x44, 0x44, 0x11, 0x11, 0x44, 0x44, 0x11, 0x11, 0x44, 0x44, 0x11, 0x11,
   0x44, 0x44, 0x11, 0x11, 0x44, 0x44, 0x11, 0x11};
Hatches_type         p3_bits[] = {
   0x00, 0x00, 0x44, 0x44, 0x00, 0x00, 0x11, 0x11, 0x00, 0x00, 0x44, 0x44,
   0x00, 0x00, 0x11, 0x11, 0x00, 0x00, 0x44, 0x44, 0x00, 0x00, 0x11, 0x11,
   0x00, 0x00, 0x44, 0x44, 0x00, 0x00, 0x11, 0x11};
Hatches_type         p4_bits[] = {
   0x80, 0x80, 0x40, 0x40, 0x20, 0x20, 0x10, 0x10, 0x08, 0x08, 0x04, 0x04,
   0x02, 0x02, 0x01, 0x01, 0x80, 0x80, 0x40, 0x40, 0x20, 0x20, 0x10, 0x10,
   0x08, 0x08, 0x04, 0x04, 0x02, 0x02, 0x01, 0x01};
Hatches_type         p5_bits[] = {
   0x20, 0x20, 0x40, 0x40, 0x80, 0x80, 0x01, 0x01, 0x02, 0x02, 0x04, 0x04,
   0x08, 0x08, 0x10, 0x10, 0x20, 0x20, 0x40, 0x40, 0x80, 0x80, 0x01, 0x01,
   0x02, 0x02, 0x04, 0x04, 0x08, 0x08, 0x10, 0x10};
Hatches_type         p6_bits[] = {
   0x44, 0x44, 0x44, 0x44, 0x44, 0x44, 0x44, 0x44, 0x44, 0x44, 0x44, 0x44,
   0x44, 0x44, 0x44, 0x44, 0x44, 0x44, 0x44, 0x44, 0x44, 0x44, 0x44, 0x44,
   0x44, 0x44, 0x44, 0x44, 0x44, 0x44, 0x44, 0x44};
Hatches_type         p7_bits[] = {
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xff, 0xff, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0xff, 0xff, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xff, 0xff,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xff, 0xff};
Hatches_type         p8_bits[] = {
   0x11, 0x11, 0xb8, 0xb8, 0x7c, 0x7c, 0x3a, 0x3a, 0x11, 0x11, 0xa3, 0xa3,
   0xc7, 0xc7, 0x8b, 0x8b, 0x11, 0x11, 0xb8, 0xb8, 0x7c, 0x7c, 0x3a, 0x3a,
   0x11, 0x11, 0xa3, 0xa3, 0xc7, 0xc7, 0x8b, 0x8b};
Hatches_type         p9_bits[] = {
   0x10, 0x10, 0x10, 0x10, 0x28, 0x28, 0xc7, 0xc7, 0x01, 0x01, 0x01, 0x01,
   0x82, 0x82, 0x7c, 0x7c, 0x10, 0x10, 0x10, 0x10, 0x28, 0x28, 0xc7, 0xc7,
   0x01, 0x01, 0x01, 0x01, 0x82, 0x82, 0x7c, 0x7c};
Hatches_type         p10_bits[] = {
   0x10, 0x10, 0x10, 0x10, 0x10, 0x10, 0xff, 0xff, 0x01, 0x01, 0x01, 0x01,
   0x01, 0x01, 0xff, 0xff, 0x10, 0x10, 0x10, 0x10, 0x10, 0x10, 0xff, 0xff,
   0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0xff, 0xff};
Hatches_type         p11_bits[] = {
   0x08, 0x08, 0x49, 0x49, 0x2a, 0x2a, 0x1c, 0x1c, 0x2a, 0x2a, 0x49, 0x49,
   0x08, 0x08, 0x00, 0x00, 0x80, 0x80, 0x94, 0x94, 0xa2, 0xa2, 0xc1, 0xc1,
   0xa2, 0xa2, 0x94, 0x94, 0x80, 0x80, 0x00, 0x00};
Hatches_type         p12_bits[] = {
   0x1c, 0x1c, 0x22, 0x22, 0x41, 0x41, 0x41, 0x41, 0x41, 0x41, 0x22, 0x22,
   0x1c, 0x1c, 0x00, 0x00, 0xc1, 0xc1, 0x22, 0x22, 0x14, 0x14, 0x14, 0x14,
   0x14, 0x14, 0x22, 0x22, 0xc1, 0xc1, 0x00, 0x00};
Hatches_type         p13_bits[] = {
   0x01, 0x01, 0x82, 0x82, 0x44, 0x44, 0x28, 0x28, 0x10, 0x10, 0x28, 0x28,
   0x44, 0x44, 0x82, 0x82, 0x01, 0x01, 0x82, 0x82, 0x44, 0x44, 0x28, 0x28,
   0x10, 0x10, 0x28, 0x28, 0x44, 0x44, 0x82, 0x82};
Hatches_type         p14_bits[] = {
   0xff, 0xff, 0x11, 0x10, 0x11, 0x10, 0x11, 0x10, 0xf1, 0x1f, 0x11, 0x11,
   0x11, 0x11, 0x11, 0x11, 0xff, 0x11, 0x01, 0x11, 0x01, 0x11, 0x01, 0x11,
   0xff, 0xff, 0x01, 0x10, 0x01, 0x10, 0x01, 0x10};
Hatches_type         p15_bits[] = {
   0x22, 0x22, 0x55, 0x55, 0x22, 0x22, 0x00, 0x00, 0x88, 0x88, 0x55, 0x55,
   0x88, 0x88, 0x00, 0x00, 0x22, 0x22, 0x55, 0x55, 0x22, 0x22, 0x00, 0x00,
   0x88, 0x88, 0x55, 0x55, 0x88, 0x88, 0x00, 0x00};
Hatches_type         p16_bits[] = {
   0x0e, 0x0e, 0x11, 0x11, 0xe0, 0xe0, 0x00, 0x00, 0x0e, 0x0e, 0x11, 0x11,
   0xe0, 0xe0, 0x00, 0x00, 0x0e, 0x0e, 0x11, 0x11, 0xe0, 0xe0, 0x00, 0x00,
   0x0e, 0x0e, 0x11, 0x11, 0xe0, 0xe0, 0x00, 0x00};
Hatches_type         p17_bits[] = {
   0x44, 0x44, 0x22, 0x22, 0x11, 0x11, 0x00, 0x00, 0x44, 0x44, 0x22, 0x22,
   0x11, 0x11, 0x00, 0x00, 0x44, 0x44, 0x22, 0x22, 0x11, 0x11, 0x00, 0x00,
   0x44, 0x44, 0x22, 0x22, 0x11, 0x11, 0x00, 0x00};
Hatches_type         p18_bits[] = {
   0x11, 0x11, 0x22, 0x22, 0x44, 0x44, 0x00, 0x00, 0x11, 0x11, 0x22, 0x22,
   0x44, 0x44, 0x00, 0x00, 0x11, 0x11, 0x22, 0x22, 0x44, 0x44, 0x00, 0x00,
   0x11, 0x11, 0x22, 0x22, 0x44, 0x44, 0x00, 0x00};
Hatches_type         p19_bits[] = {
   0xe0, 0x03, 0x98, 0x0c, 0x84, 0x10, 0x42, 0x21, 0x42, 0x21, 0x21, 0x42,
   0x19, 0x4c, 0x07, 0xf0, 0x19, 0x4c, 0x21, 0x42, 0x42, 0x21, 0x42, 0x21,
   0x84, 0x10, 0x98, 0x0c, 0xe0, 0x03, 0x80, 0x00};
Hatches_type         p20_bits[] = {
   0x22, 0x22, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x22, 0x22, 0x44, 0x44,
   0x44, 0x44, 0x44, 0x44, 0x22, 0x22, 0x11, 0x11, 0x11, 0x11, 0x11, 0x11,
   0x22, 0x22, 0x44, 0x44, 0x44, 0x44, 0x44, 0x44};
Hatches_type         p21_bits[] = {
   0xf1, 0xf1, 0x10, 0x10, 0x10, 0x10, 0x10, 0x10, 0x1f, 0x1f, 0x01, 0x01,
   0x01, 0x01, 0x01, 0x01, 0xf1, 0xf1, 0x10, 0x10, 0x10, 0x10, 0x10, 0x10,
   0x1f, 0x1f, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01};
Hatches_type         p22_bits[] = {
   0x8f, 0x8f, 0x08, 0x08, 0x08, 0x08, 0x08, 0x08, 0xf8, 0xf8, 0x80, 0x80,
   0x80, 0x80, 0x80, 0x80, 0x8f, 0x8f, 0x08, 0x08, 0x08, 0x08, 0x08, 0x08,
   0xf8, 0xf8, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80};
Hatches_type         p23_bits[] = {
   0xAA, 0xAA, 0x55, 0x55, 0x6a, 0x6a, 0x74, 0x74, 0x78, 0x78, 0x74, 0x74,
   0x6a, 0x6a, 0x55, 0x55, 0xAA, 0xAA, 0x55, 0x55, 0x6a, 0x6a, 0x74, 0x74,
   0x78, 0x78, 0x74, 0x74, 0x6a, 0x6a, 0x55, 0x55};
Hatches_type         p24_bits[] = {
   0x80, 0x00, 0xc0, 0x00, 0xea, 0xa8, 0xd5, 0x54, 0xea, 0xa8, 0xd5, 0x54,
   0xeb, 0xe8, 0xd5, 0xd4, 0xe8, 0xe8, 0xd4, 0xd4, 0xa8, 0xe8, 0x54, 0xd5,
   0xa8, 0xea, 0x54, 0xd5, 0xfc, 0xff, 0xfe, 0xff};
Hatches_type         p25_bits[] = {
   0x80, 0x00, 0xc0, 0x00, 0xe0, 0x00, 0xf0, 0x00, 0xff, 0xf0, 0xff, 0xf0,
   0xfb, 0xf0, 0xf9, 0xf0, 0xf8, 0xf0, 0xf8, 0x70, 0xf8, 0x30, 0xff, 0xf0,
   0xff, 0xf8, 0xff, 0xfc, 0xff, 0xfe, 0xff, 0xff};


#define MAXMK 100
static struct {
  int type;
  int n;
  XPoint xy[MAXMK];
} marker;                        /* point list to draw marker                 */

#if defined(CERNLIB_GIF)
XImage * ImgScaling(Display *, XImage *, int, int);
#endif

/*
 *    Set input on or off
 */
void
  setinput(inp)
int inp;
{
  XSetWindowAttributes attributes;
  unsigned long attr_mask = 0;

  if( inp == 1 ) {
    attributes.event_mask = mouse_mask | keybd_mask;
    attr_mask |= CWEventMask;
    XChangeWindowAttributes ( display, cws->window, attr_mask, &attributes);
  }
  else {
     attr_mask = 0;
     attributes.event_mask = NoEventMask;
     attr_mask |= CWEventMask;
     XChangeWindowAttributes ( display, cws->window, attr_mask, &attributes);
  }
}


/*
 * set the foreground color in GC
 */
void
  setcolor( gc, ci )
GC gc;
int ci;
{
  if( colored && ( ci < 0 || ci >= MAXCOL || !colors[ci].defined ) )
    {
     ci = 0;
    }
  else if (!colored && ci < 0 )
    {
     ci = 0;
    }
  else if (!colored && ci > 0 )
    {
     ci = 1;
    }

  XSetForeground( display, gc, colors[ci].pixel );

  /* make sure that foreground and background are different */
#if (!defined(CERNLIB_TK_WINDOWS))
  if ( XGetGCValues( display, gc, gc_mask, &gc_return) ) {
     if ( gc_return.foreground == gc_return.background )
        XSetBackground( display, gc, colors[!ci].pixel );
  } else {
     printf("**** Error: Cannot get GC values \n");
  }
#endif
}


/*
 * return null terminated and blank trimmed string
 */
char*
  string( s, l )
char *s;
int l;
{
  static char str[256];
  int i;

  if( l >= sizeof( str ) )
    l = sizeof( str ) - 1;

  for( i = 0; i < l; i++ )
    str[i] = s[i];
  while( l > 0 && str[l-1] == ' ' )
    l--;

  str[l] = '\0';
  return( str );
}

#if defined(CERNLIB_TK_WINDOWS)
void ixsgc(gc)
   GC gc;
{
   gclist[0] = *gcline = gc;
   gclist[1] = *gcmark = gc;
   gclist[2] = *gcfill = gc;
   gclist[3] = *gctext = gc;
   gclist[4] = *gcinvt = gc;
   gclist[5] = *gcdash = gc;
   gclist[6] = *gcpxmp = gc;
}

void ixdsdb()
{
     XCopyArea( display, cws->drawing, cws->window,
                *gcpxmp, 0, 0, cws->width, cws->height, 0, 0 );
}
#endif

#if defined(CERNLIB_IBM)
int isascii(c)
int c;
{
return(isprint(c)||iscntrl(c));
}
#endif


/*
 *    Allows to set the DISPLAY and the WINDOW address
 *    from outside HIGZ (ie a MOTIF program).
 */
void
  ixsdswi( dsp , win )
  Display *dsp;
  Window win;
{
   external_int = 1;
   external_window = win;
   display = dsp;
}

/*
 *    Allows KUIP to give to HIGZ the adress of three routines:
 *    1) motif_open  : to open a KUIP motif window
 *    2) motif_init  : to initalize a KUIP motif window (add the callbacks)
 *    3) motif_close : to close a KUIP motif window
 */
void
  ixmotif( dsp , open, init, close)
  Display *dsp;
  Window (*open)();
  void (*init)();
  void (*close)();
{
   display = dsp;
   motif_open  = open;
   motif_init  = init;
   motif_close = close;
}


/*
 *    SUBROUTINE IXMINIT(WID)
 *    INTEGER WID : Window ID
 *
 *    Initialize the Motif windows (add the callbacks)
 */
void
  ixminit(wid)
  int *wid;
{
  if (motif_init != NULL) (*motif_init) (*wid+1);
}


/*
 *    SUBROUTINE IXSETHN(LENHST,HOST)
 *    INTEGER LENHST      : host name length
 *    CHARACTER*(*) HOST  : host name
 *
 *    Set host name (fill global variable hname)
 */
void
  ixsethn( lenhst,
#if defined(CERNLIB_CRAY)
          fcd_host )
_fcd fcd_host;
#endif
#if defined(CERNLIB_VAX)
          dsc_host )
struct dsc$descriptor_s *dsc_host;
#endif
#if defined(CERNLIB_TK_WINDOWS)
          host,lenhost )
char *host;
int lenhost;
#endif
#if (!defined(CERNLIB_CRAY))&&(!defined(CERNLIB_VAX))&&(!defined(CERNLIB_TK_WINDOWS))
          host )
char *host;
#endif
int *lenhst;
{
#if defined(CERNLIB_CRAY)
  char *host = _fcdtocp( fcd_host );
#endif
#if defined(CERNLIB_VAX)
  char *host = dsc_host->dsc$a_pointer;
#endif

strcpy (hname,string( host, *lenhst ));
}


/*
 *    INTEGER FUNCTION IXOPNDS(LENHST,HOST)
 *    INTEGER LENHST      : host name length
 *    CHARACTER*(*) HOST  : host name
 *
 *    Open the display. Return -1 if the opening fails
 */
int
  ixopnds( lenhst,
#if defined(CERNLIB_CRAY)
          fcd_host )
_fcd fcd_host;
#endif
#if defined(CERNLIB_VAX)
          dsc_host )
struct dsc$descriptor_s *dsc_host;
#endif
#if defined(CERNLIB_TK_WINDOWS)
          host, lenhost )
char *host;
int lenhost;
#endif
#if (!defined(CERNLIB_CRAY))&&(!defined(CERNLIB_VAX))&&(!defined(CERNLIB_TK_WINDOWS))
          host )
char *host;
#endif
int *lenhst;
{
#if defined(CERNLIB_CRAY)
  char *host = _fcdtocp( fcd_host );
#endif
#if defined(CERNLIB_VAX)
  char *host = dsc_host->dsc$a_pointer;
#endif
  Pixmap pixmp1,pixmp2;
  XColor fore,back;
  char **fontlist;
  int fontcount;
  int i;
  extern char *getenv();

/*
 *              Try to open the DISPLAY
 */
  if ( !isdisp ) {
     if( display == NULL ) {
#if !defined(CERNLIB_VAX)
        if( *lenhst == 0 ) {
           display = XOpenDisplay( getenv( "DISPLAY" ) );
        } else {
           display = XOpenDisplay( string( host, *lenhst ));
        }
#endif
#if defined(CERNLIB_VAX)
        display = XOpenDisplay( string( host, *lenhst ));
#endif
        if( display == NULL ) return( -1 );
     }
  } else {
     return( 0 );
  }

  screen_number = DefaultScreen( display );
  screen        = ScreenOfDisplay( display, screen_number);
  colored       = DisplayPlanes( display, screen_number ) > 1;

  if (colored) colormap = DefaultColormap( display, screen_number);

  /* colors[1].defined = 1; default foreground */
  colors[1].pixel = BlackPixel( display, screen_number );
  /* colors[0].defined = 1; default background */
  colors[0].pixel = WhitePixel( display, screen_number );

/*
 *              Inquire the the XServer Vendor
 */
#if !defined(CERNLIB_TK_WINDOWS)
  strcpy (vendor,XServerVendor(display));
#endif
#if defined(CERNLIB_TK_WINDOWS)
  strcpy(vendor,"Microsoft");
#endif

/*
 *              Create primitives graphic contexts
 */
  for ( i = 0; i < MAXGC; i++ )
     gclist[i] = XCreateGC( display, RootWindow( display, screen_number ),
                             0, NULL );
#if (!defined(CERNLIB_TK_WINDOWS))
  if ( XGetGCValues( display, *gctext, gc_mask, &gc_return) ) {
     XSetForeground( display, *gcinvt, gc_return.background );
     XSetBackground( display, *gcinvt, gc_return.foreground );
  } else {
     printf("**** Error: Cannot get GC values \n");
  }
#endif

/*
 *              Create input echo graphic context
 */
  gcechov.foreground = BlackPixel( display, screen_number );
  gcechov.background = WhitePixel( display, screen_number );
  if(strstr(vendor,"Hewlett")) {
     gcechov.function   = GXxor;
  } else {
     gcechov.function   = GXinvert;
  }
  gcecho = XCreateGC( display, RootWindow( display, screen_number ),
                      GCForeground | GCBackground | GCFunction,
                      &gcechov );
/*
 *              Load a default Font
 */
#if !defined(CERNLIB_TK_WINDOWS)
  for ( i = 0; i < MAXFONT; i++ ) {
     font[i].id = NULL;
     strcpy( font[i].name, " " );
  }
  fontlist = XListFonts( display, "*courier*", 1, &fontcount );
  if ( fontcount != 0 ) {
     font[current_font_number].id = XLoadQueryFont( display, fontlist[0] );
     text_font = font[current_font_number].id;
     strcpy( font[current_font_number].name, "*courier*" );
     current_font_number++;
     XFreeFontNames(fontlist);
  } else {
     printf("No default font loaded \n");
  }
#endif
/*
 *              Create a null cursor
 */
  pixmp1 = XCreateBitmapFromData(display,
                                 RootWindow( display, screen_number ),
                                 null_cursor_bits, 16, 16);
  pixmp2 = XCreateBitmapFromData(display,
                                 RootWindow( display, screen_number ),
                                 null_cursor_bits, 16, 16);
  null_cursor = XCreatePixmapCursor(display,pixmp1,pixmp2,&fore,&back,0,0);

  isdisp = 1;
  return( 0 );
}


/*
 *    INTEGER FUNCTION IXOPNWI(X,Y,W,H,LENTIT,TITLE,FLAG)
 *    INTEGER X,Y         : initial window position
 *    INTEGER W,H         : initial window width and height
 *    INTEGER LENTIT      : title length
 *    CHARACTER*(*) TITLE : window title
 *    INTEGER FLAG        : FLAG<>1 allows to open a non MOTIF window
 *                          even if ixmotif has been called.
 *
 *    Open window and returns window number which can be used with IXSELWIN
 *    return -1 if window creation fails
 */
int
  ixopnwi( x, y, w, h, lentit,
#if defined(CERNLIB_CRAY)
          fcd_title, flag )
_fcd fcd_title;
#endif
#if defined(CERNLIB_VAX)
          dsc_title, flag )
struct dsc$descriptor_s *dsc_title;
#endif
#if defined(CERNLIB_TK_WINDOWS)
          title, lentitle, flag )
char *title;
int lentitle;
#endif
#if (!defined(CERNLIB_CRAY))&&(!defined(CERNLIB_VAX))&&(!defined(CERNLIB_TK_WINDOWS))
          title, flag )
char *title;
#endif
int *x, *y;
int *w, *h;
int *lentit;
int *flag;
{
#if defined(CERNLIB_CRAY)
  char *title = _fcdtocp( fcd_title );
#endif
#if defined(CERNLIB_VAX)
  char *title = dsc_title->dsc$a_pointer;
#endif
  XSetWindowAttributes attributes;
  unsigned long attr_mask = 0;
  char long_title[256];
  char host_name[64];
  XWMHints wm_hints;
  XSizeHints size_hints;
  Atom atm[1];
  Status istat;
  int wid;
  int xval, yval;
  unsigned int wval, hval, border, depth;
  Window root;

  if (external_int == 1) {
     XGetGeometry( display, external_window, &root,
                   &xval, &yval, &wval, &hval, &border, &depth );
     xval = 0;
     yval = 0;
  }
  else {
     xval = *x;
     yval = *y;
     wval = *w;
     hval = *h;
  }

/*
 *              Select next free window number
 */
      for ( wid = 0; wid < MAXWN; wid++ )
         if ( !windows[wid].open ) {
            windows[wid].open = 1;
            cws = &windows[wid];
            break;
         }
/*
 *              Create a Motif window
 */
      if (motif_open != NULL && *flag == 0) {
          cws->motif_window = (*motif_open) (wid+1, xval, yval, wval, hval);
          if ( !cws->motif_window ) return( -1 );
          XGetGeometry( display, cws->motif_window, &root,
                        &xval, &yval, &wval, &hval, &border, &depth );
      }

      if ( wid == MAXWN ) return( -1 );
/*
 *              Create window
 */
      attributes.background_pixel = colors[0].pixel;
      attr_mask |= CWBackPixel;
      attributes.border_pixel = colors[1].pixel;
      attr_mask |= CWBorderPixel;
      attributes.event_mask = NoEventMask;
      attr_mask |= CWEventMask;
      attributes.backing_store = Always;
      attr_mask |= CWBackingStore;
      if ( colored ) {
         attributes.colormap = colormap;
         attr_mask |= CWColormap;
      }
      if (motif_open != NULL && *flag == 0) {
         cws->window = XCreateWindow(display, cws->motif_window,
                       xval, yval, wval, hval, 0, CopyFromParent,
                       InputOutput, CopyFromParent,
                       attr_mask, &attributes );
         cws->motif = 1;
      } else {
         if (external_int == 1) {
#if !defined(CERNLIB_TK_WINDOWS)
            cws->window = XCreateWindow(display, external_window,
                          xval, yval, wval, hval, 0, CopyFromParent,
                          InputOutput, CopyFromParent,
                          attr_mask, &attributes );
#endif
#if defined(CERNLIB_TK_WINDOWS)
            cws->window = external_window;
#endif
            external_int = 0;
            cws->motif = 1;
            cws->motif_window = external_window;
         } else {
            cws->motif = 0;
            cws->window = XCreateWindow(display,
                          RootWindow( display, screen_number),
                          xval, yval, wval, hval, 1, CopyFromParent,
                          InputOutput, CopyFromParent,
                          attr_mask, &attributes );
            strcpy( long_title, string( title, *lentit ) );
            if (long_title[0] != '-') {
               strcat( long_title, " @ " );
#if !defined(CERNLIB_TK_WINDOWS)
               gethostname( host_name, sizeof( host_name ) );
#endif
#if defined(CERNLIB_TK_WINDOWS)
               strcat( long_title, " Win95 ");
#endif
               strcat( long_title, host_name );
            } else {
               long_title[0] = ' ';
            }
#if !defined(CERNLIB_TK_WINDOWS)
            XStoreName( display, cws->window, long_title );
#endif
            XSetIconName( display, cws->window, long_title );
         }
      }
/*
 *              Set window manager hints
 */
#if !defined(CERNLIB_TK_WINDOWS)
      memset( &wm_hints, 0, sizeof(wm_hints));
      wm_hints.flags = InputHint;
      wm_hints.input = False;  /* don't grab input focus */
      XSetWMHints( display, cws->window, &wm_hints );
#endif
/*
 *              Set window size hints
 */
#if !defined(CERNLIB_TK_WINDOWS)
      memset( &size_hints, 0, sizeof(size_hints));
      size_hints.flags = USPosition | USSize;
      size_hints.x = xval;
      size_hints.y = yval;
      size_hints.width = wval;
      size_hints.height = hval;
      XSetNormalHints( display, cws->window, &size_hints );
#endif
/*
 *              Define the position and size of the window
 *              Map the window
 */
#if !defined(CERNLIB_TK_WINDOWS)
      XMoveWindow( display, cws->window, xval, yval );
      XResizeWindow( display, cws->window, wval, hval );
      XMapWindow( display, cws->window );
/*
 *              Wait until the window is mapped (MapNotify)
 * This is not supported by all the X11 emulators (problem on Mac !!!)
 *
      XSelectInput(display, cws->window, StructureNotifyMask);
      forever {
         XEvent event;
         XNextEvent( display, &event);
         if (event.type == MapNotify) break;
      }
 */
      XFlush( display );
#endif
/*
 *              Set the Class Hint
 */
#if !defined(CERNLIB_TK_WINDOWS)
      XSetClassHint(display, cws->window, &class_hints);
#endif
/*
 *              Set Window Manager Protocols
 */
#if !defined(CERNLIB_TK_WINDOWS)
      atm[0] = XInternAtom(display, "WM_DELETE_WINDOW", False);
      istat = XSetWMProtocols(display, cws->window, atm, 1);
#endif
/*
 *              Initialise the window structure
 */
      cws->drawing        = cws->window;
      cws->buffer         = (Drawable)NULL;
      cws->double_buffer  = (int)NULL;
      cws->clip           = (int)NULL;
      cws->width          = wval;
      cws->height         = hval;

      return( wid );
}

/*
 *    SUBROUTINE IXRSCWI(WID, W, H)
 *    INTEGER WID  : Window identifier.
 *    INTEGER W    : Width
 *    INTEGER H    : Heigth
 *
 *    Rescale the window WID
 */
void
   ixrscwi(wid, w, h)
  int *wid;
  int *w;
  int *h;
{
  int i;

  tws = &windows[*wid];
  if (!tws->open) return;

  XResizeWindow( display, tws->window, *w, *h );

  if( tws->double_buffer ) {
     XFreePixmap(display,tws->buffer);
     tws->buffer = XCreatePixmap(display, RootWindow( display, screen_number),
                   *w, *h, DefaultDepth(display,screen_number));
     for( i = 0; i < MAXGC; i++ ) XSetClipMask( display, gclist[i], None );
     setcolor( *gcpxmp, 0);
     XFillRectangle( display, tws->buffer, *gcpxmp, 0, 0, *w, *h);
     setcolor( *gcpxmp, 1);
     tws->drawing = tws->buffer;
  }
  tws->width  = *w;
  tws->height = *h;
}

/*
 *    SUBROUTINE IXMOVWI(WID, X, Y)
 *    INTEGER WID  : Window identifier.
 *    INTEGER X    : Width
 *    INTEGER Y    : Heigth
 *
 *    Move the window WID
 */
void
   ixmovwi(wid, x, y)
  int *wid;
  int *x;
  int *y;
{
  int i;

  tws = &windows[*wid];
  if (!tws->open) return;

  XMoveWindow( display, tws->window, *x, *y );
}

void
  ix2bon()
{
  int i;

  if (tws->double_buffer) return;
  tws->buffer = XCreatePixmap(display, RootWindow( display, screen_number),
                tws->width, tws->height, DefaultDepth(display,screen_number));
  for( i = 0; i < MAXGC; i++ ) XSetClipMask( display, gclist[i], None );
  XCopyArea( display, tws->window, tws->buffer,
             *gcpxmp, 0, 0, tws->width, tws->height, 0, 0 );
  tws->double_buffer  = 1;
  tws->drawing        = tws->buffer;
  return;
}

void
  ix2boff()
{
  if (!tws->double_buffer) return;
  XFreePixmap(display,tws->buffer);
  tws->double_buffer = 0;
  tws->drawing       = tws->window;
  return;
}

/*
 *    SUBROUTINE IXS2BUF(WID,MODE)
 *    INTEGER WID  : Window identifier.
 *                   999 means all the opened windows.
 *    INTEGER MODE : 1 double buffer is on
 *                   0 double buffer is off
 *
 *    Set the double buffer on/off on window WID.
 */
void
  ixs2buf( wid, mode )
  int *wid;
  int *mode;
{
  int i;

  if ( *wid == 999 ) {
    for ( i = 0; i < MAXWN; i++ ) {
      tws = &windows[i];
      if (tws->open) {
        switch ( *mode ) {
        case 1 :
           ix2bon();
           break;
        default:
           ix2boff();
           break;
        }
      }
    }
  } else {
    tws = &windows[*wid];
    if (!tws->open) return;
    switch ( *mode ) {
    case 1 :
       ix2bon();
       return;
    default:
       ix2boff();
       return;
    }
  }
}

/*
 *    SUBROUTINE IXQ2BUF(WID)
 *    INTEGER WID  : Window identifier.
 *
 *    Query the double buffer value for the window WID.
 */
int
  ixq2buf( wid )
  int *wid;
{
  tws = &windows[*wid];
  if (!tws->open) {
    return -1;
  } else {
    return tws->double_buffer;
  }
}

/*
 *    INTEGER FUNCTION IXOPNPX(W, H)
 *    INTEGER W,H : Width and height of the pixmap.
 *
 *    Open a new pixmap.
 */
int
  ixopnpx(w, h)
int *w, *h;
{
  Drawable pixtemp;
  Window root;
  unsigned int wval, hval;
  int xx, yy, i ;
  unsigned int ww, hh, border, depth;
  wval = *w;
  hval = *h;

  oldwin = cws->drawing;
  pixtemp = XCreatePixmap(display, RootWindow( display, screen_number),
            wval, hval, DefaultDepth(display,screen_number));
  ispix = 1;
  XGetGeometry( display, pixtemp, &root, &xx, &yy, &ww, &hh, &border, &depth );

  for( i = 0; i < MAXGC; i++ )
    XSetClipMask( display, gclist[i], None );

  setcolor( *gcpxmp, 0);
  XFillRectangle( display, pixtemp, *gcpxmp,0 ,0 ,ww ,hh);
  setcolor( *gcpxmp, 1);

  cws->drawing = pixtemp;
  return ( pixtemp );
}


/*
 *    SUBROUTINE IXCLRPX(PIX)
 *    INTEGER PIX : Pixmap address
 *
 *    Clear the pixmap PIX.
 */
void
  ixclrpx(pix )
Drawable *pix;
{
  Window root;
  int xx, yy;
  unsigned int ww, hh, border, depth;
  XGetGeometry( display, *pix, &root, &xx, &yy, &ww, &hh, &border, &depth );
  setcolor( *gcpxmp, 0);
  XFillRectangle( display, *pix, *gcpxmp,0 ,0 ,ww ,hh);
  setcolor( *gcpxmp, 1);
  XFlush( display );
}


/*
 *    SUBROUTINE IXCLPX
 *
 *    Close the current opened pixmap.
 */
void
  ixclpx()
{
  XFlush( display );
  cws->drawing = oldwin;
  ispix = 0;
}


/*
 *    SUBROUTINE IXCPPX(PIX, XPOS, YPOS)
 *    INTEGER PIX : Pixmap address
 *    INTEGER XPOS, YPOS : Pixmap Position
 *
 *    Copy the pixmap PIX at the position XPOS YPOS.
 */
void
  ixcppx(pix, xpos, ypos)
Drawable *pix;
int *xpos, *ypos;
{
  Window root;
  int xx, yy;
  unsigned int ww, hh, border, depth;
  XGetGeometry( display, *pix, &root, &xx, &yy, &ww, &hh, &border, &depth );
  XCopyArea(display,*pix,cws->drawing,*gcpxmp,0,0,ww,hh,*xpos,*ypos);
  XFlush( display );
}


/*
 *    SUBROUTINE IXWRPX(PIX,W,H,LENNAME,PXNAME)
 *    INTEGER PIX : Pixmap address
 *    INTEGER W,H : Width and height of the pixmap.
 *    INTEGER LENNAME     : pixmap name length
 *    CHARACTER*(*) PXNAME: pixmap name
 *
 *    Write the pixmap IPX in the bitmap file PXNAME.
 */
void
  ixwrpx(pix, w, h, lenname,
#if defined(CERNLIB_CRAY)
          fcd_pxname )
_fcd fcd_pxname;
#endif
#if defined(CERNLIB_VAX)
          dsc_pxname )
struct dsc$descriptor_s *dsc_pxname;
#endif
#if defined(CERNLIB_TK_WINDOWS)
          pxname, lenpxname )
char *pxname;
int lenpxname;
#endif
#if (!defined(CERNLIB_CRAY))&&(!defined(CERNLIB_VAX))&&(!defined(CERNLIB_TK_WINDOWS))
          pxname )
char *pxname;
#endif
Drawable *pix;
int *w, *h;
int *lenname;
{
#if defined(CERNLIB_CRAY)
  char *pxname = _fcdtocp( fcd_pxname );
#endif
#if defined(CERNLIB_VAX)
  char *pxname = dsc_pxname->dsc$a_pointer;
#endif
  unsigned int wval, hval;
  wval = *w;
  hval = *h;
#if !defined(CERNLIB_TK_WINDOWS)
  XWriteBitmapFile(display,string(pxname,*lenname),*pix,wval,hval,-1,-1);
#endif
}


/*
 *    SUBROUTINE IXWIPX(PIX, XPOS, YPOS)
 *    INTEGER PIX : Pixmap address
 *    INTEGER XPOS, YPOS : Position in the current window
 *
 *    Copy the area at the position XPOS YPOS in the current
 *    window in the pixmap PIX. The area copied has the size
 *    of the pixmap PIX.
 */
void
  ixwipx(pix, xpos, ypos )
Drawable *pix;
int *xpos, *ypos;
{
  Window root;
  int xx, yy;
  unsigned int ww, hh, border, depth;
  XGetGeometry( display, *pix, &root, &xx, &yy, &ww, &hh, &border, &depth );
  XCopyArea(display,cws->drawing,*pix,*gcpxmp,*xpos,*ypos,ww,hh,0,0);
  XFlush( display );
}


/*
 *    SUBROUTINE IXRMPX(PIX)
 *    INTEGER PIX : Pixmap adress
 *
 *    Remove the pixmap PIX.
 */
void
  ixrmpx(pix)
Drawable *pix;
{
  XFreePixmap(display,*pix);
}


/*
 *    SUBROUTINE IXCLSDS
 *
 *    Delete all windows and close connection
 */
void
  ixclsds()
{
  int Motif = (int)NULL;
  int i;

  if (display == NULL) return;

  for ( i = 0; i < MAXFONT; i++ ) {
    if ( font[i].id ) {
       XFreeFont ( display, font[i].id);
       font[i].id = NULL;
    }
  }

  for ( i = 0; i < MAXWN; i++ ) {
    windows[i].open = 0;
    if ( windows[i].motif ) Motif = 1;
  }

  for ( i = 0; i < MAXCOL; i++ ) {
    if( colors[i].defined == 1 ) {
      colors[i].defined = 0;
      XFreeColors(display, colormap, &colors[i].pixel, 1, (int)NULL);
    }
  }

#if !defined(CERNLIB_TK_WINDOWS)
  if(display != NULL && Motif == (int)NULL) XCloseDisplay( display );
#endif
  display = NULL;
  cws     = NULL;
  tws     = NULL;
  isdisp  = 0;
}


/*
 *    SUBROUTINE IXCLSWI
 *
 *    Delete current window
 */
void
  ixclswi()
{
  int wid;

  XDestroyWindow( display, cws->window );

  if( cws->double_buffer ) XFreePixmap( display, cws->buffer);

  if (cws->motif && motif_close != NULL) {
      for( wid = 0; wid < MAXWN; wid++ ) {
           if (cws->motif_window == windows[wid].motif_window) {
               (*motif_close) (wid+1);
               break; }
      }
  }

  XFlush( display );

  cws->open = (int)NULL;

  for( wid = 0; wid < MAXWN; wid++ )
    if( windows[wid].open ) {
      cws = &windows[wid];
      return;
    }

#if !defined(CERNLIB_TK_WINDOWS)
  ixclsds(); /* no open window left */
#endif
#if defined(CERNLIB_TK_WINDOWS)
  IXCLSDS(); /* no open window left */
#endif
}


/*
 *    SUBROUTINE IXSELWI(WID)
 *    INTEGER WID : window number returned by IXOPNWI
 *
 *    Select window to which subsequent output is directed
 */
void
  ixselwi( wid )
int *wid;
{
  XRectangle region;
  int i;

  if( *wid < 0 || *wid >= MAXWN || !windows[*wid].open ) return;

  cws = &windows[*wid];

 if ( cws->clip && !ispix && !cws->double_buffer ) {
    region.x      = cws->xclip;
    region.y      = cws->yclip;
    region.width  = cws->wclip;
    region.height = cws->hclip;
#if !defined(CERNLIB_TK_WINDOWS)
    for( i = 0; i < MAXGC; i++ )
      XSetClipRectangles( display, gclist[i], 0, 0, &region, 1, YXBanded );
#endif
  } else {
    for( i = 0; i < MAXGC; i++ )
      XSetClipMask( display, gclist[i], None );
  }
}


/*
 *    SUBROUTINE IXSAVWI
 *
 *    Save the adress of the current active window
 */
void
  ixsavwi()
{
  sws = cws;
}


/*
 *    SUBROUTINE IXSETWI
 *
 *    Restore the adress of the saved window
 */
void
  ixsetwi()
{
  cws = sws;
}


/*
 *    SUBROUTINE IXGETGE(WID,X,Y,W,H)
 *    INTEGER WID : window identifier
 *    INTEGER X,Y : window position (output)
 *    INTEGER W,H : window size (output)
 *
 *    Return position and size of window wid
 *    if wid < 0 the size of the display is returned
 */
void
   ixgetge( wid, x, y, w, h )
int *wid;
int *x, *y;
unsigned int *w, *h;
{
  Window temp_win;
  if( *wid < 0 ) {
    *x = 0;
    *y = 0;
    *w = DisplayWidth(display,screen_number);
    *h = DisplayHeight(display,screen_number);
  } else {
    Window root;
    unsigned int border, depth;
    unsigned int width, height;

    tws = &windows[*wid];
#if defined(CERNLIB_TK_WINDOWS)
    XGetGeometry(display, tws->window, &root, x, y,
                 &width, &height, &border, &depth);
#endif
#if !defined(CERNLIB_TK_WINDOWS)
    XGetGeometry(display, tws->drawing, &root, x, y,
                 &width, &height, &border, &depth);
    (void) XTranslateCoordinates (display, tws->window,
                                 RootWindow( display, screen_number),
                                   0, 0, x, y, &temp_win);
#endif
    if( width > 0 && height > 0 ) {
      tws->width  = width;
      tws->height = height;
    }
    if( width > 1 && height > 1 ) {
      *w=tws->width-1;
      *h=tws->height-1;
    } else {
      *w=1;
      *h=1;
    }
  }
}


/*
 *    SUBROUTINE IXGETPL(NPLANES)
 *    INTEGER NPLANES     : number of bit planes
 *
 *    Get maximum number of planes
 */
void
   ixgetpl(nplanes)
int *nplanes;
{
   *nplanes = DisplayPlanes( display, screen_number );
}


/*
 *    SUBROUTINE IXRSIZ(WID)
 *    INTEGER WID     : window to be resized
 *
 *    Resize the current window if necessary
 */
void
   ixrsiz(wid)
int *wid;
{
  int i;
  int xval, yval;
  Window root;
  unsigned int wval, hval, border, depth;

  tws = &windows[*wid];

  if( !tws->motif ) return;

  XGetGeometry( display, tws->motif_window, &root,
                &xval, &yval, &wval, &hval, &border, &depth );
  XResizeWindow( display, tws->window, wval, hval );

  if( tws->double_buffer ) {
     XFreePixmap(display,tws->buffer);
     tws->buffer = XCreatePixmap(display, RootWindow( display, screen_number),
                   wval, hval, DefaultDepth(display,screen_number));
     for( i = 0; i < MAXGC; i++ ) XSetClipMask( display, gclist[i], None );
     setcolor( *gcpxmp, 0);
     XFillRectangle( display, tws->buffer, *gcpxmp, 0, 0, wval, hval);
     setcolor( *gcpxmp, 1);
     tws->drawing        = tws->buffer;
  }
  tws->width = wval;
  tws->height = hval;
}


/*
 *    SUBROUTINE IXGETHT(IL,CHHOST)
 *    INTEGER IL           : host name length (output)
 *    CHARACTER*(*) CHHOST : host name (output)
 *
 *    Return the host name given with IGWKTY.
 */
void
   ixgetht (il, chhost)
   int *il;
   char *chhost; /* host name */
{
   *il = strlen (hname);
   strcpy (chhost, hname);
}


/*
 *    SUBROUTINE IXGETWI(WKID,IDG)
 *    INTEGER WKID : Workstation identifier (input)
 *    INTEGER IDG  : Window identifier (output)
 *
 *    Return the X11 window identifier according to the
 *    Window id returned by IXOPNWI.
 */
void
   ixgetwi (wkid, idg)
   int *wkid;
   Window *idg;
{
   *idg = windows[*wkid].window;
}


/*
 *    SUBROUTINE IXCLRWI
 *
 *    Clear current window
 */
void
  ixclrwi()
{

  if ( !ispix && !cws->double_buffer ) {
     XSetWindowBackground( display, cws->drawing, colors[0].pixel );
     XClearWindow( display, cws->drawing );
  } else {
     setcolor( *gcpxmp, 0);
     XFillRectangle( display, cws->drawing, *gcpxmp,
                     0, 0, cws->width, cws->height );
     setcolor( *gcpxmp, 1);
  }
  do_raise = 1;
  XFlush( display );
}


/*
 *    SUBROUTINE IXUPDWI(MODE)
 *    INTEGER MODE : (1) or (11) The window is raised
 *                   (0) or (10) The window is not raised
 *                   (0) or (1)  no synchonisation between client and server
 *                  (10) or (11) synchonisation between client and server
 *
 *    Update display and raise current window to top of stack.
 *    Synchronise client and server once (not permanent).
 *    Copy the pixmap cws->buffer on the window cws-> window
 *    if the double buffer is on.
 */
void
  ixupdwi(mode)
  int *mode;
{
  if ( *mode == 1 || *mode == 11 ) {
     if ( do_raise ) {
        XRaiseWindow( display, cws->window );
        do_raise = 0;
     }
  }
  if ( cws->double_buffer ) {
     XCopyArea( display, cws->drawing, cws->window,
                *gcpxmp, 0, 0, cws->width, cws->height, 0, 0 );
  }
  if ( *mode == 0 || *mode == 1 ) {
    XFlush( display );
  } else {
    XSync( display ,0);
  }
}


/*
 *    SUBROUTINE IXCLIP(WID,X,Y,W,H)
 *    INTEGER WID : Window indentifier
 *    INTEGER X,Y : origin of clipping rectangle
 *    INTEGER W,H : size of clipping rectangle;
 *
 *    Set clipping region for the window wid
 */
void
  ixclip( wid, x, y, w, h )
int *wid;
int *x, *y;
int *w, *h;
{
  XRectangle region;
  int i;

  tws = &windows[*wid];
  tws->xclip = *x;
  tws->yclip = *y;
  tws->wclip = *w;
  tws->hclip = *h;
  tws->clip  = 1;
  if ( cws->clip && !ispix && !cws->double_buffer ) {
    region.x      = cws->xclip;
    region.y      = cws->yclip;
    region.width  = cws->wclip;
    region.height = cws->hclip;
#if !defined(CERNLIB_TK_WINDOWS)
    for( i = 0; i < MAXGC; i++ )
      XSetClipRectangles( display, gclist[i], 0, 0, &region, 1, YXBanded );
#endif
  }
}


/*
 *    SUBROUTINE IXNOCLI(WID)
 *    INTEGER WID : Window indentifier
 *
 *    Switch off the clipping for the window wid
 */
void
  ixnocli(wid)
int *wid;
{
  int i;

  tws       = &windows[*wid];
  tws->clip = (int)NULL;

  for( i = 0; i < MAXGC; i++ )
    XSetClipMask( display, gclist[i], None );
}


/*
 *    SUBROUTINE IXSETCO(CINDEX,R,G,B)
 *    INTEGER CINDEX : color index
 *    REAL R,G,B     : red, green, blue intensities between 0.0 and 1.0
 *
 *    Set color intensities for given color index
 */
#define BIGGEST_RGB_VALUE 65535

void
  ixsetco( cindex, r, g, b )
int *cindex;
float *r, *g, *b;
{
  XColor xcol;

  if( colored && *cindex >= 0 && *cindex < MAXCOL ) {
    xcol.red   = (unsigned short)( *r * BIGGEST_RGB_VALUE );
    xcol.green = (unsigned short)( *g * BIGGEST_RGB_VALUE );
    xcol.blue  = (unsigned short)( *b * BIGGEST_RGB_VALUE );
    xcol.flags = DoRed || DoGreen || DoBlue;
    if( colors[*cindex].defined == 1 ) {
      colors[*cindex].defined = 0;
      XFreeColors(display, colormap, &colors[*cindex].pixel, 1, (int)NULL);
    }
    if( XAllocColor( display, colormap, &xcol ) != (Status)NULL ) {
      colors[*cindex].defined = 1;
      colors[*cindex].pixel   = xcol.pixel;
      colors[*cindex].red     = *r;
      colors[*cindex].green   = *g;
      colors[*cindex].blue    = *b;
    }
  }
}


void
  ixgetcol( index, r, g, b )
int index; /* input */
float *r, *g, *b; /* output */
{
  *r = colors[index].red;
  *g = colors[index].green;
  *b = colors[index].blue;
  return ;
}


/*
 *    SUBROUTINE IXSETLN(WIDTH)
 *    INTEGER WIDTH : line width in pixels
 *
 *    Set line width
 */
void
  ixsetln( width )
int *width;
{
  if( *width == 1) {
     line_width = 0;
  }
  else {
     line_width = *width;
  }
  XSetLineAttributes( display, *gcline, line_width,
              line_style, cap_style, join_style );
  XSetLineAttributes( display, *gcdash, line_width,
              line_style, cap_style, join_style );
}


/*
 *    SUBROUTINE IXSETLS(N,DASH)
 *    INTEGER N       : length of dash list
 *    INTEGER DASH(N) : dash segment lengths
 *
 *    Set line style:
 *    if N.LE.0 use solid lines
 *    if N.GT.0 use dashed lines described by DASH(N)
 *    e.g. N=4,DASH=(6,3,1,3) gives a dashed-dotted line with dash length 6
 *    and a gap of 7 between dashes
 */
void
  ixsetls( n, dash )
int *n;
int *dash;
{
  if( *n <= 0 ) {
    line_style = LineSolid;
    XSetLineAttributes( display, *gcline, line_width,
              line_style, cap_style, join_style );
  }
  else {
    int i, j;
    dash_length = 0;
    for( i = 0, j = 0; i < sizeof( dash_list ); i++ ) {
      dash_list[i] = dash[j];
      dash_length += dash_list[i];
      if( ++j >= *n ) j = 0;
    }
    dash_offset = 0;
    line_style = LineOnOffDash;
    XSetLineAttributes( display, *gcdash, line_width,
              line_style, cap_style, join_style );
  }
}


/*
 *    SUBROUTINE IXSETLC(CINDEX)
 *    INTEGER CINDEX : color index defined my IXSETCO
 *
 *    Set color index for lines
 */
void
  ixsetlc( cindex )
int *cindex;
{
  setcolor( *gcline, *cindex );
  setcolor( *gcdash, *cindex );
}


/*
 *    SUBROUTINE IXLINE(N,XY)
 *    INTEGER N         : number of points
 *    INTEGER*2 XY(2,N) : list of points
 *
 *    Draw a line through all points
 */
void
  ixline( n, xy )
int *n;
XPoint *xy;
{
 if ( *n > 1 )
    {
       if( line_style == LineSolid )
         XDrawLines( display, cws->drawing, *gcline, xy, *n, CoordModeOrigin );
       else {
         int i;
#if !defined(CERNLIB_TK_WINDOWS)
         XSetDashes( display, *gcdash,
             dash_offset, dash_list, sizeof( dash_list ) );
#endif
         XDrawLines( display, cws->drawing, *gcdash, xy, *n, CoordModeOrigin );

         /* calculate length of line to update dash offset */
         for( i = 1; i < *n; i++ ) {
         int dx = xy[i].x - xy[i-1].x;
         int dy = xy[i].y - xy[i-1].y;
         if( dx < 0 ) dx = - dx;
         if( dy < 0 ) dy = - dy;
         dash_offset += dx > dy ? dx : dy;
         }
         dash_offset %= dash_length;
       }
    }
 else
    {
     int px,py;
     px=xy[0].x;
     py=xy[0].y;
#if defined(CERNLIB_TK_WINDOWS)
     XDrawPoints(display, cws->drawing,
        line_style == LineSolid ? *gcline : *gcdash,
        xy, *n, CoordModeOrigin );
#endif
#if !defined(CERNLIB_TK_WINDOWS)
     XDrawPoint(display, cws->drawing,
        line_style == LineSolid ? *gcline : *gcdash, px, py);
#endif
    }
  do_raise = 1;
}


/*
 *    SUBROUTINE IXSETMS(TYPE,N,XY)
 *    INTEGER TYPE      : marker type
 *    INTEGER N         : length of marker description
 *    INTEGER*2 XY(2,N) : list of points describing marker shape
 *
 *    Set marker style:
 *    if N.EQ.0 marker is a single point
 *    if TYPE.EQ.0 marker is hollow circle of diameter N
 *    if TYPE.EQ.1 marker is filled circle of diameter N
 *    if TYPE.EQ.2 marker is a hollow polygon describe by line XY
 *    if TYPE.EQ.3 marker is a filled polygon describe by line XY
 *    if TYPE.EQ.4 marker is described by segmented line XY
 *    e.g. TYPE=4,N=4,XY=(-3,0,3,0,0,-3,0,3) sets a plus shape of 7x7 pixels
 */
void
  ixsetms( type, n, xy )
int *type;
int *n;
XPoint *xy;
{
  int i;

  marker.type = *type;
  marker.n = *n < MAXMK ? *n : MAXMK;
  if( marker.type >= 2 )
    for( i = 0; i < marker.n; i++ )
      marker.xy[i] = xy[i];
}


/*
 *    SUBROUTINE IXSETMC(CINDEX)
 *    INTEGER CINDEX : color index defined my IXSETCO
 *
 *    Set color index for markers
 */
void
  ixsetmc( cindex )
int *cindex;
{
  setcolor( *gcmark, *cindex );
}


/*
 *    SUBROUTINE IXMARKE(N,XY)
 *    INTEGER N         : number of points
 *    INTEGER*2 XY(2,N) : list of points
 *
 *    Draw a marker at each point
 */
void
   ixmarke( n, xy )
int *n;
XPoint *xy;
{
  do_raise = 1;
  if( marker.n <= 0 )
    XDrawPoints( display, cws->drawing, *gcmark, xy, *n, CoordModeOrigin );
  else {
    int r = marker.n / 2;
    int m;

    for( m = 0; m < *n; m++ ) {
      int hollow = 0;

      switch( marker.type ) {
      int i;

      case 0:        /* hollow circle */
      XDrawArc( display, cws->drawing, *gcmark,
          xy[m].x - r, xy[m].y - r, marker.n, marker.n, 0, 360 * 64 );
      break;

      case 1:        /* filled circle */
      XFillArc( display, cws->drawing, *gcmark,
          xy[m].x - r, xy[m].y - r, marker.n, marker.n, 0, 360 * 64 );
      break;

      case 2:        /* hollow polygon */
      hollow = 1;
      case 3:        /* filled polygon */
      for( i = 0; i < marker.n; i++ ) {
        marker.xy[i].x += xy[m].x;
        marker.xy[i].y += xy[m].y;
      }
      if( hollow )
        XDrawLines( display, cws->drawing, *gcmark,
              marker.xy, marker.n, CoordModeOrigin );
      else
        XFillPolygon( display, cws->drawing, *gcmark,
                marker.xy, marker.n, Nonconvex, CoordModeOrigin );
      for( i = 0; i < marker.n; i++ ) {
        marker.xy[i].x -= xy[m].x;
        marker.xy[i].y -= xy[m].y;
      }
      break;

      case 4:        /* segmented line */
      for( i = 0; i < marker.n; i += 2 )
        XDrawLine( display, cws->drawing, *gcmark,
             xy[m].x + marker.xy[i].x, xy[m].y + marker.xy[i].y,
             xy[m].x + marker.xy[i+1].x, xy[m].y + marker.xy[i+1].y );
      break;
      }
    }
  }
}


/*
 *    SUBROUTINE IXSETFS(STYLE,FASI)
 *    INTEGER STYLE : fill area interior style hollow or solid
 *
 *    Set fill area style
 */
void
  ixsetfs( style, fasi )
int *style;
int *fasi;
{

  switch( *style ) {

  case 1:         /* solid */
    fill_hollow = 0;
    XSetFillStyle( display, *gcfill, FillSolid );
    break;

  case 2:         /* pattern */
    fill_hollow = 1;
    break;

  case 3:         /* hatch */
    fill_hollow = 0;
    XSetFillStyle( display, *gcfill, FillStippled );
    if( *fasi != current_fasi ) {
      if( fill_pattern != (Pixmap)NULL ) {
        XFreePixmap( display, fill_pattern );
        fill_pattern = (Pixmap)NULL;
      }
      switch( *fasi ) {
        case 1: fill_pattern = XCreateBitmapFromData(display,
                RootWindow( display, screen_number), p1_bits, 16, 16);
                break;
        case 2: fill_pattern = XCreateBitmapFromData(display,
                RootWindow( display, screen_number), p2_bits, 16, 16);
                break;
        case 3: fill_pattern = XCreateBitmapFromData(display,
                RootWindow( display, screen_number), p3_bits, 16, 16);
                break;
        case 4: fill_pattern = XCreateBitmapFromData(display,
                RootWindow( display, screen_number), p4_bits, 16, 16);
                break;
        case 5: fill_pattern = XCreateBitmapFromData(display,
                RootWindow( display, screen_number), p5_bits, 16, 16);
                break;
        case 6: fill_pattern = XCreateBitmapFromData(display,
                RootWindow( display, screen_number), p6_bits, 16, 16);
                break;
        case 7: fill_pattern = XCreateBitmapFromData(display,
                RootWindow( display, screen_number), p7_bits, 16, 16);
                break;
        case 8: fill_pattern = XCreateBitmapFromData(display,
                RootWindow( display, screen_number), p8_bits, 16, 16);
                break;
        case 9: fill_pattern = XCreateBitmapFromData(display,
                RootWindow( display, screen_number), p9_bits, 16, 16);
                break;
        case 10:fill_pattern = XCreateBitmapFromData(display,
                RootWindow( display, screen_number), p10_bits, 16, 16);
                break;
        case 11:fill_pattern = XCreateBitmapFromData(display,
                RootWindow( display, screen_number), p11_bits, 16, 16);
                break;
        case 12:fill_pattern = XCreateBitmapFromData(display,
                RootWindow( display, screen_number), p12_bits, 16, 16);
                break;
        case 13:fill_pattern = XCreateBitmapFromData(display,
                RootWindow( display, screen_number), p13_bits, 16, 16);
                break;
        case 14:fill_pattern = XCreateBitmapFromData(display,
                RootWindow( display, screen_number), p14_bits, 16, 16);
                break;
        case 15:fill_pattern = XCreateBitmapFromData(display,
                RootWindow( display, screen_number), p15_bits, 16, 16);
                break;
        case 16:fill_pattern = XCreateBitmapFromData(display,
                RootWindow( display, screen_number), p16_bits, 16, 16);
                break;
        case 17:fill_pattern = XCreateBitmapFromData(display,
                RootWindow( display, screen_number), p17_bits, 16, 16);
                break;
        case 18:fill_pattern = XCreateBitmapFromData(display,
                RootWindow( display, screen_number), p18_bits, 16, 16);
                break;
        case 19:fill_pattern = XCreateBitmapFromData(display,
                RootWindow( display, screen_number), p19_bits, 16, 16);
                break;
        case 20:fill_pattern = XCreateBitmapFromData(display,
                RootWindow( display, screen_number), p20_bits, 16, 16);
                break;
        case 21:fill_pattern = XCreateBitmapFromData(display,
                RootWindow( display, screen_number), p21_bits, 16, 16);
                break;
        case 22:fill_pattern = XCreateBitmapFromData(display,
                RootWindow( display, screen_number), p22_bits, 16, 16);
                break;
        case 23:fill_pattern = XCreateBitmapFromData(display,
                RootWindow( display, screen_number), p23_bits, 16, 16);
                break;
        case 24:fill_pattern = XCreateBitmapFromData(display,
                RootWindow( display, screen_number), p24_bits, 16, 16);
                break;
        case 25:fill_pattern = XCreateBitmapFromData(display,
                RootWindow( display, screen_number), p25_bits, 16, 16);
                break;
       default: fill_pattern = XCreateBitmapFromData(display,
                RootWindow( display, screen_number), p2_bits, 16, 16);
                break;
      }
      XSetStipple( display, *gcfill, fill_pattern );
      current_fasi = *fasi;
    }
    break;

  default:
    fill_hollow = 1;
  }
}


/*
 *    SUBROUTINE IXSETFC(CINDEX)
 *    INTEGER CINDEX : color index defined my IXSETCO
 *
 *    Set color index for fill areas
 */
void
  ixsetfc( cindex )
int *cindex;
{
  setcolor( *gcfill, *cindex );

  /* invalidate fill pattern */
  if( fill_pattern != (Pixmap)NULL ) {
    XFreePixmap( display, fill_pattern );
    fill_pattern = (Pixmap)NULL;
  }
}


/*
 *    SUBROUTINE IXFLARE(N,XY)
 *    INTEGER N         : number of points
 *    INTEGER*2 XY(2,N) : list of points
 *
 *    Fill area described by polygon
 */
void
  ixflare( n, xy )
int *n;
XPoint *xy;
{
  if( fill_hollow )
    XDrawLines( display, cws->drawing, *gcfill, xy, *n, CoordModeOrigin );

  else {
    XFillPolygon( display, cws->drawing, *gcfill,
                  xy, *n, Nonconvex, CoordModeOrigin );
  }
  do_raise = 1;
}


/*
 *    SUBROUTINE IXSETTA(TXALH,TXALV)
 *    INTEGER TXALH : horizontal text alignment
 *    INTEGER TXALV : vertical text alignment
 */
void
  ixsetta( txalh, txalv )
int *txalh;
int *txalv;
{
  switch ( *txalh ) {

  case 0 :
  case 1 : switch ( *txalv) {
           case 0 : text_align = 7;
                    break;
           case 1 :
           case 2 : text_align = 1;
                    break;
           case 3 : text_align = 4;
                    break;
           }
           break;
  case 2 : switch ( *txalv) {
           case 0 : text_align = 8;
                    break;
           case 1 :
           case 2 : text_align = 2;
                    break;
           case 3 : text_align = 5;
                    break;
           }
           break;
  case 3 : switch ( *txalv) {
           case 0 : text_align = 9;
                    break;
           case 1 :
           case 2 : text_align = 3;
                    break;
           case 3 : text_align = 6;
                    break;
           }
           break;
  }
}


/*
 *    FUNCTION IXSETTF(MODE,LENFNT,FONTNAME)
 *    INTEGER MODE       : loading flag
 *            MODE=0     : search if the font exist
 *            MODE=1     : search the font and load it if it exist
 *    INTEGER LENFNT     : font name length
 *    CHARACTER*(*) FONT : font name
 *
 *    Set text font to specified name. This function returns 0 if
 *    the specified font is found, 1 if not.
 */
int
  ixsettf( mode, lenfnt,
#if defined(CERNLIB_CRAY)
           fcd_fontname )
_fcd fcd_fontname;
#endif
#if defined(CERNLIB_VAX)
           dsc_fontname )
struct dsc$descriptor_s *dsc_fontname;
#endif
#if defined(CERNLIB_TK_WINDOWS)
           fontname, lenfontname )
char *fontname;
int lenfontname;
#endif
#if (!defined(CERNLIB_CRAY))&&(!defined(CERNLIB_VAX))&&(!defined(CERNLIB_TK_WINDOWS))
           fontname )
char *fontname;
#endif
int *mode;
int *lenfnt;
{
#if defined(CERNLIB_CRAY)
  char *fontname = _fcdtocp( fcd_fontname );
#endif
#if defined(CERNLIB_VAX)
  char *fontname = dsc_fontname->dsc$a_pointer;
#endif
  char **fontlist;
  int fontcount;
  int i;

  if ( *mode != 0 ) {
     for ( i = 0; i < MAXFONT; i++ ) {
        if (strcmp(string( fontname, *lenfnt ), font[i].name) == 0) {
           text_font = font[i].id;
           XSetFont( display, *gctext, text_font->fid);
           XSetFont( display, *gcinvt, text_font->fid);
           return(0);
        }
     }
  }

#if !defined(CERNLIB_TK_WINDOWS)
  fontlist = XListFonts( display, string( fontname, *lenfnt ), 1, &fontcount);
#endif
#if defined(CERNLIB_TK_WINDOWS)
  fontcount =0;
#endif

  if ( fontcount != 0 ) {
     if ( *mode != 0 ) {
        if ( font[current_font_number].id )
           XFreeFont ( display, font[current_font_number].id);
        font[current_font_number].id = XLoadQueryFont( display, fontlist[0]);
	text_font = font[current_font_number].id;
        XSetFont( display, *gctext, text_font->fid);
        XSetFont( display, *gcinvt, text_font->fid);
        strcpy (font[current_font_number].name,string( fontname, *lenfnt ));
        current_font_number++;
        if ( current_font_number == MAXFONT ) current_font_number = 0;
     }
     XFreeFontNames(fontlist);
     return(0);
  }
  else {
     return(1);
  }
}


/*
 *    SUBROUTINE IXSETTC(CINDEX)
 *    INTEGER CINDEX : color index defined my IXSETCO
 *
 *    Set color index for text
 */
void
  ixsettc( cindex )
int *cindex;
{
  setcolor( *gctext, *cindex );

#if (!defined(CERNLIB_TK_WINDOWS))
  if ( XGetGCValues( display, *gctext, gc_mask, &gc_return) ) {
     XSetForeground( display, *gcinvt, gc_return.background );
     XSetBackground( display, *gcinvt, gc_return.foreground );
  } else {
     printf("**** Error: Cannot get GC values \n");
  }
#endif
  XSetBackground( display, *gctext, colors[0].pixel );
}


/*
 *    SUBROUTINE IXTEXT(MODE,X,Y,ANGLE,MGN,LENTXT,TEXT)
 *    INTEGER MODE       : drawing mode
 *            MODE=0     : the background is not drawn
 *            MODE=1     : the background is drawn
 *    INTEGER X,Y        : text position
 *    REAL    ANGLE      : text angle
 *    REAL    MGN        : magnification factor
 *    INTEGER LENTXT     : text length
 *    CHARACTER*(*) TEXT : text string
 *
 *    Draw a text string using current font
 */
void
  ixtext( mode, x, y, angle, mgn, lentxt,
#if defined(CERNLIB_CRAY)
       fcd_text )
_fcd fcd_text;
#endif
#if defined(CERNLIB_VAX)
       dsc_text )
struct dsc$descriptor_s *dsc_text;
#endif
#if defined(CERNLIB_TK_WINDOWS)
       text, lentext )
char *text;
int lentext;
#endif
#if (!defined(CERNLIB_CRAY))&&(!defined(CERNLIB_VAX))&&(!defined(CERNLIB_TK_WINDOWS))
       text )
char *text;
#endif
int *mode;
int *x, *y;
float *angle;
float *mgn;
int *lentxt;
{
#if defined(CERNLIB_CRAY)
  char *text = _fcdtocp( fcd_text );
#endif
#if defined(CERNLIB_VAX)
  char *text = dsc_text->dsc$a_pointer;
#endif

#if !defined(CERNLIB_TK_WINDOWS)
  XRotSetMagnification(*mgn);

  switch ( *mode ) {

  case 0 : XRotDrawAlignedString( display, text_font, *angle,
           cws->drawing, *gctext, *x, *y, string(text,*lentxt), text_align );
           break;

  case 1 : XRotDrawAlignedImageString( display, text_font, *angle,
           cws->drawing, *gctext, *x, *y, string(text,*lentxt), text_align );
           break;

  default: break;
  }
  do_raise = 1;
#endif
}


/*
 *    SUBROUTINE IXTXTL(IW,IH,LMESS,MESS)
 *    INTEGER IW          : text width
 *    INTEGER IH          : text height
 *    INTEGER LMES        : message length
 *    CHARACTER*(*) MESS  : message
 *
 *    Return the size of a character string
 */
void
  ixtxtl( w, h, lmess,
#if defined(CERNLIB_CRAY)
       fcd_mess )
_fcd fcd_mess;
#endif
#if defined(CERNLIB_VAX)
       dsc_mess )
struct dsc$descriptor_s *dsc_mess;
#endif
#if defined(CERNLIB_TK_WINDOWS)
       mess, lenmess )
char *mess;
int lenmess;
#endif
#if (!defined(CERNLIB_CRAY))&&(!defined(CERNLIB_VAX))&&(!defined(CERNLIB_TK_WINDOWS))
       mess )
char *mess;
#endif
int *lmess;
int *w;
int *h;
{
#if defined(CERNLIB_CRAY)
  char *mess = _fcdtocp( fcd_mess );
#endif
#if defined(CERNLIB_VAX)
  char *mess = dsc_mess->dsc$a_pointer;
#endif
   *w = XTextWidth( text_font, mess, *lmess );
   *h = text_font->ascent;
}


/*
 *    SUBROUTINE IXBOX(X1,X2,Y1,Y2,MODE)
 *    INTEGER X1,Y1 : left down corner
 *    INTEGER X2,Y2 : right up corner
 *    INTEGER MODE : drawing mode
 *
 *            MODE=0 hollow
 *            MODE=1 solid
 *
 *    Draw a box
 */
void
  ixbox( x1, x2, y1, y2, mode)
int *x1, *x2, *y1, *y2;
int *mode;
{
     switch ( *mode ) {

     case 0 : XDrawRectangle( display, cws->drawing, *gcline,
                              min(*x1,*x2), min(*y1,*y2),
                              abs(*x2-*x1), abs(*y2-*y1));
              break;

     case 1 : XFillRectangle( display, cws->drawing, *gcfill,
                              min(*x1,*x2), min(*y1,*y2),
                              abs(*x2-*x1), abs(*y2-*y1));
              break;

     default: break;
     }
  do_raise = 1;
}


/*
 *    SUBROUTINE IXCA(X1,X2,Y1,Y2,NX,NY,IC)
 *    INTEGER X1,Y1 : left down corner
 *    INTEGER X2,Y2 : right up corner
 *    INTEGER NX,NY : array size
 *    INTEGER IC : array
 *
 *    Draw a cell array. The drawing is done with the pixel presicion
 *    if (X2-X1)/NX (or Y) is not a exact pixel number the position of
 *    the top rigth corner may be wrong.
 */
void
  ixca(x1, x2, y1, y2, nx, ny, ic)
int *x1, *x2, *y1, *y2, *nx, *ny;
int ic[];
{
   int i,j,icol,ix,iy,w,h,current_icol;

   current_icol = -1;
   w            = max((*x2-*x1)/(*nx),1);
   h            = max((*y1-*y2)/(*ny),1);
   ix           = *x1;

   for ( i=0; i<*nx; i++ ) {
      iy = *y1-h;
      for ( j=0; j<*ny; j++ ) {
         icol = ic[i+(*nx*j)];
         if(icol != current_icol){
            XSetForeground( display, *gcfill, colors[icol].pixel );
            current_icol = icol;
         }
         XFillRectangle( display, cws->drawing, *gcfill, ix, iy, w, h);
         iy = iy-h;
      }
      ix = ix+w;
   }
}


/*
 *    SUBROUTINE IXDRMDE(MODE)
 *    INTEGER MODE : drawing mode
 *
 *            MODE=1 copy
 *            MODE=2 xor
 *            MODE=3 invert
 *            MODE=4 set the suitable mode for cursor echo accordinf to
 *                   the vendor.
 *
 *    Set the drawing mode
 */
void
  ixdrmde( mode )
  int *mode;
{
  int i;
  switch ( *mode ) {

     case 1 :
     for ( i = 0; i < MAXGC; i++ ) XSetFunction( display, gclist[i], GXcopy);
     break;

     case 2 :
     for ( i = 0; i < MAXGC; i++ ) XSetFunction( display, gclist[i], GXxor);
     break;

     case 3 :
     for ( i = 0; i < MAXGC; i++ ) XSetFunction( display, gclist[i], GXinvert);
     break;

     case 4 :
     if(strstr(vendor,"Hewlett")) {
      for ( i = 0; i < MAXGC; i++ ) XSetFunction( display, gclist[i], GXinvert);
     } else {
      for ( i = 0; i < MAXGC; i++ ) XSetFunction( display, gclist[i], GXxor);
     }
     break;

     default: break;
  }
}


/*
 *    SUBROUTINE IXSYNC(MODE)
 *    INTEGER MODE : synchronisation on/off
 *
 *            MODE=1 on
 *            MODE<>0 off
 *
 *    Set synchronisation on or off
 */
void
  ixsync( mode )
  int *mode;
{
  switch ( *mode ) {

     case 1 :
     XSynchronize(display,1);
     break;

     default:
     XSynchronize(display,0);
     break;
  }
}


/*
 *    SUBROUTINE IXQPTR(IX,IY)
 *
 *    Output parameters:
 *
 *    INTEGER IX : X coordinate of pointer
 *    INTEGER IY : Y coordinate of pointer
 *    (both coordinates are relative to the origin of the root window)
 */
void ixqptr( ix, iy )
int *ix,*iy;
{
   Window idg;

   Window    root_return,child_return;
   int       win_x_return,win_y_return;
   int       root_x_return,root_y_return;
   unsigned int mask_return;

   XQueryPointer(display,cws->window,&root_return,
   &child_return,&root_x_return,&root_y_return,&win_x_return,
   &win_y_return,&mask_return);

   *ix = (int)root_x_return;
   *iy = (int)root_y_return;

}


/*
 *    SUBROUTINE IXWARP(IX,IY)
 *
 *    Input parameters:
 *
 *    INTEGER IX : New X coordinate of pointer
 *    INTEGER IY : New Y coordinate of pointer
 *    (both coordinates are relative to the origin of the current window)
 */
void ixwarp( ix, iy )
int *ix,*iy;
{
#if !defined(CERNLIB_TK_WINDOWS)
   XWarpPointer(display,0,cws->window,0,0,0,0,*ix,*iy);
#endif
}


/*
 *    INTEGER FUNCTION IXREQLO(MODE,CTYP,X,Y)
 *    INTEGER X,Y : cursor position out moment of button press (output)
 *    INTEGER CTYP : cursor type (input)
 *
 *            CTYP=1 tracking cross
 *            CTYP=2 cross-hair
 *            CTYP=3 rubber circle
 *            CTYP=4 rubber band
 *            CTYP=5 rubber rectangle
 *
 *    INTEGER MODE : input mode
 *
 *            MODE=0 request
 *            MODE=1 sample
 *
 *    Request locator:
 *    return button number  1 = left is pressed
 *                          2 = middle is pressed
 *                          3 = right is pressed
 *         in sample mode:
 *                         11 = left is released
 *                         12 = middle is released
 *                         13 = right is released
 *                         -1 = nothing is pressed or released
 *                         -2 = leave the window
 *                       else = keycode (keyboard is pressed)
 */
int
  ixreqlo( mode, ctyp, x, y )
int *mode, *ctyp, *x, *y;
{
  static Cursor cursor = (Cursor)LNULL;
  XEvent event;
  int button_press;
  int radius;

  setinput(1);

/*
 *   Change the cursor shape
 */
  if( cursor == (Cursor)NULL ) {
#if defined(CERNLIB_TK_WINDOWS)
     XDefineCursor( display, cws->window, null_cursor );
     XSetForeground( display, gcecho, colors[0].pixel );
#endif
#if !defined(CERNLIB_TK_WINDOWS)
     if( *ctyp > 1 ) {
        XDefineCursor( display, cws->window, null_cursor );
        XSetForeground( display, gcecho, colors[0].pixel );
     } else {
        cursor = XCreateFontCursor( display, XC_crosshair );
        XDefineCursor( display, cws->window, cursor );
     }
#endif
  }

/*
 *   Event loop
 */
  button_press = 0;

  while ( button_press == 0 ) {

     switch ( *ctyp ) {

     case 1 : break;

     case 2 : XDrawLine( display, cws->window, gcecho,
                         xloc, 0, xloc, cws->height);
              XDrawLine( display, cws->window, gcecho,
                         0, yloc, cws->width, yloc);
              break;

     case 3 : radius = (int) sqrt((double)((xloc-xlocp)*(xloc-xlocp)+
                                           (yloc-ylocp)*(yloc-ylocp)));
              XDrawArc( display, cws->window, gcecho,
                        xlocp-radius, ylocp-radius,
                        2*radius, 2*radius, 0, 23040);

     case 4 : XDrawLine( display, cws->window, gcecho,
                         xlocp, ylocp, xloc, yloc);
              break;

     case 5 : XDrawRectangle( display, cws->window, gcecho,
                              min(xlocp,xloc), min(ylocp,yloc),
                              abs(xloc-xlocp), abs(yloc-ylocp));
              break;

     default: break;
     }

     while ( XEventsQueued( display, QueuedAlready) > 1) {
        XNextEvent( display, &event);
     }
     XWindowEvent( display, cws->window, mouse_mask, &event );

     switch ( *ctyp ) {

     case 1 : break;

     case 2 : XDrawLine( display, cws->window, gcecho,
                         xloc, 0, xloc, cws->height);
              XDrawLine( display, cws->window, gcecho,
                         0, yloc, cws->width, yloc);
              break;

     case 3 : radius = (int) sqrt((double)((xloc-xlocp)*(xloc-xlocp)+
                                           (yloc-ylocp)*(yloc-ylocp)));
              XDrawArc( display, cws->window, gcecho,
                        xlocp-radius, ylocp-radius,
                        2*radius, 2*radius, 0, 23040);

     case 4 : XDrawLine( display, cws->window, gcecho,
                         xlocp, ylocp, xloc, yloc);
              break;

     case 5 : XDrawRectangle( display, cws->window, gcecho,
                              min(xlocp,xloc), min(ylocp,yloc),
                              abs(xloc-xlocp), abs(yloc-ylocp));
              break;

     default: break;
     }

     xloc = event.xbutton.x;
     yloc = event.xbutton.y;

     switch ( event.type ) {

     case LeaveNotify :
           if( *mode == 0 ) {
              forever {
                 XNextEvent( display, &event);
                 if( event.type == EnterNotify ) break;
              }
           } else {
              button_press = -2;
           }
           break;

     case ButtonPress :
          button_press =  event.xbutton.button ;
          xlocp = event.xbutton.x;
          ylocp = event.xbutton.y;
#if !defined(CERNLIB_TK_WINDOWS)
          XUndefineCursor( display, cws->window );
#endif
          cursor = (Cursor)NULL;
          break;

     case ButtonRelease :
           if( *mode == 1 ) {
              button_press =  10+event.xbutton.button ;
              xlocp = event.xbutton.x;
              ylocp = event.xbutton.y;
           }
          break;

     case KeyPress :
           if( *mode == 1 ) {
              button_press =  event.xkey.keycode;
              xlocp = event.xbutton.x;
              ylocp = event.xbutton.y;
           }
          break;

     case KeyRelease :
           if( *mode == 1 ) {
              button_press =  -event.xkey.keycode;
              xlocp = event.xbutton.x;
              ylocp = event.xbutton.y;
           }
          break;

     default : break;
     }

     if( *mode == 1 ) {
         if( button_press == 0 )
            button_press = -1;
         break;
     }
  }
  *x = event.xbutton.x;
  *y = event.xbutton.y;
  setinput(0);
  XFlush( display );
  return( button_press );
}


/*
 *    INTEGER FUNCTION IXREQST(X,Y,LEN,TEXT)
 *    INTEGER X,Y        : position where text is displayed
 *    INTEGER LEN        : text length
 *    CHARACTER*(*) TEXT : text displayed (input), edited text (output)
 *
 *    Request string:
 *    text is displayed and can be edited with Emacs-like keybinding
 *    return termination code (0 for ESC, 1 for RETURN)
 */
int
  ixreqst( x, y, len_text,
#if defined(CERNLIB_CRAY)
         fcd_text )
_fcd fcd_text;
#endif
#if defined(CERNLIB_VAX)
         dsc_text )
struct dsc$descriptor_s *dsc_text;
#endif
#if defined(CERNLIB_TK_WINDOWS)
         text, lentext )
char *text;
int lentext;
#endif
#if (!defined(CERNLIB_CRAY))&&(!defined(CERNLIB_VAX))&&(!defined(CERNLIB_TK_WINDOWS))
         text )
char *text;
#endif
int *len_text;
int *x, *y;
{
#if defined(CERNLIB_CRAY)
  char *text = _fcdtocp( fcd_text );
#endif
#if defined(CERNLIB_VAX)
  char *text = dsc_text->dsc$a_pointer;
#endif
  static Cursor cursor = (Cursor)LNULL;
  static int percent = 0;  /* bell volume */
  Window focuswindow;
  int focusrevert;
  XEvent event;
  KeySym keysym;
  int key = -1;
  int nt;         /* defined length of text */
  int pt;         /* cursor position in text */

  setinput(1);

  /* change the cursor shape */
  if( cursor == (Cursor)NULL ) {
    XKeyboardState kbstate;
#if !defined(CERNLIB_TK_WINDOWS)
    cursor = XCreateFontCursor( display, XC_question_arrow );
    XGetKeyboardControl( display, &kbstate );
    percent = kbstate.bell_percent;
#endif
#if defined(CERNLIB_TK_WINDOWS)
    cursor = (Cursor) NULL;
    percent = 100;
#endif
  }
  if( cursor != (Cursor)NULL )
    XDefineCursor( display, cws->window, cursor );
  for( nt = *len_text; nt > 0 && text[nt-1] == ' '; nt-- );
  pt = nt;
  XGetInputFocus( display, &focuswindow, &focusrevert );
  XSetInputFocus( display, cws->window, focusrevert, CurrentTime );
  while( key < 0 ) {
    char keybuf[8];
    char nbytes;
    int dx;
    int i;
    XDrawImageString( display, cws->window, *gctext,
              *x, *y, text, nt );
    dx = XTextWidth( text_font, text, nt );
    XDrawImageString( display, cws->window, *gctext,
              *x + dx, *y, " ", 1 );
    dx = pt == 0 ? 0 : XTextWidth( text_font, text, pt );
    XDrawImageString( display, cws->window, *gcinvt,
              *x + dx, *y, pt < *len_text ? &text[pt] : " ", 1 );
    XWindowEvent( display, cws->window, keybd_mask, &event );
    switch( event.type ) {
    case ButtonPress:
    case EnterNotify:
      XSetInputFocus( display, cws->window, focusrevert, CurrentTime );
      break;
    case LeaveNotify:
      XSetInputFocus( display, focuswindow, focusrevert, CurrentTime );
      break;
    case KeyPress:
      nbytes = XLookupString( &event.xkey, keybuf, sizeof( keybuf ),
                 &keysym, NULL );
      switch( keysym ) {      /* map cursor keys */
      case XK_Left:
      keybuf[0] = '\002';  /* Control-B */
      nbytes = 1;
      break;
      case XK_Right:
      keybuf[0] = '\006';  /* Control-F */
      nbytes = 1;
      break;
      }
      if( nbytes == 1 ) {
      if( isascii( keybuf[0] ) && isprint( keybuf[0] ) ) {
        /* insert character */
        if( nt < *len_text )
          nt++;
        for( i = nt - 1; i > pt; i-- )
          text[i] = text[i-1];
        if( pt < *len_text ) {
          text[pt] = keybuf[0];
          pt++;
        }
      }
      else
        switch( keybuf[0] ) {
          /* Emacs-like editing keys */

        case '\010':    /* backspace */
        case '\177':    /* delete */
          /* delete backward */
          if( pt > 0 ) {
            for( i = pt; i < nt; i++ )
            text[i-1] = text[i];
            text[nt-1] = ' ';
            nt--;
            pt--;
          }
          break;
        case '\001':    /* ^A */
          /* beginning of line */
          pt = 0;
          break;
        case '\002':    /* ^B */
          /* move backward */
          if( pt > 0 )
            pt--;
          break;
        case '\004':    /* ^D */
          /* delete forward */
          if( pt > 0 ) {
            for( i = pt; i < nt; i++ )
         text[i-1] = text[i];
            text[nt-1] = ' ';
            pt--;
          }
          break;
        case '\005':    /* ^E */
          /* end of line */
          pt = nt;
          break;

        case '\006':    /* ^F */
          /* move forward */
          if( pt < nt )
            pt++;
          break;
        case '\013':    /* ^K */
          /* delete to end of line */
          for( i = pt; i < nt; i++ )
            text[i] = ' ';
          nt = pt;
          break;
        case '\024':    /* ^T */
          /* transpose */
          if( pt > 0 ) {
            char c = text[pt];
            text[pt] = text[pt-1];
            text[pt-1] = c;
          }
          break;
        case '\012':    /* newline */
        case '\015':    /* return */
          key = 1;
          break;
        case '\033':    /* escape */
          key = 0;
          break;

        default:
          XBell( display, percent );
        }
      }
    }
  }
  XSetInputFocus( display, focuswindow, focusrevert, CurrentTime );

  if( cursor != (Cursor)NULL ) {
#if !defined(CERNLIB_TK_WINDOWS)
    XUndefineCursor( display, cws->window );
#endif
    cursor = (Cursor)LNULL;
  }

  setinput(0);

  return( key );
}

#define MAX_SEGMENT 20
void ixput_image(offset,itran,x0,y0,nx,ny,xmin,ymin,xmax,ymax,image)
             int offset,itran,x0,y0,nx,ny,xmin,ymin,xmax,ymax;
   unsigned char *image;
{
  int           i, n, x, y, xcur, x1, x2, y1, y2;
  unsigned char *jimg, *jbase, icol;
  int           nlines[256];
  XSegment      lines[256][MAX_SEGMENT];

  for (i=0; i<256; i++) nlines[i] = 0;

  x1 = x0 + xmin; y1 = y0 + ny - ymax - 1;
  x2 = x0 + xmax; y2 = y0 + ny - ymin - 1;
  jbase = image + (ymin-1)*nx + xmin;

  for (y=y2; y>=y1; y--) {
    xcur   = x1; jbase += nx;
    for (jimg = jbase, icol = *jimg++, x=x1+1; x<=x2; jimg++, x++) {
      if (icol != *jimg) {
        if (icol != itran) {
          n = nlines[icol]++;
          lines[icol][n].x1 = xcur; lines[icol][n].y1 = y;
          lines[icol][n].x2 = x-1;  lines[icol][n].y2 = y;
          if (nlines[icol] == MAX_SEGMENT) {
            setcolor(*gcline,(int)icol+offset);
            XDrawSegments(display,cws->drawing,*gcline,&lines[icol][0],
              MAX_SEGMENT);
            nlines[icol] = 0;
          }
        }
        icol = *jimg; xcur = x;
      }
    }
    if (icol != itran) {
      n = nlines[icol]++;
      lines[icol][n].x1 = xcur; lines[icol][n].y1 = y;
      lines[icol][n].x2 = x-1;  lines[icol][n].y2 = y;
      if (nlines[icol] == MAX_SEGMENT) {
        setcolor(*gcline,(int)icol+offset);
        XDrawSegments(display,cws->drawing,*gcline,&lines[icol][0],
          MAX_SEGMENT);
        nlines[icol] = 0;
      }
    }
  }

  for (i=0; i<256; i++) {
    if (nlines[i] != 0) {
     setcolor(*gcline,i+offset);
     XDrawSegments(display,cws->drawing,*gcline,&lines[i][0],nlines[i]);
    }
  }
}

#if defined(CERNLIB_GIF)
/*
 *    SUBROUTINE IXLDGIF(X0, Y0, LEN, FILE)
 *    INTEGER LEN        : file name length
 *    CHARACTER*(*) FILE : file name
 *    INTEGER X0 Y0      : position
 *
 *    Load the gif a file in the current active window.
 */
void ixldgif (x0, y0, len,
#endif
#if (defined(CERNLIB_GIF))&&(defined(CERNLIB_CRAY))
          fcd_file )
_fcd fcd_file;
#endif
#if (defined(CERNLIB_GIF))&&(defined(CERNLIB_VAX))
          dsc_file )
struct dsc$descriptor_s *dsc_file;
#endif
#if (defined(CERNLIB_GIF))&&(defined(CERNLIB_TK_WINDOWS))
          file, lenfile )
char *file;
int lenfile;
#endif
#if (defined(CERNLIB_GIF))&&(!defined(CERNLIB_CRAY))&&(!defined(CERNLIB_VAX))&&(!defined(CERNLIB_TK_WINDOWS))
          file )
char *file;
#endif
#if defined(CERNLIB_GIF)
int *len;
int *x0,*y0;
{
#endif
#if (defined(CERNLIB_GIF))&&(defined(CERNLIB_CRAY))
  char *file = _fcdtocp( fcd_file );
#endif
#if (defined(CERNLIB_GIF))&&(defined(CERNLIB_VAX))
  char *file = dsc_file->dsc$a_pointer;
#endif
#if defined(CERNLIB_GIF)
  FILE  *fd;
  long  filesize;
  unsigned char *GIFarr, *PIXarr, R[256], G[256], B[256], *j1, *j2, icol;
  int   i, j, k, width, height, ncolor, irep, offset;
  float rr, gg, bb;

  fd = fopen(string(file,*len), "r");
  if (!fd) {
    fprintf(stderr,"ixldgif: unable to open GIF file\n");
    return;
  }

  fseek(fd, 0L, 2);
  filesize = ftell(fd);
  fseek(fd, 0L, 0);

  if (!(GIFarr = (unsigned char *) calloc(filesize+256,1))) {
    fprintf(stderr,"ixldgif: unable to allocate array for gif\n");
    return;
  }

  if (fread(GIFarr, filesize, 1, fd) != 1) {
    fprintf(stderr,"ixldgif: GIF file read failed\n");
    return;
  }

  irep = GIFinfo(GIFarr, &width, &height, &ncolor);
  if (irep != 0) return;

  if (!(PIXarr = (unsigned char *) calloc((long)(width*height),1))) {
    fprintf(stderr,"ixldgif: unable to allocate array for image\n");
    return;
  }

  irep = GIFdecode(GIFarr, PIXarr, &width, &height, &ncolor, R, G, B);
  if (irep != 0) return;

  /*   S E T   P A L E T T E   */

  offset = 8;

  for (i=0; i<ncolor; i++) {
    rr = R[i]/255.;
    gg = G[i]/255.;
    bb = B[i]/255.;
    j = i+offset;
    ixsetco(&j,&rr,&gg,&bb);
  }

  /*  O U T P U T   I M A G E  */

  for (i=1; i<=height/2; i++) {
    j1 = PIXarr + (i-1)*width;
    j2 = PIXarr + (height-i)*width;
    for (k=0; k<width; k++) {
      icol = *j1; *j1++ = *j2; *j2++ = icol;
    }
  }
  ixput_image(offset,-1,*x0,*y0,width,height,0,0,width-1,height-1,PIXarr);
}

typedef unsigned char byte;     /* type used by subroutines to code&decode GIF*/
XImage *image;                  /* temporary unit for GIFencode()             */
FILE *out;                      /* output/input file for animation            */
static int first = 0;

/*
 * subroutines for GIFencode()
 */

void
  get_scline( y, width, scline )
  int y, width;
  byte scline[];
{
int i;
for(i=0;i< width;i++)
        scline[i] = XGetPixel( image, i, y);
}


void
  put_byte( b)
  byte b;
{
 if( ferror(out) == 0) fputc( b, out);
}

/*
 *    SUBROUTINE IXDOGIF( NW, NH, LEN , FILE)
 *    INTEGER NW : New width (if != 0)
 *    INTEGER NH : New height (if != 0)
 *    INTEGER LEN : File name lenght
 *    CHARACTER  FILE : File name
 *
 *    Writes the current window into a GIF file
 *
 *
 */
void
  ixdogif( nw, nh, len,
#endif
#if (defined(CERNLIB_GIF))&&(defined(CERNLIB_CRAY))
          fcd_file )
_fcd fcd_file;
#endif
#if (defined(CERNLIB_GIF))&&(defined(CERNLIB_VAX))
          dsc_file )
struct dsc$descriptor_s *dsc_file;
#endif
#if (defined(CERNLIB_GIF))&&(defined(CERNLIB_TK_WINDOWS))
          file, lenfile )
char *file;
int lenfile;
#endif
#if (defined(CERNLIB_GIF))&&(!defined(CERNLIB_CRAY))&&(!defined(CERNLIB_VAX))&&(!defined(CERNLIB_TK_WINDOWS))
          file )
char *file;
#endif
#if defined(CERNLIB_GIF)
  int *nw, *nh;
  int *len;
{
#endif
#if (defined(CERNLIB_GIF))&&(defined(CERNLIB_CRAY))
  char *file = _fcdtocp( fcd_file );
#endif
#if (defined(CERNLIB_GIF))&&(defined(CERNLIB_VAX))
  char *file = dsc_file->dsc$a_pointer;
#endif
#if defined(CERNLIB_GIF)

XImage *imgtmp;
byte  *scline;
byte  *r, *g, *b;
int *R, *G, *B;
int ncol, maxcol, i;
char file_name[80];

Window root;
int xx, yy;
unsigned int ww, hh, border, depth;

  if (!display) {
     printf("**** Error: No X11 window opened. \n");
     return;
  }

  XGetGeometry( display, cws->drawing, &root, &xx, &yy, &ww, &hh, 
                                       &border, &depth );
  if (ww != cws->width || hh != cws->height) { 
     printf("**** Error: Empty window. Nothing to convert. \n");
     return;
  }

  imgtmp = XGetImage( display, cws->drawing, 0, 0,
                      cws->width, cws->height,
                      AllPlanes, ZPixmap);

  if (*nw > 0 && *nh > 0) {
     image = ImgScaling(display, imgtmp, *nw, *nh);
  } else {
     image = imgtmp;
  }

  ImgPickPalette( display, image, &ncol, &R, &G, &B);

  r = (byte *) malloc(256*sizeof(byte));
  g = (byte *) malloc(256*sizeof(byte));
  b = (byte *) malloc(256*sizeof(byte));

  maxcol = 0;
  for(i=0;i<ncol;i++) {
     if (maxcol < R[i] ) maxcol = R[i];
     if (maxcol < G[i] ) maxcol = G[i];
     if (maxcol < B[i] ) maxcol = B[i];
     r[i] = 0; g[i] = 0; b[i] = 0;
  }

  if( maxcol != 0) {
     for( i=0;i<ncol;i++) {
        r[i] = R[i] * 255/maxcol;
        g[i] = G[i] * 255/maxcol;
        b[i] = B[i] * 255/maxcol;
     }
  }

  sprintf( file_name, "%s", string(file, *len));

  out = fopen ( file_name, "w+");

  scline = (byte *) malloc(image->width*sizeof(byte));

  if (out) {
     GIFencode( image->width, image->height,
                ncol, r, g, b, scline, get_scline, put_byte);
     fclose( out );
  } else {
     printf("**** Error: Cannot open file \n");
  }

  XFree(image); 
  free(R); free(G); free(B); free(r); free(g); free(b); free(scline);
}
#endif
#endif
