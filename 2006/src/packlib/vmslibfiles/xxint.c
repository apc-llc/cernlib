/*CMZ :          16/08/94  14.51.11  by  O.Couet*/
/*CMZ :  1.21/08 08/07/94  17.41.47  by  O.Couet*/
/*CMZ :  1.21/07 30/06/94  10.24.16  by  O.Couet*/
/*CMZ :  1.21/06 27/06/94  17.01.24  by  O.Couet*/
/*CMZ :  1.21/05 14/06/94  17.42.26  by  O.Couet*/
/*CMZ :  1.21/04 09/06/94  12.47.15  by  O.Couet*/
/*CMZ :  1.21/03 02/06/94  13.23.37  by  O.Couet*/
/*CMZ :  1.21/01 31/03/94  17.34.56  by  O.Couet*/
/*CMZ :  1.20/10 16/02/94  09.23.02  by  O.Couet*/
/*CMZ :  1.20/09 24/01/94  14.21.32  by  O.Couet*/
/*CMZ :  1.20/08 19/01/94  14.42.58  by  O.Couet*/
/*CMZ :  1.20/07 13/01/94  11.20.40  by  O.Couet*/
/*CMZ :  1.20/03 23/11/93  10.55.06  by  O.Couet*/
/*-- Author : O.Couet*/
/*
 * Fortran interface to X11 window routines for HIGZ
 */
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>
#include <math.h>
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
static char p1_bits[] = {
   0xaa, 0xaa, 0x55, 0x55, 0xaa, 0xaa, 0x55, 0x55, 0xaa, 0xaa, 0x55, 0x55,
   0xaa, 0xaa, 0x55, 0x55, 0xaa, 0xaa, 0x55, 0x55, 0xaa, 0xaa, 0x55, 0x55,
   0xaa, 0xaa, 0x55, 0x55, 0xaa, 0xaa, 0x55, 0x55};
static char p2_bits[] = {
   0x44, 0x44, 0x11, 0x11, 0x44, 0x44, 0x11, 0x11, 0x44, 0x44, 0x11, 0x11,
   0x44, 0x44, 0x11, 0x11, 0x44, 0x44, 0x11, 0x11, 0x44, 0x44, 0x11, 0x11,
   0x44, 0x44, 0x11, 0x11, 0x44, 0x44, 0x11, 0x11};
static char p3_bits[] = {
   0x00, 0x00, 0x44, 0x44, 0x00, 0x00, 0x11, 0x11, 0x00, 0x00, 0x44, 0x44,
   0x00, 0x00, 0x11, 0x11, 0x00, 0x00, 0x44, 0x44, 0x00, 0x00, 0x11, 0x11,
   0x00, 0x00, 0x44, 0x44, 0x00, 0x00, 0x11, 0x11};
static char p4_bits[] = {
   0x80, 0x80, 0x40, 0x40, 0x20, 0x20, 0x10, 0x10, 0x08, 0x08, 0x04, 0x04,
   0x02, 0x02, 0x01, 0x01, 0x80, 0x80, 0x40, 0x40, 0x20, 0x20, 0x10, 0x10,
   0x08, 0x08, 0x04, 0x04, 0x02, 0x02, 0x01, 0x01};
static char p5_bits[] = {
   0x20, 0x20, 0x40, 0x40, 0x80, 0x80, 0x01, 0x01, 0x02, 0x02, 0x04, 0x04,
   0x08, 0x08, 0x10, 0x10, 0x20, 0x20, 0x40, 0x40, 0x80, 0x80, 0x01, 0x01,
   0x02, 0x02, 0x04, 0x04, 0x08, 0x08, 0x10, 0x10};
static char p6_bits[] = {
   0x44, 0x44, 0x44, 0x44, 0x44, 0x44, 0x44, 0x44, 0x44, 0x44, 0x44, 0x44,
   0x44, 0x44, 0x44, 0x44, 0x44, 0x44, 0x44, 0x44, 0x44, 0x44, 0x44, 0x44,
   0x44, 0x44, 0x44, 0x44, 0x44, 0x44, 0x44, 0x44};
static char p7_bits[] = {
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xff, 0xff, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0xff, 0xff, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xff, 0xff,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xff, 0xff};
static char p8_bits[] = {
   0x11, 0x11, 0xb8, 0xb8, 0x7c, 0x7c, 0x3a, 0x3a, 0x11, 0x11, 0xa3, 0xa3,
   0xc7, 0xc7, 0x8b, 0x8b, 0x11, 0x11, 0xb8, 0xb8, 0x7c, 0x7c, 0x3a, 0x3a,
   0x11, 0x11, 0xa3, 0xa3, 0xc7, 0xc7, 0x8b, 0x8b};
static char p9_bits[] = {
   0x10, 0x10, 0x10, 0x10, 0x28, 0x28, 0xc7, 0xc7, 0x01, 0x01, 0x01, 0x01,
   0x82, 0x82, 0x7c, 0x7c, 0x10, 0x10, 0x10, 0x10, 0x28, 0x28, 0xc7, 0xc7,
   0x01, 0x01, 0x01, 0x01, 0x82, 0x82, 0x7c, 0x7c};

#define MAXMK 100
static struct {
  int type;
  int n;
  XPoint xy[MAXMK];
} marker;                        /* point list to draw marker                 */


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
  if( gc->values.foreground == gc->values.background )
     XSetBackground( display, gc, colors[!ci].pixel );
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
          dsc_host )
struct dsc$descriptor_s *dsc_host;
int *lenhst;
{
  char *host = dsc_host->dsc$a_pointer;

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
          dsc_host )
struct dsc$descriptor_s *dsc_host;
int *lenhst;
{
  char *host = dsc_host->dsc$a_pointer;
  Pixmap pixmp1,pixmp2;
  XColor fore,back;
  char **fontlist;
  int fontcount;
  int i;
  extern char *getenv();

/*
 *              Try to open the DISPLAY
 */
  if( display == NULL ) {
     display = XOpenDisplay( string( host, *lenhst ));
     if( display == NULL ) return( -1 );
  }

  screen_number = DefaultScreen( display );
  screen = ScreenOfDisplay( display, screen_number);
  colored = DisplayPlanes( display, screen_number ) > 1;

  if(colored) colormap = DefaultColormap( display, screen_number);

  colors[1].defined = 1; /* default foreground */
  colors[1].pixel = BlackPixel( display, screen_number );
  colors[0].defined = 1; /* default background */
  colors[0].pixel = WhitePixel( display, screen_number );

/*
 *              Inquire the the XServer Vendor
 */
  strcpy (vendor,XServerVendor(display));

/*
 *              Create primitives graphic contexts
 */
  for( i = 0; i < MAXGC; i++ )
     gclist[i] = XCreateGC( display, RootWindow( display, screen_number ),
                             0, NULL );
    XSetForeground( display, *gcinvt, (*gctext)->values.background );
    XSetBackground( display, *gcinvt, (*gctext)->values.foreground );


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
  if ( !isdisp ) {
     fontlist = XListFonts( display, "*courier*", 1, &fontcount );
     if ( fontcount != 0 ) {
        text_font = XLoadQueryFont( display, fontlist[0] );
        for ( i = 0; i < MAXFONT; i++ ) {
           font[i].id = NULL;
           strcpy( font[i].name, " " );
        }
        current_font_number = 0;
     } else {
        printf("No default font loaded \n");
     }
     isdisp = 1;
  }
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
          dsc_title, flag )
struct dsc$descriptor_s *dsc_title;
int *x, *y;
int *w, *h;
int *lentit;
int *flag;
{
  char *title = dsc_title->dsc$a_pointer;
  XSetWindowAttributes attributes;
  unsigned long attr_mask = 0;
  char long_title[256];
  char host_name[64];
  XWMHints wm_hints;
  XSizeHints size_hints;
  int wid;
  int xval, yval;
  unsigned int wval, hval, border, depth;
  Window root;

  if (external_int == 1) {
     XGetGeometry( display, external_window, &root,
                   &xval, &yval, &wval, &hval, &border, &depth );
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
            cws->window = XCreateWindow(display, external_window,
                          xval, yval, wval, hval, 0, CopyFromParent,
                          InputOutput, CopyFromParent,
                          attr_mask, &attributes );
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
               gethostname( host_name, sizeof( host_name ) );
               strcat( long_title, host_name );
            } else {
               long_title[0] = ' ';
            }
            XStoreName( display, cws->window, long_title );
            XSetIconName( display, cws->window, long_title );
         }
      }
/*
 *              Set window manager hints
 */
      wm_hints.flags = InputHint;
      wm_hints.input = False;  /* don't grab input focus */
      XSetWMHints( display, cws->window, &wm_hints );
/*
 *              Set window size hints
 */
      size_hints.flags = USPosition | USSize;
      size_hints.x = xval;
      size_hints.y = yval;
      size_hints.width = wval;
      size_hints.height = hval;
      XSetNormalHints( display, cws->window, &size_hints );

      XMoveWindow( display, cws->window, xval, yval );
      XResizeWindow( display, cws->window, wval, hval );
      XMapWindow( display, cws->window );
      XFlush( display );
/*
 *              Set the Class Hint
 */
      XSetClassHint(display, cws->window, &class_hints);
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
          dsc_pxname )
struct dsc$descriptor_s *dsc_pxname;
Drawable *pix;
int *w, *h;
int *lenname;
{
  char *pxname = dsc_pxname->dsc$a_pointer;
  unsigned int wval, hval;
  wval = *w;
  hval = *h;
  XWriteBitmapFile(display,string(pxname,*lenname),*pix,wval,hval,-1,-1);
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

  if(display == NULL) return;

  for( i = 0; i < MAXFONT; i++ ) {
    if ( font[i].id ) {
       XFreeFont ( display, font[i].id);
       font[i].id = NULL;
    }
  }

  for( i = 0; i < MAXWN; i++ ) {
    windows[i].open = 0;
    if( windows[i].motif )
       Motif = 1;
  }
  if(display != NULL && Motif == (int)NULL) XCloseDisplay( display );
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

  ixclsds(); /* no open window left */
}


/*
 *    SUBROUTINE IXSELWI(WID)
 *    INTEGER WID : window number returned by IXOPNWIN
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
    for( i = 0; i < MAXGC; i++ )
      XSetClipRectangles( display, gclist[i], 0, 0, &region, 1, YXBanded );
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
  Window junkwin;
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
     XGetGeometry(display, tws->drawing, &root, x, y,
                  &width, &height, &border, &depth);
     (void) XTranslateCoordinates (display, tws->window,
                                   RootWindow( display, screen_number),
                                   0, 0, x, y, &junkwin);
     if( width > 0 && height > 0 ) {
        tws->width  = width;
        tws->height = height;
     }
     *w=tws->width-1;
     *h=tws->height-1;
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
 *    Return the host name given with IGWKTY.
 */
void
   ixgetwi (wkid, idg)
   int *wkid;
   Window *idg;
{
   *idg = windows[*wkid-1].window;
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
    for( i = 0; i < MAXGC; i++ )
      XSetClipRectangles( display, gclist[i], 0, 0, &region, 1, YXBanded );
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
    if( XAllocColor( display, colormap, &xcol ) != (Status)NULL ) {
      colors[*cindex].defined = 1;
      colors[*cindex].pixel   = xcol.pixel;
      colors[*cindex].red     = *r;
      colors[*cindex].green   = *g;
      colors[*cindex].blue    = *b;
    }
  }
/*
  XColor xcol;
  static unsigned long  pixel[MAXCOL] = {0} ;
  int inum;
  Colormap *cm = XListInstalledColormaps (display, cws->window, &inum);

  if( !colored || *cindex < 0 || *cindex >= MAXCOL ) return;
  if (*r < 0.) *r = 0.;
  if (*g < 0.) *g = 0.;
  if (*b < 0.) *b = 0.;
  if (*r > 1.) *r = 1.;
  if (*g > 1.) *g = 1.;
  if (*b > 1.) *b = 1.;
  if (pixel[*cindex] == 0) {
     XAllocColorCells(display,colormap, 0,NULL,0,&pixel[*cindex],1); }

  xcol.pixel = pixel[*cindex];
  xcol.red = (unsigned short)( *r * BIGGEST_RGB_VALUE );
  xcol.green = (unsigned short)( *g * BIGGEST_RGB_VALUE );
  xcol.blue = (unsigned short)( *b * BIGGEST_RGB_VALUE );
  xcol.flags = DoRed || DoGreen || DoBlue;

  colors[*cindex].defined = 1;
  colors[*cindex].pixel = xcol.pixel;
  colors[*cindex].red = *r;
  colors[*cindex].green = *g;
  colors[*cindex].blue = *b;
  XStoreColor(display,DefaultColormap(display,screen_number),&xcol);

  *cm = (Colormap) XListInstalledColormaps (display, cws->window, &inum);
*/
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
 *    INTEGER CINDEX : color index defined my IXSETCOL
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
         XSetDashes( display, *gcdash,
             dash_offset, dash_list, sizeof( dash_list ) );
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
     XDrawPoint(display, cws->drawing,
        line_style == LineSolid ? *gcline : *gcdash, px, py);
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
 *    INTEGER CINDEX : color index defined my IXSETCOL
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
 *    INTEGER CINDEX : color index defined my IXSETCOL
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
           dsc_fontname )
struct dsc$descriptor_s *dsc_fontname;
int *mode;
int *lenfnt;
{
  char *fontname = dsc_fontname->dsc$a_pointer;
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

  fontlist = XListFonts( display, string( fontname, *lenfnt ), 1, &fontcount);

  if ( fontcount != 0 ) {
     if ( *mode != 0 ) {
        if ( font[current_font_number].id )
           XFreeFont ( display, font[current_font_number].id);
        text_font = XLoadQueryFont( display, fontlist[0]);
        XSetFont( display, *gctext, text_font->fid);
        XSetFont( display, *gcinvt, text_font->fid);
        font[current_font_number].id = text_font;
        strcpy (font[current_font_number].name,string( fontname, *lenfnt ));
        current_font_number++;
        if ( current_font_number == MAXFONT ) current_font_number = 0;
     }
     return(0);
  }
  else {
     return(1);
  }
}


/*
 *    SUBROUTINE IXSETTC(CINDEX)
 *    INTEGER CINDEX : color index defined my IXSETCOL
 *
 *    Set color index for text
 */
void
  ixsettc( cindex )
int *cindex;
{
  setcolor( *gctext, *cindex );

  XSetForeground( display, *gcinvt, (*gctext)->values.background );
  XSetBackground( display, *gcinvt, (*gctext)->values.foreground );
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
       dsc_text )
struct dsc$descriptor_s *dsc_text;
int *mode;
int *x, *y;
float *angle;
float *mgn;
int *lentxt;
{
  char *text = dsc_text->dsc$a_pointer;

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
       dsc_mess )
struct dsc$descriptor_s *dsc_mess;
int *lmess;
int *w;
int *h;
{
  char *mess = dsc_mess->dsc$a_pointer;
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
   XWarpPointer(display,0,cws->window,0,0,0,0,*ix,*iy);
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
     if( *ctyp > 1 ) {
        XDefineCursor( display, cws->window, null_cursor );
        XSetForeground( display, gcecho, colors[0].pixel );
     } else {
        cursor = XCreateFontCursor( display, XC_crosshair );
        XDefineCursor( display, cws->window, cursor );
     }
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
          XUndefineCursor( display, cws->window );
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
         dsc_text )
struct dsc$descriptor_s *dsc_text;
int *len_text;
int *x, *y;
{
  char *text = dsc_text->dsc$a_pointer;
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
    cursor = XCreateFontCursor( display, XC_question_arrow );
    XGetKeyboardControl( display, &kbstate );
    percent = kbstate.bell_percent;
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
    XUndefineCursor( display, cws->window );
    cursor = (Cursor)LNULL;
  }

  setinput(0);

  return( key );
}
