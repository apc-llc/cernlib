#include <windows.h>
#include <math.h>
#include "ix_win32.h"

#define ColorOffset 20
#define HIGZColorIndex(ic) PALETTEINDEX(ic+ColorOffset)

/* #include <f2c.h>  */  // +SEQ, F2C.
#define IX11_HIGZ_Input IX11_HIGZ_MSG+1
enum ListDC { offWindowDC, offHistoryDC, offPixmapDC, offMetafileDC, offPixBuffer,
              Number_of_DC_Handle,
              offClipWindowDC,offClipHistoryDC,offClipPixmapDC,
              offClipMetafileDC,
              LastExtraMember};

 static struct HIGZWindow *cHw = NULL,         // Current HIGZ Window
                          *fHw = NULL,         // First HIGZ Window
                          *lHw = NULL,         // Last  HIGZ Window
                          *lpHpixmap = NULL;   // Current pixmap
 static int Number_of_HIGZ_Windows = 0;        // Total numaber of the HIGZ
                                               // Windows


/*___________________________________________________________________*/
/*      X11 output primitives
**
**          HIGZ_Primitive
*/

enum    L_HIGZ_Primitive
     {
        IX_LINE ,      // Draw a line through all points
        IX_MARKE,      // Draw a marker ar each point
        IX_FLARE,      // Fill area described by polygon
        IX_BOX  ,      // Draw a box
        IX_CA          // Draw a cell array
     };

/*      X11 text
**
**          HIGZ_Text
*/

enum    L_HIGZ_Text
     {
        IX_TEXT,       // Draw a text string using the current font
        IX_TXTL,       // Return the width and height of character string in the current font
        IW_SETCH       // Set a height for the charatcter
     };


/*     X11 output attributes
**
**          HIGZ_Attribute
*/

enum    L_HIGZ_Attribute
     {
        IX_SETCO,      // Set color intensities for given colour index
        IX_SETLN,      // Set line width
        IX_SETLS,      // Set line style
        IX_SETLC,      // Set colour index for lines
        IX_DRMDE       // Set drawing mode
     };

/*     X11 marker style
**
**       HIGZ_Marker
*/

enum    L_HIGZ_Marker
     {
        IX_SETMS,      // Set market style
        IX_SETMC,      // Set colour indx for markers
        IX_SETFS,      // Set fill area style
        IX_SETFC,      // Set colour index for fill area

        IX_SETTA,      // Set text alignment
        IX_SETTF,      // Set text font to specified name
        IX_SETTC,      // Set colour index for text
        IX_SYNC ,      // X11 synchronization
     };


/*     X11 input functions
**
**         HIGZ_Input
 */

enum    L_HIGZ_Input
     {
        IX_REQLO,      // Request locator input.
        IX_REQST       // Request a string input
     };


/*      X11 inquiry routines
**
**         HIGZ_Inquiry
 */

enum     L_HIGZ_Inquiry
      {
        IX_GETGE,      // Returns position and size of Window
        IX_GETWI,      // Returns the X11 window identifier
        IX_GETPL,      // Returns the maximal number of planes of the display
        IX_GETCOL      // Returns the X11 colour representation
      };


/*      Pixmap manipulation
**
**         HIGZ_Pixmap
 */

enum    L_HIGZ_Pixmap
     {
        IX_OPNPX,      // Open a new pixmap
        IX_CLPX ,      // Close the current opened pixmap
        IX_CPPX ,      // Copy the pixmap
        IX_CLRPX,      // Clear the pixmap
        IX_RMPX ,      // Remove the pixmap
        IX_WRPX ,      // Write the pixmap
        IX_WIPX        // Copy the area in the current window
     };


/*             Dummies
**
**           HIGZ_Dummies
 */

enum    L_HIGZ_Dummies
     {
        IX_S2BUF,
        IX_SDSWI
     };

#define WndActHeader( arg )  LRESULT APIENTRY \
                   Wnd_(arg) (hwnd, uMsg, wParam, lParam) \
                   HWND  hwnd; UINT   uMsg; WPARAM wParam; LPARAM lParam;

/*-- Author : V.Fine */
/*
 * Fortran interface to Window/NT SDK routines for HIGZ
 */
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>
#include <math.h>



#ifdef CERNLIB_QX_SC.
#define igerr   igerr_
#define wrtlst  wrtlst_
#define clrlst  clrlst_
#define ixbord  ixbord_



#define ixclip  ixclip_
#define ixclrwi ixclrwi_
#define ixclsds ixclsds_
#define ixclswi ixclswi_
#define ixflare ixflare_
#define ixgetge ixgetge_
#define ixgetpl ixgetpl_
#define ixgetht ixgetht_
#define ixline  ixline_
#define ixmarke ixmarke_
#define ixnocli ixnocli_
#define ixopnds ixopnds_
#define ixopnwi ixopnwi_
#define ixreqlo ixreqlo_
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
#define ixminit ixminit_
#define ixca    ixca_
#endif

#ifdef CERNLIB_QXCAPT.
#define igerr   IGERR
#define wrtlst  WRLST
#define clrlst  CLRLST
#define ixbord  IXBORD



#define ixclip  type_of_call IXCLIP
#define ixclrwi type_of_call IXCLRWI 
#define ixclsds type_of_call IXCLSDS 
#define ixclswi type_of_call IXCLSWI 
#define ixflare type_of_call IXFLARE 
#define ixgetge type_of_call IXGETGE 
#define ixgetpl type_of_call IXGETPL 
#define ixgetht type_of_call IXGETHT 
#define ixline  type_of_call IXLINE 
#define ixmarke type_of_call IXMARKE 
#define ixnocli type_of_call IXNOCLI 
#define ixopnds type_of_call IXOPNDS 
#define ixopnwi type_of_call IXOPNWI 
#define ixreqlo type_of_call iXREQLO 
#define ixreqst type_of_call IXREQST 
#define ixselwi type_of_call IXSELWI 
#define ixsetco type_of_call IXSETCO 
#define ixsetfc type_of_call IXSETFC 
#define ixsetfs type_of_call IXSETFS 
#define ixsetlc type_of_call IXSETLC 
#define ixsetln type_of_call IXSETLN 
#define ixsetls type_of_call IXSETLS 
#define ixsetmc type_of_call IXSETMC 
#define ixsetms type_of_call IXSETMS 
#define ixsetta type_of_call IXSETTA 
#define ixsettc type_of_call IXSETTC 
#define ixsettf type_of_call IXSETTF 
#define ixtext  type_of_call IXTEXT 
#define ixupdwi type_of_call IXUPDWI 
#define ixbox   type_of_call IXBOX
#define ixtxtl  type_of_call IXTXTL 
#define ixdrmde type_of_call IXDRMDE 
#define ixsync  type_of_call IXSYNC 
#define ixsethn type_of_call IXSETHN 
#define ixopnpx type_of_call IXOPNPX 
#define ixclpx  type_of_call IXCLPX 
#define ixcppx  type_of_call IXCPPX 
#define ixrmpx  type_of_call IXRMPX 
#define ixwipx  type_of_call IXWIPX 
#define ixclrpx type_of_call IXCLRPX 
#define ixs2buf type_of_call IXS2BUF 
#define ixq2buf type_of_call IXQ2BUF 
#define ixwrpx  type_of_call IXWRPX 
#define ixrsiz  type_of_call IXRSIZ 
#define ixsavwi type_of_call IXSAVWI 
#define ixsetwi type_of_call IXSETWI 
#define ixrscwi type_of_call IXRSCWI 
#define ixminit type_of_call IXMINIT 
#define ixca    type_of_call IXCA 
#endif

/* #define ixmotif ixmotif_ */
/* +SELF. */

#define forever while(1)
#define LNULL 0L

#define XPoint POINT             /* Change Xpoint by X11 to POINT by Windows/NT */

#define MAXWN 20                 /* maximum number of windows                 */
static struct {
  int      open;                 /* 1 if the window is open, 0 if not         */
  int      motif;                /* 1 if the window is a motif window         */
  int      double_buffer;        /* 1 if the double buffer is on, 0 if not    */
  int width;                     /* width of the window                       */
  int height;                    /* height of the window                      */
  int clip;                      /* 1 if the clipping is on                   */
  int xclip;                     /* x coordinate of the clipping rectangle    */
  int yclip;                     /* y coordinate of the clipping rectangle    */
  int wclip;                     /* width of the clipping rectangle           */
  int hclip;                     /* height of the clipping rectangle          */

  LOGPEN    pen;       /*********    */
  LOGBRUSH  gcfill;
  LOGFONT   font;
  LOGPALETTE palette;
  BITMAP   pixmap;

} windows[MAXWN],                /* windows : list of the opened windows      */
  *cws, *tws, *sws;              /* cws : pointer to the current window       */

#define MAXCOL 256
  static int maxcol;

  struct HIGZPEN {
                   LOGPEN   pen;
                   LOGBRUSH brush;
                   int      lUserDash;
                   int      *UserDash;
                 } pen = { BS_SOLID,0, 0, 1,    //  lopnStyle, lopnWidth, lopnColor
                           BS_SOLID,   0, 0, //  lbStyle, lbColor, lbHatch
                           0, NULL};         //  n dashes, pointer to an array of dashes
  LOGBRUSH  gcfill;

  LOGFONT   HIGZFont = { 0,   // lfHeight
                         0,   // lfWidth
                         0,   // lfEscapement
                         0,   // lfOrientation (doesn't used by HIGZ)
                       400,   // lfWeigh  (Normal = 400, BOLD =800)
                     FALSE,   // lfItalic
                     FALSE,   // lfUnderline
                     FALSE,   // lfStrikeOut
              ANSI_CHARSET,   // lfCharSet
        OUT_DEFAULT_PRECIS,   // lfOutPrecision
       CLIP_DEFAULT_PRECIS,
           DEFAULT_QUALITY,
               FF_DONTCARE,   // lfPitchAndFamily;
                      };
  PALETTEENTRY HIGZcolors[MAXCOL];
  NPLOGPALETTE lpPalette; // =  {0x300, MAXCOL, HIGZcolors};
  BOOLEAN bord = {FALSE};


/*         Global variable to plot HIGZ graphics cursor */
     static RECT rect;
     static HPEN   CurPen;
     static HBRUSH CurBrush;
     static HRGN CurClipRgn;
/* ==================================================== */

/*   Common HANDLES of the graphics attributes for all HIGZ windows */

static HPALETTE hdCommonPalette = NULL;
static HBRUSH   hdCommonBrush   = NULL;
static HPEN     hdCommonPen     = NULL;
static HFONT    hdCommonFont    = NULL;
static HPEN     hdCursorPen;     // Pen to draw HIGZ locator
static HBRUSH   hdCursorBrush;   // Brush to draw HIGZ locator 3 or 5
static RECT     CommonClipRectangle;
static int      dwCommonTextAlign = TA_LEFT | TA_BASELINE;
static COLORREF dwCommonTextColor;
static BOOL     PixMapON = {FALSE};

     int ixText,iyText;
     static HWND ConWindow; //  Handler of console window

/* ==================================================== */


/* =================== */
static int ispix = 0;            /* if ispix=1 a pixmap has been opened       */

static char hname[132];          /* host name                                 */

static int external_int = 0;

static int do_raise = 0;         /* if do_raise =1 cws is raised in ixupdwi   */


/*  External variables for Windows/NT  */ // V.E. Fine 21.07.93

WNDCLASS *p_higz_display, higz_display;

HWND CurrentHIGZWindow;
HDC CurrentDC, MetaFileDC, HistoryDC;

/*  LOGPALLETE HIGZ_palette[MAXCOL];
    HPALETTE WindowPalette;
 */


/*
 * colors[1]           : foreground also used for b/w screen
 * colors[0]           : background also used for b/w screen
 * colors[2..MAXCOL-1] : colors which can be set by IXSETCO
 */
static int colored;             /* Flag if color screen                      */

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

/*
 * Input echo Graphic Context global for all windows
 */

static int fill_hollow;                     /* Flag if fill style is hollow   */
static int current_fasi = 0;                /* Current fill area style index  */
static int align_hori = -1;                 /* Align text left, center, right */
static int align_vert = -1;                 /* Align text bottom, middle, top */


static int current_font_number = 0;         /* current font number in font[]  */


/*
 * data to create fill area interior style
 */
static char p1_bits[] = {
   ~(0xaa), ~(0xaa), ~(0x55), ~(0x55), ~(0xaa), ~(0xaa), ~(0x55), ~(0x55), ~(0xaa), ~(0xaa), ~(0x55), ~(0x55),
   ~(0xaa), ~(0xaa), ~(0x55), ~(0x55), ~(0xaa), ~(0xaa), ~(0x55), ~(0x55), ~(0xaa), ~(0xaa), ~(0x55), ~(0x55),
   ~(0xaa), ~(0xaa), ~(0x55), ~(0x55), ~(0xaa), ~(0xaa), ~(0x55), ~(0x55)};
static char p2_bits[] = {
   ~(0x44), ~(0x44), ~(0x11), ~(0x11), ~(0x44), ~(0x44), ~(0x11), ~(0x11), ~(0x44), ~(0x44), ~(0x11), ~(0x11),
   ~(0x44), ~(0x44), ~(0x11), ~(0x11), ~(0x44), ~(0x44), ~(0x11), ~(0x11), ~(0x44), ~(0x44), ~(0x11), ~(0x11),
   ~(0x44), ~(0x44), ~(0x11), ~(0x11), ~(0x44), ~(0x44), ~(0x11), ~(0x11)};
static char p3_bits[] = {
   ~(0x00), ~(0x00), ~(0x44), ~(0x44), ~(0x00), ~(0x00), ~(0x11), ~(0x11), ~(0x00), ~(0x00), ~(0x44), ~(0x44),
   ~(0x00), ~(0x00), ~(0x11), ~(0x11), ~(0x00), ~(0x00), ~(0x44), ~(0x44), ~(0x00), ~(0x00), ~(0x11), ~(0x11),
   ~(0x00), ~(0x00), ~(0x44), ~(0x44), ~(0x00), ~(0x00), ~(0x11), ~(0x11)};
static char p4_bits[] = {
   ~(0x80), ~(0x80), ~(0x40), ~(0x40), ~(0x20), ~(0x20), ~(0x10), ~(0x10), ~(0x08), ~(0x08), ~(0x04), ~(0x04),
   ~(0x02), ~(0x02), ~(0x01), ~(0x01), ~(0x80), ~(0x80), ~(0x40), ~(0x40), ~(0x20), ~(0x20), ~(0x10), ~(0x10),
   ~(0x08), ~(0x08), ~(0x04), ~(0x04), ~(0x02), ~(0x02), ~(0x01), ~(0x01)};
static char p5_bits[] = {
   ~(0x20), ~(0x20), ~(0x40), ~(0x40), ~(0x80), ~(0x80), ~(0x01), ~(0x01), ~(0x02), ~(0x02), ~(0x04), ~(0x04),
   ~(0x08), ~(0x08), ~(0x10), ~(0x10), ~(0x20), ~(0x20), ~(0x40), ~(0x40), ~(0x80), ~(0x80), ~(0x01), ~(0x01),
   ~(0x02), ~(0x02), ~(0x04), ~(0x04), ~(0x08), ~(0x08), ~(0x10), ~(0x10)};
static char p6_bits[] = {
   ~(0x44), ~(0x44), ~(0x44), ~(0x44), ~(0x44), ~(0x44), ~(0x44), ~(0x44), ~(0x44), ~(0x44), ~(0x44), ~(0x44),
   ~(0x44), ~(0x44), ~(0x44), ~(0x44), ~(0x44), ~(0x44), ~(0x44), ~(0x44), ~(0x44), ~(0x44), ~(0x44), ~(0x44),
   ~(0x44), ~(0x44), ~(0x44), ~(0x44), ~(0x44), ~(0x44), ~(0x44), ~(0x44)};
static char p7_bits[] = {
   ~(0x00), ~(0x00), ~(0x00), ~(0x00), ~(0x00), ~(0x00), ~(0xff), ~(0xff), ~(0x00), ~(0x00), ~(0x00), ~(0x00),
   ~(0x00), ~(0x00), ~(0xff), ~(0xff), ~(0x00), ~(0x00), ~(0x00), ~(0x00), ~(0x00), ~(0x00), ~(0xff), ~(0xff),
   ~(0x00), ~(0x00), ~(0x00), ~(0x00), ~(0x00), ~(0x00), ~(0xff), ~(0xff)};
static char p8_bits[] = {
   ~(0x11), ~(0x11), ~(0xb8), ~(0xb8), ~(0x7c), ~(0x7c), ~(0x3a), ~(0x3a), ~(0x11), ~(0x11), ~(0xa3), ~(0xa3),
   ~(0xc7), ~(0xc7), ~(0x8b), ~(0x8b), ~(0x11), ~(0x11), ~(0xb8), ~(0xb8), ~(0x7c), ~(0x7c), ~(0x3a), ~(0x3a),
   ~(0x11), ~(0x11), ~(0xa3), ~(0xa3), ~(0xc7), ~(0xc7), ~(0x8b), ~(0x8b)};
static char p9_bits[] = {
   ~(0x10), ~(0x10), ~(0x10), ~(0x10), ~(0x28), ~(0x28), ~(0xc7), ~(0xc7), ~(0x01), ~(0x01), ~(0x01), ~(0x01),
   ~(0x82), ~(0x82), ~(0x7c), ~(0x7c), ~(0x10), ~(0x10), ~(0x10), ~(0x10), ~(0x28), ~(0x28), ~(0xc7), ~(0xc7),
   ~(0x01), ~(0x01), ~(0x01), ~(0x01), ~(0x82), ~(0x82), ~(0x7c), ~(0x7c)};

BITMAP FillBitMap = { 0,   // Specifies the bitmap type. This member must be zero
                     16,   // Specifies the width, in pixel, of the bitmap
                     16,   // Specifies the height, in pixel, of the bitmap
                      2,   // Specifies the number of bytes in each scan line.
                      1,   // Specifies the count of color planes.
                      1,   // Specifies the number of bits required to ind. th color
                &p2_bits   // points to the location of the bit values for the bitmap
                    };

#define MAXMK 100
static struct MARKER {
  int cindex;
  int type;
  int n;
  XPoint xy[MAXMK];
} marker;                        /* point list to draw marker                 */


/*___________________________________________________________________*/
/*
 *    Set input on or off
 */
void
  setinput(inp)
int inp;
{

}


/*___________________________________________________________________*/
/*
 * set the foreground color in GC
 */
void
  setcolor( gc, ci )
int gc;
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


  /* make sure that foreground and background are different */
}


/*___________________________________________________________________*/
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




/*___________________________________________________________________*/
/*
 *    Allows to set the DISPLAY and the WINDOW address
 *    from outside HIGZ (ie a MOTIF program).
 */
void
  ixsdswi( dsp , win )
  int *dsp;
  int win;
{

}

/*___________________________________________________________________*/
/*
 *    Allows KUIP to give to HIGZ the adress of three routines:
 *    1) motif_open  : to open a KUIP motif window
 *    2) motif_init  : to initalize a KUIP motif window (add the callbacks)
 *    3) motif_close : to close a KUIP motif window
 */
void
  ixmotif( dsp , open, init, close)
  int *dsp;
  int (*open)();
  void (*init)();
  void (*close)();
{
}


/*___________________________________________________________________*/
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
}

/*___________________________________________________________________*/
 LRESULT APIENTRY
         HIGZDrawing(wParam,lParam)
/* Dispatch a drawing operation either to real window or to pixmap */
WPARAM wParam;
LPARAM lParam;
 {
   if (PixMapON){
           DWORD rvalue;
           HGDIOBJ hbr,hpn;

           cHw = lpHpixmap;
           CurrentDC = lpHpixmap->hdcWindow;
           hbr = SelectObject(CurrentDC,hdCommonBrush);
           hpn = SelectObject(CurrentDC,hdCommonPen);

           rvalue = (int) HIGZ_Act(NULL,IX11_HIGZ_MSG,wParam, lParam);
/*                  Restore all attributes                               */
           SelectObject(CurrentDC,hpn);
           SelectObject(CurrentDC,hbr);
           return rvalue;
   }
   else return SendMessage(CurrentHIGZWindow,
                           IX11_HIGZ_MSG,wParam,lParam);
}

/*___________________________________________________________________*/
/*
 *    SUBROUTINE IXSETHN(LENHST,HOST)
 *    INTEGER LENHST      : host name length
 *    CHARACTER*(*) HOST  : host name
 *
 *    Set host name (fill global variable hname)
 */
void
  ixsethn( lenhst, host )
char *host;
int *lenhst;
{
 strcpy (hname,string( host, *lenhst ));
}


/*___________________________________________________________________*/
  HBITMAP CreateHIGZBuffer(hwnd)
  HWND hwnd;
  {
  HBITMAP hdBuffer,oldbit,hdNewBuffer;
  HDC     hdPixmapDC,hdWindowDC,oldbitDC,hdNewDC;
  SIZE    WindowSize,BufferSize;

     hdPixmapDC = cHw->hdcPixBuffer;
     hdWindowDC = cHw->hdcWindow;
     hdBuffer   = cHw->hbBuffer;

/*  Create New Memory Device context */
     hdNewDC = CreateCompatibleDC(NULL);
     SetMapMode (hdNewDC,MM_ISOTROPIC);
     SetBkMode  (hdNewDC,TRANSPARENT);
/*  Set HIGZ pallete */
     DeleteObject(SelectPalette(hdNewDC,hdCommonPalette,TRUE));
/*  Create a new BITMAP for double buffering  and coordinate transformation  */
     GetWindowExtEx(hdWindowDC,&WindowSize);
     GetViewportExtEx(hdWindowDC,&BufferSize);
     hdNewBuffer = CreateCompatibleBitmap(hdWindowDC,BufferSize.cx,
                                                  BufferSize.cy);
     SetWindowExtEx(hdNewDC,WindowSize.cx,WindowSize.cy,NULL);
     SetViewportExtEx(hdNewDC,BufferSize.cx,BufferSize.cy,NULL);
/*             Set new bitmap                       */
     DeleteObject(SelectObject(hdNewDC,hdNewBuffer));

     if (hdPixmapDC) { /* Copy old bitmap to new one */
        BitBlt(hdNewDC,0,0,WindowSize.cx,WindowSize.cy,
               hdPixmapDC,  0,0,SRCCOPY);
/*  Delete previous buffer */
        DeleteObject(SelectObject(hdPixmapDC,hdNewBuffer));
        DeleteDC(hdPixmapDC);
     }
     else { /*  Clear new bitmap */
         rect.left   = 0;
         rect.right  = WindowSize.cx;
         rect.top    = 0;
         rect.bottom = WindowSize.cy;
         FillRect(hdNewDC,&rect, CurBrush = CreateSolidBrush(WHITE_HIGZ_COLOR));
         DeleteObject(CurBrush);
     }


/*  Set window variable */

     cHw->hdcPixBuffer=hdNewDC;
     cHw->hbBuffer    =hdNewBuffer;
     return cHw->hbBuffer;
  }


/*___________________________________________________________________*/
  HBITMAP FlushHIGZBuffer(hwnd)
  HWND hwnd;
  {
  HBITMAP hdBuffer;
  HDC     hdPixmapDC,hdWindowDC;
  SIZE    WindowSize,BufferSize;

     hdPixmapDC = cHw->hdcPixBuffer;
     hdWindowDC = cHw->hdcWindow;
     hdBuffer   = cHw->hbBuffer;

     if (hdPixmapDC) {
       if (hdBuffer) {
         GetWindowExtEx(hdWindowDC,&WindowSize);

/*    Set an application palette                      */
        SetSystemPaletteUse(hdWindowDC,SYSPAL_NOSTATIC);
        RealizePalette(hdWindowDC);

/*           Hide HIGZCursor                          */
         BitBlt(hdWindowDC,0L,0L,WindowSize.cx,WindowSize.cy,
                hdPixmapDC,0L,0L,SRCCOPY);
/*          Restore HIGZCursor */
/*    Compare size of Window and its Buffer           */
         if (WindowSize.cx  != BufferSize.cx)
/*          One needs change a scale                  */
                            hdBuffer = CreateHIGZBuffer(hwnd);
       }
     }
    return hdBuffer;
  }

/*___________________________________________________________________*/
LRESULT APIENTRY WndHIGZ(hwnd, uMsg, wParam, lParam)
HWND   hwnd;
UINT   uMsg;
WPARAM wParam;
LPARAM lParam;
{
    PAINTSTRUCT ps;

    cHw = (struct HIGZWindow *)GetWindowLong(hwnd,ExtHIGZWindow);
    if (cHw) CurrentDC = cHw->hdcWindow;

    // HistoryDC = (HDC) GetWindowLong(hwnd,4*offHistoryDC);
    switch (uMsg) {
       case WM_LBUTTONDOWN:
       case WM_MBUTTONDOWN:
       case WM_RBUTTONDOWN:
         if (!cHw->MouseInit) break;
         cHw->MouseInit = 0;
         Wnd_REQLO(hwnd,uMsg,wParam,lParam);
         return 0;
       case WM_MOUSEMOVE:
         SetCursor(cHw->hHIGZCursor);
       case WM_RBUTTONUP:
       case WM_MBUTTONUP:
       case WM_LBUTTONUP:
         if (!cHw->MouseInit)  break;
         Wnd_REQLO(hwnd,uMsg,wParam,lParam);
         return 0;
       case WM_CREATE:
       { int iLoop;
         TEXTMETRIC *tm;
         RECT RectClient;
         HPALETTE hpallete;
         SIZE WindowSize;
         HMENU hmenu;

/* Yin cursor AND bitmask */

BYTE ANDmaskCursor[] =
                       {0xff, 0xfc, 0x7f, 0xff,   /* line 1 */
                        0xff, 0xfc, 0x7f, 0xff,   /* line 2 */
                        0xff, 0xfc, 0x7f, 0xff,   /* line 3 */
                        0xff, 0xfc, 0x7f, 0xff,   /* line 4 */

                        0xff, 0xfc, 0x7f, 0xff,   /* line 5 */

                        0xff, 0xfc, 0x7f, 0xff,   /* line 6 */
                        0xff, 0xfc, 0x7f, 0xff,   /* line 7 */
                        0xff, 0xfc, 0x7f, 0xff,   /* line 8 */

                        0xff, 0xfc, 0x7f, 0xff,   /* line 9 */
                        0xff, 0xfc, 0x7f, 0xff,   /* line 10 */
                        0xff, 0xfc, 0x7f, 0xff,   /* line 11 */
                        0xff, 0xfc, 0x7f, 0xff,   /* line 12 */

                        0xff, 0xfc, 0x7f, 0xff,   /* line 13 */
                        0xff, 0xfc, 0x7f, 0xff,   /* line 14 */

                        0x00, 0x00, 0x00, 0x01,   /* line 15 */
                        0x00, 0x00, 0x00, 0x01,   /* line 16 */
                        0x00, 0x00, 0x00, 0x01,   /* line 17 */

                        0xff, 0xfc, 0x7f, 0xff,   /* line 18 */
                        0xff, 0xfc, 0x7f, 0xff,   /* line 19 */
                        0xff, 0xfc, 0x7f, 0xff,   /* line 20 */

                        0xff, 0xfc, 0x7f, 0xff,   /* line 21 */
                        0xff, 0xfc, 0x7f, 0xff,   /* line 22 */

                        0xff, 0xfc, 0x7f, 0xff,   /* line 23 */
                        0xff, 0xfc, 0x7f, 0xff,   /* line 24 */

                        0xff, 0xfc, 0x7f, 0xff,   /* line 25 */
                        0xff, 0xfc, 0x7f, 0xff,   /* line 26 */
                        0xff, 0xfc, 0x7f, 0xff,   /* line 27 */
                        0xff, 0xfc, 0x7f, 0xff,   /* line 28 */

                        0xff, 0xfc, 0x7f, 0xff,   /* line 29 */
                        0xff, 0xfc, 0x7f, 0xff,   /* line 30 */
                        0xff, 0xfc, 0x7f, 0xff,   /* line 31 */

                        0xff, 0xff, 0xff, 0xff};  /* line 32 */

/* Yin cursor XOR bitmask */

BYTE XORmaskCursor[] =
                       {0x00, 0x02, 0x80, 0x00,   /* line 1 */
                        0x00, 0x02, 0x80, 0x00,   /* line 2 */
                        0x00, 0x02, 0x80, 0x00,   /* line 3 */
                        0x00, 0x02, 0x80, 0x00,   /* line 4 */

                        0x00, 0x02, 0x80, 0x00,   /* line 5 */

                        0x00, 0x02, 0x80, 0x00,   /* line 6 */
                        0x00, 0x02, 0x80, 0x00,   /* line 7 */
                        0x00, 0x02, 0x80, 0x00,   /* line 8 */

                        0x00, 0x02, 0x80, 0x00,   /* line 9 */
                        0x00, 0x02, 0x80, 0x00,   /* line 10 */
                        0x00, 0x02, 0x80, 0x00,   /* line 11 */
                        0x00, 0x02, 0x80, 0x00,   /* line 12 */

                        0x00, 0x02, 0x80, 0x00,   /* line 13 */
                        0x00, 0x02, 0x80, 0x00,   /* line 14 */

                        0xFF, 0xFE, 0xFF, 0xFE,   /* line 15 white border */
                        0x00, 0x00, 0x00, 0x00,   /* line 16 black cross  */
                        0xFF, 0xFE, 0xFF, 0xFE,   /* line 17 white border */

                        0x00, 0x02, 0x80, 0x00,   /* line 18 */
                        0x00, 0x02, 0x80, 0x00,   /* line 19 */
                        0x00, 0x02, 0x80, 0x00,   /* line 20 */

                        0x00, 0x02, 0x80, 0x00,   /* line 21 */
                        0x00, 0x02, 0x80, 0x00,   /* line 22 */

                        0x00, 0x02, 0x80, 0x00,   /* line 23 */
                        0x00, 0x02, 0x80, 0x00,   /* line 24 */

                        0x00, 0x02, 0x80, 0x00,   /* line 25 */
                        0x00, 0x02, 0x80, 0x00,   /* line 26 */
                        0x00, 0x02, 0x80, 0x00,   /* line 27 */
                        0x00, 0x02, 0x80, 0x00,   /* line 28 */

                        0x00, 0x02, 0x80, 0x00,   /* line 29 */
                        0x00, 0x02, 0x80, 0x00,   /* line 30 */
                        0x00, 0x02, 0x80, 0x00,   /* line 31 */

                        0x00, 0x00, 0x00, 0x00};  /* line 32 */

/* Create a custom cursor at run time. */

HCURSOR hCursor = CreateCursor(
      (HINSTANCE)GetWindowLong(hwnd,GWL_HINSTANCE),  /* app instance          */
                               15,     /* horiz pos of hot spot */
                               15,     /* vert pos of hot spot  */

                               32,     /* cursor width          */
                               32,     /* cursor height         */
                     ANDmaskCursor,    /* AND bitmask           */
                     XORmaskCursor);   /* XOR bitmask           */

/*
 *
 * AND	XOR	Display
 *
 * 0	 0 	 Black
 * 0	 1	 White
 * 1	 0	 Screen
 * 1	 1	 Reverse screen
 *
 */


/* Disable the Close postion in the System Menu */
         hmenu = GetSystemMenu(hwnd,FALSE);
         EnableMenuItem(hmenu,6 , MF_GRAYED | MF_BYPOSITION);

/* Create Extended data block                */
          cHw = malloc(sizeof(HIGZWindow));
         /*  Insert in dadata chain */
          if (fHw == NULL) {
             fHw = cHw;
             cHw->ptbw =  NULL;
           }
           else {
             lHw->ptfw = cHw;
             cHw->ptbw = lHw;
           }

/* Initial fill the data structure in */
           cHw->WinType = 0;
           cHw->ptfw                = NULL;
           cHw->hwnd                = hwnd;
           cHw->hdcWindow           = GetDC(hwnd);
           cHw->hdcPixBuffer        = (HDC) NULL;
           cHw->HIGZClipRegion      = (HRGN)NULL;
           cHw->HIGZCursor          = FALSE;
           cHw->SystemCursorVisible = TRUE;
           cHw->MouseInit           = 0;
           cHw->SetTextInput        = FALSE;
           cHw->loc.x = 0; cHw->loc.y=0; cHw->locp.x=0; cHw->locp.y=0;
           cHw->isopening = FALSE;
           cHw->hHIGZCursor = hCursor;

           SetWindowLong(hwnd,ExtHIGZWindow,(LONG)cHw);

           lHw = cHw;
           Number_of_HIGZ_Windows++;

           CurrentDC = cHw->hdcWindow;

           /* Create caret to character input */

             tm = malloc(sizeof(TEXTMETRIC));
             GetTextMetrics(CurrentDC,tm);
             cHw->dwCharX = tm->tmAveCharWidth;
             cHw->dwCharY = tm->tmHeight;
             cHw->dwAscent= tm->tmAscent;
             free(tm);

             SetMapMode    (CurrentDC,MM_ISOTROPIC);
             RectClient.left   = 0;
             RectClient.top    = 0;
             RectClient.right  = 1023;
             RectClient.bottom = 1023;

 /*          Create HistoryDC (memory MetaFile) DC to perform PAINT operation          */

//             HistoryDC = CreateEnhMetaFile(CurrentDC,"META.emf",&RectClient,NULL);
             HistoryDC = 0;
             if (HistoryDC) cHw->hdcHistory =HistoryDC;

             SetWindowExtEx(CurrentDC,
                                         RectClient.right+1,
                                         RectClient.bottom+1,
                                         NULL);
             SetBkMode(CurrentDC,TRANSPARENT);
             SetTextAlign(CurrentDC,
                          TA_BASELINE | TA_LEFT | TA_NOUPDATECP);
             if (HistoryDC)
               {
                 SetMapMode    (HistoryDC,MM_ISOTROPIC);
 /*                SetWindowExtEx(HistoryDC,
                                         RectClient.right+1,
                                         RectClient.bottom+1,
                                         NULL);
  */               SetBkMode(HistoryDC,TRANSPARENT);
                 SetTextAlign(CurrentDC,
                              TA_BASELINE | TA_LEFT | TA_NOUPDATECP);
               }
             GetClientRect(hwnd,&RectClient);
             SetViewportExtEx (CurrentDC,
                                         RectClient.right,
                                         RectClient.bottom,
                                         NULL);
             GetClientRect(hwnd,&(cHw->HIGZWindowSize));
             DPtoLP(CurrentDC,(POINT*) (&(cHw->HIGZWindowSize)),2);

          if (hdCommonPalette == NULL) {
 /* Allocate enough memory for a logical palette with
               * PALETTESIZE entries and set the size and version fields
               * of the logical palette structure.
               */
              maxcol = GetDeviceCaps(CurrentDC,SIZEPALETTE);
              lpPalette = (NPLOGPALETTE) malloc((sizeof (LOGPALETTE) +
                                                (sizeof (PALETTEENTRY) * (maxcol))));

          if(!lpPalette){
            MessageBox(hwnd, "<WM_CREATE> Not enough memory for palette.", NULL, MB_OK | MB_ICONHAND);
                PostQuitMessage (0) ;
            break;
          }

              lpPalette->palVersion    = 0x300;
              lpPalette->palNumEntries = maxcol;

              /* fill in intensities for all palette entry colors */
              GetSystemPaletteEntries(CurrentDC,0,20, lpPalette->palPalEntry);

              /*  create a logical color palette according the information
               *  in the LOGPALETTE structure.
               */
              hdCommonPalette = CreatePalette ((LPLOGPALETTE) lpPalette);
        }

              DeleteObject(SelectPalette(CurrentDC,hdCommonPalette,TRUE));
              if (HistoryDC) DeleteObject(SelectPalette(HistoryDC,hpallete,TRUE));


             return 0;

          }
       case WM_PAINT:
            if (BeginPaint(hwnd,&ps)) {
              HDC hdc;
              ENHMETAHEADER mfHeader;
              HENHMETAFILE  hmf,hmfOld;
              HRGN TempClip;
              RECT rect;

/*             Close History Metafile and create a new one  and play old   */
//             hdc=(HDC)GetWindowLong(hwnd,4*offHistoryDC);

             SetSystemPaletteUse(CurrentDC,SYSPAL_NOSTATIC);
             RealizePalette(CurrentDC);
/*    Hide HIGZ mouse locator  and text caret   */
             if (!cHw->isopening)
               if (cHw->MouseInit) DrawHIGZCursor(cHw->MouseInit);
             else cHw->isopening = FALSE;
             if (cHw->SetTextInput) HideCaret(hwnd);
             FlushHIGZBuffer(hwnd);
/*    Restore HIGZ mouse locator and text caret */
             if (cHw->SetTextInput) ShowCaret(hwnd);
             if (cHw->MouseInit) DrawHIGZCursor(cHw->MouseInit);

             hdc = HistoryDC;
             if (hdc) {
/*             Get Old metafile                                            */
                hmf = CloseEnhMetaFile(hdc);
                printf("PAINT ==>  hmf  is %x   with error code %d  \n", hmf, GetLastError());

/*             Create new metafile                                         */
                GetEnhMetaFileHeader(hmf,sizeof(ENHMETAHEADER),&mfHeader);
                printf("==> PAINT:   Rectangle retrivied by GetEnh %d %d %d %d  Error code is %d \n",
                mfHeader.rclFrame.top,mfHeader.rclFrame.right,
                mfHeader.rclFrame.left,mfHeader.rclFrame.bottom, GetLastError());

                 HistoryDC = CreateEnhMetaFile(ps.hdc,NULL,&(mfHeader.rclFrame),NULL);
                 printf(" PAINT, CreateEnh HistoryDC = %x \n", HistoryDC);

               cHw->hdcHistory = HistoryDC;
/*             Play Old metafile in new one and ti window                  */
                GetClientRect(hwnd,&rect);
                DPtoLP(ps.hdc,&rect,2);
                printf("New rectangle will be %d %d %d %d \n",rect.top, rect.left,rect.bottom,rect.right);

//                 PlayEnhMetaFile(HistoryDC,hmf,&rect);

/*                        Set clipping temporary for PAINT arear           */
 //              TempClip = SelectClipRgn(ps.hdc,
//                                       CreateRectRgnIndirect(&ps.rcPaint));

           SetSystemPaletteUse(CurrentDC,SYSPAL_NOSTATIC);
           RealizePalette(CurrentDC);
               PlayEnhMetaFile(ps.hdc,hmf,&rect);

/*                       Restore clip region                               */
//               SelectClipRgn(ps.hdc,TempClip);

/*             Delete Old metafile                                         */
                DeleteEnhMetaFile(hmf);
               }

              EndPaint(hwnd, &ps);
            }

            return  DefWindowProc(hwnd,uMsg, wParam, lParam);
       case WM_ERASEBKGND:
          {
            HRGN TempClip;
/*            Cancel  clipping temporary                                   */
            if (cHw->HIGZClipRegion)
                TempClip = SelectClipRgn((HDC)wParam,NULL);
/*    Hide HIGZ mouse locator */
            if (!cHw->isopening)
              if (cHw->MouseInit) DrawHIGZCursor(cHw->MouseInit);
            else cHw->isopening = FALSE;
              GetClientRect(hwnd,&rect);
              DPtoLP(CurrentDC,&rect,2);
              FillRect(CurrentDC,&rect, CurBrush = CreateSolidBrush(WHITE_HIGZ_COLOR));
              DeleteObject(CurBrush);
//            DefWindowProc(hwnd,uMsg, wParam, lParam);
/*    Restore HIGZ mouse locator */
            if (cHw->MouseInit) DrawHIGZCursor(cHw->MouseInit);
            if (cHw->HIGZClipRegion)
                TempClip = SelectClipRgn((HDC)wParam,TempClip);
            return TRUE;
//            return FALSE // Window will remain marked for erasing
          }
       case WM_SIZE:

/* WM_SIZE
 * fwSizeType = wParam;     -  resizing flag
 * nWidth = LOWORD(lParam); -  width of client area
 *nHeight = HIWORD(lParam); -  height of client area
 */
           SetViewportExtEx(CurrentDC, LOWORD(lParam), HIWORD(lParam), NULL);
           cHw->HIGZWindowSize.right = LOWORD(lParam);
           cHw->HIGZWindowSize.bottom = HIWORD(lParam);
           DPtoLP(CurrentDC,(POINT*) (&(cHw->HIGZWindowSize)),2);
           return 0;
  /*           return DefWindowProc(hwnd,uMsg, wParam, lParam); */
       case WM_SETFOCUS:
           if (!cHw->SetTextInput) break;
               CreateCaret(hwnd, (HBITMAP) 1, cHw->dwCharX, cHw->dwAscent);
               SetCaretPos(ixText, iyText);
               ShowCaret(hwnd);
               return 0;
       case WM_KILLFOCUS:

        /*
         *   Hide and destroy the caret when the window loses
         *   keyboard focus
         */

           if (!cHw->SetTextInput) break;
               HideCaret(hwnd);
               DestroyCaret();
               return 0;
       case WM_CHAR:
           if (cHw->SetTextInput) return Wnd_REQST(hwnd, uMsg, wParam, lParam);
           break;
       case WM_KEYDOWN:
           if      (cHw->MouseInit)    Wnd_REQLO(hwnd, uMsg, wParam, lParam);
           else if (cHw->SetTextInput) Wnd_REQST(hwnd, uMsg, wParam, lParam);
           break;
       case WM_QUERYOPEN:
           cHw->isopening = TRUE;
           break;
       case WM_DESTROY:
           DeleteDC(CurrentDC);
           DestroyCursor(cHw->hHIGZCursor);  /* 3.06.94 */
           if (HistoryDC)  CloseEnhMetaFile(HistoryDC);
           break;
//           return 0;

       case IX11_HIGZ_MSG:
          {
           DWORD rvalue;
           HGDIOBJ hbr,hpn;
           HDC hdc;
/*   Set double buffering DC */
           if (cHw->hdcPixBuffer)
                CurrentDC = cHw->hdcPixBuffer;
           SetSystemPaletteUse(CurrentDC,SYSPAL_NOSTATIC);
           RealizePalette(CurrentDC);
           hbr = SelectObject(CurrentDC,hdCommonBrush);
           hpn = SelectObject(CurrentDC,hdCommonPen);

           rvalue = (DWORD)HIGZ_Act(hwnd,uMsg, wParam, lParam);
/*                  Restore all attributes                               */
           SelectObject(CurrentDC,hpn);
           SelectObject(CurrentDC,hbr);
           return rvalue;
          }
       case IX11_HIGZ_Input:
          {
           switch (LOWORD(wParam)) {
             case IX_REQLO:
                Wnd_REQLO(hwnd,uMsg, wParam, lParam);
                break;
             case IX_REQST:      // Request a string input
                Wnd_REQST(hwnd, uMsg, wParam, lParam);
                break;
             default:
                break;
           }
          }
       default:
               break;
    }
    return DefWindowProc(hwnd,uMsg, wParam, lParam);
}

/*___________________________________________________________________*/
LPTHREAD_START_ROUTINE HIGZ_MsgLoop(ThrSem)
 HANDLE ThrSem;
 {
   MSG msg;
   HWND hwndHIGZ;
   int value;
   struct CREATEHIGZWINDOW *WinThr;

   ReleaseSemaphore(ThrSem, 1, NULL);
   while(GetMessage(&msg,NULL,0,0))
    {
       if (msg.hwnd == NULL & msg.message == IX11_HIGZ_MSG) {

         switch(HIWORD(msg.wParam)) {
           case HIGZ_Control:
             switch (LOWORD(msg.wParam)) {
               case IX_OPNWI:
/*                  Create new window                                                 */
                 WinThr = msg.lParam;
                 if ( WinThr->flag != 2)
                   hwndHIGZ = CreateWindow(
                              HIGZCLASS,           /*extended window style                       */
                              string(WinThr->title,WinThr->lentit),   /*address of window name                      */
                              WS_OVERLAPPEDWINDOW | WS_VISIBLE, /*window style   */
                              CW_USEDEFAULT, CW_USEDEFAULT,  // *x, *y,
                              WinThr->w, WinThr->h,            // *w, *h,
                              NULL,                    /*handle of parent of owner window            */
                              NULL,                    /*handle of menu, or child-window identifier  */
                              higz_display.hInstance,  /*handle of application instance              */
                              NULL);                   /*address of window-creation data             */
                 else {
                   hwndHIGZ = CreateTextClass(WinThr);
                 }

                 WinThr->hwnd = hwndHIGZ;

                 ReleaseSemaphore(WinThr->semaphore, 1, NULL);
                 break;  /* IX_OPNWI */
             default:
                 break;
             } /* End of HIGZ_Control */
           case HIGZ_Attribute:
             switch (LOWORD(msg.wParam))
               {
                default:
                   break;
                 }
           default:
               break;
          }
       }
       else {
          if (msg.message != IX11_HIGZ_MSG)
                           TranslateMessage(&msg);
          DispatchMessage(&msg);
       }
    }
    ExitThread(0);
    return 0;
 } /* HIGZ_MsgLoop */


/*___________________________________________________________________*/
/*
 *    INTEGER FUNCTION IXOPNDS(LENHST,HOST)
 *    INTEGER LENHST      : host name length
 *    CHARACTER*(*) HOST  : host name
 *
 *    Open the display. Return -1 if the opening fails
 */
int
  ixopnds( lenhst, host )
char *host;
int *lenhst;
{
 HINSTANCE hinst  = GetModuleHandle(NULL);      /* handle of current instance */
//  HINSTANCE hinst  = NULL;      /* handle of current instance */


  /* Make sure that this window hasn't been registered yet */

  if (GetClassInfo(hinst,HIGZCLASS,&higz_display))
      return 0;

  /* Set the common wndClass information. This is common for all windows
  ** of this application.
  */

  higz_display.style      = CS_OWNDC | CS_HREDRAW | CS_VREDRAW;
  higz_display.cbClsExtra = 0;

  higz_display.cbWndExtra = sizeof(LONG)+LastExtraMember*sizeof(HANDLE);

  higz_display.hCursor    = NULL;
//  higz_display.hCursor    = LoadCursor(NULL, IDC_CROSS);
//  higz_display.hCursor    = LoadCursor(hinst, hCursor);
  higz_display.hInstance  = hinst;

  /*
  ** Register the main top-level window
  */

  higz_display.lpfnWndProc   = (WNDPROC) WndHIGZ;
  higz_display.hIcon         = LoadIcon(NULL, IDI_APPLICATION);
  higz_display.hbrBackground = GetStockObject(WHITE_BRUSH);
  higz_display.lpszMenuName   = NULL;
  higz_display.lpszClassName  = HIGZCLASS;

  if (!RegisterClass(&higz_display)) {
          return -1;
      }
  else {
     HANDLE ThrSem;
/*
 *    Retrieve the applicaiton instance
 */

        /* Create thread to do loop msg */

     ThrSem = CreateSemaphore(NULL, 0, 1, NULL);

     CreateThread(NULL,0, (LPTHREAD_START_ROUTINE) HIGZ_MsgLoop,
                  (LPVOID) ThrSem, 0,  &IDThread);

     WaitForSingleObject(ThrSem, INFINITE);
     CloseHandle(ThrSem);
/*   Create  common object                */
     hdCursorPen   = GetStockObject(BLACK_PEN);
//        CreatePen(PS_SOLID,0,RGB(64,64,64));// Pen to draw HIGZ locator
     hdCursorBrush =
        GetStockObject(HOLLOW_BRUSH);    // Brush to draw HIGZ locator 3 or 5
     return 0;}
}


/*___________________________________________________________________*/
LRESULT APIENTRY
                 HIGZ_Act(hwnd, uMsg, wParam, lParam)
HWND   hwnd;
UINT   uMsg;
WPARAM wParam;
LPARAM lParam;
     {
           switch (HIWORD(wParam)) {

             case HIGZ_Control:
                 switch (LOWORD(wParam))
                   {
                     case IX_OPNDS:  // Open X11 display
                       break;
                     case IX_OPNWI:  // Open X11 window
                        break;
                     case IX_SELWI:  // Select the current X11 window
                        break;
                     case IX_CLSWI:  // Close an X11 window
                        SetSystemPaletteUse(CurrentDC,SYSPAL_STATIC);
                        RealizePalette(CurrentDC);
                        while(!PostMessage(HWND_BROADCAST,WM_SYSCOLORCHANGE, 0, 0));

                        if (HistoryDC) DeleteEnhMetaFile(CloseEnhMetaFile(HistoryDC));
                        /* Delete data structure */
                        /* Link previous and next structure */
                        if (cHw->ptbw == NULL) fHw = cHw->ptfw; /* This is the first structure in the list */
                        else cHw->ptbw->ptfw = cHw->ptfw;

                        if (cHw->ptfw == NULL) lHw = cHw->ptbw; /* This is the last structure in the list */
                        else cHw->ptfw->ptbw = cHw->ptbw;
                        /* Now the current structure is free and we may delete it */
                        free(cHw);
                        Number_of_HIGZ_Windows--;
                        DestroyWindow( hwnd );
                        return 0;
                     case IX_CLSDS:  // Close an X11 session
                        PostQuitMessage(0);
                        break;

                     case IX_SETHN:  // Set X11 host name
                       return TRUE;
                     case IX_CLRWI:  // Clear an X11 window
                       {
                      /*  HBRUSH CurBrush; */
                      /*  RECT rect;       */
                        HRGN TempClip, clip;
                        char buftext[20]= {"HI\0PAW\0\0" };

/*                        Cancel clipping temporary                  */

                        clip = cHw->HIGZClipRegion;
                        if (clip != NULL) TempClip = SelectClipRgn(CurrentDC,NULL);
                        GetClientRect(hwnd,&rect);
                        DPtoLP(CurrentDC,&rect,2);
                        FillRect(CurrentDC,&rect, CurBrush = CreateSolidBrush(WHITE_HIGZ_COLOR));
                        DeleteObject(CurBrush);
                        if (clip != NULL) TempClip = SelectClipRgn(CurrentDC,TempClip);

/*             Close History Metafile and create a new one                       */

//                        if (HistoryDC) {
                          if(FALSE) {
                          ENHMETAHEADER mfHeader;
                          HENHMETAFILE  hmf;
                           hmf = CloseEnhMetaFile(HistoryDC);
                           printf(" Metafile ID %x with error %d  for %x Histiry DC \n", hmf,GetLastError(),HistoryDC);
                           GetEnhMetaFileHeader(hmf,sizeof(ENHMETAHEADER),&mfHeader);
                           DeleteEnhMetaFile(hmf);
                           HistoryDC = CreateEnhMetaFile(CurrentDC ,NULL,&(mfHeader.rclFrame),
                             NULL);
                           printf(" HistoryDC inside od CLRWI %x with Error %d \n", HistoryDC,
                           GetLastError());
                           cHw->hdcHistory = HistoryDC;
                        }
                        return TRUE;
                       }

                     case IX_UPDWI:  // Update an X11 window
                          FlushHIGZBuffer(hwnd);
                          break;
                     case IX_RSCWI:  // Resize an X11 window
                       return TRUE;
                     case IX_CLIP :  // Define the X11 clipping rectangle
                         {
                          LPPOINT ClipRectPoint;
                          LPRECT  ClipRectFromPoint;
                          HDC     hdc;
                          HRGN    clip;

                           hdc = CurrentDC;
                           ClipRectPoint     = (LPPOINT) lParam;
                           ClipRectFromPoint = (LPPOINT) lParam;
                           LPtoDP(hdc,ClipRectPoint, 2);

                           if (clip = cHw->HIGZClipRegion)
                                           DeleteObject(clip);
/*   Select clippping for Display */
                           hdc = cHw->hdcWindow;
                           SelectClipRgn(hdc,
                                         clip = CreateRectRgnIndirect( ClipRectFromPoint));

/*   Select clippping for bitmap buffer */
                           hdc = cHw->hdcPixBuffer;
                           if (hdc)
                             SelectClipRgn(hdc,
                                           clip = CreateRectRgnIndirect( ClipRectFromPoint));
                           cHw->HIGZClipRegion = clip;
                           return (LRESULT) TRUE;
                         }
                     case IX_NOCLI:  // Deactivate the X11 clipping rectangle
                         {
                           HDC     hdc;
                           HRGN    clip;

                           clip = cHw->HIGZClipRegion;
                           if (clip != NULL) {
                             DeleteObject(clip);
                             hdc = cHw->hdcWindow;
                             SelectClipRgn(hdc,NULL);
                             /* Cancel cliiping for bitmap buffer */
                             hdc = cHw->hdcPixBuffer;
                             if (hdc) SelectClipRgn(hdc,NULL);
                             cHw->HIGZClipRegion = (HRGN) NULL;;
                           }
                           return (LRESULT) TRUE;
                         }
                  default:
                          return TRUE;
                   }
                   break;


                     /*      X11 output primitives                  */
             case  HIGZ_Primitive:
                 switch (LOWORD(wParam))
                   {
                     case IX_LINE :      // Draw a line through all points
                       {
                         struct { int n; XPoint *xy;} *line;
                         HDC     hdc;
                         int i;

                         line = lParam;
                         hdc = CurrentDC;
                         if ( line->n > 1 )
                            return Polyline(hdc,line->xy,line->n);
                         else
                            return SetPixel(hdc, line->xy[0].x,line->xy[0].y, RGB(0,0,0));
                       }
                     case IX_MARKE:      // Draw a marker ar each point
                       return  Wnd_MARKE(hwnd, uMsg, wParam, lParam);

                     case IX_FLARE:      // Fill area described by polygon
                       return Wnd_FLARE(hwnd, uMsg, wParam, lParam);
                     case IX_BOX  :      // Draw a box
                       return Wnd_BOX(hwnd, uMsg, wParam, lParam);
                     case IX_CA   :      // Draw a cell array
                         return Wnd_CA(hwnd, uMsg, wParam, lParam);
                  default:
                     return TRUE;
                   }
                   break;
                    /*      X11 text                               */
             case  HIGZ_Text:
               {
                 TEXTMETRIC *tm;
                 HGDIOBJ hdf;
/*                    Save default values and set cthe current one                  */
                 hdf    = SelectObject(CurrentDC,hdCommonFont);

                           /* Update text metric's */

                 tm = malloc(sizeof(TEXTMETRIC));
                 GetTextMetrics(CurrentDC,tm);
                 cHw->dwCharX = tm->tmAveCharWidth;
                 cHw->dwCharY = tm->tmHeight;
                 cHw->dwAscent= tm->tmAscent;
                 free(tm);

                 switch (LOWORD(wParam))
                   {
                     case IX_TEXT :      // Draw a text string using the current font
                        {
                         struct {int mode,x,y,lentxt; float angle, mgn;  char *text;} *text_msg;
                         COLORREF clrref;
                         int y_shift = 0,
                             x_shift = 0;
                         int txtA;
                         double t_rotate;

/*                     Save default values  and set current one                                */
                         clrref = SetTextColor(CurrentDC,dwCommonTextColor);
                         txtA   = SetTextAlign(CurrentDC,dwCommonTextAlign);

                         text_msg = lParam;
                    /* therefore Windows font hasn'ta  "vertical cernter attribute"
                        one should emulate it by hand
                     */
                         if (align_vert == 0) {
                            t_rotate = cos(0.1*HIGZFont.lfEscapement);
                            y_shift  = (cHw->dwAscent/2)*t_rotate;
                            x_shift  = (cHw->dwAscent/2)*sqrt(1.0-t_rotate*t_rotate);
                         }
                         ExtTextOut(CurrentDC,
                                        text_msg->x+x_shift, text_msg->y+y_shift,
                                        0, NULL,
                                        text_msg->text, text_msg->lentxt,
                                        NULL);
/*                     Restore default values                                       */
                         SetTextColor(CurrentDC,clrref);
                         SetTextAlign(CurrentDC,txtA );
                         break;
                        }
                     case IX_TXTL :      // Return the width and height of character string in the current font
                        {
                           SIZE text_size;
                           struct { int w,h, lmess; char *mess;} *sText;
                           sText = lParam;
                           GetTextExtentPoint(CurrentDC,
                                              sText->mess,sText->lmess, &text_size);
                           sText->w = text_size.cx;
                           sText->h = text_size.cy;
                           break;
                        }
                     default:
                           break;
                   }
/*                     Restore Default font                       */

                 SelectObject(CurrentDC,hdf);

                 tm = malloc(sizeof(TEXTMETRIC));
                 GetTextMetrics(CurrentDC,tm);
                 cHw->dwCharX = tm->tmAveCharWidth;
                 cHw->dwCharY = tm->tmHeight;
                 cHw->dwAscent= tm->tmAscent;
                 free(tm);

                 break;
               }
/*              X11 output attributes                             */
             case HIGZ_Attribute:
                 switch (LOWORD(wParam))
                   {
                     case IX_DRMDE:      // Set drawing mode
                       SetROP2(CurrentDC,lParam);
                       return TRUE;
                   }
                   break;
                    /*     X11 marker style                        */

             case HIGZ_Marker:
                 switch (LOWORD(wParam))
                   {
                     case IX_SETTA:      // Set text alignment
                        SetTextAlign(CurrentDC,lParam);
                        break;
                     case IX_SETTF:      // Set text font to specified name
                       {
                        TEXTMETRIC *tm;
                        DeleteObject(SelectObject(CurrentDC,
                                      CreateFontIndirect((LOGFONT*) lParam))
                                    );
                           /* Update text metric's */

                        tm = malloc(sizeof(TEXTMETRIC));
                        GetTextMetrics(CurrentDC,tm);
                        cHw->dwCharX = tm->tmAveCharWidth;
                        cHw->dwCharY = tm->tmHeight;
                        cHw->dwAscent= tm->tmAscent;
                        free(tm);

                        break;
                       }

                     case IX_SYNC :      // X11 synchronization

                  default:
                     return TRUE;
                   }
                   break;
                   /*      X11 inquiry routines                   */
             case HIGZ_Inquiry:
                 switch (LOWORD(wParam))
                   {
                     case IX_GETGE:       // Returns position and size of Window
                       GetWindowExtEx(CurrentDC,(LPSIZE)lParam);
                       return TRUE;
                     case IX_GETWI:       // Returns the X11 window identifier
                     case IX_GETPL:       // Returns the maximal number of planes of the display
                     case IX_GETCOL:      // Returns the X11 colour representation
                       {
                        struct {int index; struct tagPALETTEENTRY rgb;} *lpHIGZ_rgb;
                        HGDIOBJ hpl;

                        lpHIGZ_rgb = lParam;
                        hpl = GetCurrentObject(CurrentDC,OBJ_PAL);
                        GetPaletteEntries(hpl,lpHIGZ_rgb->index,1,&(lpHIGZ_rgb->rgb));
                        break;
                       }
                  default:
                     return TRUE;
                   }
                  break;

        /*          Pixmap manipulation                    */

             case HIGZ_Pixmap:
                 switch (LOWORD(wParam))
                {
                     case IX_CPPX :       // Copy the pixmap
                       Wnd_CPPX(hwnd, uMsg, wParam, lParam);
                       break;
                     case IX_WRPX :       // Write the pixmap
//                       Wnd_WRPX(hwnd, uMsg, wParam, lParam);
                       break;
                     case IX_WIPX :       // Copy the area in the current window
                       Wnd_WIPX(hwnd, uMsg, wParam, lParam);
                       break;
                   default:
                       return TRUE;
                }
                return TRUE;
           /*             Dummies                         */

             case HIGZ_Dummies:
                 switch (LOWORD(wParam))
                   {
                     case IX_S2BUF:
                       Wnd_S2BUF(hwnd, uMsg, wParam, lParam);
                       break;
                     case IX_SDSWI:
                  default:
                     return TRUE;
                   }
            default:
                      return TRUE;
            }
            return TRUE;
  }   /*  HIGZ_Act */

/*___________________________________________________________________*/
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
  ixopnwi( x, y, w, h, lentit,title, flag )
char *title;

int *x, *y;
int *w, *h;
int *lentit;
int *flag;
{
 HWND hwndHIGZ;
 int value;
 static struct CREATEHIGZWINDOW WinThr;

 WinThr.x = *x;
 WinThr.y = *y;
 WinThr.w = *w+2*GetSystemMetrics(SM_CXFRAME);

 WinThr.h = *h+  GetSystemMetrics(SM_CYCAPTION)
              +2*GetSystemMetrics(SM_CYFRAME)
              -  GetSystemMetrics(SM_CYBORDER);
 WinThr.lentit = *lentit;
 WinThr.title = title;


      /* Create HIGZ window */

  WinThr.semaphore = CreateSemaphore(NULL, 0, 1, NULL);

  while(!PostThreadMessage(IDThread,
         IX11_HIGZ_MSG,MAKEWPARAM(IX_OPNWI,HIGZ_Control), &WinThr));

  WaitForSingleObject(WinThr.semaphore, INFINITE);
  CloseHandle(WinThr.semaphore);

  ShowWindow(WinThr.hwnd,SW_SHOWDEFAULT);
  UpdateWindow(WinThr.hwnd);

  return (int)WinThr.hwnd;

 }


/*___________________________________________________________________*/
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
  HWND *wid;
  int *w;
  int *h;
{
   int new_w,new_h;

   new_w = *w+2*GetSystemMetrics(SM_CXFRAME);

   new_h = *h+  GetSystemMetrics(SM_CYCAPTION)
             +2*GetSystemMetrics(SM_CYFRAME)
             -  GetSystemMetrics(SM_CYBORDER);

   SetWindowPos(*wid,HWND_TOP,0,0,new_w,new_h,
                 SWP_DRAWFRAME|SWP_NOMOVE |SWP_NOZORDER );
}

/*___________________________________________________________________*/
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
  SendMessage(*wid,
              IX11_HIGZ_MSG,MAKEWPARAM(IX_S2BUF,HIGZ_Dummies),*mode);
}

/*___________________________________________________________________*/
LRESULT APIENTRY
        Wnd_S2BUF (hwnd, uMsg, wParam, lParam)
HWND  hwnd;
UINT   uMsg;
WPARAM wParam;
LPARAM lParam;
/*   Global variable used:
 *
 *   offPixmapDC  - handle of the memory device context
 *   offPixBuffer - handle of the bitmap in memory DC
 *                = NULL - means there os no double buffering
 *                         for the current window.
 */
 {
  HBITMAP hdBuffer;
  HDC     hdPixmapDC;

     hdPixmapDC = cHw->hdcPixBuffer;
     hdBuffer   = cHw->hbBuffer;

     if (lParam) {
/*         Turn double bufferring ON           */
      if (hdPixmapDC)  FlushHIGZBuffer(hwnd);
      else CreateHIGZBuffer(hwnd);
     }
     else
/*         Turn double bufferring OFF           */
      if (hdPixmapDC){
        hdBuffer = FlushHIGZBuffer(hwnd);
        DeleteDC(hdPixmapDC);
        DeleteObject(hdBuffer);

        cHw->hdcPixBuffer = NULL;
        cHw->hbBuffer = NULL;
      }
 }

/*___________________________________________________________________*/
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
  int rval;
  struct HIGZWindow *Hw;
  Hw = (struct HIGZWindow *)GetWindowLong(*wid,ExtHIGZWindow);
  rval = (int) (Hw->hbBuffer);
  if (rval) return rval;
  else return -1;
}



/*___________________________________________________________________*/
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
  static struct {SIZE PixMap;HANDLE semaphore;HBITMAP ipix;} HIGZPixmap;

  HIGZPixmap.PixMap.cx = *w;
  HIGZPixmap.PixMap.cy = *h;

 lpHpixmap  = malloc(sizeof(HIGZWindow));
 if (lpHpixmap) {
  lpHpixmap->hdcWindow =  CreateCompatibleDC(NULL);
  SetMapMode (lpHpixmap->hdcWindow,MM_ISOTROPIC);
  SetBkMode  (lpHpixmap->hdcWindow,TRANSPARENT);
  DeleteObject(SelectPalette(lpHpixmap->hdcWindow,hdCommonPalette,TRUE));

  lpHpixmap->HIGZWindowSize.right  = *w;
  lpHpixmap->HIGZWindowSize.bottom = *h;
  cHw = (struct HIGZWindow *)GetWindowLong(CurrentHIGZWindow,ExtHIGZWindow);
  lpHpixmap->hbBuffer = CreateCompatibleBitmap(cHw->hdcWindow,
                                     lpHpixmap->HIGZWindowSize.right,
                                     lpHpixmap->HIGZWindowSize.bottom);
  SelectObject(lpHpixmap->hdcWindow,lpHpixmap->hbBuffer);
  PixMapON = TRUE;
 }
 return (int) lpHpixmap->hbBuffer;
}


/*___________________________________________________________________*/
/*
 *    SUBROUTINE IXCLRPX(PIX)
 *    INTEGER PIX : Pixmap address
 *
 *    Clear the pixmap PIX.
 */
void
  ixclrpx(pix )
HGDIOBJ *pix;
{
      RECT rect;
      BITMAP Bitmap_buffer;

     if (lpHpixmap) {
       if (lpHpixmap->hbBuffer == *pix) {
        GetObject(*pix, sizeof(BITMAP),&Bitmap_buffer);
        rect.left   = 0;
        rect.top    = 0;
        rect.right  = Bitmap_buffer.bmWidth;
        rect.bottom = Bitmap_buffer.bmHeight;
        FillRect(lpHpixmap->hdcWindow,
                 &rect, CurBrush = CreateSolidBrush(WHITE_HIGZ_COLOR));
        DeleteObject(CurBrush);
       }
     }
}

/*___________________________________________________________________*/
/*
 *    SUBROUTINE IXCLPX
 *
 *    Close the current opened pixmap.
 */
void
  ixclpx()
{
 PixMapON = FALSE;
}


/*___________________________________________________________________*/
/*
 *    SUBROUTINE IXCPPX(PIX, XPOS, YPOS)
 *    INTEGER PIX : Pixmap address
 *    INTEGER XPOS, YPOS : Pixmap Position
 *
 *    Copy the pixmap PIX at the position XPOS YPOS.
 */
void
  ixcppx(pix, xpos, ypos)
int *pix;
int *xpos, *ypos;
{
  POINT point;
  point.x = *xpos;
  point.y = *ypos;
  if (lpHpixmap->hbBuffer == *pix)
     SendMessage(CurrentHIGZWindow,
                 IX11_HIGZ_MSG,MAKEWPARAM(IX_CPPX,HIGZ_Pixmap),&point);
}

/*___________________________________________________________________*/
LRESULT APIENTRY
        Wnd_CPPX (hwnd, uMsg, wParam, lParam)
                   HWND  hwnd; UINT   uMsg; WPARAM wParam; LPARAM lParam;
{
   BITMAP Bitmap_buffer;
   int w,h;
   GetObject(lpHpixmap->hbBuffer, sizeof(BITMAP),&Bitmap_buffer);
   w = Bitmap_buffer.bmWidth;
   h = Bitmap_buffer.bmHeight;
   BitBlt(CurrentDC,((POINT*)lParam)->x,((POINT*)lParam)->y,w,h,
          lpHpixmap->hdcWindow,0,0,SRCCOPY);

}



/*___________________________________________________________________*/
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
  ixwrpx(pix, w, h, lenname, pxname, lpxname )
char *pxname;
int *pix;
int *w, *h;
int *lenname;
int lpxname;
{
 unsigned int wval, hval;
  wval = *w;
  hval = *h;
/*  XWriteBitmapFile(display,string(pxname,*lenname),*pix,wval,hval,-1,-1);
*/
}


/*___________________________________________________________________*/
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
int *pix;
int *xpos, *ypos;
{
  POINT point;
  point.x = *xpos;
  point.y = *ypos;
  SendMessage(CurrentHIGZWindow,
              IX11_HIGZ_MSG,MAKEWPARAM(IX_WIPX,HIGZ_Pixmap),&point);
}

/*___________________________________________________________________*/
LRESULT APIENTRY
        Wnd_WIPX (hwnd, uMsg, wParam, lParam)
                   HWND  hwnd; UINT   uMsg; WPARAM wParam; LPARAM lParam;
{
   int w,h;
   BITMAP Bitmap_buffer;

   GetObject(lpHpixmap->hbBuffer, sizeof(BITMAP),&Bitmap_buffer);
   w = Bitmap_buffer.bmWidth;
   h = Bitmap_buffer.bmHeight;
   BitBlt(lpHpixmap->hdcWindow,0,0,w,h,
          CurrentDC,((POINT*)lParam)->x,((POINT*)lParam)->y,SRCCOPY);

}



/*___________________________________________________________________*/
/*
 *    SUBROUTINE IXRMPX(PIX)
 *    INTEGER PIX : Pixmap adress
 *
 *    Remove the pixmap PIX.
 */
void
  ixrmpx(pix)
int *pix;
{
 PixMapON = FALSE;
 DeleteDC(lpHpixmap->hdcWindow);
 DeleteObject(lpHpixmap->hbBuffer);
 free(lpHpixmap);
 lpHpixmap = NULL;
}


/*___________________________________________________________________*/
/*
 *    SUBROUTINE IXCLSDS
 *
 *    Delete all windows and close connection
 */
void
  ixclsds()
{
/* Delete common objects */
  DeleteObject(hdCursorPen);
  DeleteObject(hdCursorBrush);
  PostMessage(CurrentHIGZWindow,
              IX11_HIGZ_MSG,MAKEWPARAM(IX_CLSDS,HIGZ_Control), 0);
}


/*___________________________________________________________________*/
/*
 *    SUBROUTINE IXCLSWI
 *
 *    Delete current window
 */
void
  ixclswi()
{
  int wid;

   SendMessage(CurrentHIGZWindow,
               IX11_HIGZ_MSG,MAKEWPARAM(IX_CLSWI,HIGZ_Control), 0);

//  ixclsds(); /* no open window left */
}


/*___________________________________________________________________*/
/*
 *    SUBROUTINE IXSELWI(WID)
 *    INTEGER WID : window number returned by IXOPNWIN
 *
 *    Select window to which subsequent output is directed
 */
void
  ixselwi( wid )
HWND *wid;
{
  CurrentHIGZWindow = *wid;
}


/*___________________________________________________________________*/
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


/*___________________________________________________________________*/
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


/*___________________________________________________________________*/
/*
 *    SUBROUTINE IXGETGE(WID,X,Y,W,H)
 *    INTEGER WID : window identifier
 *    INTEGER X,Y : window position (output)
 *    INTEGER W,H : window size (output) (in Windowa/NT logical coordinat)
 *
 *    Return position and size of window wid
 *    if wid < 0 the size of the display is returned
 */
void
   ixgetge( wid, x, y, w, h )
HWND *wid;
int *x, *y;
unsigned int *w, *h;
{
  HDC junkwin;
  static SIZE win_pos;
  if( *wid < 0 ) {
    /* Retrive size of full screen in the device unit */
     junkwin = GetDC(NULL);
     *x = 0;
     *y = 0;
     *w = GetDeviceCaps(junkwin,HORZRES);
     *h = GetDeviceCaps(junkwin,VERTRES);
     ReleaseDC(NULL,junkwin);
   } else {
      junkwin = SendMessage(*wid,IX11_HIGZ_MSG,MAKEWPARAM(IX_GETGE,HIGZ_Inquiry), &win_pos);
     /* Retrive size of DISPLAY in the logical unit */
      *x = 0;
      *y = 0;
      *w = win_pos.cx;
      *h = win_pos.cy;
  }
}


/*___________________________________________________________________*/
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
   HDC  hDCGlobal;
   hDCGlobal = GetDC(NULL);
   *nplanes  = GetDeviceCaps(hDCGlobal,PLANES);
   ReleaseDC(NULL,hDCGlobal);
}


/*___________________________________________________________________*/
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

}


/*___________________________________________________________________*/
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
   GetComputerName(chhost,il);
}


/*___________________________________________________________________*/
void
   ixgetwi (wkid, idg)
   int wkid;    /* workstation identifier (input) */
   int *idg; /* window identifier (output) */
{
   *idg = 1;
}


/*___________________________________________________________________*/
/*
 *    SUBROUTINE IXCLRWI
 *
 *    Clear current window
 */
void
  ixclrwi()
{
  HIGZDrawing(MAKEWPARAM(IX_CLRWI,HIGZ_Control), 0);
}


/*___________________________________________________________________*/
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
  SendMessage(CurrentHIGZWindow,IX11_HIGZ_MSG,
              MAKEWPARAM(IX_UPDWI,HIGZ_Control), *mode);
}


/*___________________________________________________________________*/
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
HWND *wid;
int *x, *y;
int *w, *h;
{
  static RECT region;

  region.left   = *x;
  region.top    = *y;
  region.right  = *x+*w;
  region.bottom = *y+*h;

  SendMessage(*wid,IX11_HIGZ_MSG,MAKEWPARAM(IX_CLIP,HIGZ_Control), &region);

}

/*___________________________________________________________________*/
/*
 *    SUBROUTINE IWCLIP(WID,XMIN,YMIN,XMAX,YMAX)
 *    INTEGER WID : Window indentifier
 *    INTEGER XMIN,YMIN, XMAX, YMAX : clipping rectangle
 *
 *    Set clipping region for the window wid
 */
void
  iwclip( wid, xmin, ymin, xmax, ymax )
HWND *wid;
int *xmin, *ymin, *xmax, *ymax;
{
  static RECT region;

  region.left   = *xmin;
  region.top    = *ymin;
  region.right  = *xmax;
  region.bottom = *ymax;

  SendMessage(*wid,IX11_HIGZ_MSG,MAKEWPARAM(IX_CLIP,HIGZ_Control), &region);

}

/*___________________________________________________________________*/
/*
 *    SUBROUTINE IXNOCLI(WID)
 *    INTEGER WID : Window indentifier
 *
 *    Switch off the clipping for the window wid
 */
void
  ixnocli(wid)
HWND *wid;
{

  SendMessage(*wid,IX11_HIGZ_MSG,MAKEWPARAM(IX_NOCLI,HIGZ_Control), 0 );

}


/*___________________________________________________________________*/
/*
 *    SUBROUTINE IXSETCO(CINDEX,R,G,B)
 *    INTEGER CINDEX : color index
 *    REAL R,G,B     : red, green, blue intensities between 0.0 and 1.0
 *
 *    Set color intensities for given color index
 */
#define BIGGEST_RGB_VALUE 255  // 65535

void
  ixsetco( cindex, r, g, b )
int *cindex;
float *r, *g, *b;
{
  static PALETTEENTRY ChColor;
  static struct {UINT ci; struct PALETTEENTRY *lpChColor;}  NewColor;
  int i;

  if (*cindex < MAXCOL-1) {
    NewColor.ci = *cindex;
    NewColor.lpChColor = &ChColor;

    ChColor.peRed   = (BYTE) (*r*BIGGEST_RGB_VALUE);
    ChColor.peGreen = (BYTE) (*g*BIGGEST_RGB_VALUE);
    ChColor.peBlue  = (BYTE) (*b*BIGGEST_RGB_VALUE);

    ChColor.peFlags = PC_NOCOLLAPSE;

    i = NewColor.ci+ColorOffset;
    SetPaletteEntries(hdCommonPalette,i,1,&ChColor);
  }
}


/*___________________________________________________________________*/
void
  ixgetcol( index, r, g, b )
int index; /* input */
float *r, *g, *b; /* output */
{
  static struct {int index; PALETTEENTRY rgb;} HIGZ_rgb;
  HIGZ_rgb.index = index;
  SendMessage(CurrentHIGZWindow,
              IX11_HIGZ_MSG,MAKEWPARAM(IX_GETCOL,HIGZ_Inquiry),&HIGZ_rgb);
  *r = (float)HIGZ_rgb.rgb.peRed  /BIGGEST_RGB_VALUE;
  *g = (float)HIGZ_rgb.rgb.peGreen/BIGGEST_RGB_VALUE;
  *b = (float)HIGZ_rgb.rgb.peBlue /BIGGEST_RGB_VALUE;
  return ;
}

/*___________________________________________________________________*/
HPEN CreateHIGZPen(lpPen)
struct HIGZPEN *lpPen;
 {
     static int dwWidth, dwStyle;

     dwWidth = lpPen->pen.lopnWidth.x;
     if (dwWidth <= 1)  dwWidth=1;

      dwStyle = lpPen->pen.lopnStyle |
                         ((dwWidth == 1)? PS_COSMETIC:
                                          PS_GEOMETRIC | PS_INSIDEFRAME);

     if (hdCommonPen) DeleteObject(hdCommonPen);
     return ExtCreatePen( dwStyle,
                          dwWidth,
                          &(lpPen->brush),
                          lpPen->lUserDash,
                          lpPen->UserDash);
 }


/*___________________________________________________________________*/
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
  if (pen.pen.lopnWidth.x != *width)
  {
    pen.pen.lopnWidth.x = *width;
    hdCommonPen = CreateHIGZPen(&pen);
  }
}


/*___________________________________________________________________*/
/*
 *    SUBROUTINE IXSETLS(N,DASH)
 *    INTEGER N       : length of dash list
 *    INTEGER DASH(N) : dash segment lengths
 *
 *    Set line style:
 *    if N.LE.0 use pre-defined Windows style:
 *         0 - solid lines
 *        -1 - solid lines
 *        -2 - dash line
 *        -3 - dot  line
 *        -4 - dash-dot line
 *        -5 - dash-dot-dot line
 *    .LE.-6 - solid line
 *
 *    if N.GT.0 use dashed lines described by DASH(N)
 *    e.g. N=4,DASH=(6,3,1,3) gives a dashed-dotted line with dash length 6
 *    and a gap of 7 between dashes
 */
void
  ixsetls( n, dash )
int *n;
int *dash;
{
  UINT style;
  if( *n <= 0 ) {
    pen.lUserDash = 0;
    pen.UserDash = NULL;
    switch (*n)
     {
       case  0:  style = PS_SOLID;
                 break;
       case -1:  style = PS_SOLID;
                 break;
       case -2:  style = PS_DASH;
                 break;
       case -3:  style = PS_DOT;
                 break;
       case -4:  style = PS_DASHDOT;
                 break;
       case -5:  style = PS_DASHDOTDOT;
                 break;
       default:  style = PS_SOLID;
                break;
     }
   if (pen.pen.lopnStyle == style ) return;
   pen.pen.lopnStyle = style;
  }
  else
  {
    pen.pen.lopnStyle = PS_USERSTYLE;
    pen.lUserDash = *n;
    pen.UserDash = dash;
  }
  hdCommonPen = CreateHIGZPen(&pen);
}


/*___________________________________________________________________*/
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
  if (pen.pen.lopnColor != *cindex)
    {
     pen.pen.lopnColor = *cindex;
     pen.brush.lbColor = HIGZColorIndex(pen.pen.lopnColor);

     hdCommonPen = CreateHIGZPen(&pen);
    }
}


/*___________________________________________________________________*/
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
  static struct { int n; XPoint *xy;} line;
  line.n  = *n;
  line.xy = xy;
  HIGZDrawing(MAKEWPARAM(IX_LINE,HIGZ_Primitive),&line);
}


/*___________________________________________________________________*/
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


/*___________________________________________________________________*/
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
  marker.cindex = *cindex;
}


/*___________________________________________________________________*/
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
  static struct { int n; XPoint *xy; struct MARKER *marker;} markers;

  markers.n = *n;
  markers.xy = xy;
  markers.marker = &marker;

  HIGZDrawing(MAKEWPARAM(IX_MARKE,HIGZ_Primitive),&markers);

}

/*___________________________________________________________________*/
 LRESULT APIENTRY
      Wnd_MARKE(hwnd, uMsg, wParam, lParam)
 HWND   hwnd;
 UINT   uMsg;
 WPARAM wParam;
 LPARAM lParam;
  {
  struct { int n; XPoint *xy; struct MARKER *marker;} *markers;
  int m,n;
  XPoint *xy;
  COLORREF mColor;
  struct MARKER  *CurMarker;
  int ownBrush;

  markers = lParam;
  n       = markers->n;
  xy      = markers->xy;
  CurMarker  = markers->marker;

                  /* Set marker Color */
  mColor  = HIGZColorIndex(CurMarker->cindex);

  if( CurMarker->n <= 0 )
     for (m=0; m < n; m++)  SetPixel(CurrentDC, xy[m].x,xy[m].y, mColor);

  else {
    int r = CurMarker->n / 2;
    HGDIOBJ  CurBrush, CurPen;

    CurPen   = SelectObject(CurrentDC, CreatePen(PS_SOLID,0,mColor));

    switch (CurMarker -> type) {
      case 0:
      case 3:
     default:
          ownBrush = TRUE;
          CurBrush = SelectObject(CurrentDC, CreateSolidBrush(mColor));
          break;
      case 1:
      case 2:
          ownBrush = TRUE;
          CurBrush = SelectObject(CurrentDC, GetStockObject(HOLLOW_BRUSH));
          break;
      case 4:
          ownBrush = FALSE;
          break;
      }

    for( m = 0; m < n; m++ ) {
      int i;

      switch( CurMarker->type ) {

      case 0:        /* hollow circle */
      case 1:        /* filled circle */


         Pie( CurrentDC,
              xy[m].x - r, xy[m].y - r,
              xy[m].x + r, xy[m].y + r,
              xy[m].x - r, xy[m].y - r,
              xy[m].x - r, xy[m].y - r);
         break;

      case 2:        /* hollow polygon */
      case 3:        /* filled polygon */
        for( i = 0; i < CurMarker->n; i++ ) {
                                         CurMarker->xy[i].x += xy[m].x;
                                         CurMarker->xy[i].y += xy[m].y;
                                        }
        Polygon(CurrentDC,CurMarker->xy,CurMarker->n);
        for( i = 0; i < CurMarker->n; i++ ) {
          CurMarker->xy[i].x -= xy[m].x;
          CurMarker->xy[i].y -= xy[m].y;
         }
      break;

      case 4:        /* segmented line */
      for( i = 0; i < CurMarker->n; i += 2 )
       {
        MoveToEx(CurrentDC,xy[m].x + CurMarker->xy[i].x, xy[m].y + CurMarker->xy[i].y,NULL);
        LineTo(CurrentDC,xy[m].x + CurMarker->xy[i+1].x, xy[m].y + CurMarker->xy[i+1].y);
       }
       break;
      }
    }


    if (ownBrush) DeleteObject(SelectObject(CurrentDC, CurBrush));
    DeleteObject( SelectObject(CurrentDC, CurPen ));
   }
   return (LRESULT)TRUE;
 }



/*___________________________________________________________________*/
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
  if (hdCommonBrush != NULL) DeleteObject(hdCommonBrush);
  switch( *style ) {

  case 0:         /* solid */
    gcfill.lbStyle = BS_HOLLOW;
    hdCommonBrush = GetStockObject(HOLLOW_BRUSH);
    break;
  case 1:         /* solid */
    gcfill.lbStyle = BS_SOLID;
//    if (gcfill.lbColor)
       hdCommonBrush = CreateSolidBrush(HIGZColorIndex(gcfill.lbColor));
//    else
//       hdCommonBrush = GetStockObject(WHITE_BRUSH);
    break;

  case 2:         /* pattern */
    gcfill.lbStyle = BS_PATTERN;
    gcfill.lbHatch = &FillBitMap;
      switch( *fasi ) {
        case 1: FillBitMap.bmBits = &p1_bits;
                break;
        case 2: FillBitMap.bmBits = &p2_bits;
                break;
        case 3: FillBitMap.bmBits = &p3_bits;
                break;
        case 4: FillBitMap.bmBits = &p4_bits;
                break;
        case 5: FillBitMap.bmBits = &p5_bits;
                break;
        case 6: FillBitMap.bmBits = &p6_bits;
                break;
        case 7: FillBitMap.bmBits = &p7_bits;
                break;
        case 8: FillBitMap.bmBits = &p8_bits;
                break;
        case 9: FillBitMap.bmBits = &p9_bits;
                break;
       default: FillBitMap.bmBits = &p2_bits;
                break;
        }
       hdCommonBrush =
           CreatePatternBrush(CreateBitmapIndirect((LPBITMAP)gcfill.lbHatch));
       break;

  case 3:         /* hatch */
      gcfill.lbStyle = BS_HATCHED;
      switch (*fasi)
        {
          case 1: gcfill.lbHatch = HS_BDIAGONAL;
                  break;
          case 2: gcfill.lbHatch = HS_CROSS;
                  break;
          case 3: gcfill.lbHatch = HS_DIAGCROSS;
                  break;
          case 4: gcfill.lbHatch = HS_FDIAGONAL;
                  break;
          case 5: gcfill.lbHatch = HS_HORIZONTAL;
                  break;
          case 6: gcfill.lbHatch = HS_VERTICAL;
                  break;
         default: gcfill.lbHatch = HS_FDIAGONAL;
                  break;
        }
        hdCommonBrush =
           CreateHatchBrush(gcfill.lbHatch,HIGZColorIndex(gcfill.lbColor));
        break;
 default:         /* solid */
      gcfill.lbStyle = BS_NULL;
//      if (gcfill.lbColor)
         hdCommonBrush = CreateSolidBrush(HIGZColorIndex(gcfill.lbColor));
//      else
//         hdCommonBrush = GetStockObject(WHITE_BRUSH);
      break;

 }
}


/*___________________________________________________________________*/
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
  if (gcfill.lbColor != *cindex) {
   gcfill.lbColor = *cindex;

   switch (gcfill.lbStyle)
     {
       case BS_HOLLOW  :
       case BS_PATTERN :
/*                 Hollow and Patten styles needn't a change of brush */
           break;
       case BS_HATCHED :
          if (hdCommonBrush) DeleteObject(hdCommonBrush);
          hdCommonBrush =
             CreateHatchBrush(gcfill.lbHatch,HIGZColorIndex(gcfill.lbColor));
          break;
       case BS_SOLID   :
       default         :
          if (hdCommonBrush) DeleteObject(hdCommonBrush);
//          if (gcfill.lbColor)
            hdCommonBrush = CreateSolidBrush(HIGZColorIndex(gcfill.lbColor));
//          else
//            hdCommonBrush = GetStockObject(WHITE_BRUSH);
          break;
     }
   }
}


/*___________________________________________________________________*/
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
  static struct {int n; XPoint *xy;} polygon;

  polygon.n = *n;
  polygon.xy = xy;
  HIGZDrawing(MAKEWPARAM(IX_FLARE,HIGZ_Primitive),&polygon);
}

/*___________________________________________________________________*/
 LRESULT APIENTRY
       Wnd_FLARE(hwnd, uMsg, wParam, lParam)
 HWND   hwnd;
 UINT   uMsg;
 WPARAM wParam;
 LPARAM lParam;
 {
   static struct {int n; XPoint *xy;} *polygon;
   static COLORREF CurTextColor;
   static HGDIOBJ lpCurPen;

   polygon = lParam;

   if (!bord & gcfill.lbStyle != BS_HOLLOW)
               lpCurPen = SelectObject(CurrentDC, GetStockObject(NULL_PEN));


   if (gcfill.lbStyle == BS_PATTERN) {
             CurTextColor = SetTextColor(CurrentDC,HIGZColorIndex(gcfill.lbColor));
             Polygon(CurrentDC,polygon->xy,polygon->n);
             SetTextColor(CurrentDC,CurTextColor);
   }
   else {
             Polygon(CurrentDC,polygon->xy,polygon->n);
   }
   if (!bord & gcfill.lbStyle != BS_HOLLOW)
             DeleteObject(SelectObject(CurrentDC,lpCurPen));

   return TRUE;
}

/*___________________________________________________________________*/
/*
 *    SUBROUTINE IXSETTA(ALIGNH,ALIGNV)
 *    INTEGER ALIGNH : horizontal text alignment
 *    INTEGER ALIGNV : vertical text alignment
 *
 *    Set text alignment:
 *    if ALIGNH.EQ.2 align center
 *    if ALIGHH.EQ.3 align right
 *              else align left
 *
 *    if ALIGNV.EQ.1 align top
 *    if ALIGNV.EQ.2 align top
 *    if ALIGNV.EQ.3 align middle
 *              else align base
 */
void
  ixsetta( align_h, align_v )
int *align_h;
int *align_v;
{
  dwCommonTextAlign = 0;
  switch( *align_h ) {

  case 2:
    dwCommonTextAlign |= TA_CENTER;
    break;

  case 3:
    dwCommonTextAlign |= TA_RIGHT;
    break;

  default:
    dwCommonTextAlign |= TA_LEFT;
  }


  switch( *align_v ) {

  case 1:
  case 2:
    dwCommonTextAlign |= TA_TOP;
    align_vert = 1;
    break;

  case 3:
    dwCommonTextAlign |= TA_BASELINE;
    align_vert = 0;
    break;

  default:
    dwCommonTextAlign |= TA_BASELINE;
    align_vert = -1;
  }
}

/*___________________________________________________________________*/
/*
 *    FUNCTION IXSETTF(MODE,LENFNT,FONTNAME)
 *    INTEGER *MODE       : Option message
 *                 [0]     : = 1 - Italic
 *                 [1]     : = BOLD
 *                 [2]     : = Height of the character
 *                 [3]     : = Angle
 *                 [4]     : = Precision
 *    INTEGER LENFNT     : font name length
 *    CHARACTER*(*) FONT : font name
 *
 *    Set text font to specified name. This function returns 0 if
 *    the specified font is found, 1 if not.
 */
int
  ixsettf( mode, lenfnt,  fontname )
char *fontname;
int *mode;
int *lenfnt;
{
   HIGZFont.lfItalic = (BYTE) mode[0];
   HIGZFont.lfWeight = (LONG) mode[1]*100;
   HIGZFont.lfHeight = (LONG) mode[2]*1.1; // To account "tail"
 //  HIGZFont.lfWidth  = (LONG) mode[2]*1.2
   HIGZFont.lfEscapement   = (LONG)mode[3];
   HIGZFont.lfOutPrecision = 0; // (LONG)mode[4];
   strcpy(HIGZFont.lfFaceName,fontname);

   if (hdCommonFont != NULL) DeleteObject(hdCommonFont);
   hdCommonFont = CreateFontIndirect(&HIGZFont);

  return 0;
 }


/*___________________________________________________________________*/
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
   dwCommonTextColor = HIGZColorIndex((COLORREF)(*cindex));
}


/*___________________________________________________________________*/
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
  ixtext( mode, x, y, angle, mgn, lentxt, text )
char *text;
int *mode;
int *x, *y;
float *angle;
float *mgn;
int *lentxt;
{
  int dx = 0;
  int dy = 0;
  static struct {int mode,x,y,lentxt; float angle, mgn;  char *text;} text_msg;

  text_msg.mode = *mode;
  text_msg.x    = *x;
  text_msg.y    = *y;
  text_msg.angle = *angle;
  text_msg.mgn   = *mgn;
  text_msg.text  = text;
  text_msg.lentxt=*lentxt;

  HIGZDrawing(MAKEWPARAM(IX_TEXT,HIGZ_Text), &text_msg);

//  do_raise = 1;
}


/*___________________________________________________________________*/
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
  ixtxtl( w, h, lmess, mess )
char *mess;
int *lmess;
int *w;
int *h;
{
   static struct { int w,h, lmess; char *mess;} sText;
   HIGZDrawing(MAKEWPARAM(IX_TXTL,HIGZ_Text), &sText);
   *w = sText.w;
   *h = sText.h;
}


/*___________________________________________________________________*/
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
   static struct { int  x1, y1, x2, y2, mode;} box;

   box.mode  = *mode;
   box.x1    = min(*x1,*x2);
   box.x2    = max(*x1,*x2);
   box.y1    = min(*y1,*y2);
   box.y2    = max(*y1,*y2);

   HIGZDrawing(MAKEWPARAM(IX_BOX,HIGZ_Primitive), &box);

}

/*___________________________________________________________________*/
 LRESULT APIENTRY
       Wnd_BOX(hwnd, uMsg, wParam, lParam)
 HWND   hwnd;
 UINT   uMsg;
 WPARAM wParam;
 LPARAM lParam;
 {
  struct { int  x1, y1, x2, y2, mode;} *box;
  HGDIOBJ NewBrush, lpCurPen;
  static COLORREF CurTextColor;

  box = lParam;
  if (box->mode == 0) {
     if (box->x1 == box->x2 | box->y1 == box->y2) {
/*          Draw a line  instead the rectangle            */
         MoveToEx(CurrentDC,box->x1,box->y1,NULL);
         LineTo  (CurrentDC,box->x2,box->y2);
     }
     else {
         CurBrush = SelectObject(CurrentDC, GetStockObject(HOLLOW_BRUSH));
         Rectangle(CurrentDC,box->x1, box->y1, box->x2, box->y2);
         DeleteObject(SelectObject(CurrentDC,CurBrush));
     }
  }
  else {
//                             CurBrush = GetCurrentObject(CurrentDC,OBJ_BRUSH);
     if (gcfill.lbStyle == BS_PATTERN) {
         CurTextColor = SetTextColor(CurrentDC,HIGZColorIndex(gcfill.lbColor));
         FillRect(CurrentDC, (RECT*)box, hdCommonBrush);
         SetTextColor(CurrentDC,CurTextColor);
     }
     else
         FillRect(CurrentDC, (RECT*)box, hdCommonBrush);
  }
  return TRUE;
 }

/*___________________________________________________________________*/
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

   static struct { int x1, x2, y1, y2, nx, ny; int *ic;} cell;

   cell.x1 = *x1;
   cell.x2 = *x2;
   cell.y1 = *y1;
   cell.y2 = *y2;

   cell.nx = *nx;
   cell.ny = *ny;
   cell.ic = ic;

   cell.x1 = *x1;
   HIGZDrawing(MAKEWPARAM(IX_CA,HIGZ_Primitive), &cell);
}

/*___________________________________________________________________*/
 LRESULT APIENTRY
       Wnd_CA(hwnd, uMsg, wParam, lParam)
 HWND   hwnd;
 UINT   uMsg;
 WPARAM wParam;
 LPARAM lParam;
   {
    int i,j,icol,ix,iy,w,h,hh,current_icol;
    struct { int x1, x2, y1, y2, nx, ny; int *ic;} *cell;
    HBRUSH CurCABrush = NULL;
    RECT box;
    cell = lParam;

    current_icol = -1;
    w            = max((cell->x2-cell->x1)/(cell->nx),1);
    h            = max((cell->y1-cell->y2)/(cell->ny),1);
    ix           = cell->x1;

    box.bottom = cell->y1;
    box.top    = cell->y1;
    box.left   = cell->x1;
    box.right  = box.left+h;
    for ( i=0; i<cell->nx; i++ ) {
      box.top -= h;
      for ( j=0; j<cell->ny; j++ ) {
         icol = cell->ic[i+(cell->nx*j)];
         if(icol != current_icol){
            if (CurCABrush != NULL) DeleteObject(CurCABrush);
            CurCABrush = CreateSolidBrush(HIGZColorIndex(icol));
            current_icol = icol;
         }
         FillRect(CurrentDC, &box, CurCABrush);
         box.bottom = box.top;
         box.top -= h;
      }
      box.left = box.right;
      box.right += w;
    }

    DeleteObject(CurCABrush);
    return TRUE;
   }


/*___________________________________________________________________*/
/*
 *    SUBROUTINE IXDRMDE(MODE)
 *    INTEGER MODE : drawing mode
 *
 *            MODE=1 copy       R2_COPYPEN
 *            MODE=2 xor     -  R2_XORPEN
 *            MODE=3 invert  -  R2_NOT
 *
 *    Set the drawing mode
 */
void
  ixdrmde( mode )
  int *mode;
{
  static int OP[3] = {R2_COPYPEN, R2_XORPEN, R2_NOT};
  HIGZDrawing(MAKEWPARAM(IX_DRMDE,HIGZ_Attribute), OP[*mode]);
}


/*___________________________________________________________________*/
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
  int i;
}


/*___________________________________________________________________*/
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
 */
int
  ixreqlo( mode, ctyp, x, y )
int *mode, *ctyp, *x, *y;
{
  static struct {int mode, ctyp, x, y, button; HANDLE semaphore;} HIGZmouse;

  HIGZmouse.x    = *x;
  HIGZmouse.y    = *y;
  HIGZmouse.ctyp = *ctyp;
  HIGZmouse.mode = *mode;
  HIGZmouse.semaphore = CreateSemaphore(NULL, 0, 1, NULL);

  SendMessage(CurrentHIGZWindow,
      IX11_HIGZ_Input,MAKEWPARAM(IX_REQLO,HIGZ_Input),&HIGZmouse);

  WaitForSingleObject(HIGZmouse.semaphore, INFINITE);
  CloseHandle(HIGZmouse.semaphore);

  *x = HIGZmouse.x;
  *y = HIGZmouse.y;
  return HIGZmouse.button;

}

/*___________________________________________________________________*/
 LRESULT APIENTRY
       Wnd_REQLO(hwnd, uMsg, wParam, lParam)
 HWND   hwnd;
 UINT   uMsg;
 WPARAM wParam;
 LPARAM lParam;
   {
    /* Global for draw cursor utilities variables */
     static struct {int mode, ctyp, x, y, button; HANDLE semaphore;} *lpHIGZmouse;


     SetSystemPaletteUse(cHw->hdcWindow,SYSPAL_NOSTATIC);
     RealizePalette(cHw->hdcWindow);

     cHw->button_press =  0;
     cHw->button_up    =  0;
     switch (uMsg) {
       case WM_RBUTTONUP:
         cHw->button_up++;
       case WM_MBUTTONUP:
         cHw->button_up++;
       case WM_LBUTTONUP:
         cHw->button_up++;
         cHw->button_press++;
       case WM_RBUTTONDOWN:
         cHw->button_press++;
       case WM_MBUTTONDOWN:
         cHw->button_press++;
       case WM_LBUTTONDOWN:
         cHw->button_press++;
       case WM_MOUSEMOVE:

              /* Clean an old position */

        DrawHIGZCursor(lpHIGZmouse->ctyp);

             /* Plot new cursor position */

        cHw->loc.x = (LONG) (MAKEPOINTS(lParam).x);
        cHw->loc.y = (LONG) (MAKEPOINTS(lParam).y);
        DPtoLP(CurrentDC,&(cHw->loc),1);
        DrawHIGZCursor(lpHIGZmouse->ctyp);

  /*  Reset system cursor near the bord id frame */

         if (lpHIGZmouse->mode) {
           if      (cHw->button_press == 0)
                       cHw->button_press = -1;
           else if (cHw->button_press == 4)
                       cHw->button_press = cHw->button_up+10;
           RestoreHIGZ(lpHIGZmouse->ctyp);
           lpHIGZmouse->x = cHw->loc.x ;  lpHIGZmouse->y = cHw->loc.y ;
           lpHIGZmouse->button = cHw->button_press;

//           RestoreDC(CurrentDC,-1);
           ReleaseSemaphore(lpHIGZmouse->semaphore,1L,NULL);

          }
         else if (cHw->button_press > 0 & cHw->button_up ==0) {
           RestoreHIGZ(lpHIGZmouse->ctyp);
           lpHIGZmouse->x = cHw->loc.x ;  lpHIGZmouse->y = cHw->loc.y;
           lpHIGZmouse->button = cHw->button_press;

           ReleaseSemaphore(lpHIGZmouse->semaphore,1L,NULL);
          }

         break;
       case IX11_HIGZ_Input:
         lpHIGZmouse = lParam;
         cHw->loc.x  = lpHIGZmouse->x; cHw->loc.y  =  lpHIGZmouse->y;
         cHw->locp.x = lpHIGZmouse->x; cHw->locp.y =  lpHIGZmouse->y;

         HIGZCursorInit(hwnd,lpHIGZmouse->ctyp);
         break;
       default:
         break;
       }
}

     DrawHIGZCursor( ctyp)
     int ctyp;
     {
        int radius, CurMxX, CurMxY;
        POINT loc,locp;
        loc.x = cHw->loc.x;
        loc.y = cHw->loc.y;

        locp.x = cHw->locp.x;
        locp.y = cHw->locp.y;

        CurMxX = cHw->HIGZWindowSize.right;
        CurMxY = cHw->HIGZWindowSize.bottom;

        switch ( ctyp ) {

        case 1 : /*  Default HIGZ window cursor is CROSS  -> do nothing here */
                 /*
                  *  MoveToEx(CurrentDC,max(0,loc.x-10),     loc.y,NULL);
                  *  LineTo  (CurrentDC,min(CurMxX,loc.x+10),loc.y);

                  *  MoveToEx(CurrentDC,loc.x,max(0,loc.y-10)     ,NULL);
                  *  LineTo  (CurrentDC,loc.x,min(CurMxY,loc.y+10));
                  */
                 break;

        case 2 : MoveToEx(CurrentDC,0,     loc.y,NULL);
                 LineTo  (CurrentDC,CurMxX,loc.y);

                 MoveToEx(CurrentDC,loc.x,0,    NULL);
                 LineTo  (CurrentDC,loc.x,CurMxY);

                 break;

        case 3 : radius = (int) sqrt((double)((loc.x-locp.x)*(loc.x-locp.x)+
                                              (loc.y-locp.y)*(loc.y-locp.y)));
                 Pie(CurrentDC,locp.x-radius,locp.y-radius,
                               locp.x+radius,locp.y+radius,
                               locp.x-radius,locp.y-radius,
                               locp.x-radius,locp.y-radius);
                 break;

        case 4 : MoveToEx(CurrentDC,loc.x, loc.y, NULL);
                 LineTo  (CurrentDC,locp.x,locp.y);
                 break;

        case 5 : Rectangle(CurrentDC,locp.x, locp.y, loc.x, loc.y);
                 break;
        default:
                 break;
      }
   }

/*___________________________________________________________________*/
   HIGZCursorInit(hwnd,ctyp)
   HWND hwnd;
   int ctyp;
   {
        SaveDC(CurrentDC);
   /* Create Brush or Pen to draw HIGZ graphics cursor */

        SelectObject(CurrentDC,hdCursorPen);
        if (ctyp == 3 | ctyp == 5)
            SelectObject(CurrentDC, hdCursorBrush);

   /* Suspend current clipping */

        SelectClipRgn(CurrentDC, NULL);


   /* Set a new mix mode to XOR */

        SetROP2(CurrentDC,R2_NOT);

   /* Set Cursor on the screen */
        DrawHIGZCursor(ctyp);
        cHw->MouseInit = ctyp;
    }

/*___________________________________________________________________*/
RestoreHIGZ(int ctyp)
  {
    cHw->MouseInit = 0;
/* Clean cursor off the screen */
    DrawHIGZCursor(ctyp);
    RestoreDC(CurrentDC,-1);
  }


/*___________________________________________________________________*/
/*
 *    INTEGER FUNCTION IXREQST(X,Y,TEXT)
 *    INTEGER X,Y        : position where text is displayed
 *    CHARACTER*(*) TEXT : text displayed (input), edited text (output)
 *
 *    Request string:
 *    text is displayed and can be edited with Emacs-like keybinding
 *    return termination code (0 for ESC, 1 for RETURN)
 */
int
 ixreqst( x, y, len_text,  text )
char *text;
int *len_text;
int *x, *y;
{
   static struct {int x, y, len_tex; char *text;
                  int BreakKey; HANDLE semaphore;}
                  HIGZText;
   HIGZText.x = *x;
   HIGZText.y = *y;
   HIGZText.len_tex = *len_text;
   HIGZText.text    = text;
   HIGZText.BreakKey = -1;
   HIGZText.semaphore = CreateSemaphore(NULL, 0, 1, NULL);

   SendMessage(CurrentHIGZWindow,
         IX11_HIGZ_Input,MAKEWPARAM(IX_REQST,HIGZ_Input),&HIGZText);

   WaitForSingleObject(HIGZText.semaphore, INFINITE);
   CloseHandle(HIGZText.semaphore);

   return HIGZText.BreakKey;

}

/*___________________________________________________________________*/
 int CharWidth(ch)
 char ch;
    {int LenOfChar;
       GetCharWidth(CurrentDC, (UINT) ch, (UINT) ch,  &LenOfChar);
       return LenOfChar;}

/*___________________________________________________________________*/
 LRESULT APIENTRY
         Wnd_REQST(hwnd, uMsg, wParam, lParam)
 HWND   hwnd;
 UINT   uMsg;
 WPARAM wParam;
 LPARAM lParam;
   {
#define ixg(x) (ix0+x)
#define iyg(y) (iy0+y)
 /*             Application message                     */
    static struct {int x, y, len_tex; char *text;
                  int BreakKey; HANDLE semaphore;}
                  *lpHIGZText;

   static char ch;        // entered symbol
   static char *lpInstr;  // Input line buffer
   static int nCur,       // Current text cursor postion
              lStr,       // Current length of the entered string
                pt,       // Current symbol position
              BreakKey,   // Code of the entered "End of Line" terminator
              nCharWidth, // Width of the current symbol
              LenLine,    // Pixel length of the entered string
              ix0,iy0;
    static    char *Instr;     // temporary buffer


   static int insert;
   int i;

      switch (uMsg) {
       case WM_CHAR:
         switch(wParam) {

           case 0x0A: /* line feed       */
           case 0x0D: /* carriage return */
                     (lpHIGZText->BreakKey)++;
           case 0x1B: /* escape          */
                     (lpHIGZText->BreakKey)++;

                     cHw->SetTextInput = FALSE;
                     HideCaret(hwnd);
                     DestroyCaret();
                     if (lpHIGZText->len_tex > 0) {
                       for (i=0;i<lStr;i++)                     lpInstr[i] = Instr[i];
                       for (i=lStr; i<lpHIGZText->len_tex; i++) lpInstr[i]=' ';
                       free(Instr);
                     }


/*          End of text input from keyboard
 *            and restore previous active window
 */
              SetActiveWindow(ConWindow);
              SetFocus(ConWindow);

              ReleaseSemaphore(lpHIGZText->semaphore,1L,NULL);
              break;

           case 0x09: /* Tab */
           case 0x08: /* backspace */
              break;
           default:   /* all other letters */
             ch = (TCHAR) wParam;
             HideCaret(hwnd);

                 if (nCur != lStr){/*             Clear the old tail of the string            */
                                   COLORREF CurTextCol;
                                   CurTextCol = SetTextColor(CurrentDC,GetBkColor(CurrentDC));
                                   TextOut(CurrentDC,ixg(ixText),iyg(iyText),
                                           &Instr[nCur],lStr-nCur);
                                   SetTextColor(CurrentDC,CurTextCol);
                                  }
                 if (insert==1)    {/*                      Shift string                       */
                                   for (i=lStr; i>nCur; i--) Instr[i] = Instr[i-1];
                                   /*   Increasing of the acctual length                       */
                                   lStr++;
                                   Instr[lStr] = 0x0;
                                  }
                 else if (nCur == lStr) lStr++;
                 else // reduce length of string
                      LenLine -= CharWidth(Instr[nCur]);

/*             Put enreted symbol in place                 */
             Instr[nCur] = ch;
/*            Out the  new tail of the text                 */
             TextOut(CurrentDC,ixg(ixText),iyg(iyText),
                     &Instr[nCur],lStr-nCur);

             LenLine += CharWidth(ch); // pixel length of entered string
             ixText  += CharWidth(ch); // Reset Graphics position for next input
             nCur++;
             break;
          }
         SetCaretPos(ixg(ixText),iyg(iyText-(cHw->dwAscent) ));
         ShowCaret(hwnd);
         return 0;
       case WM_KEYDOWN:
         HideCaret(hwnd);
         switch(wParam) {
           case VK_LEFT:
               nCur--;
               ixText -= CharWidth(Instr[nCur]);
               break;
           case VK_RIGHT:
               ixText += CharWidth(Instr[nCur]);
               nCur++;
               break;
           case VK_HOME:
               ixText = 0;
               nCur = 0;
               break;
           case VK_END:
               ixText = LenLine;
               nCur = lStr;
               break;
           case VK_INSERT:
                insert = -insert;
                break;
           case VK_DELETE:
/*             Clear the old tail of the string            */
                          { COLORREF CurTextCol;
                            CurTextCol = SetTextColor(CurrentDC,GetBkColor(CurrentDC));
                            TextOut(CurrentDC,ixg(ixText),iyg(iyText), &Instr[nCur],lStr-nCur);
                            SetTextColor(CurrentDC,CurTextCol);
                          }
/*           Reduce current lenght of string               */
                   LenLine -= CharWidth(Instr[nCur]);

                    for (i=nCur; i<lStr;i++)
                      Instr[i] = Instr[i+1];
                    lStr = max(lStr-1, nCur);
                    if (lStr != nCur)
                       TextOut(CurrentDC,ixg(ixText),iyg(iyText), &Instr[nCur],lStr-nCur);
                else
                    Beep(1000L,100L);
                 break;

           case VK_BACK:
               nCur--;
               if (nCur >= 0) {
/*             Clear the old tail of the string            */
                    ixText -= CharWidth(Instr[nCur]);
                    { COLORREF CurTextCol;
                      CurTextCol = SetTextColor(CurrentDC,GetBkColor(CurrentDC));
                      TextOut(CurrentDC,ixg(ixText),iyg(iyText), &Instr[nCur],lStr-nCur);
                      SetTextColor(CurrentDC,CurTextCol);
                    }
/*             Reduce current lenght of string               */
                   LenLine -= CharWidth(Instr[nCur]);

                    for (i=nCur; i<lStr;i++)
                      Instr[i] = Instr[i+1];
                    lStr = max(lStr-1, nCur);
                    if (lStr != nCur)
                       TextOut(CurrentDC,ixg(ixText),iyg(iyText), &Instr[nCur],lStr-nCur);
               } else {
                 Beep(1000L,100L);
                 nCur = 0;
               }
               break;
        default:
              return  DefWindowProc(hwnd,uMsg, wParam, lParam);
         }
        SetCaretPos(ixg(ixText),iyg(iyText-(cHw->dwAscent)));
        ShowCaret(hwnd);
        return 0;  // User procedure SHOULD return ZERO if it is process
       case IX11_HIGZ_Input:
           lpHIGZText = lParam;
 /*       Write  TEXT to the pointed position on the screen */
           ix0 =  lpHIGZText->x;
           iy0 =  lpHIGZText->y;
           ixText = 0;
           iyText = 0;
/*        Init of string input                 */
          lpInstr = lpHIGZText->text;
          nCur    = 0;
          lStr    = 0;
          insert  = 1;

          if (lpHIGZText->len_tex >0 ) {
            Instr = malloc(lpHIGZText->len_tex);
            Instr[0] = 0x0;
          }
          LenLine = 0;


/*         Create and set Caret             */

           CreateCaret(hwnd, (HBITMAP) 1, cHw->dwCharX, cHw->dwAscent);
           SetCaretPos(ixg(ixText),iyg(iyText-(cHw->dwAscent)));
           ShowCaret(hwnd);

           SetFocus(hwnd);
           ConWindow = SetActiveWindow(hwnd);

           cHw->SetTextInput = TRUE;

           break;
       default: break;
     }
   }


/*___________________________________________________________________*/
/*      SUBROUTINE IGERR(ERRTEX,RNAME)
*.===========>
*.
*.   This prints on the screen the error message (ERRTEX) and
*. the routine name (RNAME). This routine sets also IQUEST(1)
*. to 1. If RNAME equal ' ' only a warning is printed and
*. IQUEST(1) is sets to 0.
*.
*. _Input parameters:
*.
*.  INTEGER ERRTEX : Error text .
*.  CHARACTER RNAME : Routine name .
*.
*.
*/

 void igerr(errtex,rname,lerr,lname)
   char *errtex, *rname;
   int lerr, lname;
   {
    struct {
        int iquest[100];
    } quest_;

#define quest_1 quest_

     LPCTSTR lpszText;  /* address of text in message box       */
     LPCTSTR lpszTitle; /* address of title of message box      */
     lpszText = malloc(lerr);
     strcpy(lpszText,string(errtex,lerr));
     if (lname <= 1 & rname[0] == ' ') {
     /* This is a warning message */
       lpszTitle = malloc(15);
       strcpy(lpszTitle,"*** Warning ***");
       MessageBox(NULL, lpszText, lpszTitle,
                  MB_OK | MB_SETFOREGROUND | MB_ICONEXCLAMATION);
       quest_1.iquest[0] = 0;
     }
     else {
     /* This is an error message */
       lpszTitle = malloc(lname+18);
       strcpy(lpszTitle,"*** ERROR in ");
       strcat(lpszTitle,string(rname,lname));
       strcat(lpszTitle," ***");
       MessageBox(NULL, lpszText, lpszTitle,
                  MB_OK | MB_SETFOREGROUND | MB_ICONHAND);
       quest_1.iquest[0] = 1;
     }
     free(lpszTitle);
     free(lpszText);
   }

/*___________________________________________________________________*/
 wrtlst(wid, mess, lmess)
 HWND *wid;
 char *mess;
 int *lmess;
 {
   SendMessage(*wid,LB_ADDSTRING,0, (LPARAM) string(mess,*lmess));
 }

/*___________________________________________________________________*/
 clrlst(wid)
 HWND *wid;
 {
   int n;
   if (n = SendMessage(*wid,LB_GETCOUNT,0,0)   !=   LB_ERR)
           SendMessage(*wid,LB_DELETESTRING,(WPARAM) n, 0);
 }

  ixbord()
  {bord = ~bord;}

