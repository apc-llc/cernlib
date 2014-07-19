/*
 * $Id: tkhigz.c,v 1.1.1.1 1996/02/14 13:10:26 mclareni Exp $
 *
 * $Log: tkhigz.c,v $
 * Revision 1.1.1.1  1996/02/14 13:10:26  mclareni
 * Higz
 *
 */
#include "higz/pilot.h"
#if defined(CERNLIB_TKTCL)
/*CMZ :  1.23/06 22/11/95  10.12.40  by  O.Couet*/
/*-- Author :    O.Couet   22/11/95*/
/*
 * tkHigz.c --
 *
 *    This module implements a widget for higz to draw in.
 *    Several widgets can exist but only one is the active one
 *    and will be used to draw in.
 *    Based on the Square widget.
 *
 * Copyright (c) 1991-1993 The Regents of the University of California.
 * All rights reserved.
 *
 * Permission is hereby granted, without written agreement and without
 * license or royalty fees, to use, copy, modify, and distribute this
 * software and its documentation for any purpose, provided that the
 * above copyright notice and the following two paragraphs appear in
 * all copies of this software.
 *
 * IN NO EVENT SHALL THE UNIVERSITY OF CALIFORNIA BE LIABLE TO ANY PARTY FOR
 * DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES ARISING OUT
 * OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN IF THE UNIVERSITY OF
 * CALIFORNIA HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 * THE UNIVERSITY OF CALIFORNIA SPECIFICALLY DISCLAIMS ANY WARRANTIES,
 * INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
 * AND FITNESS FOR A PARTICULAR PURPOSE.  THE SOFTWARE PROVIDED HEREUNDER IS
 * ON AN "AS IS" BASIS, AND THE UNIVERSITY OF CALIFORNIA HAS NO OBLIGATION TO
 * PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR MODIFICATIONS.
 */

#include "higz/cfortran.h"
#if !defined(VMS) && !defined(_WINDOWS)
#include "higz/tkConfig.h"
#endif
#include "higz/tk.h"

/*
 * A data structure of the following type is kept for each higz
 * widget managed by this file:
 */

typedef struct {
    Tk_Window tkwin;    /* Window that embodies the higz.  NULL
                * means window has been deleted but
                * widget record hasn't been cleaned up yet. */
    Display *display;      /* X's token for the window's display. */
    Tcl_Interp *interp;    /* Interpreter associated with widget. */

    /*
     * Information used when displaying widget:
     */

    Tk_3DBorder bgBorder;  /* Used for drawing background. */
    Tk_3DBorder fgBorder;  /* For drawing by higz. */
    int relief;         /* Indicates whether window as a whole is
                * raised, sunken, or flat. */
    GC gc;        /* Graphics context for copying from
                * off-screen pixmap onto screen. */
    int updatePending;     /* Non-zero means a call to HigzDisplay
                * has already been scheduled. */
    int height;         /* The height of the plot window */
    int width;       /* The width of the plot window */
    int higzwin;     /* Higz win id */
} Higz;

/*
 * Information used for argv parsing.
 */

static Tk_ConfigSpec configSpecs[] = {
    {TK_CONFIG_BORDER, "-background", "background", "Background",
      "#cdb79e", Tk_Offset(Higz, bgBorder), TK_CONFIG_COLOR_ONLY},
    {TK_CONFIG_BORDER, "-background", "background", "Background",
      "white", Tk_Offset(Higz, bgBorder), TK_CONFIG_MONO_ONLY},
    {TK_CONFIG_SYNONYM, "-bg", "background", (char *) NULL,
      (char *) NULL, 0, 0},
    {TK_CONFIG_SYNONYM, "-fg", "foreground", (char *) NULL,
      (char *) NULL, 0, 0},
    {TK_CONFIG_BORDER, "-foreground", "foreground", "Foreground",
      "#b03060", Tk_Offset(Higz, fgBorder), TK_CONFIG_COLOR_ONLY},
    {TK_CONFIG_BORDER, "-foreground", "foreground", "Foreground",
      "black", Tk_Offset(Higz, fgBorder), TK_CONFIG_MONO_ONLY},
    {TK_CONFIG_PIXELS, "-height", "height", "Height",
      "400", Tk_Offset(Higz, height), 0},
    {TK_CONFIG_PIXELS, "-width", "width", "Width",
      "400", Tk_Offset(Higz, width), 0},
    {TK_CONFIG_END, (char *) NULL, (char *) NULL, (char *) NULL,
      (char *) NULL, 0, 0}
};

/*
 * Forward declarations for procedures defined later in this file:
 */

static int     HigzConfigure _ANSI_ARGS_((Tcl_Interp *interp,
                Higz *higzPtr, int argc, char **argv,
                int flags));
static void    HigzDestroy _ANSI_ARGS_((ClientData clientData));
static void    HigzDisplay _ANSI_ARGS_((ClientData clientData));
static void    HigzEventProc _ANSI_ARGS_((ClientData clientData,
                XEvent *eventPtr));
static int     HigzWidgetCmd _ANSI_ARGS_((ClientData clientData,
                Tcl_Interp *, int argc, char **argv));

PROTOCCALLSFFUN0(INT,OPENHIGZWINDOW,openhigzwindow);
#define OPENHIGZWINDOW \
      CCALLSFFUN0(OPENHIGZWINDOW,openhigzwindow)

#define SELECTHIGZWINDOW(I) \
      CCALLSFSUB1(SELECTHIGZWINDOW,selecthigzwindow,INT,I)


/*
 * Global variable to keep track of currently active Higz widget
 * (yes we should not do global vars, maybe removed later
 */

static char *  higz_current;


/*
 *--------------------------------------------------------------
 *
 * HigzCmd --
 *
 *    This procedure is invoked to process the "higz" Tcl
 *    command.  It creates a new "higz" widget.
 *
 * Results:
 *    A standard Tcl result.
 *
 * Side effects:
 *    A new widget is created and configured.
 *
 *--------------------------------------------------------------
 */

int
HigzCmd(clientData, interp, argc, argv)
    ClientData clientData; /* Main window associated with
                * interpreter. */
    Tcl_Interp *interp;    /* Current interpreter. */
    int argc;        /* Number of arguments. */
    char **argv;     /* Argument strings. */
{
    Tk_Window main = (Tk_Window) clientData;
    Higz *higzPtr;
    Tk_Window tkwin;

    if (argc < 2) {
      Tcl_AppendResult(interp, "wrong # args:  should be \"",
         argv[0], " pathName ?options?\"", (char *) NULL);
      return TCL_ERROR;
    }

    tkwin = Tk_CreateWindowFromPath(interp, main, argv[1], (char *) NULL);
    if (tkwin == NULL) {
      return TCL_ERROR;
    }
    Tk_SetClass(tkwin, "Higz");

    /*
     * Allocate and initialize the widget record.
     */

    higzPtr = (Higz *) ckalloc(sizeof(Higz));
    higzPtr->tkwin = tkwin;
    higzPtr->display = Tk_Display(tkwin);
    higzPtr->interp = interp;
    higzPtr->bgBorder = NULL;
    higzPtr->fgBorder = NULL;
    higzPtr->relief = TK_RELIEF_FLAT;
    higzPtr->gc = None;
    higzPtr->updatePending = 0;

    Tk_MakeWindowExist( tkwin );

    Tk_CreateEventHandler(higzPtr->tkwin, ExposureMask|StructureNotifyMask,
          HigzEventProc, (ClientData) higzPtr);
    Tcl_CreateCommand(interp, Tk_PathName(higzPtr->tkwin), HigzWidgetCmd,
          (ClientData) higzPtr, (void (*)()) NULL);
    if (HigzConfigure(interp, higzPtr, argc-2, argv+2, 0) != TCL_OK) {
      Tk_DestroyWindow(higzPtr->tkwin);
      return TCL_ERROR;
    }

    ixsdswi( higzPtr->display, Tk_WindowId(higzPtr->tkwin) );
    higzPtr->higzwin = OPENHIGZWINDOW;

    interp->result = Tk_PathName(higzPtr->tkwin);
    return TCL_OK;
}

/*
 *--------------------------------------------------------------
 *
 * HigzWidgetCmd --
 *
 *    This procedure is invoked to process the Tcl command
 *    that corresponds to a widget managed by this module.
 *    See the user documentation for details on what it does.
 *
 * Results:
 *    A standard Tcl result.
 *
 * Side effects:
 *    See the user documentation.
 *
 *--------------------------------------------------------------
 */

static int
HigzWidgetCmd(clientData, interp, argc, argv)
    ClientData clientData;    /* Information about higz widget. */
    Tcl_Interp *interp;       /* Current interpreter. */
    int argc;           /* Number of arguments. */
    char **argv;        /* Argument strings. */
{
    Higz *higzPtr = (Higz *) clientData;
    int result = TCL_OK;
    int length;
    char c;

    if (argc < 2) {
      Tcl_AppendResult(interp, "wrong # args: should be \"",
         argv[0], " option ?arg arg ...?\"", (char *) NULL);
      return TCL_ERROR;
    }
    Tk_Preserve((ClientData) higzPtr);
    c = argv[1][0];
    length = strlen(argv[1]);
    if ((c == 'c') && (strncmp(argv[1], "configure", length) == 0)) {
      if (argc == 2) {
          result = Tk_ConfigureInfo(interp, higzPtr->tkwin, configSpecs,
             (char *) higzPtr, (char *) NULL, 0);
      } else if (argc == 3) {
          result = Tk_ConfigureInfo(interp, higzPtr->tkwin, configSpecs,
             (char *) higzPtr, argv[2], 0);
      } else {
          result = HigzConfigure(interp, higzPtr, argc-2, argv+2,
             TK_CONFIG_ARGV_ONLY);
      }
    } else if ((c == 's') && (strncmp(argv[1], "select", length) == 0)) {
      if ( argc != 2 ) {
          Tcl_AppendResult(interp, "wrong # args: should be \"",
             argv[0], " select\"", (char *) NULL);
          goto error;
      }

      if ( higz_current != 0 ) {
         strcpy( interp->result, higz_current );
         free( higz_current );
      }

      higz_current = strdup( Tk_PathName( higzPtr->tkwin ) );
      SELECTHIGZWINDOW( higzPtr->higzwin );

    } else if ((c == 'g') && (strncmp(argv[1], "geometry", length) == 0)) {
      Window   root;
      int   xval, yval;
      unsigned int   wval, hval, border, depth;

      XGetGeometry( higzPtr->display, Tk_WindowId(higzPtr->tkwin),
         &root, &xval, &yval, &wval, &hval, &border, &depth );

      printf( "x=%d y=%d w=%d h=%d b=%d d=%d\n", xval, yval, wval, hval,
         border, depth );
    } else {
      Tcl_AppendResult(interp, "bad option \"", argv[1],
         "\":  must be configure, position, or size", (char *) NULL);
      goto error;
    }
    if (!higzPtr->updatePending) {
      Tk_DoWhenIdle(HigzDisplay, (ClientData) higzPtr);
      higzPtr->updatePending = 1;
    }
    Tk_Release((ClientData) higzPtr);
    return result;

    error:
    Tk_Release((ClientData) higzPtr);
    return TCL_ERROR;
}

/*
 *----------------------------------------------------------------------
 *
 * HigzConfigure --
 *
 *    This procedure is called to process an argv/argc list in
 *    conjunction with the Tk option database to configure (or
 *    reconfigure) a higz widget.
 *
 * Results:
 *    The return value is a standard Tcl result.  If TCL_ERROR is
 *    returned, then interp->result contains an error message.
 *
 * Side effects:
 *    Configuration information, such as colors, border width,
 *    etc. get set for higzPtr;  old resources get freed,
 *    if there were any.
 *
 *----------------------------------------------------------------------
 */

static int
HigzConfigure(interp, higzPtr, argc, argv, flags)
    Tcl_Interp *interp;       /* Used for error reporting. */
    Higz *higzPtr;         /* Information about widget. */
    int argc;           /* Number of valid entries in argv. */
    char **argv;        /* Arguments. */
    int flags;          /* Flags to pass to
                   * Tk_ConfigureWidget. */
{
    if (Tk_ConfigureWidget(interp, higzPtr->tkwin, configSpecs,
          argc, argv, (char *) higzPtr, flags) != TCL_OK) {
      return TCL_ERROR;
    }

    /*
     * Set the background for the window and create a graphics context
     * for use during redisplay.
     */

    Tk_SetWindowBackground(higzPtr->tkwin,
          Tk_3DBorderColor(higzPtr->bgBorder)->pixel);
    if ( higzPtr->gc == None ) {
      XGCValues gcValues;
      gcValues.function = GXcopy;
      gcValues.graphics_exposures = False;
      higzPtr->gc = Tk_GetGC(higzPtr->tkwin,
         GCFunction|GCGraphicsExposures, &gcValues);
    }

    /*
     * Register the desired geometry for the window.  Then arrange for
     * the window to be redisplayed.
     */

    Tk_GeometryRequest(higzPtr->tkwin, higzPtr->width, higzPtr->height);
    if (!higzPtr->updatePending) {
      Tk_DoWhenIdle(HigzDisplay, (ClientData) higzPtr);
      higzPtr->updatePending = 1;
    }
    return TCL_OK;
}

/*
 *--------------------------------------------------------------
 *
 * HigzEventProc --
 *
 *    This procedure is invoked by the Tk dispatcher for various
 *    events on higzs.
 *
 * Results:
 *    None.
 *
 * Side effects:
 *    When the window gets deleted, internal structures get
 *    cleaned up.  When it gets exposed, it is redisplayed.
 *
 *--------------------------------------------------------------
 */

static void
HigzEventProc(clientData, eventPtr)
    ClientData clientData; /* Information about window. */
    XEvent *eventPtr;      /* Information about event. */
{
    Higz *higzPtr = (Higz *) clientData;
    Tk_Window tkwin = higzPtr->tkwin;

#define IGRSIZ(IWKID) \
      CCALLSFSUB1(IGRSIZ,igrsiz,INT,IWKID)

    if (eventPtr->type == Expose) {
      if (!higzPtr->updatePending) {
          Tk_DoWhenIdle(HigzDisplay, (ClientData) higzPtr);
          higzPtr->updatePending = 1;
      }
    } else if (eventPtr->type == ConfigureNotify) {

      Window   root;
      int   xval, yval, wiid;
      unsigned int   wval, hval, border, depth;
      float rval;

      XGetGeometry( higzPtr->display, Tk_WindowId(higzPtr->tkwin),
         &root, &xval, &yval, &wval, &hval, &border, &depth );

      IGRSIZ( higzPtr->higzwin );

      if (!higzPtr->updatePending) {
          Tk_DoWhenIdle(HigzDisplay, (ClientData) higzPtr);
          higzPtr->updatePending = 1;
      }

    } else if (eventPtr->type == DestroyNotify) {
      Tcl_DeleteCommand(higzPtr->interp, Tk_PathName(higzPtr->tkwin));
      higzPtr->tkwin = NULL;
      if (higzPtr->updatePending) {
          Tk_CancelIdleCall(HigzDisplay, (ClientData) higzPtr);
      }
      Tk_EventuallyFree((ClientData) higzPtr, HigzDestroy);
    }
}

/*
 *--------------------------------------------------------------
 *
 * HigzDisplay --
 *
 *    This procedure redraws the contents of a higz window.
 *    It is invoked as a do-when-idle handler, so it only runs
 *    when there's nothing else for the application to do.
 *
 * Results:
 *    None.
 *
 * Side effects:
 *    Information appears on the screen.
 *
 *--------------------------------------------------------------
 */

static void
HigzDisplay(clientData)
    ClientData clientData; /* Information about window. */
{
    Higz *higzPtr = (Higz *) clientData;
    Tk_Window tkwin = higzPtr->tkwin;
    Drawable d;

    higzPtr->updatePending = 0;
    if (!Tk_IsMapped(tkwin)) {
      return;
    } else {
#ifdef _WINDOWS
#define IXUPDWI(I) \
      CCALLSFSUB1(IXUPDWI,ixupdwi,INT,I)
      IXUPDWI( 1 );
#endif
    }
}

/*
 *----------------------------------------------------------------------
 *
 * HigzDestroy --
 *
 *    This procedure is invoked by Tk_EventuallyFree or Tk_Release
 *    to clean up the internal structure of a higz at a safe time
 *    (when no-one is using it anymore).
 *
 * Results:
 *    None.
 *
 * Side effects:
 *    Everything associated with the higz is freed up.
 *
 *----------------------------------------------------------------------
 */

static void
HigzDestroy(clientData)
    ClientData clientData; /* Info about higz widget. */
{
    Higz *higzPtr = (Higz *) clientData;

#define ICLWK(IWK) CCALLSFSUB1(ICLWK,iclwk,INT,IWK)

    ICLWK(higzPtr->higzwin);
    Tk_FreeOptions(configSpecs, (char *) higzPtr, higzPtr->display, 0);
    if (higzPtr->gc != None) {
      Tk_FreeGC(higzPtr->display, higzPtr->gc);
    }
    ckfree((char *) higzPtr);
}
#endif
