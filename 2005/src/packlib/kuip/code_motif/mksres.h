/*
 * $Id: mksres.h,v 1.2 1996/10/24 12:13:40 cremel Exp $
 *
 * $Log: mksres.h,v $
 * Revision 1.2  1996/10/24 12:13:40  cremel
 * Version 2.07/10
 * Add resource higzMessGeometry to have control on the size and position of
 * the "HigzOutput" window (e.g. for the locator). To change the size
 * and position of this window one must add in his .Xdefaults (or .Xresources)
 * file the 2 following lines:
 *
 * Paw++*kuipScroll1.defaultPosition: False
 * Paw++*higzMessGeometry: 200x250+650+650
 *
 * Revision 1.1.1.1  1996/03/08 15:33:12  mclareni
 * Kuip
 *
 */
#ifndef _mksres_
#define _mksres_

/* General application resources */
typedef struct {
    int        dc_interval;           /* double click interval */
    String     Help_font;             /* font for help text */
    Boolean    double_buffer;         /* double buffer mode */
    int        bell_vol;              /* bell volume */
    int        center_command;        /* command panel position : */
                                      /*  1 --> center (True) */
                                      /*  0 --> normal (cursor position) */
                                      /* -1 --> (0,0) */
    Boolean    panel_interactive;     /* allow panel editing and close */
    Boolean    no_kxterm;             /* no kxterm window (normal xterm) */
    Boolean    no_mbrowser;           /* no main browser window */
    String     higzmess_geometry;     /* geometry of HigzOutput window */
    String     kxterm_geometry;       /* geometry of kxterm */
    String     kxterm_text_font;      /* font for the text widgets in kxterm */
    String     kxterm_font;           /* font for kxterm */
    /* Cursor Fonts: full list can be found in /usr/include/X11/cursorfont.h */
    int        main_cs;               /* cursor font for main window */
    int        graph_cs;              /* cursor font for graphics window */
    int        input_cs;              /* cursor font for question/input */
    int        wait_cs;               /* cursor font for wait */
    int        help_cs;               /* cursor font for help */
    int        busy_cs;               /* cursor font for program busy */
    int        blck_cs;               /* cursor font for blocked input */
    int        selc_cs;               /* cursor font for file selection */
    String     scrolled_cmdpanel;     /* scrolled window for command panel */
                                      /* "auto", "always" or "never" */
} SresRec, *SresRecPtr;

/* #define XmNdoubleClickInterval "doubleClickInterval" */
#define XmNhelpFont         "helpFont"
#define XmNdoubleBuffer     "doubleBuffer"
#define XmNgBell            "gBell"
#define XmNcenterCommand    "centerCommand"
#define XmNpanelInteractive "panelInteractive"
#define XmNnoKxterm         "noKxterm"
#define XmNnoMBrowser       "noMBrowser"
#define XmNhigzMessGeometry "higzMessGeometry"
#define XmNkxtermGeometry   "kxtermGeometry"
#define XmNkxtermTextFont   "kxtermTextFont"
#define XmNkxtermFont       "kxtermFont"
#define XmNmainCursor       "mainCursor"
#define XmNgraphCursor      "graphCursor"
#define XmNinputCursor      "inputCursor"
#define XmNwaitCursor       "waitCursor"
#define XmNhelpCursor       "helpCursor"
#define XmNbusyCursor       "busyCursor"
#define XmNblckCursor       "blckCursor"
#define XmNselcCursor       "selcCursor"
#define XmNscrolledCmdPanel "scrolledCmdPanel"

/* #define XmCDoubleClickInterval "DoubleClickInterval" */
#define XmCHelpFont         "HelpFont"
#define XmCDoubleBuffer     "DoubleBuffer"
#define XmCGBell            "GBell"
#define XmCCenterCommand    "CenterCommand"
#define XmCPanelInteractive "PanelInteractive"
#define XmCNoKxterm         "NoKxterm"
#define XmCNoMBrowser       "NoMBrowser"
#define XmCHigzMessGeometry "HigzMessGeometry"
#define XmCKxtermGeometry   "KxtermGeometry"
#define XmCKxtermTextFont   "KxtermTextFont"
#define XmCKxtermFont       "KxtermFont"
#define XmCMainCursor       "MainCursor"
#define XmCGraphCursor      "GraphCursor"
#define XmCInputCursor      "InputCursor"
#define XmCWaitCursor       "WaitCursor"
#define XmCHelpCursor       "HelpCursor"
#define XmCBusyCursor       "BusyCursor"
#define XmCBlckCursor       "BlckCursor"
#define XmCSelcCursor       "SelcCursor"
#define XmCScrolledCmdPanel "ScrolledCmdPanel"

#endif /* _mksres_ */

