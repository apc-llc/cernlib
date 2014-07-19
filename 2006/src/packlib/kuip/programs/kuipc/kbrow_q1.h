/*
 * $Id: kbrow_q1.h,v 1.2 1996/04/16 13:42:56 gunter Exp $
 *
 * $Log: kbrow_q1.h,v $
 * Revision 1.2  1996/04/16 13:42:56  gunter
 * Add kkern.c routines by including the routine from code_kuip; this resolves
 * missing symbols to build kuipc without any library.
 *
 * Revision 1.1  1996/04/16 13:29:36  gunter
 * Mods to compile kuipc.
 *  The header files can be deleted and remade from the .m4 files. Use gnu's m4,
 *  ie.:
 *  gmake headers M4=/usr/local/bin/m4
 *  And then
 *  gmake kuipc
 *
 * Revision 1.1.1.1  1996/03/08 15:33:00  mclareni
 * Kuip
 *
 */


"typedef unsigned long KmPixmap; /* Pixmap from <X11/X.h>                   */ \
typedef void *KmWidget;         /* Widget from <X11/Intrinsic.h>           */ \
typedef void *KmCalldata;       /* XmAnyCallbackStruct from <Motif/Xm.h>   */ \
                                /*                                         */ \
typedef enum {                  /*                                         */ \
  BRACT_OPEN = 0,               /*                                         */ \
  BRACT_ROOT = 1,               /*                                         */ \
  BRACT_CONT = 2,               /*                                         */ \
  BRACT_GRAF = 3                /*                                         */ \
} BrActTag;                     /*                                         */ \
                                /*                                         */ \
typedef enum {                  /*                                         */ \
  BrActUpdate    = 0x01,        /* browser window has to be updated ('!')  */ \
  BrActSeparator = 0x02,        /* put separator in menu ('/')             */ \
  BrActToggle    = 0x04,        /* register as toggle button               */ \
  BrActToggleOn  = 0x08,        /* toggle state is on                      */ \
  BrActSensitive = 0x10         /* button is sensitive                     */ \
} BrActFlag;                    /*                                         */ \
                                /*                                         */ \
"
