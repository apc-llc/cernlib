/*
 * $Id: kbrow_q4.h,v 1.2 1996/04/16 13:42:58 gunter Exp $
 *
 * $Log: kbrow_q4.h,v $
 * Revision 1.2  1996/04/16 13:42:58  gunter
 * Add kkern.c routines by including the routine from code_kuip; this resolves
 * missing symbols to build kuipc without any library.
 *
 * Revision 1.1  1996/04/16 13:29:39  gunter
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


"typedef enum {                  /*                                         */ \
  KmButtSensitive       = 0x00, /* sensitive button                        */ \
  KmButtNonSensitive    = 0x01, /* non-sensitive button ('NS')             */ \
  KmButtToggleSensitive = 0x02, /* toggle-sensitive button ('TS')          */ \
  KmButtSensitivityMask = 0x03, /* mask for sensitivity type               */ \
  KmButtSeparator       = 0x04, /* put separator in menu ('/')             */ \
  KmButtBrowser         = 0x08  /* button is in main browser ('BR')        */ \
} KmButtFlag;                   /*                                         */ \
                                /*                                         */ \
typedef struct _KmButton {      /*                                         */ \
  struct _KmButton *next;       /* button label or menu item               */ \
  char       *menu;             /* menu name or NULL for buttons           */ \
  char       *label;            /* button label or menu item               */ \
  SUBROUTINE *call_F;           /* callback routine (FORTRAN)              */ \
  IntFunc    *call_C;           /* callback routine (C)                    */ \
  char       *action;           /* name of callback routine                */ \
  char       *mnemo;            /* button mnemonic                         */ \
  char       *accel;            /* button accelerator                      */ \
  char       *accel_text;       /* button accelerator text                 */ \
  KmButtFlag  flags;            /* sensitivity type etc.                   */ \
  KmWidget    widget;           /* Motif widget ID                         */ \
} KmButton;                     /*                                         */ \
                                /*                                         */ \
"
