/*
 * $Id: kbrow_q2.h,v 1.2 1996/04/16 13:42:57 gunter Exp $
 *
 * $Log: kbrow_q2.h,v $
 * Revision 1.2  1996/04/16 13:42:57  gunter
 * Add kkern.c routines by including the routine from code_kuip; this resolves
 * missing symbols to build kuipc without any library.
 *
 * Revision 1.1  1996/04/16 13:29:37  gunter
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


"typedef struct _BrAction {      /*                                         */ \
  struct _BrAction *next;       /* link to next action binding             */ \
  BrActFlag   flags;            /*                                         */ \
  char       *text;             /* text line in menu                       */ \
  char       *user_text;        /* user text overriding CDF text (malloced)*/ \
  char       *accel;            /* accelerator                             */ \
  char       *exec;             /* action commands                         */ \
  SUBROUTINE *call_F;           /* action routine                          */ \
  IntFunc    *call_C;           /* action function                         */ \
  BrActTag    tag;              /* for which window the action is defined  */ \
  struct _BrClass *class;       /* pointer to BrClass in case of open menu */ \
} BrAction;                     /*                                         */ \
                                /*                                         */ \
typedef struct _BrClass {       /*                                         */ \
  struct _BrClass *next;        /* link to next browsable class            */ \
  char       *name;             /* unique identifier name                  */ \
  char       *title;            /* title for popup menu (maybe NULL)       */ \
  SUBROUTINE *scan_km_F;        /* user function scanning the directory    */ \
  pCharFunc  *scan_km_C;        /* user function scanning the directory    */ \
  SUBROUTINE *scan_br_F;        /* user function scanning for browsables   */ \
  pCharFunc  *scan_br_C;        /* user function scanning for browsables   */ \
  BrAction   *root;             /* list of actions in root window          */ \
  BrAction   *open;             /* list of actions in open menu            */ \
} BrClass;                      /*                                         */ \
                                /*                                         */ \
"
