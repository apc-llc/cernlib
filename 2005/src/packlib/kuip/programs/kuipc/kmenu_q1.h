/*
 * $Id: kmenu_q1.h,v 1.2 1996/04/16 13:42:59 gunter Exp $
 *
 * $Log: kmenu_q1.h,v $
 * Revision 1.2  1996/04/16 13:42:59  gunter
 * Add kkern.c routines by including the routine from code_kuip; this resolves
 * missing symbols to build kuipc without any library.
 *
 * Revision 1.1  1996/04/16 13:29:42  gunter
 * Mods to compile kuipc.
 *  The header files can be deleted and remade from the .m4 files. Use gnu's m4,
 *  ie.:
 *  gmake headers M4=/usr/local/bin/m4
 *  And then
 *  gmake kuipc
 *
 * Revision 1.1.1.1  1996/03/08 15:33:01  mclareni
 * Kuip
 *
 */


"typedef enum {                  /*                                         */ \
  KmFLAG_FORGET = 0x01,         /* last value is not kept for Motif panels */ \
  KmFLAG_MINUS  = 0x02,         /* -VALUE is not an abbrev for CHOPT=VALUE */ \
  KmFLAG_QUOTE  = 0x04,         /* do not remove quotes                    */ \
  KmFLAG_VARARG = 0x08,         /* append additional args to this param.   */ \
  KmFLAG_CONST  = 0x10,         /* do not allow to assign a value          */ \
  KmFLAG_HIDDEN = 0x20,         /* do not show in menus                    */ \
  KmFLAG_SEPARATE = 0x40        /* treat arguments as separate tokens      */ \
} KmParFlag;                    /*                                         */ \
                                /*                                         */ \
typedef enum {                  /*                                         */ \
  KmTYPE_CHAR   = 'C',          /* character string                        */ \
  KmTYPE_FILE   = 'F',          /* file name                               */ \
  KmTYPE_INT    = 'I',          /* integer                                 */ \
  KmTYPE_OPTION = 'O',          /* option                                  */ \
  KmTYPE_REAL   = 'R'           /* real                                    */ \
} KmParType;                    /*                                         */ \
                                /*                                         */ \
"
