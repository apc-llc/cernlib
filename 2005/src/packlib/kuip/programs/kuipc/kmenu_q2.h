/*
 * $Id: kmenu_q2.h,v 1.2 1996/04/16 13:43:00 gunter Exp $
 *
 * $Log: kmenu_q2.h,v $
 * Revision 1.2  1996/04/16 13:43:00  gunter
 * Add kkern.c routines by including the routine from code_kuip; this resolves
 * missing symbols to build kuipc without any library.
 *
 * Revision 1.1  1996/04/16 13:29:43  gunter
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

"typedef struct {                /* file name                               */ \
  char         *filter_default; /* filter wildcard                         */ \
  char         *filter_current; /* current filter                          */ \
} KmParFile;                    /*                                         */ \
                                /*                                         */ \
typedef struct {                /*                                         */ \
  char         *range_lower;    /* lower value of range                    */ \
  char         *range_upper;    /* upper value of range                    */ \
  char         *slider_lower;   /* lower limit for slider                  */ \
  char         *slider_upper;   /* upper limit for slider                  */ \
  int           decimals;       /* number of decimals used for slider      */ \
} KmParInt;                     /*                                         */ \
                                /*                                         */ \
typedef struct {                /* option                                  */ \
  char        **text;           /* explanations (parallel to range_value)  */ \
  int          *mutex;          /* mutex group to which text belongs       */ \
  int          *radio;          /* radio group to which text belongs       */ \
} KmParOption;                  /*                                         */ \
                                /*                                         */ \
"
