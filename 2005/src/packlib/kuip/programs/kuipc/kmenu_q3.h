/*
 * $Id: kmenu_q3.h,v 1.2 1996/04/16 13:43:01 gunter Exp $
 *
 * $Log: kmenu_q3.h,v $
 * Revision 1.2  1996/04/16 13:43:01  gunter
 * Add kkern.c routines by including the routine from code_kuip; this resolves
 * missing symbols to build kuipc without any library.
 *
 * Revision 1.1  1996/04/16 13:29:44  gunter
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


"typedef KmParInt KmParReal;     /* real and int have the same fields       */ \
                                /*                                         */ \
typedef struct {                /*                                         */ \
  char         *name;           /* parameter name                          */ \
  int           abbrev;         /* minimum length that name is recognized  */ \
  char         *prompt;         /* prompt string                           */ \
  char         *dfault;         /* default value                           */ \
  char         *last;           /* last value for Motif panel (malloced)   */ \
  int           width;          /* width of input field                    */ \
  int           range_count;    /* number of items in range_value          */ \
  char        **range_value;    /* list of allowed values                  */ \
  int           select_count;   /* number of items in select_count         */ \
  char        **select_value;   /* list of values for selection box        */ \
  KmParFlag     flags;          /* special flags                           */ \
  KmParType     type;           /* parameter type                          */ \
  void        *ptype;           /* structure pointer selected by type      */ \
} KmParameter;                  /*                                         */ \
                                /*                                         */ \
"
