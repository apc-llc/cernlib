/*
 * $Id: klink_q1.h,v 1.2 1996/04/16 13:42:59 gunter Exp $
 *
 * $Log: klink_q1.h,v $
 * Revision 1.2  1996/04/16 13:42:59  gunter
 * Add kkern.c routines by including the routine from code_kuip; this resolves
 * missing symbols to build kuipc without any library.
 *
 * Revision 1.1  1996/04/16 13:29:41  gunter
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


"struct {                        /*                                         */ \
  /* indirect calls to avoid linking HIGZ                                  */ \
  IntFunc    *graf_info_C;      /* pass display, open and close (ixmotif)  */ \
  SUBROUTINE *graf_size_F;      /* resize window (IGRSIZ)                  */ \
  SUBROUTINE *graf_pick_F;      /* identifying graphics objects (IGOBJ)    */ \
  SUBROUTINE *graf_attr_F;      /* set attributes (IGSET)                  */ \
  SUBROUTINE *graf_close_F;     /* close workstation (ICLWK)               */ \
  /* optional routines for Motif customization                             */ \
  pCharFunc  *user_FallBk_C;    /* get application fallbacks               */ \
  IntFunc    *user_TopWid_C;    /* pass toplevel widget identifiers        */ \
} klnkaddr;                     /*                                         */ \
"
