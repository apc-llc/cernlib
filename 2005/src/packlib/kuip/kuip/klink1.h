struct {                        /*                                         */ __
  /* indirect calls to avoid linking HIGZ                                  */ __
  IntFunc    *graf_info_C;      /* pass display, open and close (ixmotif)  */ __
  SUBROUTINE *graf_size_F;      /* resize window (IGRSIZ)                  */ __
  SUBROUTINE *graf_pick_F;      /* identifying graphics objects (IGOBJ)    */ __
  SUBROUTINE *graf_attr_F;      /* set attributes (IGSET)                  */ __
  SUBROUTINE *graf_close_F;     /* close workstation (ICLWK)               */ __
  /* optional routines for Motif customization                             */ __
  pCharFunc  *user_FallBk_C;    /* get application fallbacks               */ __
  IntFunc    *user_TopWid_C;    /* pass toplevel widget identifiers        */ __
} klnkaddr;                     /*                                         */ __
