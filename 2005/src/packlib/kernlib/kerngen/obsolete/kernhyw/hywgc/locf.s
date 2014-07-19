*
* $Id: locf.s,v 1.1.1.1 1996/02/15 17:52:51 mclareni Exp $
*
* $Log: locf.s,v $
* Revision 1.1.1.1  1996/02/15 17:52:51  mclareni
* Kernlib
*
*
       SYMDEF  LOCF
LOCF   XED     0,1
       ARL     18
       STA     0,QU
       TRA     2,1
       END
#ifdef CERNLIB_TCGEN_LOCF
#undef CERNLIB_TCGEN_LOCF
#endif
