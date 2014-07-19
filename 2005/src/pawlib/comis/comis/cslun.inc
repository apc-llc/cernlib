*
* $Id: cslun.inc,v 1.1.1.1 1996/02/26 17:16:35 mclareni Exp $
*
* $Log: cslun.inc,v $
* Revision 1.1.1.1  1996/02/26 17:16:35  mclareni
* Comis
*
*
#ifndef CERNLIB_COMIS_CSLUN_INC
#define CERNLIB_COMIS_CSLUN_INC
*
*
* cslun.inc
*
      COMMON/CSUNIT/LUNINP,LUNPM,LUNFIL,LUNLOG,LUNMAP,LUNLIB,
     +                     ISTPM,ISTFIL,ISTLOG,ISTMAP,ISTLIB
      INTEGER LUNARR(5),ISTARR(5)
      EQUIVALENCE(LUNARR,LUNPM), (ISTARR,ISTPM)

#endif
