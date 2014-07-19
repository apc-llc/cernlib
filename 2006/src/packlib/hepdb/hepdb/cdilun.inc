*
* $Id: cdilun.inc,v 1.1.1.1 1996/02/28 16:24:02 mclareni Exp $
*
* $Log: cdilun.inc,v $
* Revision 1.1.1.1  1996/02/28 16:24:02  mclareni
* Hepdb, cdlib, etc
*
*
#ifndef CERNLIB_HEPDB_CDILUN_INC
#define CERNLIB_HEPDB_CDILUN_INC
*
*
* cdilun.inc
*
      INTEGER      NLUN
      PARAMETER    (NLUN=6)
      DIMENSION    LUNARR(NLUN)
      INTEGER      LUNARR
      DATA         LUNARR(1)/1/,LUNARR(2)/2/,LUNARR(3)/3/,
     +             LUNARR(4)/4/,LUNARR(5)/7/,LUNARR(6)/8/

#endif
