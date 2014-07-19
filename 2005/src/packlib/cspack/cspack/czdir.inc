*
* $Id: czdir.inc,v 1.1.1.1 1996/03/08 15:44:17 mclareni Exp $
*
* $Log: czdir.inc,v $
* Revision 1.1.1.1  1996/03/08 15:44:17  mclareni
* Cspack
*
*
#ifndef CERNLIB_CSPACK_CZDIR_INC
#define CERNLIB_CSPACK_CZDIR_INC
*
*
* czdir.inc
*
      PARAMETER (MAXFIL=20)
      COMMON/CZDIR1/CHRZ(MAXFIL),FLNAME(MAXFIL)
      CHARACTER*64 FLNAME
      CHARACTER*32 CHRZ
      COMMON/CZDIR2/IDIR,NCHRZ,LUNRZ(MAXFIL)
*

#endif
