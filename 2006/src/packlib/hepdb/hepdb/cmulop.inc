*
* $Id: cmulop.inc,v 1.1.1.1 1996/02/28 16:24:05 mclareni Exp $
*
* $Log: cmulop.inc,v $
* Revision 1.1.1.1  1996/02/28 16:24:05  mclareni
* Hepdb, cdlib, etc
*
*
#ifndef CERNLIB_HEPDB_CMULOP_INC
#define CERNLIB_HEPDB_CMULOP_INC
*
*
* cmulop.inc
*
      PARAMETER       (NMLMCM=10)
      COMMON /CMULOP/ INDXCM(NMLMCM), KEYVCM(NMLMCM), KTYPCM(NMLMCM)
     +              , LFKYCM(NMLMCM), NOCCCM(NMLMCM), NMULCM
*

#endif
