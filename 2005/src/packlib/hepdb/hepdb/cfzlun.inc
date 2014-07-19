*
* $Id: cfzlun.inc,v 1.1.1.1 1996/02/28 16:24:05 mclareni Exp $
*
* $Log: cfzlun.inc,v $
* Revision 1.1.1.1  1996/02/28 16:24:05  mclareni
* Hepdb, cdlib, etc
*
*
#ifndef CERNLIB_HEPDB_CFZLUN_INC
#define CERNLIB_HEPDB_CFZLUN_INC
*
*
* cfzlun.inc
*
      PARAMETER       (MACTCF=1, MNKYCF=2, MOPTCF=3, MPATCF=4, MPRECF=5)
      PARAMETER       (MXKPCF=5, MPARCF=5, MFLGCF=5, MDELCF=5)
      PARAMETER       (MINSCF=6, MWDPCF=6, MKEPCF=6, MTIMCF=7, MRECCF=7)
      PARAMETER       (NHEDCF=300, NWFMCF=16)
      COMMON /CFZLUN/ IHEDCF(NHEDCF), IOFMCF(NWFMCF), LUFZCF, NUFZCF
     +              , NWHDCF
*

#endif
