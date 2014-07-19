*
* $Id: caopts.inc,v 1.1.1.1 1996/02/28 16:24:04 mclareni Exp $
*
* $Log: caopts.inc,v $
* Revision 1.1.1.1  1996/02/28 16:24:04  mclareni
* Hepdb, cdlib, etc
*
*
#ifndef CERNLIB_HEPDB_CAOPTS_INC
#define CERNLIB_HEPDB_CAOPTS_INC
*
*
* caopts.inc
*
      PARAMETER       (MXKYCA=100)
      COMMON /CAOPTS/ IOPACA, IOPBCA, IOPCCA, IOPDCA, IOPECA, IOPFCA
     +              , IOPGCA, IOPHCA, IOPICA, IOPJCA, IOPKCA, IOPLCA
     +              , IOPMCA, IOPNCA, IOPOCA, IOPPCA, IOPQCA, IOPRCA
     +              , IOPSCA, IOPTCA, IOPUCA, IOPVCA, IOPWCA, IOPXCA
     +              , IOPYCA, IOPZCA, IOKYCA(MXKYCA), IPRBCA, IPRECA
*

#endif
