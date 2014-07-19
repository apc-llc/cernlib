*
* $Id: pawc.inc,v 1.1.1.1 1995/12/12 14:36:23 mclareni Exp $
*
* $Log: pawc.inc,v $
* Revision 1.1.1.1  1995/12/12 14:36:23  mclareni
* Imported sources
*
*
#ifndef CERNLIB_SIGMA_PAWC_INC
#define CERNLIB_SIGMA_PAWC_INC
*
*
* pawc.inc
*
*CMZ :  1.08/00 18/11/92  14.57.21  by  Unknown
*-- Author :
      COMMON/PAWC/ NWPAW,IXPAWC,IHBOOK,IXHIGZ,IXKUIP,IFENCE(5),
     +              LMAIN(1), WS(9989)
      DIMENSION IQ(2),Q(2),LQ(8000)
      EQUIVALENCE (LQ(1),LMAIN(1))
      EQUIVALENCE (IQ(1),LQ(9))
      EQUIVALENCE ( Q(1),LQ(9))
      COMMON/QUEST/ IQUEST(100)
      COMMON/SIFKCO/ IOCEL,IONAME,IOPAR,IOPARV,IOVEC,IOVECN
      DIMENSION DYNA (4096)
      EQUIVALENCE (Q(1),DYNA(1))
#include "sigma/sitab.inc"

#endif
