*
* $Id: pccsel.inc,v 1.2 1996/03/01 16:50:14 cernlib Exp $
*
* $Log: pccsel.inc,v $
* Revision 1.2  1996/03/01 16:50:14  cernlib
* All includes  had picked up a completely wrong #if defined(CERNLIB_UNIX), ie.
*  the real commons/declarations were missing on non-unix.
*
* Revision 1.1.1.1  1996/03/01 11:38:49  mclareni
* Paw
*
*
#ifndef CERNLIB_PAW_PCCSEL_INC
#define CERNLIB_PAW_PCCSEL_INC
*
*
* pccsel.inc
*
*CMZ :  2.03/04 21/07/93  18.40.35  by  Fons Rademakers
*-- Author :
      COMMON/PCCSEL/IOPTCS(5),CSIZE
      EQUIVALENCE (IOPTCS(1),IOPTCR),(IOPTCS(2),IOPTCB)
      EQUIVALENCE (IOPTCS(3),IOPTCM),(IOPTCS(4),IOPTCC)
      EQUIVALENCE (IOPTCS(5),IOPTCN)
      COMMON/PCCSE2/CHCSEL,CSOPT
      CHARACTER*80 CHCSEL
      CHARACTER*8  CSOPT
*

#endif
