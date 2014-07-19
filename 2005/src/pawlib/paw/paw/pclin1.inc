*
* $Id: pclin1.inc,v 1.2 1996/03/01 16:50:18 cernlib Exp $
*
* $Log: pclin1.inc,v $
* Revision 1.2  1996/03/01 16:50:18  cernlib
* All includes  had picked up a completely wrong #if defined(CERNLIB_UNIX), ie.
*  the real commons/declarations were missing on non-unix.
*
* Revision 1.1.1.1  1996/03/01 11:38:50  mclareni
* Paw
*
*
#ifndef CERNLIB_PAW_PCLIN1_INC
#define CERNLIB_PAW_PCLIN1_INC
*
*
* pclin1.inc
*
*CMZ :  2.02/00 13/04/93  18.00.48  by  Rene Brun
*-- Author :
      PARAMETER (KNTMAX=20,KNTMA1=KNTMAX+1,KNTMA2=KNTMAX*KNTMAX)
      COMMON/PCLINT/AR(KNTMA2),DEIGVA(KNTMAX),DWORK(KNTMAX)
     + ,SS(KNTMAX),S(KNTMAX),XX(KNTMAX),XA(KNTMAX),AM(KNTMA2)
     + ,EIGVEC(KNTMA2),EIGVAL(KNTMA1),XSI(KNTMAX),R(KNTMAX)

#endif
