*
* $Id: mkcde.inc,v 1.1.1.1 1996/02/15 17:55:06 mclareni Exp $
*
* $Log: mkcde.inc,v $
* Revision 1.1.1.1  1996/02/15 17:55:06  mclareni
* Kernlib
*
*
#ifndef CERNLIB_KERNGENT_MKCDE_INC
#define CERNLIB_KERNGENT_MKCDE_INC
*
*
* mkcde.inc
*
      COMMON /PARAM/  IQBITW, IQCHAW, ITB, NLINES, ITIMES, TIMERD
     +,               IFLGU, LUNGU, ZERGU, ZEROV(5), ZERLEV, LOGLEV
     +,               NFAIPR,NEACHP, NFAILT,NFAIL, NTEST,MTESTV(20)
      COMMON //       A(1000), B(1010),  IBCD(47),INTG(100)
                                  DIMENSION    IA(1000),     IB(1000)
                                  EQUIVALENCE (IA(1),A(1)), (IB(1),B(1))
C-----------------------------------------------------------------------

#endif
