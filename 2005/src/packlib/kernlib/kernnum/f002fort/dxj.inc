*
* $Id: dxj.inc,v 1.1.1.1 1996/02/15 17:48:50 mclareni Exp $
*
* $Log: dxj.inc,v $
* Revision 1.1.1.1  1996/02/15 17:48:50  mclareni
* Kernlib
*
*
*
* dxj.inc
*
#if defined(CERNLIB_NUMAP)||defined(CERNLIB_NUMCD)||defined(CERNLIB_NUMCV)||defined(CERNLIB_NUMIB2)||defined(CERNLIB_NUMND)
          JX  =  (LOCF(X2) - LOCF(X)) / 2
#endif
#if defined(CERNLIB_NUMCR)||defined(CERNLIB_NUMUC)
          JX  =  (LOC(X2) - LOC(X)) / 2
#endif
#if defined(CERNLIB_NUMDE)
          JX  =  (%LOC(X2) - %LOC(X)) / 8
#endif
#if defined(CERNLIB_NUMIB1)
          JX  =  (IADDR(X2) - IADDR(X)) / 8
#endif
