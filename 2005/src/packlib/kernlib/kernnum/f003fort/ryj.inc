*
* $Id: ryj.inc,v 1.1.1.1 1996/02/15 17:48:55 mclareni Exp $
*
* $Log: ryj.inc,v $
* Revision 1.1.1.1  1996/02/15 17:48:55  mclareni
* Kernlib
*
*
*
* ryj.inc
*
#if defined(CERNLIB_NUMAP)||defined(CERNLIB_NUMCD)||defined(CERNLIB_NUMCV)||defined(CERNLIB_NUMIB2)||defined(CERNLIB_NUMND)
          JY  =   LOCF(Y2) - LOCF(Y)
#endif
#if defined(CERNLIB_NUMCR)||defined(CERNLIB_NUMUC)
          JY  =   LOC(Y2) - LOC(Y)
#endif
#if defined(CERNLIB_NUMDE)
          JY  =   (%LOC(Y2) - %LOC(Y)) / 4
#endif
#if defined(CERNLIB_NUMIB1)
          JY  =   (IADDR(Y2) - IADDR(Y)) / 4
#endif
