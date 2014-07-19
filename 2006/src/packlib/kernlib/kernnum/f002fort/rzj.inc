*
* $Id: rzj.inc,v 1.1.1.1 1996/02/15 17:48:50 mclareni Exp $
*
* $Log: rzj.inc,v $
* Revision 1.1.1.1  1996/02/15 17:48:50  mclareni
* Kernlib
*
*
*
* rzj.inc
*
#if defined(CERNLIB_NUMAP)||defined(CERNLIB_NUMCD)||defined(CERNLIB_NUMCV)||defined(CERNLIB_NUMIB2)||defined(CERNLIB_NUMND)
          JZ  =   LOCF(Z2) - LOCF(Z)
#endif
#if defined(CERNLIB_NUMCR)||defined(CERNLIB_NUMUC)
          JZ  =   LOC(Z2) - LOC(Z)
#endif
#if defined(CERNLIB_NUMDE)
          JZ  =   (%LOC(Z2) - %LOC(Z)) / 4
#endif
#if defined(CERNLIB_NUMIB1)
          JZ  =   (IADDR(Z2) - IADDR(Z)) / 4
#endif
