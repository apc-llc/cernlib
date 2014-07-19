*
* $Id: ffluns.inc,v 1.1.1.1 1996/03/08 11:50:40 mclareni Exp $
*
* $Log: ffluns.inc,v $
* Revision 1.1.1.1  1996/03/08 11:50:40  mclareni
* Ffread
*
*
#ifndef CERNLIB_FFREAD_FFLUNS_INC
#define CERNLIB_FFREAD_FFLUNS_INC
*
*
* ffluns.inc
*
#if defined(CERNLIB_CDC)
      PARAMETER (LUNDIN = L"INPUT" ,
     *           LUNDUT = L"OUTPUT",
     *           LUNMAX =        99)
#endif
#if defined(CERNLIB_LUNF77)
      PARAMETER (LUNDIN =   5,
     *           LUNDUT =   6,
     *           LUNMAX =  99)
#endif
#if defined(CERNLIB_VAX)||defined(CERNLIB_DECS)
      PARAMETER (LUNDIN =   5,
     *           LUNDUT =   6,
     *           LUNMAX = 119)
#endif
C
C     LUNDIN - default input  LUN
C     LUNDUT - default output LUN
C     LUNMAX - maximum LUN allowed
C

#endif
