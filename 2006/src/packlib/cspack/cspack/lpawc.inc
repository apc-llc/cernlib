*
* $Id: lpawc.inc,v 1.1.1.1 1996/03/08 15:44:16 mclareni Exp $
*
* $Log: lpawc.inc,v $
* Revision 1.1.1.1  1996/03/08 15:44:16  mclareni
* Cspack
*
*
#ifndef CERNLIB_CSPACK_LPAWC_INC
#define CERNLIB_CSPACK_LPAWC_INC
*
*
* lpawc.inc
*
#if defined(CERNLIB_OS9)
      PARAMETER(NWPAWC=10000)
#endif
#if !defined(CERNLIB_OS9)
      PARAMETER(NWPAWC=900000)
#endif

#endif
