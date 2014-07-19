*
* $Id: hcbont.inc,v 1.1.1.1 1996/01/16 17:07:50 mclareni Exp $
*
* $Log: hcbont.inc,v $
* Revision 1.1.1.1  1996/01/16 17:07:50  mclareni
* First import
*
*
#ifndef CERNLIB_HBOOK_HCBONT_INC
#define CERNLIB_HBOOK_HCBONT_INC
*
*
* hcbont.inc
*
*CMZ :  4.21/10 01/03/94  11.54.50  by  Rene Brun
*-- Author :    Rene Brun   10/01/94
*
*-- ATTENTION: when changing anything in these common blocks check
*--            also $HCNTPAW.
*
      PARAMETER      (MAXNP = 1000, MAXNC = 8000)
      PARAMETER      (MAXDUM=50007-4*MAXNP-3)
      REAL           XONT(MAXNP), YONT(MAXNP), ZONT(MAXNP), WONT(MAXNP)
      REAL           DUMCR4(MAXDUM)
      INTEGER        KONTYP, NONT,NCHONT
      CHARACTER*4    CHONT(MAXNC)
*
      COMMON /PAWCR4/KONTYP,NONT,XONT,YONT,ZONT,WONT,NCHONT,DUMCR4
      COMMON /PAWC32/ CHONT
*

#endif
