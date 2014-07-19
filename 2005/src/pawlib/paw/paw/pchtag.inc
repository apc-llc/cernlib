*
* $Id: pchtag.inc,v 1.2 1996/03/01 16:50:18 cernlib Exp $
*
* $Log: pchtag.inc,v $
* Revision 1.2  1996/03/01 16:50:18  cernlib
* All includes  had picked up a completely wrong #if defined(CERNLIB_UNIX), ie.
*  the real commons/declarations were missing on non-unix.
*
* Revision 1.1.1.1  1996/03/01 11:38:47  mclareni
* Paw
*
*
#ifndef CERNLIB_PAW_PCHTAG_INC
#define CERNLIB_PAW_PCHTAG_INC
*
*
* pchtag.inc
*
*CMZ :  2.07/02 29/05/95  13.23.39  by  O.Couet
*-- Author :    O.Couet   26/04/95
      PARAMETER (NTAG=100,NCHTAG=32,NCHBIG=512)
      CHARACTER*(NCHTAG) CHTAG
      CHARACTER*(NCHBIG) CHBIG
      COMMON /PCHTAG/ CHTAG(NTAG)
      EQUIVALENCE (CHTAG(1),CHBIG)

#endif
