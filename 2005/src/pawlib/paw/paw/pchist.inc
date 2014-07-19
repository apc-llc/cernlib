*
* $Id: pchist.inc,v 1.2 1996/03/01 16:50:16 cernlib Exp $
*
* $Log: pchist.inc,v $
* Revision 1.2  1996/03/01 16:50:16  cernlib
* All includes  had picked up a completely wrong #if defined(CERNLIB_UNIX), ie.
*  the real commons/declarations were missing on non-unix.
*
* Revision 1.1.1.1  1996/03/01 11:38:47  mclareni
* Paw
*
*
#ifndef CERNLIB_PAW_PCHIST_INC
#define CERNLIB_PAW_PCHIST_INC
*
*
* pchist.inc
*
*CMZ :  2.04/15 13/03/94  13.15.29  by  Fons Rademakers
*-- Author :    Fons Rademakers   13/03/94
      CHARACTER*64    HPATH
      COMMON /PCHIST/ HPATH
*

#endif
