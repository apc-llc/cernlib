*
* $Id: pcmatw.inc,v 1.2 1996/03/01 16:50:21 cernlib Exp $
*
* $Log: pcmatw.inc,v $
* Revision 1.2  1996/03/01 16:50:21  cernlib
* All includes  had picked up a completely wrong #if defined(CERNLIB_UNIX), ie.
*  the real commons/declarations were missing on non-unix.
*
* Revision 1.1.1.1  1996/03/01 11:38:50  mclareni
* Paw
*
*
#ifndef CERNLIB_PAW_PCMATW_INC
#define CERNLIB_PAW_PCMATW_INC
*
*
* pcmatw.inc
*
*CMZ :  2.02/00 13/04/93  18.00.48  by  Rene Brun
*-- Author :
      CHARACTER*32    OUTSTR(512)
      COMMON /PCMATW/ OUTSTR
*

#endif
