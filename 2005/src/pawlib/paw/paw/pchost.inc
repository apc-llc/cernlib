*
* $Id: pchost.inc,v 1.2 1996/03/01 16:50:17 cernlib Exp $
*
* $Log: pchost.inc,v $
* Revision 1.2  1996/03/01 16:50:17  cernlib
* All includes  had picked up a completely wrong #if defined(CERNLIB_UNIX), ie.
*  the real commons/declarations were missing on non-unix.
*
* Revision 1.1.1.1  1996/03/01 11:38:48  mclareni
* Paw
*
*
#ifndef CERNLIB_PAW_PCHOST_INC
#define CERNLIB_PAW_PCHOST_INC
*
*
* pchost.inc
*
*CMZ :  2.04/12 11/02/94  17.41.30  by  Fons Rademakers
*-- Author :    Fons Rademakers   10/08/93
      COMMON/CIPIAF/ISPROTO
      COMMON/CCPIAF/CHHOST, CHUSER, CHVERS, CONDIR
      CHARACTER*16  CHHOST, CHUSER, CHVERS
      CHARACTER*32  CONDIR
*

#endif
