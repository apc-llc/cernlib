*
* $Id: pccmd.inc,v 1.2 1996/03/01 16:50:13 cernlib Exp $
*
* $Log: pccmd.inc,v $
* Revision 1.2  1996/03/01 16:50:13  cernlib
* All includes  had picked up a completely wrong #if defined(CERNLIB_UNIX), ie.
*  the real commons/declarations were missing on non-unix.
*
* Revision 1.1.1.1  1996/03/01 11:38:47  mclareni
* Paw
*
*
#ifndef CERNLIB_PAW_PCCMD_INC
#define CERNLIB_PAW_PCCMD_INC
*
*
* pccmd.inc
*
*CMZ :  2.04/13 18/02/94  17.38.08  by  Fons Rademakers
*-- Author :    Fons Rademakers   18/02/94
      LOGICAL        NTCMD
      COMMON /PCCMD/ NTCMD
*

#endif
