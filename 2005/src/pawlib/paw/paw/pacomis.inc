*
* $Id: pacomis.inc,v 1.2 1996/03/01 16:49:52 cernlib Exp $
*
* $Log: pacomis.inc,v $
* Revision 1.2  1996/03/01 16:49:52  cernlib
* All includes  had picked up a completely wrong #if defined(CERNLIB_UNIX), ie.
*  the real commons/declarations were missing on non-unix.
*
* Revision 1.1.1.1  1996/03/01 11:38:48  mclareni
* Paw
*
*
#ifndef CERNLIB_PAW_PACOMIS_INC
#define CERNLIB_PAW_PACOMIS_INC
*
*
* pacomis.inc
*
*CMZ :  2.04/11 11/01/94  09.14.16  by  Rene Brun
*-- Author :    Rene Brun   06/05/93
      PARAMETER (NCOMIS=30000)
      COMMON/COMIS/CS(NCOMIS)

#endif
