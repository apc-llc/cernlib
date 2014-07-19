*
* $Id: pcargs.inc,v 1.2 1996/03/01 16:50:11 cernlib Exp $
*
* $Log: pcargs.inc,v $
* Revision 1.2  1996/03/01 16:50:11  cernlib
* All includes  had picked up a completely wrong #if defined(CERNLIB_UNIX), ie.
*  the real commons/declarations were missing on non-unix.
*
* Revision 1.1.1.1  1996/03/01 11:38:49  mclareni
* Paw
*
*
#ifndef CERNLIB_PAW_PCARGS_INC
#define CERNLIB_PAW_PCARGS_INC
*
*
* pcargs.inc
*
*CMZ :  2.04/05 26/11/93  11.40.00  by  Rene Brun
*-- Author :
      CHARACTER*80  CHARGS
      COMMON/PCARGS/CHARGS
*

#endif
