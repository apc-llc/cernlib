*
* $Id: pcfitf.inc,v 1.2 1996/03/01 16:50:15 cernlib Exp $
*
* $Log: pcfitf.inc,v $
* Revision 1.2  1996/03/01 16:50:15  cernlib
* All includes  had picked up a completely wrong #if defined(CERNLIB_UNIX), ie.
*  the real commons/declarations were missing on non-unix.
*
* Revision 1.1.1.1  1996/03/01 11:38:49  mclareni
* Paw
*
*
#ifndef CERNLIB_PAW_PCFITF_INC
#define CERNLIB_PAW_PCFITF_INC
*
*
* pcfitf.inc
*
*CMZ :  2.02/00 13/04/93  18.00.48  by  Rene Brun
*-- Author :
      PARAMETER(MAXOP=10)
      COMMON/PCFITF/NOPER,LOPER(MAXOP),IFTYPE(MAXOP),NPF(MAXOP)
*

#endif
