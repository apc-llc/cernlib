*
* $Id: fpadr.inc,v 1.2 1996/03/01 16:49:47 cernlib Exp $
*
* $Log: fpadr.inc,v $
* Revision 1.2  1996/03/01 16:49:47  cernlib
* All includes  had picked up a completely wrong #if defined(CERNLIB_UNIX), ie.
*  the real commons/declarations were missing on non-unix.
*
* Revision 1.1.1.1  1996/03/01 11:38:47  mclareni
* Paw
*
*
#ifndef CERNLIB_PAW_FPADR_INC
#define CERNLIB_PAW_FPADR_INC
*
*
* fpadr.inc
*
*CMZ :  2.07/08 16/08/95  15.34.10  by  O.Couet
*-- Author :    O.Couet   16/08/95
* To store the adresses of FPHSTART and FPVSTART
      COMMON /FPADR/ JMFPH, JMFPV

#endif
