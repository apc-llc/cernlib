*
* $Id: pcrang.inc,v 1.2 1996/03/01 16:50:26 cernlib Exp $
*
* $Log: pcrang.inc,v $
* Revision 1.2  1996/03/01 16:50:26  cernlib
* All includes  had picked up a completely wrong #if defined(CERNLIB_UNIX), ie.
*  the real commons/declarations were missing on non-unix.
*
* Revision 1.1.1.1  1996/03/01 11:38:49  mclareni
* Paw
*
*
#ifndef CERNLIB_PAW_PCRANG_INC
#define CERNLIB_PAW_PCRANG_INC
*
*
* pcrang.inc
*
*CMZ :  2.07/01 09/05/95  11.07.00  by  O.Couet
*-- Author :
* IC[X,Y][1,2] is the range limited by 1 and the number of bins
* IB[X,Y][1,2] is the non limited range
      COMMON/PCRANG/ICRANG,ICX1,ICX2,ICY1,ICY2,IBX1,IBX2,IBY1,IBY2
*

#endif
