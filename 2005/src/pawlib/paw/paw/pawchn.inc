*
* $Id: pawchn.inc,v 1.2 1996/03/01 16:49:56 cernlib Exp $
*
* $Log: pawchn.inc,v $
* Revision 1.2  1996/03/01 16:49:56  cernlib
* All includes  had picked up a completely wrong #if defined(CERNLIB_UNIX), ie.
*  the real commons/declarations were missing on non-unix.
*
* Revision 1.1.1.1  1996/03/01 11:38:48  mclareni
* Paw
*
*
#ifndef CERNLIB_PAW_PAWCHN_INC
#define CERNLIB_PAW_PAWCHN_INC
*
*
* pawchn.inc
*
*CMZ :  2.02/01 10/05/93  00.52.38  by  Fons Rademakers
*-- Author :    Fons Rademakers   13/04/93
      LOGICAL         CHAIN
      CHARACTER*128   CFILE
      COMMON /PAWCHN/ CHAIN, NCHEVT, ICHEVT
      COMMON /PAWCHC/ CFILE
*
      CHARACTER*80    CURCHN
      COMMON /PAWCH2/ CURCHN
*
      LOGICAL         CHNXMT
      COMMON /PAWCH3/ CHNXMT
*

#endif
