*
* $Id: pcaddr.inc,v 1.2 1996/03/01 16:50:10 cernlib Exp $
*
* $Log: pcaddr.inc,v $
* Revision 1.2  1996/03/01 16:50:10  cernlib
* All includes  had picked up a completely wrong #if defined(CERNLIB_UNIX), ie.
*  the real commons/declarations were missing on non-unix.
*
* Revision 1.1.1.1  1996/03/01 11:38:50  mclareni
* Paw
*
*
#ifndef CERNLIB_PAW_PCADDR_INC
#define CERNLIB_PAW_PCADDR_INC
*
*
* pcaddr.inc
*
*CMZ :  2.04/15 13/03/94  12.11.10  by  Fons Rademakers
*-- Author :
      COMMON /PCADDR/ JMPANT, JMLAB, JMROW, JMCHAIN, JNTWINI, JNTWCHK,
     +                JMHIST
*

#endif
