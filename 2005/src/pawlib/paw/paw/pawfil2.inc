*
* $Id: pawfil2.inc,v 1.2 1996/03/01 16:49:59 cernlib Exp $
*
* $Log: pawfil2.inc,v $
* Revision 1.2  1996/03/01 16:49:59  cernlib
* All includes  had picked up a completely wrong #if defined(CERNLIB_UNIX), ie.
*  the real commons/declarations were missing on non-unix.
*
* Revision 1.1.1.1  1996/03/01 11:38:48  mclareni
* Paw
*
*
#ifndef CERNLIB_PAW_PAWFIL2_INC
#define CERNLIB_PAW_PAWFIL2_INC
*
#if defined(CERNLIB_IBMMVS)
* IF=IBMMVS.
*
* pawfil2.inc
*
*CMZ :  2.02/00 13/04/93  18.00.48  by  Rene Brun
*-- Author :
      CHARACTER*255 PAWSYS,PAWLOG
       DATA PAWSYS/'.R01UTL.PAWSYS.KUMAC'/
       DATA PAWLOG/'PAWLOGON'/
*
#endif

#endif
