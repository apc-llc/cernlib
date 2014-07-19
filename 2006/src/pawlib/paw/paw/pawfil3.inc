*
* $Id: pawfil3.inc,v 1.2 1996/03/01 16:50:00 cernlib Exp $
*
* $Log: pawfil3.inc,v $
* Revision 1.2  1996/03/01 16:50:00  cernlib
* All includes  had picked up a completely wrong #if defined(CERNLIB_UNIX), ie.
*  the real commons/declarations were missing on non-unix.
*
* Revision 1.1.1.1  1996/03/01 11:38:49  mclareni
* Paw
*
*
#ifndef CERNLIB_PAW_PAWFIL3_INC
#define CERNLIB_PAW_PAWFIL3_INC
*
#if defined(CERNLIB_VAX)
* IF=VAX.
*
* pawfil3.inc
*
*CMZ :  2.02/00 13/04/93  18.00.48  by  Rene Brun
*-- Author :
      CHARACTER*255 PAWSYS,PAWLOG
       DATA PAWSYS/'CERN_ROOT:[EXE]PAWSYS.KUMAC'/
       DATA PAWLOG/'pawlogon.kumac'/
*
#endif

#endif
