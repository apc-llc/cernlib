*
* $Id: pawfil5.inc,v 1.2 1996/03/01 16:50:01 cernlib Exp $
*
* $Log: pawfil5.inc,v $
* Revision 1.2  1996/03/01 16:50:01  cernlib
* All includes  had picked up a completely wrong #if defined(CERNLIB_UNIX), ie.
*  the real commons/declarations were missing on non-unix.
*
* Revision 1.1.1.1  1996/03/01 11:38:47  mclareni
* Paw
*
*
#ifndef CERNLIB_PAW_PAWFIL5_INC
#define CERNLIB_PAW_PAWFIL5_INC
*
*
* pawfil5.inc
*
#if defined(CERNLIB_MSDOS)
*CMZ :  2.04/12 15/02/94  08.50.12  by  Rene Brun
*-- Author :    Rene Brun   15/02/94
      CHARACTER*255 PAWSYS,PAWLOG
       DATA PAWSYS/'\\cern\\pro\\bin\\pawsys.kumac'/
       DATA PAWLOG/'pawlogon.kum'/
*
#endif

#endif
