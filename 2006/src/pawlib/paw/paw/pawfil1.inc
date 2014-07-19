*
* $Id: pawfil1.inc,v 1.2 1996/03/01 16:49:58 cernlib Exp $
*
* $Log: pawfil1.inc,v $
* Revision 1.2  1996/03/01 16:49:58  cernlib
* All includes  had picked up a completely wrong #if defined(CERNLIB_UNIX), ie.
*  the real commons/declarations were missing on non-unix.
*
* Revision 1.1.1.1  1996/03/01 11:38:48  mclareni
* Paw
*
*
#ifndef CERNLIB_PAW_PAWFIL1_INC
#define CERNLIB_PAW_PAWFIL1_INC
*
#if defined(CERNLIB_IBM) && !defined(CERNLIB_IBMMVS)
* IF=IBM,IF=-IBMMVS.
*
* pawfil1.inc
*
*CMZ :  2.02/00 13/04/93  18.00.48  by  Rene Brun
*-- Author :
      CHARACTER*255 PAWSYS,PAWLOG
      DATA PAWSYS/'/PAWSYS KUMAC *'/
      DATA PAWLOG/'/PAWLOGON KUMAC *'/
*
* THE FOLLOWING IS NEEDED WHILE KUIP DOES NOT UNDERSTAND VS-FORTRAN
* FILES
*
      CHARACTER*255 PAWSY1,PAWLO1,PAWSY2,PAWLO2
       DATA PAWSY1/'/PAWSYS KUMAC *'/
       DATA PAWLO1/'/PAWLOGON KUMAC *'/
       DATA PAWSY2/'PAWSYS.KUMAC.*'/
       DATA PAWLO2/'PAWLOGON.KUMAC.*'/
*
#endif

#endif
