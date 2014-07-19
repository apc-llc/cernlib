*
* $Id: pawcom.inc,v 1.2 1996/03/01 16:14:09 cernlib Exp $
*
* $Log: pawcom.inc,v $
* Revision 1.2  1996/03/01 16:14:09  cernlib
* pawcom had picked up a completely wrong #if defined(CERNLIB_UNIX), ie.
*  the real commons/declarations were missing on non-unix.
*
* Revision 1.1.1.1  1996/03/01 11:38:48  mclareni
* Paw
*
*
#ifndef CERNLIB_PAW_PAWCOM_INC
#define CERNLIB_PAW_PAWCOM_INC
*
*
* pawcom.inc
*
*CMZ :  2.03/07 03/09/93  14.10.38  by  Fons Rademakers
*-- Author :
      COMMON/PAWCOM/ID,NUM,JOFSET,IDOLD,KHRIN
      COMMON/PAWCID/CHID,CHCASE
      CHARACTER*128 CHID
      CHARACTER*4 CHCASE
*

#endif
