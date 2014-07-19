*
* $Id: pcchar.inc,v 1.2 1996/03/01 16:50:12 cernlib Exp $
*
* $Log: pcchar.inc,v $
* Revision 1.2  1996/03/01 16:50:12  cernlib
* All includes  had picked up a completely wrong #if defined(CERNLIB_UNIX), ie.
*  the real commons/declarations were missing on non-unix.
*
* Revision 1.1.1.1  1996/03/01 11:38:49  mclareni
* Paw
*
*
#ifndef CERNLIB_PAW_PCCHAR_INC
#define CERNLIB_PAW_PCCHAR_INC
*
*
* pcchar.inc
*
*CMZ :  2.07/17 17/11/95  12.01.12  by  O.Couet
*-- Author :
      PARAMETER (NCHAR=128)
      CHARACTER*(NCHAR) CHTITL,CHUTIT,CHFILE,CHTEMP
      COMMON/PCCHAR/CHTITL,CHUTIT,CHFILE,CHTEMP

#endif
