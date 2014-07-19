*
* $Id: jbyt4.inc,v 1.1.1.1 1996/01/16 17:07:53 mclareni Exp $
*
* $Log: jbyt4.inc,v $
* Revision 1.1.1.1  1996/01/16 17:07:53  mclareni
* First import
*
*
#ifndef CERNLIB_HBOOK_JBYT4_INC
#define CERNLIB_HBOOK_JBYT4_INC
*
*
* jbyt4.inc
*
*CMZ :  4.19/00 19/04/93  09.55.52  by  Rene Brun
*-- Author :
*
      JBIT (IZW,IZP)      = AND(RSHIFT(IZW,IZP-1),1)
      JBYT (IZW,IZP,NZB) =  ISHFTR(LSHIFT(IZW,33-IZP-NZB),32-NZB)

#endif
