*
* $Id: q_jbit.inc,v 1.1.1.1 1996/02/15 17:53:03 mclareni Exp $
*
* $Log: q_jbit.inc,v $
* Revision 1.1.1.1  1996/02/15 17:53:03  mclareni
* Kernlib
*
*
*
* q_jbit.inc
*
      JBIT (IZW,IZP)    = SHFTR (SHFTL(IZW,32-IZP), 31)
      JBYT (IZW,IZP,NZB)= SHFTR (SHFTL(IZW,33-IZP-NZB), 32-NZB)
