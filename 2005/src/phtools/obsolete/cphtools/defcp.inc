*
* $Id: defcp.inc,v 1.1.1.1 1996/03/22 16:42:46 mclareni Exp $
*
* $Log: defcp.inc,v $
* Revision 1.1.1.1  1996/03/22 16:42:46  mclareni
* Phtools
*
*
*
* defcp.inc
*
#if defined(CERNLIB_SINGLE)
      COMPLEX
#endif
#if defined(CERNLIB_DOUBLE)
      COMPLEX*16
#endif
