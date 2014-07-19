*
* $Id: impcp.inc,v 1.1.1.1 1996/03/22 16:42:45 mclareni Exp $
*
* $Log: impcp.inc,v $
* Revision 1.1.1.1  1996/03/22 16:42:45  mclareni
* Phtools
*
*
*
* impcp.inc
*
#if defined(CERNLIB_DOUBLE)
      IMPLICIT COMPLEX*16(A-H,I,O-Z)
#endif
#if defined(CERNLIB_SINGLE)
      IMPLICIT COMPLEX   (A-H,I,O-Z)
#endif
