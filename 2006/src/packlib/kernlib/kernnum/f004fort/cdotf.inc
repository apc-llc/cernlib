*
* $Id: cdotf.inc,v 1.1.1.1 1996/02/15 17:49:00 mclareni Exp $
*
* $Log: cdotf.inc,v $
* Revision 1.1.1.1  1996/02/15 17:49:00  mclareni
* Kernlib
*
*
*
* cdotf.inc
*
#if !defined(CERNLIB_NUMCDBLE)
          COMPLEX             A, B, SUM, DOTF
          DOTF(A,B,SUM)  =  A*B + SUM
#endif
#if defined(CERNLIB_NUMCDBLE)
          COMPLEX             A, B
          COMPLEX*16          SUM, DC, DOTF
          DC(A)          =  DCMPLX(DBLE(REAL(A)),DBLE(AIMAG(A)))
          DOTF(A,B,SUM)  =  DC(A)*DC(B) + SUM
#endif
