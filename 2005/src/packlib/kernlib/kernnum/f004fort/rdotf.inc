*
* $Id: rdotf.inc,v 1.1.1.1 1996/02/15 17:49:00 mclareni Exp $
*
* $Log: rdotf.inc,v $
* Revision 1.1.1.1  1996/02/15 17:49:00  mclareni
* Kernlib
*
*
*
* rdotf.inc
*
#if !defined(CERNLIB_NUMRDBLE)
          REAL                A, B, SUM, DOTF
          DOTF(A,B,SUM)  =  A*B + SUM
#endif
#if defined(CERNLIB_NUMRDBLE)
          REAL                A, B
          DOUBLE PRECISION    SUM, DOTF
          DOTF(A,B,SUM)  =  DBLE(A)*DBLE(B) + SUM
#endif
