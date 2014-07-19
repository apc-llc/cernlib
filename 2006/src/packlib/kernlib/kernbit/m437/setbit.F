*
* $Id: setbit.F,v 1.1.1.1 1996/02/15 17:47:50 mclareni Exp $
*
* $Log: setbit.F,v $
* Revision 1.1.1.1  1996/02/15 17:47:50  mclareni
* Kernlib
*
*
#include "kernbit/pilot.h"
#if defined(CERNLIB_FORTRAN)||defined(CERNLIB_NORD)||defined(CERNLIB_PDP)||defined(CERNLIB_UNIVAC)||defined(CERNLIB_CRAY)||defined(CERNLIB_UNIX)
      SUBROUTINE SETBIT ( I, M, L)
C--      SUBROUTINE TO SET A SINGLE BIT IN A CONTINUOUS STRING
C--      DIMENSION M - DANGEROUS FOR SOME COMPILERS !
      DIMENSION M(2)
C--      DEFAULT FORTRAN VALUE OF INWORD= 32 BITS PER WORD
      DATA INWORD / 32/
#if defined(CERNLIB_DOUBLE)
C--     INWORD FOR IBM,VAX OR NORD ( 32 BITS PER WORD)
      INWORD=32
#endif
#if defined(CERNLIB_PDP)||defined(CERNLIB_UNIVAC)
C--     INWORD FOR PDP (DEC-10 OR 20) OR UNIVAC ( 36 BITS PER WORD)
      INWORD=36
#endif
#if defined(CERNLIB_CDC)
C--     INWORD FOR CDC ( 60 BITS PER WORD)
      INWORD=60
#endif
#if (defined(CERNLIB_SINGLE))&&(!defined(CERNLIB_CDC))
C--     INWORD FOR CRAY ( 64 BITS PER WORD) +CONVEX(SINGLE MODE)
      INWORD=64
#endif
C--      DO SOME INTEGER (TRUNCATED) ARITHMETIC
      IWORD= (I-1)/INWORD + 1
      IBIT=  IWORD*INWORD - I + 1
      CALL SBIT ( L, M(IWORD), IBIT)
      RETURN
      END
#endif
