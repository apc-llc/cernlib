*
* $Id: getarg.F,v 1.1.1.1 1996/02/15 17:51:09 mclareni Exp $
*
* $Log: getarg.F,v $
* Revision 1.1.1.1  1996/02/15 17:51:09  mclareni
* Kernlib
*
*
#include "kernapo/pilot.h"
#if defined(CERNLIB_QF_APO)
      SUBROUTINE GETARG (JPARA,CHPARA)
C
C CERN PROGLIB#         GETARG          .VERSION KERNAPO  1.20  900328
C ORIG. 22/03/90  JZ
C
C-    APOLLO inplementation of UNIX function

      CHARACTER    CHPARA*(*)
      INTEGER*2    PGM_$GET_ARG, JARG, NCHARG, NCHMAX


      JARG   = JPARA
      NCHMAX = LEN (CHPARA)
      NCHARG = PGM_$GET_ARG (JARG, CHPARA, ISTAT)

      IF (NCHARG.GE.NCHMAX)        RETURN
      CHPARA(NCHARG+1:NCHMAX) = ' '
      RETURN
      END
#ifdef CERNLIB_CCGEN_GETARG
#undef CERNLIB_CCGEN_GETARG
#endif
#ifdef CERNLIB_TCGEN_GETARG
#undef CERNLIB_TCGEN_GETARG
#endif
#endif
