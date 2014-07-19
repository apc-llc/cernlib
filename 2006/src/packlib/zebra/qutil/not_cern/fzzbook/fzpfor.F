*
* $Id: fzpfor.F,v 1.1.1.1 1996/03/06 10:47:21 mclareni Exp $
*
* $Log: fzpfor.F,v $
* Revision 1.1.1.1  1996/03/06 10:47:21  mclareni
* Zebra
*
*
* ----------------------------------------------------------
#include "sys/CERNLIB_machine.h"
#include "_zebra/pilot.h"
      SUBROUTINE FZPFOR(IOFORM,NOFORM,ICFORM,NCFORM)
#include "dzc1.inc"
#include "zunit.inc"
#include "bkfoparq.inc"

      CHARACTER CHROUT*(*)
      INTEGER ICFORM(*),IOFORM(*)
      PARAMETER (LENLOC=200)
      CHARACTER CLOC*(LENLOC),CTYP(4)*1
      PARAMETER (CHROUT = 'FZPFOR')

      DATA CTYP/'B','I','F','H'/
#if (defined(CERNLIB_DEBUGON))&&(defined(CERNLIB_VFORT))
#include "debugvf2.inc"
#endif

      CLOC   = ' '
      NCH    = 0
      NREP   = 0

      DO 100 I=1,NOFORM
          NF     = IOFORM(I)/10000
          N      = IOFORM(I)-10000*NF
          IF (NF.EQ.0)  THEN
              IF (N.GT.0)  THEN
                  NREP = N
                  IREP = NCH + 1
              ELSE
                  NCH=NCH+1
                  CLOC(NCH:) = '/'
                  NREP = 0
                  NCH=NCH+1
              ENDIF
          ELSEIF (NF.GT.0.AND.NF.LT.5)  THEN
              WRITE (CLOC(NCH+1:),'(I4,A)') N,CTYP(NF)
              NCH=NCH+5
          ELSE
              IEND = NCH
              DO 10 J=1,NREP-1
                  CLOC(NCH+1:) = CLOC(IREP:IEND)
                  NCH          = NCH + IEND - IREP + 1
                  IF (NCH.GE.LENLOC) THEN
                      WRITE(IQPRNT,'(''0FZPFOR Format too long '',/,
     X                1X,A,/,'' Bank treated as bit string'')')
     X                ' '//CLOC(1:NCH)
                      ICFORM(1) = IFOBIQ
                      NCFORM    = 1
                                                           GO TO 999
                  ENDIF
   10         CONTINUE
          ENDIF
  100 CONTINUE


      I      = 1
   20 IF (I.GT.NCH)                                        GO TO 30
          IF (CLOC(I:I).EQ.' ') THEN
              CLOC(I:NCH-1) = CLOC(I+1:NCH)
              NCH           = NCH - 1
          ENDIF
          I      = I + 1
                                                           GO TO 20


   30 CALL MZIOCH(ICFORM,NCFORM,CLOC(1:NCH))

  999 RETURN
      END
