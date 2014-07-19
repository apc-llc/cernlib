*
* $Id: getf.F,v 1.1.1.1 1996/03/06 10:47:20 mclareni Exp $
*
* $Log: getf.F,v $
* Revision 1.1.1.1  1996/03/06 10:47:20  mclareni
* Zebra
*
*
#include "sys/CERNLIB_machine.h"
#include "_zebra/pilot.h"
      SUBROUTINE GETF(FIN,FOUT,CHIN,CHOU)
#if defined(CERNLIB_QMVAX)
      IMPLICIT INTEGER(A-Z)
      EXTERNAL CLI$_ABSENT,CLI$_DEFAULTED,CLI$_NEGATED,CLI$_PRESENT
#include "rzclun.inc"
#endif
#if defined(CERNLIB_QMIBM)
      CHARACTER*80  PARM
      CHARACTER*(*) SPACES
#endif
#if defined(CERNLIB_QMCV64)
      INTEGER*4 IARGC,NARG,IV1,IV2
#endif
      CHARACTER*(*) FIN, FOUT
      CHARACTER*2   CHIN, CHOU
      DATA IV1/1/, IV2/2/
      FIN  = ' '
      FOUT = ' '
#if defined(CERNLIB_QMVAX)
      RECODE = CLI$GET_VALUE('FIN',FIN,)
      IF(.NOT.RECODE) RECODE=LIB$SIGNAL(%VAL(RECODE))
      RECODE = CLI$GET_VALUE('FOUT',FOUT,)
      IF(.NOT.RECODE) RECODE=LIB$SIGNAL(%VAL(RECODE))
#endif
#if (defined(CERNLIB_QMVAX))&&(defined(CERNLIB_RFRX)||defined(CERNLIB_RFRA))
*
*     Allow for relative organisation files
*
      IRELAT = 1
      IF((CLI$PRESENT('RELATIVE') .EQ. %LOC(CLI$_NEGATED)) .OR.
     +   (CLI$PRESENT('RELATIVE') .EQ. %LOC(CLI$_ABSENT))) IRELAT = 0
#endif
#if defined(CERNLIB_QMCRY)
      NARG = IARGC()
      IF(NARG.LT.1) THEN
        WRITE(6,1001) CHIN
        READ (5,1000) FIN
      ELSE
        LPAR = IARGV(1,FIN)
      END IF
      IF(NARG.LT.2) THEN
        WRITE(6,1001) CHOU
        READ (5,1000) FOUT
      ELSE
        LPAR = IARGV(2,FOUT)
      END IF
#endif
#if defined(CERNLIB_QMCVX)||defined(CERNLIB_QMHPX)||defined(CERNLIB_QMIBX)||defined(CERNLIB_QMIRT)||defined(CERNLIB_QMSGI)||defined(CERNLIB_QMSUN)||defined(CERNLIB_QMVMI)||defined(CERNLIB_QMALT)||defined(CERNLIB_QMDOS)||defined(CERNLIB_QMLNX)||defined(CERNLIB_QMNXT)
      NARG = IARGC()
      IF(NARG.LT.IV1) THEN
        WRITE(6,1001) CHIN
        READ (5,1000) FIN
      ELSE
        CALL GETARG(IV1,FIN)
        LPAR = LENOCC(FIN)
      END IF
      IF(NARG.LT.IV2) THEN
        WRITE(6,1001) CHOU
        READ (5,1000) FOUT
      ELSE
        CALL GETARG(IV2,FOUT)
        LPAR = LENOCC(FOUT)
      END IF
#endif
#if defined(CERNLIB_QMIBM)
      PARM = ' '
      CALL GOPARM(LENGTH,PARM)
      PARM = SPACES(PARM,1)
      CALL CLTOU(PARM)
      DO 10 J=1,LENOCC(PARM)
        IF(PARM(J:J).EQ.'.') PARM(J:J) = ' '
  10  CONTINUE
      INIT = 1
      IEND = INIT + INDEX(PARM(INIT:80),' ')-2
      IF(INIT.GT.IEND) THEN
        WRITE(6,1001) CHIN
        READ (5,1000,END=100,ERR=100) PARM
        PARM = SPACES(PARM,1)
        CALL CLTOU(PARM)
        DO 20 J=1,LENOCC(PARM)
          IF(PARM(J:J).EQ.'.') PARM(J:J)=' '
  20    CONTINUE
        INIT = 1
        IEND = INIT + INDEX(PARM(INIT:80),' ')-2
        IF(INIT.GT.IEND) GO TO 100
      END IF
      IF(IEND-INIT+1.GT.8) GO TO 100
      FIN(2:9) = PARM(INIT:IEND)
      INIT = IEND + 2
      IEND = INIT + INDEX(PARM(INIT:80),' ')-2
      IF(INIT.GT.IEND.OR.IEND-INIT+1.GT.8) GO TO 100
      FIN(11:18) = PARM(INIT:IEND)
      INIT = IEND + 2
      IEND = INIT + INDEX(PARM(INIT:80),' ')-2
      ILEN = IEND-INIT+1
      IF(ILEN.EQ.1) THEN
        FIN(20:21) = PARM(INIT:IEND)
        INIT = IEND + 2
        IEND = INIT + INDEX(PARM(INIT:80),' ')-2
      ELSE IF(ILEN.EQ.2) THEN
        IF(INDEXA(PARM(INIT:IEND)).EQ.1.AND.
     +  INDEXN(PARM(INIT:IEND)).EQ.2) THEN
          FIN(20:21) = PARM(INIT:IEND)
          INIT = IEND + 2
          IEND = INIT + INDEX(PARM(INIT:80),' ')-2
        ELSE
          FIN(20:21) = '*'
        END IF
      ELSE
        FIN(20:21) = '*'
      END IF
C
      IF(INIT.GT.IEND) THEN
        WRITE(6,1001) CHOU
        READ(5,1000,END=100,ERR=100) PARM
        PARM = SPACES(PARM,1)
        CALL CLTOU(PARM)
        DO 30 J=1,LENOCC(PARM)
          IF(PARM(J:J).EQ.'.') PARM(J:J)=' '
  30    CONTINUE
        INIT = 1
        IEND = INIT + INDEX(PARM(INIT:80),' ')-2
        IF(INIT.GT.IEND) GO TO 100
      END IF
      IF(IEND-INIT+1.GT.8) GO TO 100
      FOUT(2:9) = PARM(INIT:IEND)
      INIT = IEND + 2
      IEND = INIT + INDEX(PARM(INIT:80),' ')-2
      IF(INIT.GT.IEND.OR.IEND-INIT+1.GT.8) GO TO 100
      FOUT(11:18) = PARM(INIT:IEND)
      INIT = IEND + 2
      IEND = INIT + INDEX(PARM(INIT:80),' ')-2
      IF(INIT.GT.IEND) THEN
        FOUT(20:21) = 'A'
      ELSE IF(IEND-INIT+1.GT.2) THEN
        GO TO 100
      ELSE
        FOUT(20:21) = PARM(INIT:IEND)
      END IF
      RETURN
C
 100  WRITE(6,'('' Invalid file name'')')
      STOP
#endif
#if defined(CERNLIB_QMAPO)
      LPAR = PGM_$GET_ARG(INT2(1),FIN,ISTAT,INT2(LEN(FIN)))
      IF(ISTAT.NE.0) THEN
        CALL VFMT_$WRITE2(CHIN//'_file: %$',0,0)
        READ(5,1000) FIN
      END IF
      LPAR = PGM_$GET_ARG(INT2(2),FOUT,ISTAT,INT2(LEN(FOUT)))
      IF(ISTAT.NE.0) THEN
        CALL VFMT_$WRITE2(CHOU//'_file: %$',0,0)
        READ(5,1000) FOUT
      END IF
#endif
1000  FORMAT(A)
#if defined(CERNLIB_QMCVX)||defined(CERNLIB_QMCRY)||defined(CERNLIB_QMHPX)||defined(CERNLIB_QMIRT)||defined(CERNLIB_QMSGI)||defined(CERNLIB_QMVMI)||defined(CERNLIB_QMSUN)||defined(CERNLIB_QMALT)||defined(CERNLIB_QMDOS)||defined(CERNLIB_QMLNX)||defined(CERNLIB_QMNXT)
1001  FORMAT(A,'_file: ',$)
#endif
#if defined(CERNLIB_QMIBX)||defined(CERNLIB_QMIBM)
1001  FORMAT(1X,A,'_file: ')
#endif
      END
