*
* $Id: cvfixto.F,v 1.1.1.1 1996/02/15 17:51:00 mclareni Exp $
*
* $Log: cvfixto.F,v $
* Revision 1.1.1.1  1996/02/15 17:51:00  mclareni
* Kernlib
*
*
#include "kernapo/pilot.h"
      PROGRAM CVFIXTO

%INCLUDE  '/sys/ins/base.ins.ftn'
%INCLUDE  '/sys/ins/ios.ins.ftn'

      PARAMETER   (NCHNAM=80)
      INTEGER*2    PGM_$GET_ARG
      INTEGER*2    NARGS, ncharg, JARG
      CHARACTER    CHTARG*(NCHNAM)
      CHARACTER    TEXT*(NCHNAM)
      CHARACTER    CHTYP*4

      INTEGER*2    IDSTROLD,IDSTRNEW, IRTOLD, IRTYPE
      DIMENSION    IUID(2)
      PARAMETER   (IUIDREC=768, IUIDUA=785)


      CALL PGM_$GET_ARGS (NARGS,LPOIARG)
      NARGS  = NARGS - 1
      NBYTES = 0
      IF (NARGS-3)           12, 24, 21
   12 PRINT 9009,
 9009 FORMAT (' CVFIXTO: program to copy a file'
     F,' with fixed-length records'
     F/10X,'changing the Apollo attributes to one of 2 types:'
     F/10X,'type = F2    fixed-length with system control-word'
C    F/15X,     '= F1    fixed-length without control-word'
     F/15X,     '= UASC  simple stream of bytes'
     F/15X,     '  (R taken as F, A taken as U)'
     F/9X,' Please give  CVFIXTO type source target [nbytes]')
      STOP

C--                Get parameter 4, record size

   21 ncharg = PGM_$GET_ARG (INT2(4),TEXT,ISTAT)
      NTXT = ncharg
      NBYTES = ICDECI (TEXT,1,NTXT)

C--                Set the type of the output file from parameter 1

   24 ncharg = PGM_$GET_ARG (INT2(1),TEXT,ISTAT)
      NTXT = ncharg
      TEXT(NTXT+1:) = ' '
      CALL CLTOU (TEXT)

      IUID(2) = 0
      IF (TEXT(1:1).EQ.'R')  TEXT(1:1)='F'
      IF (TEXT(1:1).EQ.'A')  TEXT(1:1)='U'
      IF (TEXT(1:1).EQ.'F')  THEN
          KIND    = 2
          IUID(1) = IUIDREC
          IRTYPE  = IOS_$F2
          CHTYP   = 'f2'
C-        format F1 is not currently implemented in IOS
C         IF (TEXT(2:2).EQ.'1')  THEN
C             KIND    = 1
C             IRTYPE  = IOS_$F1
C             CHTYP   = 'F1'
C           ENDIF
        ELSE
          IF (TEXT(1:1).NE.'U')  GO TO 12
          KIND    = 3
          IUID(1) = IUIDUA
          CHTYP   = 'uasc'
        ENDIF

C--                Get target filename, and CREATE

      ncharg = PGM_$GET_ARG (INT2(3),CHTARG,ISTAT)
      NCHTAR = ncharg
      CHTARG(NCHTAR+1:NCHNAM) = '       '

      CALL FLOPER (2, CHTARG(1:NCHTAR), 9)

      CALL IOS_$CREATE (CHTARG,ncharg
     +,                     IUID
     +,                     IOS_$NO_PRE_EXIST_MODE
     +,                     IOS_$WRITE_OPT
     +,                     IDSTRNEW, ISTAT)
      ISTATC = ISTAT

C--                Get source file-name, and OPEN

      ncharg = PGM_$GET_ARG (INT2(2),TEXT,ISTAT)
      NTXT = ncharg
      TEXT(NTXT+1:NCHNAM) = '       '

      IDSTROLD = IOS_$OPEN (TEXT,ncharg,0,ISTAT)
      IF (ISTAT.NE.0)  THEN
          PRINT 9034, ISTAT
 9034     FORMAT (' ******* OPEN failed, ISTAT=',Z9,' hex')
          STOP
        ENDIF

      IRTOLD = IOS_$INQ_REC_TYPE (IDSTROLD,ISTAT)
      IF (IRTOLD.EQ.IOS_$UNDEF)    GO TO 37

      LENFIR = IOS_$INQ_CUR_REC_LEN (IDSTROLD,ISTAT)

      PRINT 9035, LENFIR
 9035 FORMAT ('  Apollo thinks the record-length on the source file is'
     F,I6,' bytes/r')

      IF (NBYTES.EQ.0)  NBYTES = MAX (0,LENFIR)

C--                Set the record type of the target file

   37 IF (NBYTES.EQ.0)  NBYTES = 3600
      IF (KIND.LT.3)  THEN
          CALL IOS_$SET_REC_TYPE (IDSTRNEW,IRTYPE,NBYTES,ISTAT)
          IF (ISTAT.NE.0)  ISTATC=ISTAT
        ENDIF

      IF (ISTATC.NE.0)  THEN
          PRINT 9036, ISTATC
 9036 FORMAT     (' ******* CREATE failed, ISTAT=',Z9,' hex')
          STOP
        ENDIF

C----              Copy file

      PRINT 9041, CHTYP,TEXT(1:NTXT),CHTARG(1:NCHTAR),NBYTES
 9041 FORMAT ('  Convert to ',A/4X,'from file ',A/6X,'to file ',A
     F/14X,'Lrec =',I5,' bytes')

      NROLD  = 0
      NWCEIN = NBYTES/4

   43 NWR = IOS_$GET (IDSTROLD
     +,                IOS_$NO_REC_BNDRY_OPT
     +,                MBUF, NBYTES, ISTAT)
      IF   (ISTAT.NE.0)           GO TO 46

      CALL IOS_$PUT (IDSTRNEW,0,MBUF,NBYTES,ISTAT)
      IF (ISTAT.NE.0)        GO TO 45
      NROLD = NROLD + 1
      GO TO 43

   45 PRINT 9045, NROLD,ISTAT
 9045 FORMAT (' ******* PUT failed after',I6,
     F' records, ISTAT=',Z9,' hex')
      STOP

   46 IF (ISTAT.EQ.16#05040008)        GO TO 71
      IF (ISTAT.EQ.16#01270009)        GO TO 71
      PRINT 9048, NROLD,ISTAT
 9048 FORMAT (' ******* GET failed after',I6,
     F' records, ISTAT= ',Z9,' hex')
      STOP

C----              End

   71 PRINT 9071, NROLD
 9071 FORMAT (6X,I7,' records copied.')

      CALL IOS_$CLOSE (IDSTROLD,ISTAT)
      CALL IOS_$CLOSE (IDSTRNEW,ISTAT)
      CALL PGM_$EXIT
      END
