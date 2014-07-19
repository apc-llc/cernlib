*
* $Id: kacopy.F,v 1.1.1.1 1996/03/08 11:40:51 mclareni Exp $
*
* $Log: kacopy.F,v $
* Revision 1.1.1.1  1996/03/08 11:40:51  mclareni
* Kapack
*
*
#include "kapack/pilot.h"
      SUBROUTINE KACOPY(LUNIN,LUNOUT,MODE,IRC)
*
*.....COPY, LOAD, UNLOAD OR EXTEND A KA-FILE
*
#include "kapack/kax000.inc"
#include "kapack/kax020.inc"
#include "kapack/kax030.inc"
#include "kapack/kax050.inc"
#include "kapack/kax0a0.inc"
*
#if defined(CERNLIB_CRAY)
      CHARACTER*2 CHALUN
#endif
      INTEGER IFREE(1:4)
*
*-----------------------------------------------------------------------
*
      CALL KAXINN(LUNIN)
*
*.....CHECK THE ARGUMENTS
      IF ( LUNOUT.LT.1 .OR. LUNOUT.GT.MAXLUN )  GO TO 11
      IF ( MODE.LT.1 .OR.
     +    (MODE.GT.3.AND.MODE.LT.10) .OR.
     +     MODE.GT.1000 )  GO TO 12
*
*.....OPEN THE INPUT FILE AND READ THE FIRST BLOCK
      NBLOCK = 1
      IF ( MODE .EQ. 2 )  THEN
         REWIND LUNIN
         READ(LUNIN,ERR=91,IOSTAT=IOS)  (IA(I),I=1,15),
     +                                  (IA(J),J=16,IA(15))
         IF ( IOS .NE. 0 )  GO TO 91
      ELSE
         CALL KAXINI(LUNIN)
         CALL KAXRD(NBLOCK,IA,LBLK)
      ENDIF
      NXFREE = IA( 3)
      NB     = IA(14)
      LBKIN  = IA(15)
*
*.....OPEN THE OUTPUT FILE
      IF ( MODE .EQ. 1 )  THEN
         REWIND LUNOUT
      ELSE IF ( LBKS(LUNOUT) .EQ. 0 )  THEN
#if defined(CERNLIB_UNIX)
         WRITE(MSG,1000) LUNOUT
 1000    FORMAT('CRNKA28A KAMAKE: ENTER THE NAME OF THE KA-FILE FOR',
     +          ' UNIT ',I3)
         CALL KAXMSG(ISYSWR,MSG)
         READ(ISYSRD,1001) MSG
 1001    FORMAT(A)
#endif
#if defined(CERNLIB_CRAY)
         WRITE(CHALUN,'(I2)') LUNOUT
         MSG = 'fort.'//CHALUN(INDEX(CHALUN,' ')+1:2)
#endif
#if !defined(CERNLIB_NORD)
         OPEN  ( ACCESS = 'DIRECT',
#endif
#if defined(CERNLIB_NORD)
         OPEN  ( BUFFER_SIZE = LWORD * LBKIN,
     +           MODE        = 'SEGMENT',
#endif
#if defined(CERNLIB_UNIX)||defined(CERNLIB_CRAY)
     +           FILE   =  MSG(1:INDEX(MSG,' ')-1),
#endif
     +           ERR    =  92,
     +           FORM   = 'UNFORMATTED',
     +           IOSTAT =  IOS,
     +           RECL   =  LWORD*LBKIN,
#if defined(CERNLIB_VAX)
     +           STATUS =  'UNKNOWN',
#endif
#if defined(CERNLIB_CRAY)
     +           STATUS =  'NEW',
#endif
     +           UNIT   =  LUNOUT )
         IF ( IOS .NE. 0 )  GO TO 92
         LBKS(LUNOUT) = -1
      ELSE
         GO TO 93
      ENDIF
*
*.....WRITE THE FIRST BLOCK
      IF ( MODE .EQ. 1 )  THEN
         WRITE(LUNOUT,ERR=94,IOSTAT=IOS)  (IA(I),I=1,LBKIN)
         IF ( IOS .NE. 0 )  GO TO 94
      ELSE
         WRITE(LUNOUT,ERR=94,IOSTAT=IOS,REC=NBLOCK)  (IA(I),I=1,LBKIN)
         IF ( IOS .NE. 0 )  GO TO 94
      ENDIF
*
*.....COPY THE REMAINING BLOCKS
      DO  1  NBLOCK = 2, NB
         IF ( MODE .EQ. 2 )  THEN
            READ(LUNIN,END=96,ERR=91,IOSTAT=IOS)  (IA(I),I=1,LBKIN)
            IF ( IOS .NE. 0 )  GO TO 91
         ELSE
            CALL KAXRD(NBLOCK,IA,LBKIN)
         ENDIF
         IF ( IA(1) .NE. NBLOCK )  GO TO 95
         IF ( MODE .EQ. 1 )  THEN
            WRITE(LUNOUT,ERR=94,IOSTAT=IOS)  (IA(I),I=1,LBKIN)
            IF ( IOS .NE. 0 )  GO TO 94
         ELSE
            WRITE(LUNOUT,ERR=94,IOSTAT=IOS,REC=NBLOCK) (IA(I),I=1,LBKIN)
            IF ( IOS .NE. 0 )  GO TO 94
         ENDIF
 1    CONTINUE
*
*.....CHAIN THE NEW FREE BLOCKS IF EXPANSION HAS BEEN REQUESTED
      IF ( MODE .LT. 10 )  THEN
         NEW = 0
         NXFREE= IA(3)
      ELSE
         NEW      = NINT(REAL(NB)*REAL(MODE)/100.0)
         IFREE(2) = 0
         IFREE(3) = NXFREE
         IFREE(4) = 0
         DO  2  NBLOCK = NB+NEW, NB+1, -1
            IFREE(1) = NBLOCK
            WRITE(LUNOUT,ERR=94,IOSTAT=IOS,REC=NBLOCK) IFREE
            IF ( IOS .NE. 0 )  GO TO 94
            IFREE(3) = NBLOCK
 2       CONTINUE
         CALL KAXRD(1,IA,LBKIN)
         IA( 2) = IA( 2) + NEW
         IA( 3) = IFREE(1)
         IA(14) = NB + NEW
         WRITE(LUNOUT,ERR=94,IOSTAT=IOS,REC=1) (IA(I),I=1,LBKIN)
         IF ( IOS .NE. 0 )  GO TO 94
      ENDIF
*
*.....VERIFY THAT THE LAST BLOCK WAS WRITTEN CORRECTLY IF MODE>1
      IF ( MODE .GT. 1 )  THEN
         IA(1) = -1
         IA(3) = -1
         READ(LUNOUT,ERR=97,IOSTAT=IOS,REC=NB+NEW) ( IA(I), I=1,3 )
         IF ( IOS .NE. 0 )  GO TO 97
         IF ( IA(1).NE.NB+NEW .OR. IA(3).NE.NXFREE )  GO TO 97
      ENDIF
*
*.....SUCCESSFUL COMPLETION
      IRC = 0
      IF ( MODE .EQ. 1 )  WRITE(MSG,101) NB, LUNIN, LUNOUT
      IF ( MODE .EQ. 2 )  WRITE(MSG,102) NB, LUNIN, LUNOUT
      IF ( MODE .EQ. 3 )  WRITE(MSG,103) NB, LUNIN, LUNOUT
      IF ( MODE .GE. 10)  WRITE(MSG,110) NB, LUNIN, LUNOUT, NEW, MODE
      CALL KAXMSG(LUNLOG,MSG)
      RETURN
*
*.....ERROR PROCESSING
 11   IRC = 1
      WRITE(MSG,111) LUNOUT, MAXLUN
      GO TO 99
*
 12   IRC = 1
      WRITE(MSG,112) MODE
      GO TO 99
*
 91   WRITE(MSG,191) NBLOCK, LUNIN, IOS
      CALL KAXMSG(LUNERR,MSG)
      CALL KAXEND
*
 92   WRITE(MSG,192) LUNOUT, IOS
      CALL KAXMSG(LUNERR,MSG)
      CALL KAXEND
*
 93   WRITE(MSG,193) LUNOUT
      CALL KAXMSG(LUNERR,MSG)
      CALL KAXEND
*
 94   WRITE(MSG,194) NBLOCK, LUNOUT, IOS
      CALL KAXMSG(LUNERR,MSG)
      CALL KAXEND
*
 95   WRITE(MSG,195) LUNIN, NBLOCK, IA(1)
      CALL KAXMSG(LUNERR,MSG)
      CALL KAXEND
*
 96   WRITE(MSG,196) LUNIN, NBLOCK, NB, IOS
      CALL KAXMSG(LUNERR,MSG)
      CALL KAXEND
*
 97   WRITE(MSG,197) LUNOUT, IOS
      CALL KAXMSG(LUNERR,MSG)
      CALL KAXEND
*
 99   CALL KAXMSG(LUNERR,MSG)
      IF ( RETURN )  RETURN
      CALL KAXEND
*
 101  FORMAT('CRNKA280 KACOPY: ',I11,
     +       ' BLOCKS SUCCESSFULLY UNLOADED FROM THE KA-FILE ON UNIT ',
     +        I3,' TO THE SEQUENTIAL FILE ON UNIT ',I3)
 102  FORMAT('CRNKA280 KACOPY: ',I11,' BLOCKS SUCCESSFULLY LOADED ',
     +       ' FROM THE SEQUENTIAL FILE ON UNIT ',I3,
     +       ' TO THE KA-FILE ON UNIT ',I3)
 103  FORMAT('CRNKA280 KACOPY: ',I11,
     +       ' BLOCKS SUCCESSFULLY COPIED FROM THE KA-FILE ON UNIT ',I3,
     +       ' TO THE KA-FILE ON UNIT ',I3)
 110  FORMAT('CRNKA280 KACOPY: ',I11,
     +       ' BLOCKS SUCCESSFULLY COPIED FROM THE KA-FILE ON UNIT ',I3,
     +       ' TO THE KA-FILE ON UNIT ',I3,' WITH THE ADDITION OF ',I11,
     +       ' (',I11,'%) NEW BLOCKS')
 111  FORMAT('CRNKA281 KACOPY: ',I11,' IS NOT AN ACCEPTABLE UNIT NUMBER
     +        FOR THE OUTPUT FILE, IT MUST BE IN THE RANGE 1 - ',I3)
 112  FORMAT('CRNKA282 KACOPY: ',I11,' IS NOT AN ACCEPTABLE MODE, IT
     +        MUST BE IN THE RANGE 1-3 OR 10-1000')
 191  FORMAT('CRNKA283 KACOPY: I/O ERROR WHILE READING BLOCK ',I11,
     +       ' FROM UNIT ',I3,', IOSTAT = ',I11)
 192  FORMAT('CRNKA284 KACOPY: ERROR WHILE OPENING THE FILE ON UNIT ',
     +        I3,', IOSTAT = ',I11)
 193  FORMAT('CRNKA285 KACOPY: ATTEMPT TO OVERWRITE AN EXISTING KA-FILE
     +        ON UNIT ',I3)
 194  FORMAT('CRNKA286 KACOPY: I/O ERROR WHILE WRITING BLOCK ',I11,
     +       ' TO UNIT ',I3,', IOSTAT = ',I11)
 195  FORMAT('CRNKA287 KACOPY: INVALID BLOCK NUMBER OR IDENTIFIER ON
     +        UNIT ',I3,', NUMBER = ',I11,', IDENTIFIER = ',I11)
 196  FORMAT('CRNKA288 KACOPY: EOF ENCOUNTERED ON UNIT ',I3,
     +       ' WHILE READING BLOCK ',I11,
     +       ' FROM THE KA-FILE WHICH SHOULD CONTAIN ',I11,' BLOCKS, ',
     +       ' IOSTAT = ',I11)
 197  FORMAT('CRNKA289 KACOPY: ERROR(S) OCCURRED DURING CREATION OF THE
     +        NEW KA-FILE ON UNIT ',I3,', IOSTAT = ',I11)
*
      END
