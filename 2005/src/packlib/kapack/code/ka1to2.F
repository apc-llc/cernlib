*
* $Id: ka1to2.F,v 1.1.1.1 1996/03/08 11:40:51 mclareni Exp $
*
* $Log: ka1to2.F,v $
* Revision 1.1.1.1  1996/03/08 11:40:51  mclareni
* Kapack
*
*
#include "kapack/pilot.h"
      SUBROUTINE KA1TO2(LUN)
*
*.....CONVERT A KA-FILE FROM VERSION 1 TO VERSION 2
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
*
*-----------------------------------------------------------------------
*
      CALL KAXINN(LUN)
*
*.....OPEN THE KA-FILE
      IF ( KAXRCL(LUNKAF) .LE. 0 )  GO TO 91
      IF ( LBKS(LUNKAF) .EQ. 0 )  THEN
#if defined(CERNLIB_UNIX)
         WRITE(MSG,1000) LUNKAF
 1000    FORMAT('CRNKA506 KA1TO2: ENTER THE NAME OF THE KA-FILE FOR',
     +          ' UNIT ',I3)
         CALL KAXMSG(ISYSWR,MSG)
         READ(ISYSRD,1001) MSG
 1001    FORMAT(A)
#endif
#if defined(CERNLIB_CRAY)
         WRITE(CHALUN,'(I2)') LUNKAF
         MSG = 'fort.'//CHALUN(INDEX(CHALUN,' ')+1:2)
#endif
#if !defined(CERNLIB_NORD)
         OPEN  ( ACCESS = 'DIRECT',
#endif
#if defined(CERNLIB_NORD)
         OPEN  ( BUFFER_SIZE = KAXRCL ( LUNKAF ),
     +           MODE        = 'SEGMENT',
#endif
#if defined(CERNLIB_UNIX)||defined(CERNLIB_CRAY)
     +           FILE   =  MSG(1:INDEX(MSG,' ')-1),
#endif
     +           ERR    =  92,
     +           FORM   = 'UNFORMATTED',
     +           IOSTAT =  IOS,
     +           RECL   =  KAXRCL(LUNKAF),
#if defined(CERNLIB_VAX)
     +           STATUS =  'UNKNOWN',
#endif
#if defined(CERNLIB_CRAY)
     +           STATUS =  'OLD',
#endif
     +           UNIT   =  LUNKAF )
         IF ( IOS .NE. 0 )  GO TO 92
      ENDIF
*
*.....VALIDATE THE FILE ATTRIBUTES
      READ(LUNKAF,ERR=93,IOSTAT=IOS,REC=1) (IA(I),I=1,NBCW1)
      IF ( IOS .NE. 0 )  GO TO 93
      IF ( IA(13) .NE. 1 )  GO TO 94
      IF ( IA(15) .GT. LBUF )  GO TO 95
*
*.....STORE FILE-DEPENDENT PARAMETERS
      NBLKS  = IA(14)
      LBLK   = IA(15)
      MAXPAK = IA(16)
*
*.....DO THE CONVERSION FOR ALL NON-FREE BLOCKS
      DO  1  NBLOCK = NBLKS, 1, -1
         CALL KAXRD(NBLOCK,IA,LBLK)
         IF ( IA(4).NE.0 .AND. IA(8).LE.1 )  THEN
            CALL KAZ1A2
            IF ( NBLOCK .EQ. 1 )  IA(13) = 2
            CALL KAXWRT(NBLOCK,IA,LBLK)
         ENDIF
 1    CONTINUE
*
*.....SUCCESSFUL COMPLETION
      WRITE(MSG,100) LUNKAF
      CALL KAXMSG(LUNLOG,MSG)
      STOP
*
*.....ERROR PROCESSING
 91   WRITE(MSG,191) LUNKAF
      GO TO 99
*
 92   WRITE(MSG,192) LUNKAF, IOS
      GO TO 99
*
 93   WRITE(MSG,193) LUNKAF, IOS
      GO TO 99
*
 94   WRITE(MSG,194) LUNKAF
      GO TO 99
*
 95   WRITE(MSG,195) IA(15), LUNKAF, LBUF
      GO TO 99
*
 99   CALL KAXMSG(LUNERR,MSG)
      CALL KAXEND
*
 100  FORMAT('CRNKA500 KA1TO2: CONVERSION COMPLETE FOR THE KA-FILE ON
     +        UNIT ',I3)
 191  FORMAT('CRNKA501 KA1TO2: UNABLE TO OBTAIN THE BLOCK LENGTH FOR
     +        THE KA-FILE ON UNIT ',I3)
 192  FORMAT('CRNKA502 KA1TO2: ERROR WHILE OPENING THE KA-FILE ON
     +        UNIT ',I3,', IOSTAT = ',I11)
 193  FORMAT('CRNKA503 KA1TO2: I/O ERROR WHILE READING BLOCK 1 FROM
     +        UNIT ',I3,', IOSTAT = ',I11)
 194  FORMAT('CRNKA504 KA1TO2: THE KA-FILE ON UNIT ',I3,
     +       ' CANNOT BE CONVERTED AS IT IS NOT A VERSION 1 KA-FILE')
 195  FORMAT('CRNKA505 KA1TO2: THE BLOCK LENGTH OF ',I11,' WORDS ON
     +        UNIT ',I3,' EXCEEDS THE BUFFER LENGTH OF ',I11,' WORDS')
*
      END
