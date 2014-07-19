*
* $Id: kamake.F,v 1.1.1.1 1996/03/08 11:40:51 mclareni Exp $
*
* $Log: kamake.F,v $
* Revision 1.1.1.1  1996/03/08 11:40:51  mclareni
* Kapack
*
*
#include "kapack/pilot.h"
      SUBROUTINE KAMAKE(LUN,NRECS,NWORDS,IRC)
*
*.....CREATE A NEW KA-FILE
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
      EQUIVALENCE (ISIZE,SIZE)
*
*-----------------------------------------------------------------------
*
      CALL KAXINN(LUN)
*
*.....ENSURE THERE IS NOT ALREADY A KA-FILE ATTACHED TO THE UNIT
      IF ( LBKS(LUNKAF) .NE. 0 )  GO TO 21
*
*.....VALIDATE THE ARGUMENTS
      IF ( NWORDS .EQ. 0 )  THEN
         ISIZE = NRECS
         IF ( SIZE .LT. 0.0 )  GO TO 13
      ELSE
         IF ( NRECS .LT. 1 )  GO TO 11
         IF ( NWORDS .LT. 0 )  GO TO 12
      ENDIF
*
*.....CALCULATE THE KEY PACKING FACTOR AND THE MAXIMUM KEY VALUE
      N      = MAXINT
      MAXKEY = 0
      MAXPAK = 0
 1    IF ( N/99 .NE. 0 )  THEN
         MAXKEY = MAXKEY*100 + 99
         MAXPAK = MAXPAK + 1
         N      = N/100
         GO TO 1
      ENDIF
      IF ( MAXPAK .LT. 1 )  GO TO 91
*
*.....ENSURE THAT THE BUFFER LENGTH IS SUFFICIENT
      MINBUF = NBCW1 + 3*(MAXNAM/MAXPAK+6)
      IF ( LBUF .LT. MINBUF )  GO TO 92
*
*.....CALCULATE THE NUMBER OF BLOCKS, NB, REQUIRED
      IF ( NWORDS .EQ. 0 )  THEN
         NB     = 1 024.0 * 1 024.0 * SIZE / REAL ( LBUF*LWORD ) + 1.
      ELSE
         AVKEY  = REAL( (MAXNAM/MAXPAK + 2)/2 )
         AVRECL = REAL( NWORDS ) + AVKEY
         RPB    = REAL( LBUF - NBCW ) / AVRECL
         BLOCKS = REAL( NRECS ) / RPB
         NB     = MAX( INT( 1.1 * BLOCKS ) , 5 )
      ENDIF
*
*.....OPEN THE FILE FOR DIRECT ACCESS
#if defined(CERNLIB_UNIX)
         WRITE(MSG,1000) LUNKAF
 1000    FORMAT('CRNKA08A KAMAKE: ENTER THE NAME OF THE KA-FILE FOR',
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
      OPEN  ( BUFFER_SIZE = LBUF * LWORD,
     +        MODE        = 'SEGMENT',
#endif
#if defined(CERNLIB_UNIX)||defined(CERNLIB_CRAY)
     +        FILE   =  MSG(1:INDEX(MSG,' ')-1),
#endif
     +        ERR    =  93,
     +        FORM   = 'UNFORMATTED',
     +        IOSTAT =  IOS,
     +        RECL   =  LBUF * LWORD,
#if defined(CERNLIB_VAX)
     +        STATUS =  'UNKNOWN',
#endif
#if defined(CERNLIB_CRAY)
     +        STATUS =  'NEW',
#endif
     +        UNIT   =  LUNKAF )
      IF ( IOS .NE. 0 )  GO TO 93
      LBKS(LUNKAF) = -1
*
*.....FORMAT AND WRITE BLOCK 1
      CALL UZERO(IA,1,LBUF)
      IA( 1) = 1
      IA( 2) = NB - 1
      IA( 3) = 2
      IA( 4) = 1
      IA( 5) = 1
      IA( 6) = NBCW1 + 1
      IA( 7) = IA(6) + 3
      IA( 8) = 0
      IA( 9) = 0
      IA(10) = 0
      IA(11) = 0
      IA(12) = 0
      IA(13) = KAVRSN
      IA(14) = NB
      IA(15) = LBUF
      IA(16) = MAXPAK
      IA(17) = 0
      IA(18) = 0
      IA(19) = 0
      IA(20) = 0
      IA(21) = 0
      IA(22) = 0
      IA(23) = 0
      IA(24) = 0
*
      IA(NBCW1+1) = 3
      IA(NBCW1+2) = 2
      IA(NBCW1+3) = MAXKEY
*
      WRITE(LUNKAF,ERR=94,IOSTAT=IOS,REC=1)  ( IA(I), I=1,LBUF )
      IF ( IOS .NE. 0 )  GO TO 94
*
*.....FORMAT AND WRITE BLOCKS 2 - (LAST-1)
      CALL UZERO(IA,1,IA(7)-1)
*
      DO  2  NBLOCK = 2, NB-1
      IA(1) = NBLOCK
      IA(3) = NBLOCK + 1
      WRITE(LUNKAF,ERR=94,IOSTAT=IOS,REC=NBLOCK)  ( IA(I), I=1,LBUF )
      IF ( IOS .NE. 0 )  GO TO 94
 2    CONTINUE
*
*.....FORMAT AND WRITE THE LAST BLOCK
      IA(1) = NB
      IA(3) = 0
      WRITE(LUNKAF,ERR=94,IOSTAT=IOS,REC=NB)  ( IA(I), I=1,LBUF )
      IF ( IOS .NE. 0 )  GO TO 94
*
*.....VERIFY THAT THE LAST BLOCK WAS WRITTEN CORRECTLY
      IA(1) = -1
      IA(3) = -1
      READ(LUNKAF,ERR=95,IOSTAT=IOS,REC=NB) ( IA(I), I=1,3 )
      IF ( IOS .NE. 0 )  GO TO 95
      IF ( IA(1).NE.NB .OR. IA(3).NE.0 )  GO TO 95
*
*.....SUCCESSFUL COMPLETION
      IRC = 0
      IF ( NWORDS .EQ. 0 )  THEN
         WRITE(MSG,100) SIZE, LUNKAF, NB, LBUF*LWORD
      ELSE
         WRITE(MSG,101) LUNKAF, NRECS, NWORDS, NB, LBUF
      ENDIF
      CALL KAXMSG(LUNLOG,MSG)
      RETURN
*
*.....ERROR PROCESSING
 11   IRC = 1
      WRITE(MSG,111) NRECS
      GO TO 99
*
 12   IRC = 1
      WRITE(MSG,112) NWORDS
      GO TO 99
*
 13   IRC = 1
      WRITE(MSG,113) NWORDS
      GO TO 99
*
 21   IRC = 2
      WRITE(MSG,121) LUNKAF
      GO TO 99
*
 91   WRITE(MSG,191)
      CALL KAXMSG(LUNERR,MSG)
      CALL KAXEND
*
 92   WRITE(MSG,192) LBUF, MINBUF
      CALL KAXMSG(LUNERR,MSG)
      CALL KAXEND
*
 93   WRITE(MSG,193) LUNKAF, IOS
      CALL KAXMSG(LUNERR,MSG)
      CALL KAXEND
*
 94   WRITE(MSG,194) NBLOCK, LUNKAF, IOS
      CALL KAXMSG(LUNERR,MSG)
      CALL KAXEND
*
 95   WRITE(MSG,195) LUNKAF, IOS
      CALL KAXMSG(LUNERR,MSG)
      CALL KAXEND
*
 99   CALL KAXMSG(LUNERR,MSG)
      IF ( RETURN )  RETURN
      CALL KAXEND
*
 100  FORMAT('CRNKA080 KAMAKE: KA-FILE OF ',G11.6,
     +       ' MEGABYTES CREATED ON UNIT ',I3,
     +       ', (',I11,' BLOCKS OF ',I11,' BYTES FORMATTED)')
 101  FORMAT('CRNKA080 KAMAKE: KA-FILE CREATED ON UNIT ',I3,' FOR ',
     +        I11,' RECORDS OF AVERAGE LENGTH ',I11,' WORDS, (',
     +        I11,' BLOCKS OF ',I11,' WORDS FORMATTED)')
 111  FORMAT('CRNKA081 KAMAKE: ',I11,' IS NOT A VALID VALUE FOR THE
     +        NUMBER OF RECORDS')
 112  FORMAT('CRNKA082 KAMAKE: ',I11,' IS NOT A VALID VALUE FOR THE
     +        AVERAGE RECORD LENGTH')
 113  FORMAT('CRNKA089 KAMAKE: ',G11.6,' IS NOT A VALID VALUE FOR THE
     +        FILE SIZE')
 121  FORMAT('CRNKA083 KAMAKE: ATTEMPT TO CREATE A NEW KA-FILE ON
     +        UNIT ',I3,' WHICH ALREADY HAS A KA-FILE ATTACHED')
 191  FORMAT('CRNKA084 KAMAKE: INVALID KEY PACKING FACTOR, KAPACK SYSTEM
     +        OR INSTALLATION ERROR')
 192  FORMAT('CRNKA085 KAMAKE: THE BUFFER LENGTH OF ',I11,
     +       ' IS SMALLER THAN THE MINIMUM OF ',I11,
     +       ', KAPACK SYSTEM OR INSTALLATION ERROR')
 193  FORMAT('CRNKA086 KAMAKE: UNABLE TO OPEN A NEW KA-FILE ON UNIT ',
     +        I3,', IOSTAT = ',I11)
 194  FORMAT('CRNKA087 KAMAKE: UNABLE TO WRITE BLOCK ',I11,' TO THE
     +        KA-FILE BEING CREATED ON UNIT ',I3,', IOSTAT = ',I11)
 195  FORMAT('CRNKA088 KAMAKE: ERROR(S) OCCURRED DURING CREATION OF THE
     +        KA-FILE ON UNIT ',I3,', IOSTAT = ',I11)
*
      END
