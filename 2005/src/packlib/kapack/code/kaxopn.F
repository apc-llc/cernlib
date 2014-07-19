*
* $Id: kaxopn.F,v 1.1.1.1 1996/03/08 11:40:53 mclareni Exp $
*
* $Log: kaxopn.F,v $
* Revision 1.1.1.1  1996/03/08 11:40:53  mclareni
* Kapack
*
*
#include "kapack/pilot.h"
      SUBROUTINE KAXOPN
*
*.....OPEN AN EXISTING KA-FILE
*
#include "kapack/kax000.inc"
#include "kapack/kax020.inc"
#include "kapack/kax030.inc"
#include "kapack/kax050.inc"
#include "kapack/kax0b0.inc"
*
      CHARACTER VERB1*3, VERB2*3
#if defined(CERNLIB_CRAY)
      CHARACTER*2 CHALUN
#endif
*
*-----------------------------------------------------------------------
*
*.....CHECK THE RECORD LENGTH
      IF ( KAXRCL(LUNKAF) .LE. 0 )  GO TO 91
*
*.....OPEN THE FILE FOR DIRECT ACCESS
      IF ( LBKS(LUNKAF) .EQ. 0 )  THEN
#if defined(CERNLIB_UNIX)
         WRITE(MSG,1000) LUNKAF
 1000    FORMAT('CRNKA24A KAXOPN: ENTER THE NAME OF THE KA-FILE FOR',
     +          ' UNIT ',I3)
         CALL KAXMSG(ISYSWR,MSG)
         READ(ISYSRD,1001) MSG
 1001    FORMAT(A)
#endif
#if defined(CERNLIB_CRAY)
         WRITE(CHALUN,'(I2)') LUNKAF
         MSG='fort.'//CHALUN(INDEX(CHALUN,' ')+1:2)
#endif
#if (!defined(CERNLIB_VAX))&&(!defined(CERNLIB_NORD))
         OPEN  ( ACCESS = 'DIRECT',
#endif
#if defined(CERNLIB_NORD)
         OPEN  ( BUFFER_SIZE = KAXRCL ( LUNKAF ),
     +           MODE        = 'SEGMENT',
#endif
#if defined(CERNLIB_UNIX)||defined(CERNLIB_CRAY)
     +           FILE   =  MSG(1:INDEX(MSG,' ')-1),
#endif
#if !defined(CERNLIB_VAX)
     +           ERR    =  92,
     +           FORM   = 'UNFORMATTED',
     +           IOSTAT =  IOS,
     +           RECL   =  KAXRCL(LUNKAF),
#endif
#if defined(CERNLIB_CRAY)
     +           STATUS = 'OLD',
#endif
#if !defined(CERNLIB_VAX)
     +           UNIT   =  LUNKAF )
#endif
#if defined(CERNLIB_VAX)
         IF ( MODIFY )  THEN
            OPEN  ( ACCESS = 'DIRECT',
     +              ERR    = 92,
     +              FORM   = 'UNFORMATTED',
     +              IOSTAT = IOS,
     +              RECL   = KAXRCL(LUNKAF),
     +              SHARED,
     +              STATUS = 'UNKNOWN',
     +              UNIT   = LUNKAF )
         ELSE
            OPEN  ( ACCESS = 'DIRECT',
     +              ERR    = 92,
     +              FORM   = 'UNFORMATTED',
     +              IOSTAT = IOS,
     +              READONLY,
     +              SHARED,
     +              STATUS = 'OLD',
     +              UNIT   = LUNKAF )
         ENDIF
#endif
         IF ( IOS .NE. 0 )  GO TO 92
         LBKS(LUNKAF) = -1
      ENDIF
*
*.....VALIDATE THE FILE ATTRIBUTES
      READ(LUNKAF,ERR=93,IOSTAT=IOS,REC=1) (IB(I),I=1,NBCW1)
      IF ( IOS .NE. 0 )  GO TO 93
      IF ( IB(13) .NE. KAVRSN )  GO TO 94
      IF ( IB(15) .GT. LBUF )    GO TO 95
*
*.....STORE FILE-DEPENDENT PARAMETERS
      LBKS(LUNKAF) = IB(15)
      NBKS(LUNKAF) = IB(14)
      NFBK(LUNKAF) = IB( 2)
      MAXP(LUNKAF) = IB(16)
      MAXK(LUNKAF) = 0
      DO  1  I = 1, MAXP(LUNKAF)
         MAXK(LUNKAF) = MAXK(LUNKAF)*100 + 99
 1    CONTINUE
*
*.....CALCULATE THE NUMBER AND PERCENTAGE OF FREE BLOCKS
      NBUSED = IB(14) - IB(2)
      PCFREE = REAL(IB(2)) / REAL(IB(14)) * 100.0
      PCUSED = 100.0 - PCFREE
      VERB1 = 'ARE'
      IF ( NBUSED .EQ. 1 )  VERB1 = 'IS'
      VERB2 = 'ARE'
      IF ( IB(2)  .EQ. 1 )  VERB2 = 'IS'
      WRITE(MSG,100) LUNKAF, IB(14), NBUSED, PCUSED,
     +               VERB1, IB(2), PCFREE, VERB2
      CALL KAXMSG(LUNLOG,MSG)
      IF ( PCFREE .LT. 10.0 )  THEN
         WRITE(MSG,101) LUNKAF, IB(2), PCFREE
         CALL KAXMSG(LUNLOG,MSG)
      ELSE IF ( PCFREE .LT. 20.0 )  THEN
         WRITE(MSG,102) LUNKAF, IB(2), PCFREE
         CALL KAXMSG(LUNLOG,MSG)
      ENDIF
*
*.....SUCCESSFUL COMPLETION
      RETURN
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
 95   WRITE(MSG,195) IB(15), LUNKAF, LBUF
      GO TO 99
*
 99   CALL KAXMSG(LUNERR,MSG)
      CALL KAXEND
*
 100  FORMAT('CRNKA240 KAXOPN: KA-FILE OPENED ON UNIT ',I3,
     +       '; THE FILE CONTAINS ',I11,' BLOCKS OF WHICH ',
     +       I11,' (',F5.1,'%) ',A,' USED AND ',
     +       I11,' (',F5.1,'%) ',A,' UNUSED')
 101  FORMAT('CRNKA241 KAXOPN: **** SERIOUS WARNING ****, THE KA-FILE
     +        ON UNIT ',I3,' CONTAINS ONLY ',I11,' (',F5.1,'%) ',
     +       ' FREE BLOCKS')
 102  FORMAT('CRNKA242 KAXOPN: *** WARNING ***, THE KA-FILE
     +        ON UNIT ',I3,' CONTAINS ONLY ',I11,' (',F5.1,'%) ',
     +       ' FREE BLOCKS')
 191  FORMAT('CRNKA243 KAXOPN: UNABLE TO OBTAIN THE BLOCK LENGTH FOR
     +        THE KA-FILE ON UNIT ',I3)
 192  FORMAT('CRNKA244 KAXOPN: ERROR WHILE OPENING THE KA-FILE ON
     +        UNIT ',I3,', IOSTAT = ',I11)
 193  FORMAT('CRNKA245 KAXOPN: I/O ERROR WHILE READING BLOCK 1 FROM
     +        UNIT ',I3,', IOSTAT = ',I11)
 194  FORMAT('CRNKA246 KAXOPN: THE KA-FILE ON UNIT ',I3,
     +       ' IS INCOMPATIBLE WITH THIS VERSION OF KAPACK AND SHOULD',
     +       ' BE CONVERTED BY SUBROUTINE KA1TO2, (SEE THE PAM TITLE',
     +       ' OR CNL 175 FOR DETAILS)')
 195  FORMAT('CRNKA247 KAXOPN: THE BLOCK LENGTH OF ',I11,' WORDS ON
     +        UNIT ',I3,' EXCEEDS THE BUFFER LENGTH OF ',I11,' WORDS')
*
      END
