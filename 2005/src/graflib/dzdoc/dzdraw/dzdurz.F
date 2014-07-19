*
* $Id: dzdurz.F,v 1.1.1.1 1996/03/04 16:13:19 mclareni Exp $
*
* $Log: dzdurz.F,v $
* Revision 1.1.1.1  1996/03/04 16:13:19  mclareni
* Dzdoc/Zebpack
*
*
#include "dzdoc/pilot.h"
      SUBROUTINE DZDURZ(LUNSRC,LUNRZF, CHDOC,CHRZFI,CHSUB, IRET)
      INTEGER LUNSRC,LUNRZF,IRET
      CHARACTER*(*) CHDOC,CHRZFI,CHSUB
      COMMON/QUEST/IQUEST(100)
#include "zebra/zebq.inc"
#include "zebra/mzca.inc"
#include "zebra/mzcb.inc"
#include "zebra/mzcc.inc"
#include "zebra/mzcn.inc"
#include "dzdoc/dzdocc.inc"
      INTEGER  NWKEY,MAXKEY,LRECL,NKEYS,NKEYSB, NUPD, NCYCLE
      PARAMETER(NWKEY = 2,MAXKEY=2000,LRECL=256 )
      INTEGER  KEYS(NWKEY,MAXKEY), KEYSBF(NWKEY,MAXKEY),
     &         NOLDCY(MAXKEY), MARKUP(MAXKEY)
 
      CHARACTER*8 CHTAG(NWKEY)
      DATA CHTAG/'Bank-ID','Bank-ID'/
      CALL DZDINI
      IRET=0
      IC1000=1000000
*
      CALL RZSTAT('//RZDOC',2,'Q')
      IF(IQUEST(11).LE.0)THEN
         CALL RZOPEN(LUNRZF,'RZDOC',CHRZFI,'U',LRECL,IRET)
         IF(IRET .NE. 0)GOTO 50
         CALL RZFILE(LUNRZF,'RZDOC','U')
      ELSE
*         WRITE(*,*)'RZ-file was already open'
      ENDIF
 
      IF(CHDOC.EQ.' ')GOTO 50
      CALL RZCDIR('//RZDOC',' ')
      IF(CHSUB .NE. ' ')THEN
         CALL RZCDIR(CHSUB,' ')
         IF(IQUEST(1) .NE. 0)THEN
            WRITE(*,'(A,A)')'Will create: ',CHSUB
            CALL RZMDIR(CHSUB,NWKEY,'HH',CHTAG)
            CALL RZCDIR(CHSUB,' ')
         ENDIF
      ENDIF
      CALL KUOPEN(LUNSRC,CHDOC,'OLD',IRET)
      IF(IRET .NE. 0)GOTO 50
 
      CALL RZKEYS(NWKEY,MAXKEY,KEYSBF,NKEYSB)
      DO 10   I=1,NKEYSB
         CALL RZIN(0,LBQBKD,2,KEYSBF(1,I),IC1000,'C')
         NOLDCY(I) = IQUEST(6)
   10 CONTINUE
 
      CALL DZDOCM(LUNSRC,0,'R')
      CLOSE(UNIT=LUNSRC)
*      CALL RZLDIR(' ',' ')
 
      CALL RZKEYS(NWKEY,MAXKEY,KEYS,NKEYS)
      NUPD = 0
      NEWKEY = 0
      LBQBKD=0
      LDQBKD=0
      MAXCYC = 1
      DO 30   I=1,NKEYSB
         CALL RZIN(0,LBQBKD,2,KEYS(1,I),IC1000,'C')
         NCYCLE = IQUEST(6)
         IF( KEYS(1,I) .NE. KEYSBF(1,I) .OR.KEYS(2,I) .NE. KEYSBF(2,I))
     +   THEN
            WRITE(*,'(A,I5)')
     +      ' Order of keys changed at',I
            GOTO 30
         ELSE
            IF(NCYCLE .EQ. NOLDCY(I))THEN
               MARKUP(I) = 0
               GOTO 30
            ENDIF
         ENDIF
         MARKUP(I) = 1
         IF(LBQBKD .NE. 0)THEN
            CALL MZDROP(0,LBQBKD,' ')
            LBQBKD = 0
         ENDIF
         CALL RZIN(0,LBQBKD,2,KEYS(1,I),IC1000,'D')
         IF(IQUEST(6) .GT. MAXCYC)MAXCYC = IQUEST(6)
         IF(LDQBKD .NE. 0)THEN
            CALL MZDROP(0,LDQBKD,' ')
            LDQBKD = 0
         ENDIF
         CALL RZIN(0,LDQBKD,2,KEYS(1,I),NOLDCY(I),'D')
         ND = IQ(LBQBKD+KQSP-1)
         IF(ND .NE. IQ(LDQBKD+KQSP-1))THEN
            NUPD = NUPD + 1
            WRITE(*,'(A,2A4)')' Update Key: ',KEYS(1,I),KEYS(2,I)
            GOTO 30
         ENDIF
         DO 20   K=1,ND
            IF(IQ(LBQBKD+KQSP+K) .NE. IQ(LDQBKD+KQSP+K))THEN
               NUPD = NUPD + 1
               WRITE(*,'(A,2A4)')' Update Key: ',KEYS(1,I),KEYS(2,
     +         I)
               GOTO 30
            ENDIF
   20    CONTINUE
 
         MARKUP(I) = 0
         CALL RZDELK(KEYS(1,I), IC1000, ' ')
 
   30 CONTINUE
 
      NEWKEY = NKEYS - NKEYSB
      IF(NKEYS .GT. NKEYSB)THEN
         DO 40   I=NKEYSB+1,NKEYS
            WRITE(*,'(A,2A4)')' Add Key: ',KEYS(1,I),KEYS(2,I)
   40    MARKUP(I) = 1
      ENDIF
      IF(LDQBKD .NE. 0)THEN
         CALL MZDROP(0,LDQBKD,' ')
         LDQBKD = 0
      ENDIF
      IF(LBQBKD .NE. 0)THEN
         CALL MZDROP(0,LBQBKD,' ')
         LBQBKD = 0
      ENDIF
      WRITE(*,'(I6,A)')NEWKEY,' key(s) added'
      WRITE(*,'(I6,A)')NUPD  ,' key(s) updated'
   50 CONTINUE
      END
**********************************************************************
