*
* $Id: dzdmrz.F,v 1.1.1.1 1996/03/04 16:13:19 mclareni Exp $
*
* $Log: dzdmrz.F,v $
* Revision 1.1.1.1  1996/03/04 16:13:19  mclareni
* Dzdoc/Zebpack
*
*
#include "dzdoc/pilot.h"
      SUBROUTINE DZDMRZ(LUNSRC,LUNRZF,CHDOC,CHRZFI,CHSUB,IRET)
      INTEGER LUNSRC,LUNRZF,IRET
      CHARACTER*(*) CHDOC,CHRZFI,CHSUB
      COMMON/QUEST/IQUEST(100)
#include "dzdoc/dzdocc.inc"
      INTEGER  NWKEY,MAXKEY,LRECL,NKEYS,ICYCLE
      PARAMETER(NWKEY = 2,MAXKEY=2000,LRECL=256)
      INTEGER  KEYS(NWKEY,MAXKEY), IC1000
      CHARACTER*8 CHTAG(2)
      CHARACTER*4  CHIDBK,CHIDUP
      LOGICAL EXST
      DATA CHTAG/'Bank-ID','Bank-ID'/
      CALL DZDINI
*      CALL DZEZER
 
      IRET=0
      CALL RZCDIR('//RZDOC',' ')
      CALL RZSTAT('//RZDOC',2,'Q')
      IF(IQUEST(11).LE.0)THEN
         INQUIRE(FILE=CHRZFI, EXIST=EXST)
         IF(EXST)THEN
            WRITE(*,*)'RZ-file exists already, rename or delete it'
            IRET=1
            GOTO 30
         ENDIF
         CALL RZOPEN(LUNRZF,'RZDOC',CHRZFI,'NX',LRECL,IRET)
         IF(IRET.NE.0)GOTO 30
         CALL RZMAKE(LUNRZF,'RZDOC',2,'HH',CHTAG,5000,'X')
      ELSE
*         WRITE(*,*)'RZ-file was already open'
         IRET=0
      ENDIF
      IRET=0
      IF(CHSUB .NE. ' ')THEN
         CALL RZMDIR(CHSUB,2,'HH',CHTAG)
         CALL RZCDIR(CHSUB,' ')
      ENDIF
 
      IF(CHDOC.EQ.' ')GOTO 30
      IC1000=1000000
      CALL KUOPEN(LUNSRC,CHDOC,'OLD',IRET)
 
*--     Read the input file and build the RZ file
 
      CALL DZDOCM(LUNSRC,0,'R')
      CLOSE(UNIT=LUNSRC)
      CALL RZSAVE
 
C--     Check the RZ file
*      CALL RZSTAT('  ',9,' ')
*      CALL RZLDIR('  ',' ')
 
      CALL RZKEYS(2,MAXKEY,KEYS,NKEYS)
      WRITE(6,'(I6,A)')NKEYS, ' keys entered into RZ-file'
      LBQBKD = 0
      DO 10   I=1,NKEYS
*      IF(LBQBKD .NE. 0)THEN
*           CALL MZDROP(0,LBQBKD,' ')
*           LBQBKD = 0
*      ENDIF
         CALL RZIN(0,LBQBKD,2,KEYS(1,I),IC1000,'C')
         ICYCLE = IQUEST(6)
         IF(ICYCLE .NE. 1)THEN
            CALL UHTOC(KEYS(1,I),4,CHIDBK,4)
            CALL UHTOC(KEYS(2,I),4,CHIDUP,4)
            WRITE(*,'(A,A,A,A)') CHIDBK,'/',CHIDUP,
     +     ' Multiply defined entry'
         ENDIF
   10 CONTINUE
   30 CONTINUE
      END
***********************************************************************
