*
* $Id: dzdgky.F,v 1.1.1.1 1996/03/04 16:13:04 mclareni Exp $
*
* $Log: dzdgky.F,v $
* Revision 1.1.1.1  1996/03/04 16:13:04  mclareni
* Dzdoc/Zebpack
*
*
#include "dzdoc/pilot.h"
      SUBROUTINE DZDGKY
*      IMPLICIT NONE
      INTEGER IQUEST(100)
      COMMON/QUEST/IQUEST
#include "dzdzbrinc.inc"
      INTEGER NCH, KNUMB, ISTORE, IIST, IFC, IC, NPAR, NWKEY, I
      CHARACTER*80 CHPATH
      CHARACTER*80 CHSAVE
      CHARACTER*10 CKNUMB, CMD, CHFORM
      CHARACTER*8  CHTAG(10)
*--
      CALL KUPATL(CMD,NPAR)
 
      CALL KUGETC(CHPATH,NCH)
      IF(NCH.LE.0)GOTO 999
      IF(CMD.NE.'SH_KEY_DEF' .AND. CMD.NE.'SH_RZLDIR')THEN
         CALL KUGETC(CKNUMB,NCH)
         IF(NCH.LE.0)GOTO 999
         CALL DZDCTI(CKNUMB,KNUMB)
         IF(KNUMB.LE.0)GOTO 999
      ENDIF
      ISTORE=0
      IIST = 1
      CALL SBYT(ISTORE,IIST,27,6)
*     init link area for store 0
 
      IF(LZEFLG(ISTORE).EQ.0)THEN
         CALL MZLINK(IIST,'DZDZEB',
     &   LZEBLK(1,ISTORE),LZEBLK(3,ISTORE),LZEBLK(3,ISTORE))
         LZEFLG(ISTORE)=1
      ENDIF
      IF(LZEBLK(1,ISTORE).NE.0)CALL MZDROP(IIST,LZEBLK(1,ISTORE),' ')
      LZEBLK(1,ISTORE)=0
      IFC=INDEX(CHPATH,'R_')
      IF(IFC.LE.0)GOTO 999
      CALL RZCDIR(CHSAVE,'R')
      IC=1000000
      CALL RZCDIR('//'//CHPATH(IFC+2:),' ')
      IF(CMD.EQ.'SH_RZLDIR')THEN
         IF(IAPPFL.NE.0)THEN
            CALL RZLDIR(' ','A')
         ELSE
            CALL RZLDIR(' ',' ')
         ENDIF
      ELSE IF(CMD.EQ.'GET_KEY')THEN
         CALL RZIN(IIST,LZEBLK(1,ISTORE),1,KNUMB,IC,'S')
         IF(IQUEST(1).NE.0)THEN
            WRITE(*,*)'Error from RZIN',IQUEST(1)
         ELSE
            WRITE(*,*)
     &      'RZIN, words read: ',IQUEST(12), ' into Store 0, Div 1'
            CALL IZPICT('DZDISP','SQ')
            CALL IZPICT('DZDISP','M')
            CALL ISTXFP(6,0)
            CALL DZDISP(IIST,LZEBLK(1,ISTORE),CTDDOC,'D'//CCOL//CCACT,
     &      IWKZEB,IWMZEB,ILOZEB, 0)
         ENDIF
      ELSE
         CALL RZKEYD(NWKEY,CHFORM,CHTAG)
         WRITE(*,*)'Total keywords:', NWKEY
         NWKEY=MIN(NWKEY,9)
         IF(CMD.EQ.'SH_KEY_DEF')THEN
            WRITE(*,*)' Nr Form Tag'
            DO 10 I=1,NWKEY
10          WRITE(*,'(I4,A,A,A,A)')I,'  ',CHFORM(I:I),'  ',CHTAG(I)
         ELSE
            CALL RZIN(0,LL,0,KNUMB,0,'CS')
            WRITE(*,*)' Nr Tag        Value'
            DO 20 I=1,NWKEY
            IF(CHFORM(I:I).EQ.'H' .OR. CHFORM(I:I).EQ.'A')THEN
               WRITE(*,'(I3,A,A,A,A)')I,' ',CHTAG(I),'  ',IQUEST(20+I)
            ELSE
               WRITE(*,'(I3,A,A,A,I11)')I,' ',CHTAG(I),'  ',IQUEST(20+I)
            ENDIF
20          CONTINUE
         ENDIF
      ENDIF
      CALL RZCDIR(CHSAVE,' ')
 999  END
***********************************************************************
