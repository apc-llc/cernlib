*
* $Id: dzdlo1.F,v 1.1.1.1 1996/03/04 16:12:56 mclareni Exp $
*
* $Log: dzdlo1.F,v $
* Revision 1.1.1.1  1996/03/04 16:12:56  mclareni
* Dzdoc/Zebpack
*
*
#include "dzdoc/pilot.h"
      SUBROUTINE DZDLO1(CHBANK,CHPRE,LUNL,IFLAG)
      CHARACTER*8 CHBANK,CTEMP
      CHARACTER*(*) CHPRE
*     IFLAG:    1  INTEGER statement with 'INTEGER'
*               2                    continued
*               3  PARAMETER statement
*               4                      continued
      CHARACTER*4 CHB,CHPINT
      INTEGER KEYVEC(2)
#include "dzdoc/bkwrp.inc"
#include "dzdoc/tapes.inc"
*         From DZEBRA
#include "zebra/zbcdk.inc"
      INTEGER IFITEM
      SAVE IFITEM
      DATA IFITEM/0/
 
      CALL UCTOH(CHBANK(1:4),KEYVEC(1),4,4)
      CALL UCTOH(CHBANK(5:8),KEYVEC(2),4,4)
      CALL DZDPIN(KEYVEC,'RBG')
      IF(IQUEST(1) .NE. 0)THEN
            WRITE(*,*)' Bank not found ',CHBANK
            GOTO 999
      ENDIF
      ILCP=INDXBC(CHPRE,' ')
      IF(ILCP.LE.0)THEN
         CHPINT='LO'
         ILCP=2
      ELSE
         ILCP = MIN(ILCP,LEN(CHPINT))
         CHPINT=CHPRE(1:ILCP)
      ENDIF
*      IFITEM=0
      LUPD = IQUEST(11)
 
*     find link offset in up-bank
 
       NSUP = IQ(KQSP+LUPD+5)
       IF(NSUP .LE. 0)GOTO 999
 
       IF(IFLAG.LE.2)THEN
 
           IF(IFLAG.EQ.1)THEN
               IFITEM=0
               WRITE(LUNL,'(A)')'+KEEP,LKOFF'//CHBANK(1:4)
               CALL DZDPLN(LUNL,'INTEGER',2)
           ENDIF
           DO 10 I=1,NSUP
           IF(IQ(KQSP+LUPD+10+I).EQ.0)GOTO 10
           CALL UHTOC(IQ(KQSP+LUPD+10+I),4,CHB,4)
           ILC=INDXBC(CHB,' ')
           IF(ILC .LE.0)GOTO 10
           IF(IFITEM.EQ.0)THEN
               CALL DZDPLN(LUNL,' '//CHPINT(1:ILCP)//CHB(1:ILC),1)
               IFITEM=1
           ELSE
               CALL DZDPLN(LUNL,','//CHPINT(1:ILCP)//CHB(1:ILC),1)
           ENDIF
10         CONTINUE
       ELSE
           IF(IFLAG.EQ.3)THEN
                CALL DZDPLN(LUNL,'PARAMETER(',2)
                IFITEM=0
           ENDIF
           DO 20 I=1,NSUP
           IF(IQ(KQSP+LUPD+10+I).EQ.0)GOTO 20
           CALL UHTOC(IQ(KQSP+LUPD+10+I),4,CHB,4)
           ILC=INDXBC(CHB,' ')
           IF(ILC .LE.0)GOTO 20
           IF(IFITEM.EQ.0)THEN
               IFITEM=1
           ELSE
               CALL DZDPLN(LUNL,',',1)
           ENDIF
           CALL DZDPLN(LUNL,CHPINT(1:ILCP)//CHB(1:ILC),1)
           WRITE(CTEMP,'(I8)')I
           IFC=INDEXC(CTEMP,' ')
           CALL DZDPLN(LUNL,'='//CTEMP(IFC:),1)
20         CONTINUE
        ENDIF
*       CALL DZDPLN(LUNL,')',0)
*       CALL DZDPLN(LUNL,' ',99)
999    END
***************************************************************
