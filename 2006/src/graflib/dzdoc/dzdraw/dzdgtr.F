*
* $Id: dzdgtr.F,v 1.1.1.1 1996/03/04 16:13:19 mclareni Exp $
*
* $Log: dzdgtr.F,v $
* Revision 1.1.1.1  1996/03/04 16:13:19  mclareni
* Dzdoc/Zebpack
*
*
#include "dzdoc/pilot.h"
      SUBROUTINE DZDGTR(CHKEY,CHOPT,LTOPD)
      CHARACTER*8 CHKEY
      CHARACTER*(*) CHOPT
*     CHOPT:
      INTEGER KEY(2),KEYI(2),KEYIN(2)
#include "dzdoc/bkwrp.inc"
*----- End of Seq
      CHARACTER*12 CTEMP
*----
      CALL UCTOH('NONE',IDNONE,4,4)
      CALL UCTOH('NOTU',IDNOTU,4,4)
      CALL UCTOH(CHKEY,KEY,4,8)
      CALL DZDPIN(KEY,'RBG')
      IF(IQUEST(1) .NE. 0)THEN
           LTOPD = 0
           GOTO 999
      ENDIF
      LDOC = IQUEST(11)
      IDHTB=KEY(1)
 
*     are there down links
 
*    highest number of documented down banks
 
      NSBD = IQ(KQSP+LDOC+5)
      NS =   IQ(KQSP+LDOC+2)
      IF(NS.LT.0 .AND. NSBD .GT. IQ(KQSP+LDOC+2))NS = NSBD
 
*     get up link
 
      CALL UHTOC(IQ(KQSP+LDOC+9),4,CTEMP,4)
      CALL CLTOU(CTEMP(1:4))
      IF (CTEMP(1:4) .NE. 'NONE')THEN
 
*     get doc for up-bank
 
            CTEMP(5:8) = '****'
            NFKEY = 0
   10       CONTINUE
            CALL DZDWCS(CTEMP,KEYI,NFKEY)
            IF(KEYI(1) .EQ. 0)GOTO 40
*            IF(KEYI(2) .NE. IDHTB)GOTO 5
            CALL DZDPIN(KEYI,'RBG')
            IF(IQUEST(1) .NE. 0)GOTO 40
            LUPD = IQUEST(11)
 
*     find link offset in up-bank
 
            NSUP = IQ(KQSP+LUPD+5)
            DO 20 I=1,NSUP
            IF(IQ(KQSP+LUPD+10+I) .EQ. IDHTB)THEN
                  JBIAS = I
                  GOTO 30
            ENDIF
   20       CONTINUE
 
*      not found, go and try next
 
            GOTO 10
 
   30       CONTINUE
 
*      lift up - bank
 
            CALL DZDLFT(LUPD,LLTOP,LUP,1)
            IDHUP = KEYI(1)
 
*       lift bank itself and continue with down banks
 
            CALL DZDLFT(LDOC,LUP,LT,-JBIAS)
*       has it a next bank?
            IDHN = IQ(KQSP+LDOC+10)
            IF(IDHN.NE.0 .AND. IDHN .NE. IDNONE)THEN
                KEYI(1) = IDHN
                CALL DZDPIN(KEYI,'RGB')
                IF(IQUEST(1).EQ.0)THEN
                    LDOC=IQUEST(11)
                    CALL DZDLFT(LDOC,LT,LN,0)
                ENDIF
            ENDIF
            GOTO 50
      ENDIF
 
 
   40 CONTINUE
 
*     no up-bank found, lift bank as top bank
 
      CALL DZDLFT(LDOC,LLTOP,LT,1)
      CALL UCTOH('NONE',IDHUP,4,4)
   50 CONTINUE
 
*
*
*       init links
*
      NBK = 0
      LORIG = LT
      LUP = LT
 
      IF(NS .EQ. 0)GOTO 90
      LD2 = LT
 
*       get next down link
 
   60   CONTINUE
      JB = LUP - LORIG + 1
      NS = IQ(KQSP+LUP-2)
*
   70 CONTINUE
      IF(JB .LE. NS)THEN
 
            IDHD = IQ(KQSP+LDOC+10+JB)
            IF(IDHD .NE. 0)THEN
*                IF(IDHD.EQ.IDSTOP)THEN
*                   WRITE(*,'(A,A4)')' Bank reached: ',IDSTOP
*                ENDIF
                KEYI(1) = IDHD
                KEYI(2) = IQ(KQSP+LDOC+7)
                CALL DZDPIN(KEYI,'RGB')
                IF(IQUEST(1) .NE. 0)THEN
                    IF(KEYI(1).NE.IDNOTU)THEN
                        CALL UHTOC(KEYI(1),4,CTEMP(1:4),4)
                        CALL DZDLFX(LD2,-JB,CTEMP(1:4))
                    ENDIF
                    JB=JB+1
                    GOTO 70
                ELSE
                    LDOC= IQUEST(11)
                    CALL DZDLFT(LDOC,LD2,LD1,-JB)
*       has it a next bank?
                    LN=0
                    NBBOOK=0
                    LDD1=LD1
                    IDHN = IQ(KQSP+LDOC+10)
                    IF(IDHN.NE.0)THEN
75                      CALL UHTOC(IDHN,4,CTEMP,4)
                        CALL CLTOU(CTEMP(1:4))
                        IF(CTEMP(1:4).NE.'NONE')THEN
                            KEYIN(1) = IDHN
                            KEYIN(2) = KEYI(2)
                            CALL DZDPIN(KEYIN,'RGB')
                            NBBOOK=NBBOOK+1
                            IF(IQUEST(1).EQ.0)THEN
                               LDOC=IQUEST(11)
                               CALL DZDLFT(LDOC,LD1,LN,0)
                               IF(IQ(KQSP+LDOC+10).NE.0)THEN
                                  IF(IQ(KQSP+LDOC+10).NE.IDHN)THEN
                                     IDHN=IQ(KQSP+LDOC+10)
                                     LD1=LN
                                     GOTO 75
                                  ENDIF
                               ENDIF
                            ENDIF
                        ENDIF
                    ENDIF
*                   restore old doc
                    IF(NBBOOK.GT.0)THEN
                       CALL DZDPIN(KEYI,'RGB')
                       LDOC=IQUEST(11)
                       LD1=LDD1
                    ENDIF
                ENDIF
            ELSE
                JB=JB+1
                GOTO 70
            ENDIF
 
*             go down
 
            LUP = LD1
            LORIG = LD1
            LD2 = LD1
            GOTO 60
      ENDIF
 
*     has it a next bank
 
      LN=LQ(KQSP+LUP)
      IF(LN .NE. 0)THEN
*        follow only if HIDs different and doc is there?
         IF(IQ(KQSP+LUP-4).NE.IQ(KQSP+LN-4))THEN
            KEYI(1) = IQ(KQSP+LN-4)
            KEYI(2) = IQ(KQSP+LQ(KQSP+LUP+1)-4)
            CALL DZDPIN(KEYI,'RGB')
            IF(IQUEST(1) .EQ. 0)THEN
               LDOC=IQUEST(11)
               LUP = LN
               LORIG = LN
               L = LN
               LD2=LN
               GOTO 60
            ENDIF
         ENDIF
      ENDIF
*       look if it is end of a linear structure
*       i.e. origin and up link are in different banks
 
   80   CONTINUE
      LUP = LQ(KQSP+LD2+1)
      LORIG = LQ(KQSP+LD2+2)
 
*       look if back at top bank
 
      IF(LUP .LE. 1) GOTO 90
      IF(IQ(KQSP+LUP-4).EQ.IDHUP .AND. IQ(KQSP+LD2-4).EQ.IDHTB)GOTO 90
 
      NSU = IQ(KQSP+LUP-2)
      IF(LORIG .GE. LUP .OR. LORIG .LT. LUP-NSU)THEN
 
*             its different, step back
 
            NBK = NBK + 1
            LD2 = LORIG
            GOTO 80
      ENDIF
 
*       go up
 
      NBK = NBK + 1
      LD2 = LUP
      LUU = LQ(KQSP+LD2+1)
      IF(LUU .LE. 2)THEN
          KEYI(2) = KEY(2)
      ELSE
          KEYI(2) = IQ(KQSP+LUU-4)
      ENDIF
      KEYI(1) = IQ(KQSP+LD2-4)
      CALL DZDPIN(KEYI,'RGB')
      LDOC = IQUEST(11)
      GOTO 60
 
*       all done, but top bank
 
   90   CONTINUE
*
      NBK = NBK + 1
      LTOPD = LT
      IQUEST(11) = LTOPD
      IQUEST(1) = 0
*
*      WRITE(*,'(A,I8)')' DZDGTR: Total # of banks in tree: ',NBK
  999 END
***********************************************************************
