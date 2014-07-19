*
* $Id: dzdhtm.F,v 1.1.1.1 1996/03/04 16:13:19 mclareni Exp $
*
* $Log: dzdhtm.F,v $
* Revision 1.1.1.1  1996/03/04 16:13:19  mclareni
* Dzdoc/Zebpack
*
*
#include "dzdoc/pilot.h"
      SUBROUTINE DZDHTM(ISTORE,L,CFLAG)
*
*     walk through a d/s at L and write doc in html
*     CFLAG  'S' one bank only
*            'T' bank tree (default)
*            'N' no data words
*            'P' plain text (no hyperlinks)
*
C+SEQ,ZUNIT.
#include "zebra/zebq.inc"
#include "zebra/mzca.inc"
#include "zebra/mzcb.inc"
C++CDE,ZBCDK.         From DZEBRA
C++SEQ,DZDPRM
C++SEQ,DZDOCC.
      CHARACTER*(*) CFLAG
      CHARACTER*16 CLTOP
      INTEGER L,LN,LGO,LRET,LDOWN, IFC,ILC, IHNONE, LUN
      PARAMETER (LUN=65)
      CHARACTER*8 ANCHOR
      CHARACTER*80 CGET
      CHARACTER*4 CLINK
      CHARACTER*5 CSUFFI
      CHARACTER*16 CLDOWN
      CHARACTER*16 DZE2US
      INTEGER KEYVEC(2)
*---
      CALL MZSDIV(ISTORE,-7)
      CALL UCTOH('NONE',IHNONE,4,4)
      IF(INDEX(CFLAG,'P').EQ. 0)THEN
         CSUFFI='.html'
      ELSE
         CSUFFI='.txt'
      ENDIF      
      WRITE(CLTOP,'(A4,I7.7,A)')IQ(KQS+L-4),L,CSUFFI
      CLTOP=DZE2US(CLTOP)
      OPEN(LUN,FILE=CLTOP)
*      CALL KUOPEN(LUN,CLTOP,'UNKNOWN',IRET)
      IF(LQ(KQS+L+1).EQ.0)THEN
         CLTOP=' '
      ELSE
         WRITE(CLTOP,'(A4,I7.7,A)')
     +   IQ(KQS+LQ(KQS+L+1)-4),LQ(KQS+L+1),CSUFFI
         CLTOP=DZE2US(CLTOP)
      ENDIF
      IF(INDEX(CFLAG,'P').EQ. 0)THEN
         WRITE(LUN,'(A)')'<html>'
         WRITE(LUN,'(A)')'<head>'
         WRITE(LUN,'(A)')'<body>'
         WRITE(LUN,'(A)')'<isindex>'
         WRITE(LUN,'(A)')'</head>'
         WRITE(LUN,'(A)')'<body>'
         WRITE(LUN,'(A)')'<pre>'
         ENDIF
      LRET=-L
 20   CONTINUE
      LGO=LRET
      CALL DZDWTR(ISTORE,LGO,LRET)
      IF(LRET.NE.0)THEN
*         WRITE(*,'(A,A4,I10)')'Found ',IQ(KQS+LRET-4),LRET
         KEYVEC(1)=IQ(KQS+LRET-4)
         IF(LQ(KQS+LRET+1).GT.1)THEN
            KEYVEC(2)=IQ(KQS+LQ(KQS+LRET+1)-4)
         ELSE
            KEYVEC(2)=IHNONE
         ENDIF
         I1000=100000
         LQBKD=0
         CALL RZIN(0,LQBKD,2,KEYVEC,I1000,'D')
         IF (LQBKD .EQ. 0)THEN
            NLT = 0
            NLU = 0
            WRITE(*,'(A,A4)')'No doc for bank ',KEYVEC(1)
         ELSE
            IP0=KQSP+LQBKD+1
            
            CALL DZDGGI(IQ(IP0),'CL',' ',CGET,IFC,ILC)
            WRITE(ANCHOR,'(I8.8)')LRET
            IF(INDEX(CFLAG,'P').EQ. 0)
     +      WRITE(LUN,'(A)')'<a name="L'//ANCHOR//'"><b>'
            IF(ILC.GT.0)WRITE(LUN,'(A)')
     +      'Bank IDH '//CGET(1:4)//' '//CGET(5:ILC)
            IF(INDEX(CFLAG,'P').EQ. 0)
     +      WRITE(LUN,'(A)')'</b></a>'
            CALL DZDGGI(IQ(IP0),'AU',' ',CGET,IFC,ILC)
            IF(ILC.GT.0)WRITE(LUN,'(A,A,A)')'Author(s) ',CGET(IFC:ILC)
            CALL DZDGGI(IQ(IP0),'VE',' ',CGET,IFC,ILC)
            IF(ILC.GT.0)WRITE(LUN,'(A,A,A)')'Version  ',CGET(IFC:ILC)
            NID=IQ(KQS+LRET-5) 
            WRITE(LUN,'(A,I6)')'NumId     ',NID         
            NL=IQ(KQS+LRET-3) 
            NS=IQ(KQS+LRET-2)  
            WRITE(LUN,'(A,I6)')'Str Links ',NS         
            NR=NL-NS  
            WRITE(LUN,'(A,I6)')'Ref Links ',NR         
            ND=IQ(KQS+LRET-1) 
            WRITE(LUN,'(A,I6)')'NData     ',ND
            NIO = JBYT(IQ(KQS+LRET),19,4)
            IOFFBS = - (NIO + NL + 8 + 1)
            CALL DZDIOC(IQ(KQS+LRET+IOFFBS),CGET,ILC)
            IF(ILC.GT.0)WRITE(LUN,'(A,A,A)')'IO-Char   ',CGET(1:ILC)
            
            WRITE(LUN,'(A)')'----------  System links ----------'
            CALL UHTOC(KEYVEC(2),4,CLINK,4)
            IF(CLINK.NE.'NONE')THEN
               IF(INDEX(CFLAG,'P').EQ. 0)THEN
                  WRITE(ANCHOR,'(I8.8)')LQ(KQS+LRET+1)
*                 is it very first bank
                  IF(LRET.EQ.L)THEN
                     NCH=LNBLNK(CLTOP)
                     WRITE(LUN,'(A,A)')'Up-Link   '//
     +               '<a href="'//CLTOP(1:NCH)//'#L'//ANCHOR//'"><b>'//
     +               CLINK//'</b></a>'
                  ELSE
                     WRITE(LUN,'(A,A)')'Up-Link   '//
     +               '<a href="#L'//ANCHOR//'"><b>'//
     +               CLINK//'</b></a>'
     
                  ENDIF
               ELSE
                  WRITE(LUN,'(A,A)')'Up-Link   '//CLINK
               ENDIF
*              handle Origin link eventually 
            ELSE           
               WRITE(LUN,'(A,A)')'Up-Link   '//CLINK
            ENDIF
            LN=LQ(KQS+LRET)
            IF(LN.NE.0)THEN
               CALL UHTOC(IQ(KQS+LN-4),4,CLINK,4)
               IF(INDEX(CFLAG,'P').EQ. 0)THEN
                  WRITE(ANCHOR,'(I8.8)')LN
                  WRITE(LUN,'(A,A)')'Next-Link '//
     +            '<a href="#L'//ANCHOR//'"><b>'//
     +            CLINK//'</b></a>'
               ELSE
                   WRITE(LUN,'(A,A)')'Next-Link '//CLINK
               ENDIF
            ENDIF
            IF(NS.GT.0)THEN
               WRITE(LUN,'(A)')'--------- Struct links ----------'
               DO I=1,NS
                  LDOWN=LQ(KQS+LRET-I)
                  IF(LDOWN.NE.0)THEN
                     CALL UHTOC(IQ(KQS+LDOWN-4),4,CLINK,4)
                     CALL DZDGGI(IQ(IP0),'LI',CLINK,CGET,IFC,ILC)
                     IF(INDEX(CFLAG,'P').EQ. 0)THEN
                        WRITE(ANCHOR,'(I8.8)')LDOWN
                        IF(INDEX(CFLAG,'S').NE.0)THEN
                           WRITE(CLDOWN,'(A4,I7.7,A5)')
     +                     CLINK,LDOWN,'.html'
                           CLDOWN=DZE2US(CLDOWN)
                           WRITE(LUN,'(I6,A)')I,
     +                     ' <a href="'//CLDOWN//'#L'//
     +                     ANCHOR//'"><b>'//
     +                     CLINK//'</b></a> '//CGET(1:ILC)
                        ELSE
                           WRITE(LUN,'(I6,A)')I,
     +                     ' <a href="#L'//
     +                     ANCHOR//'"><b>'//
     +                     CLINK//'</b></a> '//CGET(1:ILC)
                        ENDIF
                     ELSE
                        WRITE(LUN,'(I6,A)')I,CLINK//' '//CGET(1:ILC)
                     ENDIF
                  ENDIF
               ENDDO
            ENDIF       
            IF(ND.GT.0 .AND. INDEX(CFLAG,'N').EQ. 0)THEN
               CALL DZDDWD(ISTORE,LRET,' ',1,ND,LUN)
            ENDIF
            CALL MZDROP(0,LQBKD,' ')
            LQBKD=0
         ENDIF
         IF(INDEX(CFLAG,'S').EQ.0)GOTO 20
      ENDIF 
      IF(INDEX(CFLAG,'P').EQ. 0)THEN
         WRITE(LUN,'(A)')'</pre>'
         WRITE(LUN,'(A)')'</body>'
         WRITE(LUN,'(A)')'</html>'
      ENDIF 
  999 CONTINUE
      RETURN
      END
*******************************************************************
