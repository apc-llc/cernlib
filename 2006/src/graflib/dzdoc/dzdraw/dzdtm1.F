*
* $Id: dzdtm1.F,v 1.1.1.1 1996/03/04 16:13:07 mclareni Exp $
*
* $Log: dzdtm1.F,v $
* Revision 1.1.1.1  1996/03/04 16:13:07  mclareni
* Dzdoc/Zebpack
*
*
#include "dzdoc/pilot.h"
      SUBROUTINE DZDTM1(L,LUN,CHOPT)
      CHARACTER*(*) CHOPT
#include "zebra/zebq.inc"
#include "zebra/mzca.inc"
#include "zebra/mzcb.inc"
#include "zebra/mzcc.inc"
#include "zebra/mzioc.inc"
*  BANK FORMAT PARAMETERS  (IO CHARACERISTIC)
#include "zebra/bkfoparq.inc"
*--------
      CHARACTER*4 CHBANK,CHUP
      CHARACTER*12 CTEMP
      CHARACTER*60 CLINE
 
      IFC=1
      IOPTAQ=INDEX(CHOPT,'A')
      IOPTDQ=INDEX(CHOPT,'D')
      IOPTSQ=INDEX(CHOPT,'S')
      IOPTVQ=INDEX(CHOPT,'V')
*     holl Id of bank
      CALL UHTOC(IQ(KQS+L-4),4,CHBANK,4)
      WRITE(LUN,
     &          '(''+DECK,'',A,''.'',20(''-''),''    '',A,
     &          ''    '',30(''-'') )' )
     &          CHBANK,CHBANK
      WRITE(LUN,'(A)')'*B..'//CHBANK//'    Template only'
      IF(IOPTAQ.NE.0)THEN
         WRITE(LUN,'(A,A)')'*B.AU    Nomen nescio'
      ENDIF
      IF(IOPTVQ.NE.0)THEN
         WRITE(LUN,'(A)')'*B.VE    1.00'
      ENDIF
 
      IF(IOPTSQ.NE.0)THEN
         CALL UHTOC(IQTABV(KQT+11),4,CTEMP,8)
         CTEMP(9:)=' '
         WRITE(LUN,'(A)')'*B.ST    '//CTEMP
      ENDIF
 
      IF(IOPTDQ.NE.0)THEN
         DO 3 IDIV=1,20
            IFAD = LQSTA(KQT+IDIV)
            ILAD = LQEND(KQT+IDIV)
            IF(L.GE.IFAD.AND.L.LE.ILAD)THEN
               CALL UHTOC(IQDN1(KQT+IDIV),4,CTEMP(1:4),4)
               CALL UHTOC(IQDN2(KQT+IDIV),4,CTEMP(5:8),4)
               CTEMP(9:)=' '
               WRITE(LUN,'(A)')'*B.DV    '//CTEMP
               GOTO 4
            ENDIF
3        CONTINUE
         WRITE(*,*)'division not found, L',L
4        CONTINUE
      ENDIF
      NL = IQ(KQS+L-3)
      CALL DZDPDL(LUN,'*B.NL    ',NL)
      NS=IQ(KQS+L-2)
      CALL DZDPDL(LUN,'*B.NS    ',NS)
      ND=IQ(KQS+L-1)
      CALL DZDPDL(LUN,'*B.ND    ',ND)
 
*     IO-characteristic
      NIO = JBYT(IQ(KQS+L),19,4)
      IOFFBS = - (NIO + NL + 8 + 1)
      CALL DZDIOC(IQ(KQS+L+IOFFBS),CLINE,NCH)
      WRITE(LUN,'(A,A)')'*B.IO    ',CLINE(1:NCH)
 
*      IOBYTE = JBYT(IQ(KQS+L+IOFFBS),17,16)
*      IF(NIO.GT.0 .OR. IOBYTE.GT.7)THEN
*         IDSAME=0
*      ELSE IF(IOBYTE.EQ.0)THEN
*         IDSAME=1
*      ELSE
*         IDSAME=1
*      ENDIF
 
*     holl Id of UP bank
 
      LU = LQ(KQS+L+1)
      CTEMP=' '
      IF(LU.GT.2)THEN
         CALL UHTOC(IQ(KQS+LU-4),4,CHUP,4)
         LO = LQ(KQS+L+2)
         NSUP=IQ(KQS+LU-2)
         JB1= LU-LO
         IF(JB1.GT.0 .AND. JB1.LE.NSUP)THEN
            WRITE(CTEMP,'(I12)')-JB1
            IFC=INDEXC(CTEMP,' ')
         ENDIF
      ELSE
         CHUP='NONE'
      ENDIF
      WRITE(LUN,'(A)')'*B.UP    '//CHUP//'   '//CTEMP(IFC:)
 
*     holl Id of NEXT bank
 
      L1 = LQ(KQS+L)
      IF(L1.GT.2)THEN
         CALL UHTOC(IQ(KQS+L1-4),4,CHBANK,4)
         WRITE(LUN,'(A)')'*B.NX    '//CHBANK
      ENDIF
 
*     down links
 
      IF(NS.GT.0)THEN
         WRITE(LUN,'(A)')'*B.LINK'
*        all the same ?
         IF(NS.GT.1)THEN
            ILSAME=1
            L1 = LQ(KQS+L-1)
            ID1 = IQ(KQS+L1-4)
            DO 5 I=2,NS
               L1 = LQ(KQS+L-I)
               IF(L1.NE.0)THEN
                  IF(ID1.NE.IQ(KQS+L1-4))THEN
                     ILSAME=0
                     GOTO 6
                  ENDIF
               ENDIF
5           CONTINUE
         ELSE
            ILSAME=0
         ENDIF
6        CONTINUE
         IF(ILSAME.EQ.1)THEN
            CALL DZDPDL(LUN,'*B.REP    ',NS)
            NS=1
         ENDIF
         DO 10 I=1,NS
            L1 = LQ(KQS+L-I)
            IF(L1.NE.0)THEN
               WRITE(CTEMP,'(I12)')I
               IFC=INDEXC(CTEMP,' ')
               CALL UHTOC(IQ(KQS+L1-4),4,CHBANK,4)
               WRITE(LUN,'(A,T10,A)')'*B.'//CTEMP(IFC:),CHBANK
            ENDIF
10       CONTINUE
         IF(ILSAME.EQ.1)WRITE(LUN,'(A)')'*B/REP'
         WRITE(LUN,'(A)')'*B/LINK'
      ENDIF
 
*     data words
 
      IREPTR=0
      IF(ND.GT.0)THEN
         WRITE(LUN,'(A)')'*B.DATA'
         NDSEQ=0
*        go through cracked IO-char in array MFO
         DO 100 JFOCUR = 1,JFOEND,2
            IF (JFOCUR.EQ.JFOREP+1) THEN
*           start of trailing region (/)
               WRITE(LUN,'(A)')'*B.REP   FOREVER'
               IREPTR=1
            ENDIF
            ITYPE  = MFO(JFOCUR)
 
*            is this the same as NWSEC<0??
*            IF (ITYPE.EQ.IFOSEQ) THEN
*            ENDIF
 
            NWSEC  = MFO(JFOCUR+1)
            ITYPE  = MIN(ITYPE,8)
            IF (NWSEC.LT.0) THEN
*           indefinite length sector (-) (all same kind)
               NREST=ND-NDSEQ
               CALL DZDPDL(LUN,'*B.REP    ',NREST)
               CALL DZDPDL(LUN,'*B.',NDSEQ+1)
               WRITE(LUN,'(A)')'*B/REP'
               GOTO 101
            ELSEIF (NWSEC.EQ.0 .OR. ITYPE.EQ.IFOSEQ) THEN
*           dynamic sector (*) (current data word = rep count
               CALL DZDPDL(LUN,'*B.',NDSEQ+1)
               NDSEQ=NDSEQ+1
               WRITE(LUN,'(A)')
     &         '*B.REP    BITS0031 ! previous word = rep count'
               CALL DZDPDL(LUN,'*B.',NDSEQ+1)
               NDSEQ=NDSEQ+1
               WRITE(LUN,'(A)')'*B/REP'
            ELSE
*           fixed length sector
               IF(NWSEC.GT.10)THEN
                  CALL DZDPDL(LUN,'*B.REP    ',NWSEC)
                  CALL DZDPDL(LUN,'*B.',NDSEQ+1)
                  WRITE(LUN,'(A)')'*B/REP'
                  NDSEQ=NDSEQ+NWSEC
               ELSE
                  DO 90 I=1,NWSEC
                     NDSEQ=NDSEQ+1
                     CALL DZDPDL(LUN,'*B.',NDSEQ)
   90             CONTINUE
               ENDIF
            ENDIF
  100    CONTINUE
         IF(IREPTR.EQ.1)WRITE(LUN,'(A)')'*B/REP'
  101    CONTINUE
         WRITE(LUN,'(A)')'*B/DATA'
      ENDIF
      WRITE(LUN,'(A)')'*B/'
999   RETURN
      END
**********************************************************************
