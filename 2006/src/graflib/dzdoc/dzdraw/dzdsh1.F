*
* $Id: dzdsh1.F,v 1.1.1.1 1996/03/04 16:13:02 mclareni Exp $
*
* $Log: dzdsh1.F,v $
* Revision 1.1.1.1  1996/03/04 16:13:02  mclareni
* Dzdoc/Zebpack
*
*
#include "dzdoc/pilot.h"
      SUBROUTINE DZDSH1(IXDIV,LIN,CMD)
      INTEGER IXDIV,LIN
      CHARACTER*(*) CMD
#include "dzdzbrinc.inc"
#include "zebra/zebq.inc"
#include "zebra/zunit.inc"
#include "zebra/mzca.inc"
#include "zebra/mzcb.inc"
#include "zebra/mzcc.inc"
#include "zebra/mzcn.inc"
#include "zebra/mzbits.inc"
      INTEGER        INFLUN,INFSTA, INFOFZ(40)
      COMMON/FZSTAT/ INFLUN,INFSTA, INFOFZ
*--
      CHARACTER CQSTAK*13,CQINFO*40
      PARAMETER (NLICHQ=130,NSTCHQ=8,NDVCHQ=8,NBKCHQ=4 )
      CHARACTER CQLINE*(NLICHQ),CQMAP(10)*(NLICHQ)
      CHARACTER CQSTOR*(NSTCHQ),CQDIV*(NDVCHQ),CQID*(NBKCHQ)
      COMMON /DZC1CH/ CQSTOR,CQDIV,CQID,CQMAP,CQSTAK,CQINFO
      EQUIVALENCE (CQLINE,CQMAP)
*--
      CHARACTER*1 CHMORE
      CHARACTER*4 CHDROP, CVALUE, CHYENO
      CHARACTER*12 CINTV
      CHARACTER*30 CTEMP
      INTEGER  IDH, LL, LUSAVE, NC, N1, N2, K, I16X, IOTYP
      REAL RVALUE
      LOGICAL BTEST
      SAVE LUSAVE
*
      LUSAVE=IQPRNT
      IF(CTDDOC.NE.' ')THEN
         CALL RZCDIR(CWDSAV,'R')
         CALL RZCDIR(CTDDOC,' ')
      ENDIF
      CTEMP='dzdoc.scratch'
      LUNTMP=0
      IF(
     &   INDEX(CMD,'_TREE')  .EQ.0 .AND.
     &   INDEX(CMD,'MARK_NT').EQ.0 .AND.
     &   INDEX(CMD,'_MORE')  .EQ.0 .AND.
     &   INDEX(CMD,'_DROP')  .EQ.0 .AND.
     &   INDEX(CMD,'_FOUT')  .EQ.0 .AND.
     &   INDEX(CMD,'_MODIFY').EQ.0 .AND.
     &   INDEX(CMD,'_VECT')  .EQ.0)THEN
         IF(LNEWWI)THEN
            LUNTMP=LUPTMP
            CALL KUOPEN(LUNTMP,CTEMP,'UNKNOWN',ISTAT)
            IQPRNT=LUNTMP
         ELSE
            IQPRNT=6
            LUNTMP=6
         ENDIF
      ENDIF
      IF      (INDEX(CMD,'_CONT').NE.0)THEN
         IF(CTDDOC.NE.' ' .AND. INDEX(CMD,'DZ').EQ.0)THEN
            CALL DZDDWD(IXDIV,LIN,CHOPD1,0,0,LUNTMP)
         ELSE
            CALL DZSHOW(' ', IXDIV,LIN,
     &                  CHOPD1,0,0,0,0)
         ENDIF
      ELSE IF(INDEX(CMD,'_TREE').NE.0 .OR. INDEX(CMD,'_MORE').NE.0)THEN
         CALL IZPICT('DZDISP','SQ')
         CALL IZPICT('DZDISP','M')
         CALL MZSDIV(IXDIV,-7)
*        it is dropped meanwhile?
 
         LL=LIN
         IF(JBIT(IQ(LL+KQS),IQDROP).NE.0)THEN
            WRITE(*,*)'Bank got dropped meanwhile'
            GOTO 999
         ENDIF
*         CALL IACWK(IWKZEB)
         IF(INDEX(CMD,'_MORE').NE.0)THEN
            CHMORE='G'
         ELSE
            CHMORE=' '
         ENDIF
         CALL ISTXFP(6,0)
         CALL DZDISP(IXDIV,LIN,CTDDOC,'D'//CHMORE//CCOL//CCACT,
     &   IWKZEB,IWMZEB,ILOZEB, 0)
*         CALL IUWK(0,1)
*         CALL IDAWK(IWKZEB)
 
      ELSE IF(INDEX(CMD,'_FOUT').NE.0)THEN
         CALL KUGETI(LUNFOU)
         CALL FZINFO(LUNFOU)
         IF(INFLUN.NE.LUNFOU)THEN
            WRITE(*,*)'FZ file not open, LUN:',LUNFOU
            GOTO 999
         ENDIF
         IF(.NOT.BTEST(INFSTA,11))THEN
            WRITE(*,*)'No write permission on LUN:',LUNFOU
            GOTO 999
         ENDIF
         CALL KUGETC(CVALUE,NC)
         IF(NC.GT.0)THEN
            CALL UCTOH(CVALUE,IDH,4,4)
            IF(IDH.NE.IQ(LIN+KQS-4))THEN
               WRITE(*,*)'Skip bank(tree): ',CVALUE
               GOTO 999
            ENDIF
         ENDIF
         CALL KUGETC(CHDROP,NC)
         CALL KUGETC(CHYENO,NC)
         IF(CHYENO(1:1).EQ.'Y')THEN
            NUHOUT=NUH
         ELSE
            NUHOUT=0
         ENDIF
         IF(LIN.EQ.0 .OR. INMFLG.EQ.1)THEN
*           header only
            CHDROP='Z'
         ENDIF
         IF(LIN .EQ. 0 .AND. NUHOUT.EQ.0)THEN
             WRITE(*,*)'No d/s .AND. no header'
             GOTO 999
         ENDIF
         CALL FZOUT(LUNFOU,IXDIV,LIN,1,CHDROP,IOCHRD,NUHOUT,IUHEAD)
 
      ELSE IF(INDEX(CMD,'_DROP').NE.0)THEN
         CALL KUGETC(CHDROP,NC)
         CALL MZDROP(IXDIV,LIN,CHDROP)
 
      ELSE IF(INDEX(CMD,'_MODIFY').NE.0)THEN
         CALL KUGETI(N1)
         CALL KUGETI(N2)
         IF(N2.EQ.0)N2=N1
         CALL KUGETC(CINTV,NC)
         CALL KUGETR(RVALUE)
         CALL KUGETS(CVALUE,NC)
         IF(N1.GT.IQ(LIN+KQS-1))THEN
            WRITE(*,*)' N1 bigger ND'
            GOTO 999
         ENDIF
         IF(N2.GT.IQ(LIN+KQS-1) .OR.N2 .LT. N1)THEN
            WRITE(*,*)' N2 > ND or N2 < N1'
            GOTO 999
         ENDIF
*         get old value
         IF(N1.GT.0)THEN
            CALL DZSHOW(' ',IXDIV, LIN,'Q',1,0,N1,N1)
         ELSE
            WRITE(CQLINE(12:22),'(I11)') IQ(LIN+KQS+N1)
         ENDIF
         IF(INDEX(CQLINE(12:22),'"').NE.0)THEN
            IOTYP=5
         ELSEIF(INDEX(CQLINE(12:22),'.').NE.0)THEN
            IOTYP=3
         ELSEIF(N1.EQ.-4 .AND. N2.EQ.-4)THEN
            IOTYP=5
         ELSE
            IOTYP=2
         ENDIF
         IF(N1.EQ.N2)THEN
            WRITE(6,'(A,I6,A,A,A,Z9)') ' Word: ',N1,' Old value was: ',
     +      CQLINE(12:22) ,' Hex:',IQ(LIN+KQS+N1)
         ELSE
            CALL DZSHOW('Old values were: ', IXDIV, LIN,' ',
     +      1,0,N1,N2)
         ENDIF
         IF(IOTYP.EQ.5)THEN
            DO 100 K=N1,N2
  100       CALL UCTOH(CVALUE,IQ(LIN+KQS+K),4,4)
         ELSEIF(IOTYP.EQ.3)THEN
            DO 110 K=N1,N2
  110       Q(LIN+KQS+K)=RVALUE
         ELSE
            I16X=INDEX(CINTV,'16X')
            IF(I16X.NE.0) CINTV(I16X:I16X+2)='  $'
            CALL DZDCTI(CINTV,INTEVL)
            DO 120 K=N1,N2
  120       IQ(LIN+KQS+K)=INTEVL
         ENDIF
      ELSE IF(INDEX(CMD,'_SYS').NE.0)THEN
         CALL DZDSYW(IXDIV, LIN, LUNTMP)
      ELSE IF(INDEX(CMD,'_DOC').NE.0)THEN
         CALL DZDSDO(IXDIV, LIN,LUNTMP,' ' ,'R')
      ELSE IF(INDEX(CMD,'_EDIT').NE.0)THEN
          CALL DZDED1(IXDIV, LIN)
*         CALL DZDSDO(IXDIV, LIN,LUNTMP,' ' ,'IR')
      ELSE IF(INDEX(CMD,'MARK_NT').NE.0)THEN
         CALL DZDMNT(IXDIV, LIN)
      ELSE IF(INDEX(CMD,'SURVEY').NE.0)THEN
         CALL DZSURV('--- Zbrowse ----', IXDIV, LIN)
      ELSE IF(INDEX(CMD,'_VECT').NE.0)THEN
         CALL DZDPUV(IXDIV, LIN)
      ENDIF
 
      IF(LUNTMP.EQ.LUPTMP)THEN
         CLOSE(LUNTMP)
         LUNTMP=0
         IF(EDITSV)THEN
            CALL KUESVR(CTEMP, ISTAT)
         ELSE
            CALL KUEDIT(CTEMP, ISTAT)
         ENDIF
      ENDIF
999   IQPRNT=LUSAVE
      IF(CTDDOC.NE.' ')CALL RZCDIR(CWDSAV,' ')
      END
*********************************************************************
