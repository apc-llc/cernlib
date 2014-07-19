*
* $Id: dzddtr.F,v 1.1.1.1 1996/03/04 16:13:19 mclareni Exp $
*
* $Log: dzddtr.F,v $
* Revision 1.1.1.1  1996/03/04 16:13:19  mclareni
* Dzdoc/Zebpack
*
*
#include "dzdoc/pilot.h"
      SUBROUTINE DZDDTR(ISTORE,LIN,LUNS,LUNL,LUNM,IWMETA,ITMETA,CHOPT)
      CHARACTER*(*) CHOPT
*.                       'P' PostScript
*.                       'C' colored
*.                       'N' opening and closing WS is done by caller
*.                       'T' D/S generated from documentation
*.                       'R' Rz-file with documentation available
*.                       'B' draw boxes as <>
*.                       '1' draw banks od 1 level only if not the
*.                           complete 2nd level fits
*.                       'M' mark subtree complete (avoid drawing 
*.                           subtrees more then once (T implies M)
      CHARACTER*7 CTEMP
*      CHARACTER*4 CHODRW
      CHARACTER*8 CTEMP1
      CHARACTER*80 CLINE
#include "dzdprm.inc"
#include "zebra/zunit.inc"
#include "dzdoc/bkwrp.inc"
#if defined(CERNLIB_BSLASH)
#include "dzdoc/bslash2.inc"
#endif
#if !defined(CERNLIB_BSLASH)
#include "dzdoc/bslash1.inc"
#endif
      LOGICAL DOPDOC, NOTDON
      INTEGER MALD
      PARAMETER (MALD=50)
      INTEGER IPCSEQ,ILABEL, IALD(2,MALD), IPALD, LALD, LGO, L
      DATA IPCSEQ/0/,ILABEL/0/
 
      PAGECM=14./20.
      IFOMED=0
      LFCSAV=LFCOL
      IF(INDEX(CHOPT,'C').NE.0)THEN
         LFCOL=1
      ELSE
         LFCOL=0
      ENDIF
      CALL ISFASI(-103)
*      IF(INDEX(CHOPT,'1').NE.0)THEN
*         CHODRW='RT1'
*      ELSE
*         CHODRW='RT'
*      ENDIF
      IF(INDEX(CHOPT,'0').NE.0)ILABEL=0
      YBSING = 0.85
*      WRITE(IQPRNT,'(A,A4)') ' Draw tree below: ',IQ(KQS+L-4)
      NBK = 0
      NBDOCT = 0
      IPALD=0
      LALD=0
      CALL UZERO(IALD,1,2*MALD)

      L=-LIN
   10 CONTINUE
      LGO=L
      CALL DZDWTR(ISTORE,LGO,L)
      IF(L.EQ.LIN)WRITE(IQPRNT,'(A,A4)')' Draw tree below: ',IQ(KQS+L-4)
      IF(L .NE. 0)THEN
         ILKACT = 0

20       CONTINUE
         CALL UHTOC(IQ(L+KQS-4),4,CTEMP(1:4),4)
         WRITE(CTEMP(5:7),'(I3.3)')IPCSEQ
         CTEMP1(1:4) = CTEMP(1:4)
         ILKSAV = ILKACT
         CALL DZDRA1(ISTORE,L,ILKACT,CHOPT,
     &               0.,0.,.TRUE.,IFLCUR,YTOP,YBOTTM)
         LU = LQ(KQS+L+1)
         IF(LU .GT. 1)THEN
               CALL UHTOC(IQ(KQS+LU-4),4,CTEMP1(5:8),4)
         ELSE
               CTEMP1(5:8) = 'NONE'
         ENDIF

*            remember complete subtrees
         IF(ILKACT.GT.0)THEN
            ILLAST=ILKACT
         ELSE
            ILLAST=IQ(KQS+L-2)
         ENDIF
         DO 25 I=ILKSAV+1,ILLAST
            IF(IAND(IQ(LQ(L+KQS-I)+KQS),ALDBIT).NE.0)THEN
               IPALD=IPALD+1
               IF(IPALD.GT.MALD)IPALD=1
               IF(LALD.LT.MALD) LALD=LALD+1
               IALD(1,IPALD) = IQ(LQ(L+KQS-I)+KQS-4)
               IALD(2,IPALD) = IQ(L +KQS-4)
            ENDIF
25       CONTINUE
 
*        check for complete subtree
         NOTDON=.TRUE.
         DO 26 I=1,LALD
            IF(IQ(L +KQS-4).EQ. IALD(1,I) .AND.
     &         IQ(LU+KQS-4).EQ. IALD(2,I))THEN
                  NOTDON=.FALSE.
                  GOTO 27
            ENDIF
26       CONTINUE
27       CONTINUE
 
         ILKACT = ILKSAV
*           output part
         IF(  (YBOTTM .LT. YBSING
     &       .OR. ILKACT.GT.0)
     &       .AND.NOTDON)      THEN
             IPCSEQ=IPCSEQ+1
             IF(IPCSEQ. GT. 999)IPCSEQ=1
             IFBL = INDEX(CTEMP(1:4),' ')
             IF(IFBL .GT. 0 .AND. IFBL .LE. 4)THEN
                DO 30 I=IFBL,4
30              CTEMP(I:I)='X'
             ENDIF
             IXMM=140
             IYMM=(1.-YBOTTM)*140.
             WRITE(CLINE,'(A,2I5)')
     +          '%%PICTURE '//CTEMP(1:7),-IXMM,IYMM
             IF(INDEX(CHOPT,'P').EQ.0)THEN
                 XAPA = .1465
                IF(INDEX(CHOPT,'N').EQ.0)CALL IACWK(IWMETA)
                CALL IGRNG(XAPA*100.,XAPA*100.*(1.-YBOTTM))
                CALL ICLRWK(IWMETA,1)
                IF(ITMETA.EQ. -111)THEN
                    CALL IPPSTR(CLINE(1:28))
                    CALL IPPSTR('@ ')
                ENDIF
             ELSEIF(INDEX(CHOPT,'P').NE.0 .OR.
     +              LUNM.GT.0)THEN
                IF(LUNM.GT.0)THEN
                   IFOMED=2
                   PAMM10=73.25
                   IPCSIZ=0
                   LUNGRA=LUNM
                   CALL DZDPLN(LUNGRA,CLINE,0)
                   CALL DZDPLN(LUNGRA,' ',-99)
                ENDIF
                CALL ICLRWK(0,0)
                CALL DZDRAW(0,L,ILKACT,CHOPT)
                IF(LUNM.GT.0)THEN
                   CALL DZDPLN(LUNGRA,' ',-99)
*                   CALL DZDPLN(LUNGRA,' showpage',0)
                   CALL DZDPLN(LUNGRA,' ',-99)
                ENDIF
             ENDIF
             IF(LUNS .GT. 0)THEN
                 CALL DZDGDS(CTEMP1,' ',CLINE,NLT)
                 IF(NLT .LE. 0)THEN
                      CLINE = ' '
                      NLT=1
                 ENDIF
                 IF(IFOMED.LE.2)THEN
                     WRITE(LUNS,'(A,I3.3,A)') '<FIG ID=T'
     +               ,IPCSEQ,'>'
                     WRITE(LUNS,'(A,A,A)')'<PICTURE NAME='
     +               ,CTEMP,'S>'
                     WRITE(LUNS,'(A,A,A/A)')'<FIGCAP>',CTEMP1(1:4)
     +               ,'  ',CLINE(1:NLT)
                     WRITE(LUNS,'(A)')'</FIG>'
                 ENDIF
             ENDIF
         ENDIF
*        dont write documentation more than once if:
*        more than one picture needed and for next-banks
*        with the same Holl Id
         LN=LQ(KQS+L)
         IF(ILKACT.NE.0)THEN
            DOPDOC=.FALSE.
         ELSE
            DOPDOC=.TRUE.
            IF(LN.NE.0)THEN
               IF(IQ(KQS+LN-4).EQ.IQ(KQS+L-4))THEN
                 DOPDOC=.FALSE.
               ENDIF
            ENDIF
         ENDIF
         IF(DOPDOC)THEN
            IF(LU.EQ.0)THEN
               IF(IQ(KQS+L-1).GE.4)THEN
                  CALL UHTOC(IQ(KQS+L+4),4,CTEMP1(5:8),4)
               ELSE
                   CTEMP1(5:8)='NONE'
               ENDIF
            ENDIF
            CALL DZDGDS(CTEMP1,' ',CLINE,NLT)
            IF(NLT .GT. 0)THEN
               NBDOCT=NBDOCT+1
               ILC=INDXBC(CLINE,' ')
            ELSE
               ILC=0
            ENDIF
            NBK = NBK + 1
            IF(LUNL.GT.0)WRITE(LUNL,'(A)')CTEMP1
*               IF(LU.EQ.0)CTEMP1(5:8)='****'
*              output part
            IF(LUNS.GT.0)THEN
               WRITE(LUNS,'(A)')'<LISTING>'
               CALL DZDOCO(LUNS,CTEMP1,'RH')
               WRITE(LUNS,'(A)')'</LISTING>'
               WRITE(LUNS,'(A,A)')'<I1>',CTEMP1(1:4)
            ENDIF
         ENDIF
         IF(ILKACT .NE. 0)GOTO 20
         GOTO 10
      ENDIF
*
      WRITE(IQPRNT,'(A,I8,A)')' Total # of banks in tree: ',NBK
      WRITE(IQPRNT,'(A,I8,A)')' # of documented banks     ',NBDOCT
      CALL ISFASI(-5)
      LFCOL=LFCSAV
      IFOMED=0
      END
*********************************************************************
