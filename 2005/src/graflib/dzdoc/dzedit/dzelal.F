*
* $Id: dzelal.F,v 1.1.1.1 1996/03/04 16:13:22 mclareni Exp $
*
* $Log: dzelal.F,v $
* Revision 1.1.1.1  1996/03/04 16:13:22  mclareni
* Dzdoc/Zebpack
*
*
#include "dzdoc/pilot.h"
      SUBROUTINE DZELAL
#include "dzeditkeep.inc"
#if defined(CERNLIB_BSLASH)
#include "dzdoc/bslash2.inc"
#endif
#if !defined(CERNLIB_BSLASH)
#include "dzdoc/bslash1.inc"
#endif
      CHARACTER*4 CKEY1,CKEY2, CHOYNO
 
      LSINGL=.FALSE.
      GOTO 10
 
      ENTRY DZELSG
      LSINGL = .TRUE.
      CALL KUGETC(CHBSBK,NCHA)
      CALL KUGETC(CHBSUP,NCHA1)
 
   10 CONTINUE
      IF(LUNRZ.EQ.0)THEN
         WRITE(LUNOUT,*)' No RZ-file open'
         GOTO 999
      ENDIF
      CALL KUGETS(CHLIST,NCLIST)
      CALL KUGETC(CHLOPT,NCLOPT)
      CALL KUGETC(CHOYNO,NCOYNO)
 
      IF(INDEX(CHLOPT,'P').NE.0 .OR. INDEX(CHLOPT,'H').NE.0)THEN
         IF(ISCROP .EQ. 0)THEN
            LUNSC1=LUPSC1
            CALL KUOPEN(LUNSC1,'dzedit.scrat1','UNKNOWN',ISTAT)
            LUNLST = LUNSC1
            ISCROP = 1
         ENDIF
         IF(INDEX(CHLOPT,'P').NE.0)THEN
            IPOSTF = 1
            IF(NCLIST.LE.0)THEN
               IF(LSINGL)THEN
                  CHLIST=CHBSBK(1:NCHA)//'_'//CHBSBK(1:NCHA)//'.ps'
               ELSE
                  CHLIST='DZEDIT_LIST.ps'
               ENDIF
            ENDIF
         ENDIF
      ELSE
         IF(NCLIST .NE. 0)THEN
            LUNLST=LUPLST
            IF(ILSTOP .EQ. 0)THEN
               CALL KUOPEN(LUNLST,CHLIST,'UNKNOWN',ISTAT)
               ILSTOP = 1
            ENDIF
         ELSE
            LUNLST=6
         ENDIF
      ENDIF
      IF(INDEX(CHLOPT,'S').NE.0
     +   .OR.  IPOSTF.NE.0
     +   .AND. ISGMOP.EQ.0)THEN
         ISGMOP = 1
         WRITE(LUNLST,'(A)')'<!DOCTYPE SGMLGUID>'
         WRITE(LUNLST,'(A)')'<SET TAG=H1 ITEM=NAME VALUE="BankId">'
         WRITE(LUNLST,'(A)')'<GDOC>'
         WRITE(LUNLST,'(A)')'<TITLEP>'
         WRITE(LUNLST,'(A)')'<TITLE>ZEBRA BANK DOCUMENTATION'
         WRITE(LUNLST,'(A)')'<TITLE> FROM '
         WRITE(LUNLST,'(A)')'<TITLE>'//CHRZF
         WRITE(LUNLST,'(A)')'<DATE>'
         WRITE(LUNLST,'(A)')'</TITLEP>'
         WRITE(LUNLST,'(A)')'<BODY>'
      ENDIF
 
*      IQPRNT = LUNLST
      CHBSEL(1:4) = CHBSBK
      CHBSEL(5:8) = CHBSUP
      CHOPDZ = 'R'
      IF(INDEX(CHLOPT,'S').NE.0 .OR. IPOSTF.NE.0) CHOPDZ(3:3) = 'S'
      IF(CHOYNO(1:1).EQ. 'Y') CHOPDZ(4:4) = 'O'
      IF(INDEX(CHLOPT,'I').NE.0)CHOPDZ(5:5) = 'I'
      CALL KUCMD ('DZEDIT','   ','SW')
      CALL KUPVAL('DRAWONETREE','CHBSBK',0,0.,CHBSBK,'D')
      CALL KUPVAL('DRAWONETREE','CHBSUP',0,0.,CHBSUP,'D')
      CALL KUPVAL('LISTONEBANK','CHBSBK',0,0.,CHBSBK,'D')
      CALL KUPVAL('LISTONEBANK','CHBSUP',0,0.,CHBSUP,'D')
      CALL KUCMD (BS,' ','SW')
      IF(LSINGL)THEN
         CALL DZDOCO(LUNLST,CHBSEL,CHOPDZ)
         GOTO 999
      ENDIF
      CALL RZKEYS(NWKEY,MAXKEY,KEYS,NKEYS)
      DO 5  I=1,NKEYS
   5   MARKUP(I) = I
      II = 1000000
      CALL RZIN(IXDZDS,LSUPL,2,KEYS(1,1),II,'C')
      DO 30   I=1,NKEYS-1
         DO 20   K=I+1,NKEYS
            CALL UHTOC(KEYS(1,I),4,CKEY1,4)
            CALL UHTOC(KEYS(1,K),4,CKEY2,4)
            IF( LLE(CKEY1, CKEY2))GOTO 20
            KEYSAV = KEYS(1,I)
            KEYS(1,I) = KEYS(1,K)
            KEYS(1,K) = KEYSAV
            KEYSAV = KEYS(2,I)
            KEYS(2,I) = KEYS(2,K)
            KEYS(2,K) = KEYSAV
            KEYSAV = MARKUP(I)
            MARKUP(I) = MARKUP(K)
            MARKUP(K) = KEYSAV
 
   20    CONTINUE
   30 CONTINUE
      IF(LISTSG) CHOPDZ(3:3) = 'S'
      DO 40   I=1,NKEYS
         IF(LSINGL .AND. MARKUP(I) .EQ. 0)GOTO 40
         IF(IDATCH .GT. 0 .OR. ITIMCH .GT. 0)THEN
            II = 1000000
            CALL RZIN(IXDZDS,LSUPL,2,KEYS(1,I),II,'C')
            IF(IQUEST(50) .LE. 0)GOTO 40
            CALL RZDATE(IQUEST(71),IDATE,ITIME,1)
            IF(IDATCH .GT. 0 .AND. IDATE .LT. IDATCH)GOTO 40
            IF(ITIMCH .GT. 0 .AND. ITIME .LT. ITIMCH)GOTO 40
         ENDIF
         CALL UHTOC(KEYS(1,I),4,CHKEY(1:4),4)
         CALL UHTOC(KEYS(2,I),4,CHKEY(5:8),4)
         IF(INDEX(CHLOPT,'I').NE.0)WRITE(LUNLST,'(A,A,A,A)')
     &   '*******************************************  ',
     &    CHKEY(1:4),' / ',CHKEY(5:8)
         CALL DZDOCO(LUNLST,CHKEY,CHOPDZ)
   40 CONTINUE
  999 END
***********************************************************************
