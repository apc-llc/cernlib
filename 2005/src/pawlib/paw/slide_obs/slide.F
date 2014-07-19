*
* $Id: slide.F,v 1.1.1.1 1996/03/01 11:39:29 mclareni Exp $
*
* $Log: slide.F,v $
* Revision 1.1.1.1  1996/03/01 11:39:29  mclareni
* Paw
*
*
*CMZ :  1.10/00 30/10/90  11.48.27  by  O.Couet
*-- Author : A.Risso
      SUBROUTINE SLIDE(STRI)
      CHARACTER*(*) STRI
      CHARACTER*80 STR
      PARAMETER (NLOOP=8)
      COMMON /SLIDE1/ XB1,XB2,YB1,YB2,HC
      PARAMETER (ILINE=99)
      PARAMETER (MGUIDL=199)
      COMMON /KCGUID/ GUID(MGUIDL)
      CHARACTER*80 GUID
      CHARACTER*80 LINECP(99)
      CHARACTER*80 LINE(99)
      EQUIVALENCE (LINE(1),GUID(1)),(LINECP(1),GUID(100))
      COMMON /SLIDE15/ NWNB
      COMMON /SLIDE16/ HL
      COMMON /SLIDE17/ CHMAX,CHMIN,CHMN,PRIMO1,
     +                 IC,RLNSPC,ITMAR,IBMAR
      LOGICAL PRIMO1
      COMMON /SLIDE18/ ERR
      COMMON /SLIDE22/ ERROLD,ERLOOP
      COMMON /SLIDE23/ CHSAVE,CHOLD,STOP,NOWRT
      LOGICAL STOP,NOWRT
      LOGICAL FIRST
      COMMON /SLIDE25/ LSTOPN
      COMMON /SLIDE27/ FONT,IFONT,OLDFNT,OLDPRE,PREC,OLDCOL,ICOL
      COMMON /SLIDE20/ NEWLIN,NWLN
      CHARACTER*80 NEWLIN,NWLN
      CHARACTER*40 MSX(10)
      CHARACTER*1 BUF
      CHARACTER*1 CHBUF
      SAVE ITOTLN
      SAVE JK,NOMOD
      DATA NOMOD/0/
      DATA JK /1/
      DATA (MSX(K),K=1,6)/
     +'Text too long or CHMAX/CHMIN too big',
     +'Too many command',
     +'Line without spaces between the words',
     +'Open list not equal to close one',
     +'There are more then 99 lines',
     +'Syntax error in one command'/
*.______________________________________
*
      STR=STRI
*
*         Initialisation
*
      IF (JK.EQ.1) THEN
*
*        max character height
*
         CHMAX=2.
*
*        min character height
*
         CHMIN=0.001
*
*        line spacing ( % of character height)
*
         RLNSPC=.55
*
*        top margin
*
         ITMAR=0
*
*        bottom margin
*
         IBMAR=0
*
*        other initializations
*
         CALL SLINIT
*
         HC =CHMAX
         CHMN =CHMIN
         CHSAVE=CHMAX
         CHOLD =0.
         PRIMO1=.TRUE.
         STOP =.FALSE.
         NOWRT =.FALSE.
         ERROLD=0
         ERLOOP=0
         IC=1
      ENDIF
*
      IF (JK.GT.ILINE) THEN
         CALL IGERR(MSX(5),'APPLICATION SLIDE')
         RETURN
      ENDIF
      LINECP(JK)=STR

      IF (NOMOD.EQ.4) THEN
          NOMOD=0
          GO TO 3
      ENDIF
  2   IF(NOMOD.EQ.1)THEN
         JPOS=INDEX(STR,'</XMP>')
         IF (JPOS.NE.0.AND.LENOCC(STR)-6.LT.JPOS) THEN
*         </XMP> is present but it is at the end of line
*         elaboration of the line will start next line
             NOMOD=4
             GO TO 101
         ELSEIF(JPOS.NE.0) THEN
*         </XMP> is present and it isn't at the end of line
             NOMOD=0
             GO TO 5
         ELSE
             GO TO 101
         ENDIF
      ENDIF
      IF (NOMOD.EQ.2) THEN
          NOMOD=1
          GO TO 2
      ENDIF
  3   JPOS=INDEX(STR,'<XMP>')
      IF (JPOS.NE.0.AND.LENOCC(STR)-5.LT.JPOS) THEN
*         <XMP> is present but it is at the end of line
*         non elaboration of the line will start next line
          NOMOD=2
      ELSEIF(JPOS.NE.0) THEN
*         <XMP> is present and it isn't at the end of line
          NOMOD=1
          GO TO 101
      ENDIF
  5   CONTINUE
*
*     elimination of excess blanks between two words
*
      NEWLIN=' '
      NWLN  =' '
      J=80
      JH=1
      TWOCMD=0
      FIRST=.FALSE.
*
*     elimination of blanks
*
   10 CHBUF = LINECP(JK)(J:J)
      IF (CHBUF.EQ.' '.AND.J.GT.1) THEN
         J = J - 1
         GO TO 10
      ENDIF
*
*     take a word from the line
*
   20 IF (CHBUF.NE.' ') THEN
         IF (TWOCMD.NE.0.AND.CHBUF.EQ.'>'.AND.FIRST) THEN
            NEWLIN=NWLN(2:)
            NWLN=NEWLIN
            TWOCMD=0
         ENDIF
         FIRST=.FALSE.
         NEWLIN = CHBUF // NWLN
         NWLN=NEWLIN
         J = J - 1
         IF (J.GE.1) THEN
            CHBUF = LINECP(JK)(J:J)
            GO TO 20
         ENDIF
      ENDIF
   30 BUF=LINECP(JK)(JH:JH)
      IF (BUF.EQ.' ') THEN
         JH=JH+1
         GO TO 30
      ENDIF
      IF (J.GT.JH)  THEN
         IF(NWLN(1:1).EQ.'<') THEN
            FIRST=.TRUE.
            TWOCMD=1
         ENDIF
         NEWLIN = ' ' // NWLN
         NWLN=NEWLIN
         GO TO 10
      ENDIF
      LINECP(JK)=NEWLIN
  101 CONTINUE
      STR=LINECP(JK)
      ITOTLN=JK
      JK=JK+1
   40 CALL SLIDEM(STR)
*
      IF(ERR.EQ.7.OR.ERR.EQ.8.OR.ERR.EQ.9) THEN
         NOWRT=.TRUE.
      ENDIF
*
      IF (STR(1:6).NE.'<DRAW>'.AND.IC.EQ.1) RETURN
*
      IF (NOWRT.AND.IC.EQ.1) THEN
         JK=1
         RETURN
      ENDIF
*
      IF (STR(1:6).NE.'<DRAW>'.AND.IC.GT.1) GO TO 60
*
      IF (NOWRT.AND.IC.GT.1) GO TO 95
      GO TO 70
   50 JK=1
      CALL SLINIT
   60 STR=LINECP(JK)
      JK=JK+1
      GO TO 40
   70 CONTINUE
*
*     ERR=1   text too long
*     ERR=2   too many commands
*     ERR=3   line without space between the words
*     ERR=4   open list not equal to close one
*     ERR=5   there are more then 99 lines
*     ERR=6   syntax error in one command
*     ERR=7   general error during check of parameters
*     ERR=8   element of list without open list
*     ERR=9   bold parameter not correct
*     ERR=10  bold command skipped
*     ERR=11  hplot escape character skipped
*     ERR=12  bullet command skipped
*
      IF(LSTOPN.NE.0)ERR=4
      IF (ERR.EQ.1.OR.ERR.EQ.3.OR.ERR.EQ.4) THEN
         NWNB=99999
         ERROLD=ERR
         ERLOOP=IC
         ERR=0
      ENDIF
      IF (ERR.EQ.2.OR.ERR.EQ.6.OR.ERR.EQ.9) THEN
         CALL IGERR (MSX(INT(ERR)),'APPLICATION SLIDE')
         RETURN
      ENDIF
*
*     check high of char
*
      HL= HC+RLNSPC*HC
      TOP=HL*ITMAR
      BOT=HL*IBMAR
      IF (STOP) GO TO 80
      IF ((NWNB*HL+TOP+BOT).GT.(YB2-YB1)) THEN
         IF (IC.LT.NLOOP.AND.CHMAX.NE.CHMIN) THEN
*
*            try to reduce
*
            CHSAVE=HC
            HC=(HC+CHMN)/2
            IC=IC+1
            GO TO 50
         ELSE
*
*           text exced the page
*
            IF (CHOLD.NE.0) THEN
               STOP=.TRUE.
               HC=CHOLD
               HCPR=INT(HC*1000)
               CALL IGSA(0)
***               PRINT*,'Character height used : ',HCPR/1000
               ERROLD=0
               GO TO 50
            ELSE
               IF(ERROLD.NE.0) THEN
                  CALL IGERR(MSX(INT(ERROLD)),'APPLICATION SLIDE')
               ELSE
                  CALL IGERR(MSX(1),'APPLICATION SLIDE')
               ENDIF
               CALL SLINIT
               JK=1
               RETURN
            ENDIF
         ENDIF
      ELSE
         IF (IC.LT.NLOOP.AND.CHMAX.NE.CHMIN) THEN
*
*           try to expand
*
            IF (HC.LT.CHMAX) THEN
               CHOLD=HC
               CHMN =HC
               HC =(HC+CHSAVE)/2
               IC =IC+1
               GO TO 50
            ELSE
*
*              print the text
*
               GO TO 80
            ENDIF
         ELSE
*
*           this is the correct size
*
            STOP=.TRUE.
            HC=CHOLD
            IF (ABS(CHMAX-CHMIN).LE.(0.00001))HC=CHMAX
            HCPR=INT(HC*1000)
            CALL IGSA(0)
***            PRINT*,'Character height used : ',HCPR/1000
            ERROLD=0
            IF (ABS(CHMAX-CHMIN).LE.(0.00001)) THEN
               GO TO 80
            ELSE
               GO TO 50
            ENDIF
         ENDIF
      ENDIF
   80 CONTINUE
*
*     print error messages
*
      IF (ERROLD.NE.0.AND.ERLOOP.EQ.IC) THEN
         CALL IGERR(MSX(INT(ERROLD)),'APPLICATION SLIDE')
         RETURN
      ENDIF
      IF (ERR.EQ.10) THEN
         CALL IGERR('BOLD command skipped',' ')
      ELSEIF(ERR.EQ.11) THEN
         CALL IGERR('HPLOT escape character skipped',' ')
      ELSEIF(ERR.EQ.12) THEN
         CALL IGERR('SET BULLET command skipped',' ')
      ENDIF
*
*     writing of text
*
      FONT=IFONT
      NBTOT=0
      DO 90 I=1,NWNB
         LL=LENOCC(LINE(I))
         IF (LL.GT.0) THEN
            NBOLD=NBTOT+1
            NBTOT=NBTOT+LL
            CALL SLIACT(NBOLD,NBTOT,I)
         ELSE
            CALL IGTEXT(XB1,YB2-(ITMAR+I)*HL,' ',HC,0.,'L')
         ENDIF
   90 CONTINUE
   95 CALL SLINIT
      JK=1
      END
