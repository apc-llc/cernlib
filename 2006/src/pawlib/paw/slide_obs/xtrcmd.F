*
* $Id: xtrcmd.F,v 1.1.1.1 1996/03/01 11:39:30 mclareni Exp $
*
* $Log: xtrcmd.F,v $
* Revision 1.1.1.1  1996/03/01 11:39:30  mclareni
* Paw
*
*
*CMZ :  1.10/00 28/09/90  16.35.29  by  O.Couet
*-- Author : A.Risso
      SUBROUTINE XTRCMD(I)
      CHARACTER*20 CMD
      PARAMETER (MGUIDL=199)
      COMMON /KCGUID/ GUID(MGUIDL)
      CHARACTER*80 GUID
      CHARACTER*80 LINECP(99)
      CHARACTER*80 LINE(99)
      EQUIVALENCE (LINE(1),GUID(1)),(LINECP(1),GUID(100))
      PARAMETER (MAXCMD=1000)
      COMMON /SLIDE1/ XB1,XB2,YB1,YB2,HC
      COMMON /SLIDE7/  LSTCMD(MAXCMD,2),PARCMD(MAXCMD,2),JCD
      COMMON /SLIDE8/ ITOT
      COMMON /SLIDE11/ SYMLON
      COMMON /SLIDE18/ ERR
      COMMON /SLIDE24/ KOLD
      COMMON /SLIDE27/ FONT,IFONT,OLDFNT,OLDPRE,PREC,OLDCOL,ICOL
      COMMON /SLIDE28/ FACTL
      COMMON /SLIDE34/ FACT(13)
      COMMON /SLIDE30/ TSPACE(4,1)
      COMMON /SLIDE31/ STRPNT,SPNT
      COMMON /SLIDE35/ VALPRO,VALNPR
      COMMON /SLIDE9/ BLSPAC
      COMMON /SLIDE40/ MODIF,XMP1,XMP2,CUTLN
      LOGICAL MODIF,XMP1,XMP2
      CHARACTER*3 STRPNT,SPNT
      CHARACTER*3 SP
      COMMON /QUEST/IQUEST(100)
      CHARACTER*1 CHBUF
      PARAMETER (MAXTOK=10)
      INTEGER   TOK(2,MAXTOK)
      PARAMETER(ITCMD=13)
      CHARACTER*20 CMND(ITCMD)
      DIMENSION ICDNUM(ITCMD)
      DATA (CMND(NC),NC=1,ITCMD)/
     +'<NOP>','<COLOR','</FONT>','<BOLD',
     +'</BOLD>','<SPC>','<SYM>','<SPC1>',
     +'<CF>','<SETBUL','<XMP>','</XMP>','</COLOR>'/
      DATA (ICDNUM(NC),NC=1,ITCMD)/
     +-4,1,2,9,10,11,12,13,14,15,16,17,18/
*.______________________________________
*
*        extraction of commands from line
*
      JV=JCD
      IP=1
   10 CMD='<'
      K = LENOCC(CMD)
      IPOS = INDEX(LINE(I)(IP:),CMD(1:K))
      IF (IPOS.NE.0) IPOS = IP+IPOS-1
      IF (IPOS.GT.0) THEN
         IOLD=IPOS
         IP=IPOS
         JCDOLD=JCD
         NC=1
   20    CMD=CMND(NC)
         K = LENOCC(CMD)
         IPOS = INDEX(LINE(I)(IP:),CMD(1:K))
         IF (IPOS.NE.0) IPOS = IP+IPOS-1
         IF (IPOS.GT.0.AND.IPOS.EQ.IOLD) THEN
            JCD=JCD+1
            IF (JCD.GT.MAXCMD) THEN
               ERR=2
               RETURN
            ENDIF
            LSTCMD(JCD,1)=IPOS+ITOT+1

            CMD=CMND(NC)
            CALL TKECMD(I,CMD,IPOS)
            IACT=ICDNUM(NC)
            LSTCMD(JCD,2)=IACT
            CMD='>'
            IF (IACT.EQ.1) THEN
               OLDCOL=ICOL
               K = LENOCC(CMD)
               IPOS2 = INDEX(LINE(I)(IP:),CMD(1:K))
               IPOS2 = IPOS2+IP-1
               CMD=LINE(I)(IPOS:IPOS2)
               IF (LENOCC(CMD).GT.1) THEN
                  CMD=LINE(I)(IPOS:IPOS2-1)
                  CALL KILEXP(CMD,' ',' ',' ',MAXTOK,TOK,NTOK,' ')
                  IF (NTOK.NE.1) THEN
                     CALL IGERR('Number of color parameter not correct',
     +               'APPLICATION SLIDE')
                     ERR=7
                     RETURN
                  ENDIF
                  CALL KICTON(CMD(TOK(1,1):TOK(2,1)),ICOL,COL)
                  IF (IQUEST(1).NE.0) THEN
                CALL IGERR('COLOR parameter not correct', 'APPLICAT'
     +               //'ION SLIDE')
                     ERR=7
                     RETURN
                  ENDIF
                  CMD=LINE(I)(IPOS:IPOS2)
               ELSE
                  CMD='>'
               ENDIF
               CALL TKECMD(I,CMD,IPOS)
               PARCMD(JCD,1)=ICOL
               IF (LINE(I)(IPOS+1:IPOS+1).NE.' '.AND.
     +                 LINE(I)(IPOS:IPOS).NE.' ')THEN
                   LSTCMD(JCD,1)=IPOS+ITOT
               ELSEIF (LINE(I)(IPOS+1:IPOS+1).NE.' ')THEN
                   LSTCMD(JCD,1)=IPOS+ITOT
               ENDIF
            ELSEIF(IACT.EQ.2) THEN
               PARCMD(JCD,1)=OLDFNT
               PARCMD(JCD,2)=OLDPRE
               FONT=OLDFNT
               PREC=OLDPRE
               IF (FONT.EQ.0) THEN
                  FACTL=1
               ELSEIF((FONT.LE.-1..AND.FONT.GE.-7.).OR.
     +                FONT.EQ.-12..OR.FONT.EQ.-13.) THEN
                  FONT=OLDFNT
                  FACTL=FACT(ABS(INT(FONT)))
               ELSEIF(FONT.LE.-8..AND.FONT.GE.-11.) THEN
                  FONT=OLDFNT
                  FACTL=1
               ELSE
                  FACTL=1
               ENDIF
               IF(FONT.LE.-8.AND.FONT.GE.-11) THEN
                  BLSPAC=FACT(-INT(FONT))*HC
*              Default value for initial left space for list
                  VALNPR=3*BLSPAC
               ELSE
*              blank space : formula taked in IGTEXT_routine
                  SCALE=HC/21
                  BLSPAC=SCALE*7*FACTL
*              Default value for initial left space for list
                  VALPRO=6*BLSPAC
               ENDIF
            ELSEIF(IACT.EQ.9) THEN
               K = LENOCC(CMD)
               IPOS2 = INDEX(LINE(I)(IP:),CMD(1:K))
               IPOS2 = IPOS2+IP-1
               CMD=LINE(I)(IPOS:IPOS2)
               IF (LENOCC(CMD).GT.1) THEN
                  CMD=LINE(I)(IPOS:IPOS2-1)
                  CALL KILEXP(CMD,' ',' ',' ',MAXTOK,TOK,NTOK,' ')
                  IF (NTOK.GT.2) THEN
                     CALL IGERR('Number of bold parameter not correct',
     +               'APPLICATION SLIDE')
                     ERR=7
                     RETURN
                  ENDIF
                  IF (NTOK.GE.1)THEN
                  CALL KICTON(CMD(TOK(1,1):TOK(2,1)),IPASS,PASS1)
                  IF (PASS1.LT.1) GO TO 30
                  PARCMD(JCD,1)=PASS1
                  IF (IQUEST(1).NE.0) GO TO 30
                  ENDIF
                  IF (NTOK.EQ.2)THEN
                  CALL KICTON(CMD(TOK(1,2):TOK(2,2)),ICSHI,CSHI1)
                  IF (CSHI1.LT.1) GO TO 30
                  PARCMD(JCD,2)=CSHI1
                  ENDIF
   30             IF (IQUEST(1).NE.0) THEN
                     CALL IGERR('BOLD parameter not correct', 'APPLICAT'
     +               //'ION SLIDE')
                     ERR=7
                     RETURN
                  ENDIF
                  CMD=LINE(I)(IPOS:IPOS2)
               ELSE
                  CMD='>'
               ENDIF
               CALL TKECMD(I,CMD,IPOS)
               IF (FONT.NE.0.) THEN
                  ERR=10
                  LSTCMD(JCD,1)=0
                  LSTCMD(JCD,2)=0
                  PARCMD(JCD,1)=0
                  PARCMD(JCD,2)=0
                  JCD=JCD-1
               ENDIF
            ELSEIF(IACT.EQ.10) THEN
               IF (FONT.NE.0.) THEN
                  LSTCMD(JCD,1)=0
                  LSTCMD(JCD,2)=0
                  JCD=JCD-1
               ENDIF
            ELSEIF(IACT.EQ.11) THEN
*
*              if some commands were in the remainder of cut_line
*              they are shifted after next subroutine-call
*
               CALL SHTCMD
               TSPACE(1,1)=TSPACE(1,1)+1
               TSPACE(2,1)=I
            ELSEIF(IACT.EQ.12) THEN
               LSTCMD(JCD,1)=IPOS+ITOT
               IF (KOLD.NE.I) THEN
                  PARCMD(JCD,1)=SYMLON
                  KOLD=I
               ELSE
                  LSTCMD(JCD,1)=0
                  LSTCMD(JCD,2)=0
                  PARCMD(JCD,1)=0
                  JCD=JCD-1
               ENDIF
               CALL SHTCMD
            ELSEIF(IACT.EQ.13) THEN
               LSTCMD(JCD,1)=IPOS+ITOT
               CALL SHTCMD
            ELSEIF(IACT.EQ.14) THEN
               LSTCMD(JCD,1)=IPOS+ITOT
            ELSEIF(IACT.EQ.15) THEN
               K = LENOCC(CMD)
               IPOS2 = INDEX(LINE(I)(IP:),CMD(1:K))
               IPOS2 = IPOS2+IP-1
               CMD=LINE(I)(IPOS:IPOS2)
               IF (LENOCC(CMD).GT.1) THEN
                  CMD=LINE(I)(IPOS:IPOS2-1)
                  CALL KILEXP(CMD,' ',' ',' ',MAXTOK,TOK,NTOK,' ')
                  IF (NTOK.NE.1) THEN
                     CALL IGERR('Parameter in start point list',
     +               'APPLICATION SLIDE')
                     ERR=7
                     RETURN
                  ENDIF
                  CMD=LINE(I)(IPOS:IPOS2)
               ENDIF
               JH=1
   40          CHBUF=CMD(JH:JH)
               IF (CHBUF.EQ.' ') THEN
                  JH=JH+1
                  GO TO 40
               ENDIF

               SP=CMD(JH:JH)//'  '
*
*               skip of escape characters of HPLOT
*
               IF (SP.EQ.'>'.OR.SP.EQ.'<'.OR. SP.EQ.'['.OR.SP.EQ.']'
     +         .OR. SP.EQ.'"'.OR.SP.EQ.'#'.OR. SP.EQ.'^'.OR.SP.EQ.'?'
     +         .OR. SP.EQ.'!'.OR.SP.EQ.'&'.OR. SP.EQ.'$') THEN
                  ERR=11
               ELSE
                  IF (FONT.EQ.0.) THEN
                     ERR=12
                  ELSE
                     SPNT=CMD(JH:JH)//'  '
                  ENDIF
               ENDIF
               CALL TKECMD(I,CMD,IPOS)
               LSTCMD(JCD,1)=0
               LSTCMD(JCD,2)=0
               JCD=JCD-1
            ELSEIF(IACT.EQ.16) THEN
               LSTCMD(JCD,1)=0
               LSTCMD(JCD,2)=0
               JCD=JCD-1
               MODIF=.FALSE.
               CUTLN=1
               XMP1=.TRUE.
            ELSEIF(IACT.EQ.17) THEN
               LSTCMD(JCD,1)=0
               LSTCMD(JCD,2)=0
               JCD=JCD-1
               XMP2=.TRUE.
            ELSEIF(IACT.EQ.18) THEN
               PARCMD(JCD,1)=OLDCOL
               IF (LINE(I)(IPOS+1:IPOS+1).NE.' '.AND.
     +                 LINE(I)(IPOS:IPOS).NE.' ')THEN
                   LSTCMD(JCD,1)=IPOS+ITOT
               ELSEIF (LINE(I)(IPOS+1:IPOS+1).NE.' ')THEN
                   LSTCMD(JCD,1)=IPOS+ITOT
               ENDIF
            ELSEIF(IACT.EQ.18) THEN
               PARCMD(JCD,1)=OLDFNT
            ENDIF
*
         ENDIF
         NC=NC+1
         IF(NC.LE.ITCMD.AND.JCD.EQ.JCDOLD) GO TO 20
         IF (JCD.EQ.JCDOLD) IP=IP+1
         GO TO 10
      ENDIF
*
*     reposition of command at end of line
*
      IF (JV.LT.JCD) THEN
         JV1=JCD
         LLINE=LENOCC(LINE(I))
   50    IF (LSTCMD(JV1,1).GT.ITOT+LLINE) THEN
            LSTCMD(JV1,1)=ITOT+LLINE+1
            JV1=JV1-1
            IF (JV1.GT.JV) GO TO 50
         ENDIF
      ENDIF
      END
