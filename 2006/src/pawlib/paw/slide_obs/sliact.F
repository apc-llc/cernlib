*
* $Id: sliact.F,v 1.1.1.1 1996/03/01 11:39:29 mclareni Exp $
*
* $Log: sliact.F,v $
* Revision 1.1.1.1  1996/03/01 11:39:29  mclareni
* Paw
*
*
*CMZ :  1.10/00 30/10/90  11.48.27  by  O.Couet
*-- Author : A.Risso
      SUBROUTINE SLIACT(LOW,JUP,I)
      COMMON /SLIDE1/ XB1,XB2,YB1,YB2,HC
      PARAMETER (MGUIDL=199)
      COMMON /KCGUID/ GUID(MGUIDL)
      CHARACTER*80 GUID
      CHARACTER*80 LINECP(99)
      CHARACTER*80 LINE(99)
      EQUIVALENCE (LINE(1),GUID(1)),(LINECP(1),GUID(100))
      PARAMETER (MAXCMD=1000)
      COMMON /SLIDE7/  LSTCMD(MAXCMD,2),PARCMD(MAXCMD,2),JCD
      COMMON /SLIDE9/ BLSPAC
      COMMON /SLIDE16/ HL
      COMMON /SLIDE17/ CHMAX,CHMIN,CHMN,PRIMO1,
     +                 IC,RLNSPC,ITMAR,IBMAR
      LOGICAL PRIMO1
      COMMON /SLIDE27/ FONT,IFONT,OLDFNT,OLDPRE,PREC,OLDCOL,ICOL
      COMMON /SLIDE28/ FACTL
      COMMON /SLIDE34/ FACT(13)
      COMMON /SLIDE29/ PASS,CSHI
      LOGICAL NOCMD,FIRST,FIRSTC
      COMMON /SLIDE26/ADISP
      INTEGER OPLS
      COMMON /SLIDE30/ TSPACE(4,1)
      COMMON /SLIDE32/ HLIST(20),OPLS
      COMMON /SLIDE35/ VALPRO,VALNPR
*.______________________________________
*
      XB1OLD=XB1
*
*     define left margin
*
      XB1=XB1+TSPACE(3,1)
*
      OLDFNT=FONT
      NOCMD =.TRUE.
      FIRST =.TRUE.
      LL=LENOCC(LINE(I))
      L=1
      KST=1
      IF (JCD.GE.1) THEN
   10    IF (LSTCMD(KST,1).GE.LOW.AND.LSTCMD(KST,1).LE.JUP) THEN
            K=KST
   20       IF (LSTCMD(K,1).GE.LOW.AND.LSTCMD(K,1).LE.JUP) THEN
               JACT=LSTCMD(K,2)
               IF (JACT.EQ.-1.OR.JACT.EQ.-2)THEN
                  OPLS=OPLS+1
                  IF (ADISP.EQ.0.) THEN
                     IF (FONT.LE.-8.AND.FONT.GE.-11) THEN
*                       initial displacement for non_proport.fonts
                        ADISP=ADISP+VALNPR
                     ELSE
*                       initial displacement for proportional fonts
                        ADISP=ADISP+VALPRO
                     ENDIF
                     HLIST(OPLS)=ADISP
                  ELSE
                     ADISP=HLIST(OPLS)
                     HLIST(OPLS+1)=0.
                  ENDIF
*
               ELSEIF (JACT.EQ.0.OR.JACT.EQ.-3) THEN
                  OPLS=OPLS-1
                  IF (FONT.LE.-8.AND.FONT.GE.-11) THEN
                     IF (ABS(ADISP-VALNPR).LE.(0.00001)) THEN
                        ADISP=0.
                        HLIST(1)=0.
                     ELSE
                        ADISP=HLIST(OPLS)
                        HLIST(OPLS+1)=0.
                     ENDIF
                  ELSE
                     IF (ABS(ADISP-VALPRO).LE.(0.00001)) THEN
                        ADISP=0.
                        HLIST(1)=0.
                     ELSE
                        ADISP=HLIST(OPLS)
                        HLIST(OPLS+1)=0.
                     ENDIF
                  ENDIF
               ELSEIF(JACT.EQ.-5) THEN
                  PFONT=PARCMD(K,1)
                  FONT=PFONT
                  PPREC=PARCMD(K,2)
                  PREC=PPREC
                  IF (PFONT.EQ.0.) THEN
                      CALL IGSET('TXFP',2.)
                  ELSE
                      CALL IGSET('TXFP',-(ABS(PFONT*10.)+PPREC))
                  ENDIF
                  IF(PFONT.LE.-8.AND.PFONT.GE.-11) THEN
                     BLSPAC=FACT(-INT(PFONT))*HC
*                 Default value for initial left space for list
                     VALNPR=3*BLSPAC
                  ELSE
*                 blank space : formula taked in IGTEXT_routine
                     SCALE=HC/21
                     BLSPAC=SCALE*7*FACTL
*                 Default value for initial left space for list
                     VALPRO=6*BLSPAC
                  ENDIF
                ENDIF
*
               K=K+1
               IF (K.LE.JCD) GO TO 20
            ENDIF
            NOCMD=.FALSE.
            M= LSTCMD(KST,1)-LOW+1
            HLONG=0
            IF (M.GT.1) THEN
*
*             write the first part of line (without command)
*
               BLANK=0.
               IF (FONT.LE.-8.AND.FONT.GE.-11) THEN
                  HLONG= FACT(-INT(FONT))*(M-1)*HC
               ELSEIF(LENOCC(LINE(I)(1:M-1)).EQ.0) THEN
                  HLONG=BLSPAC*(M-1)
               ELSE
                  MCP=1
  15              IF(LINE(I)(MCP:MCP).EQ.' '.AND.MCP.LE.M-1) THEN
                      MCP=MCP+1
                      GO TO 15
                  ENDIF
                 CALL IGTEXT(1.,1.,LINE(I)(1:M-1),HC,HLONG,'S')
                 IF (MCP.NE.1)THEN
                   BLANK=(MCP-1)*BLSPAC
                   HLONG=HLONG+BLANK/FACTL
                 ENDIF
               ENDIF
               NLONG=LENOCC(LINE(I)(1:M-1))
*              add spaces after part of writing line
               IF(NLONG.NE.M-1.AND..NOT.
     +           (FONT.LE.-8.AND.FONT.GE.-11)) THEN
                   HLONG=HLONG+(M-1-NLONG)*BLSPAC/FACTL
               ENDIF
               HLONG=ADISP+HLONG*FACTL
               CALL IGSET('TXAL',0.)
               CALL IGSET('TANG',0.)
               CALL IGSET('CHHE',HC)
               CALL ITX(XB1+ADISP+BLANK,YB2-(ITMAR+I)*HL,LINE(I)(1:M-1))
            ELSE
               M=1
            ENDIF
            FIRSTC=.TRUE.
            DO 40 K=KST,JCD
   30          IF (LSTCMD(K,1).GE.LOW.AND.LSTCMD(K,1).LE.JUP) THEN
                  JACT=LSTCMD(K,2)
*   no operation
                  IF (JACT.EQ.-4) THEN

                  ELSEIF (JACT.EQ.1) THEN
*   colors
                     ICOLOR=PARCMD(K,1)
                     CALL ISTXCI(ICOLOR)
                  ELSEIF (JACT.EQ.2) THEN
*   </FONT>
                  PFONT=PARCMD(K,1)
                  PPREC=PARCMD(K,2)
                  FONT=PFONT
                  PREC=PPREC
                  IF (PFONT.EQ.0.) THEN
                      CALL IGSET('TXFP',2.)
                  ELSE
                      CALL IGSET('TXFP',-(ABS(PFONT*10.)+PPREC))
                  ENDIF
                  IF(PFONT.LE.-8.AND.PFONT.GE.-11) THEN
                     BLSPAC=FACT(-INT(PFONT))*HC
*                 Default value for initial left space for list
                     VALNPR=3*BLSPAC
                  ELSE
*                 blank space : formula taked in IGTEXT_routine
                     SCALE=HC/21
                     BLSPAC=SCALE*7*FACTL
*                 Default value for initial left space for list
                     VALPRO=6*BLSPAC
                  ENDIF
                  ELSEIF (JACT.EQ.9) THEN
*   start bold
                     IF (PARCMD(K,1).NE.0) PASS=PARCMD(K,1)
                     IF (PARCMD(K,2).NE.0) CSHI=PARCMD(K,2)
                     CALL IGSET('PASS',PASS)
                     CALL IGSET('CSHI',CSHI)
                  ELSEIF (JACT.EQ.10) THEN
*   end   bold
                     CALL IGSET('PASS',1.)
                  ELSEIF (JACT.EQ.11) THEN
*   space(s)
                     IF (LSTCMD(K,1).GE.LOW) THEN
                        HLONG=HLONG+BLSPAC
                     ENDIF
                  ELSEIF (JACT.EQ.12) THEN
*   displacement 1.list_char
                     IF (HLIST(OPLS+1).NE.0) THEN
                        HLONG=HLIST(OPLS+1)
                     ENDIF
                     FIRST=.FALSE.

                  ELSEIF (JACT.EQ.13) THEN
*   space1
                     IF(FONT.LE.-8.AND.FONT.GE.-11)THEN
                        HLONG=HLONG+BLSPAC
                     ELSE
                        HLONG=HLONG+2*BLSPAC
                     ENDIF
                     HLIST(OPLS+1)=HLONG
                     IF (FONT.EQ.0.) THEN
                        CALL IGSET('TXFP',2.)
                     ELSE
                        CALL IGSET('TXFP',-(ABS(FONT*10.)+PREC))
                     ENDIF
                  ELSEIF (JACT.EQ.14) THEN
*   change font for bullett
                     CALL IGSET('TXFP',141.)
                  ELSEIF (JACT.EQ.18) THEN
*   </COLOR>
                     CALL ISTXCI(INT(PARCMD(K,1)))
                  ENDIF
*
                  IF (M.EQ.1.AND.FIRST) THEN
                     HLONG=ADISP
                     IF (JACT.EQ.11) HLONG=HLONG+BLSPAC
                     FIRST=.FALSE.
                  ENDIF
                  BLANK=0.
               IF (LENOCC(LINE(I)(M:LL)).GT.0.AND.K+1.LE.JCD) THEN
*               there is something to write and un other command
                     KPOS=LSTCMD(K+1,1)
                     IF (KPOS.GE.LSTCMD(K,1)+1.AND.KPOS.LE.JUP) THEN
*                   next command is interior this line
                        MIDDLE=KPOS-LOW
                        BLANK=0.
                        IF (MIDDLE.GE.M) THEN
                           HLON2=0
                           LLN=MIDDLE-M+1
                           IF (FONT.LE.-8.AND.FONT.GE.-11) THEN
                              HLON2=FACT(-INT(FONT))*LLN*HC
                           ELSEIF (LENOCC(LINE(I)(M:MIDDLE)).EQ.0) THEN
                              HLON2=LLN*BLSPAC
                           ELSE
                              MCP=M
  25                          IF(LINE(I)(MCP:MCP).EQ.' '
     +                                  .AND.MCP.LE.MIDDLE) THEN

                                 MCP=MCP+1
                                 GO TO 25
                              ENDIF
                      CALL IGTEXT(1.,1.,LINE(I)(M:MIDDLE),HC,HLON2,'S')
                              IF (MCP.NE.M)THEN
                                  BLANK=(MCP-M)*BLSPAC
                                  HLON2=HLON2+BLANK/FACTL
                              ENDIF
                           ENDIF
                           CALL IGSET('TXAL',0.)
                           CALL IGSET('TANG',0.)
                           CALL IGSET('CHHE',HC)
                           CALL ITX(XB1+HLONG+BLANK,YB2-(ITMAR+I)*HL,
     +                              LINE(I)(M:MIDDLE))
                           NLONG=LENOCC(LINE(I)(M:MIDDLE))
                           IF(NLONG.NE.LLN.AND..NOT.
     +                       (FONT.LE.-8.AND.FONT.GE.-11)) THEN
                              HLON2=HLON2+(LLN-NLONG)*BLSPAC/FACTL
                           ENDIF
                           M=MIDDLE+1
                           HLONG=HLONG+HLON2*FACTL
                        ENDIF
                     ELSEIF (KPOS.GT.JUP.AND.FIRSTC) THEN
*                   next command is in next line
                        IF (LSTCMD(K+1,2).EQ.13) THEN
                           IF(FONT.LE.-8.AND.FONT.GE.-11)THEN
                              HLIST(OPLS+1)=HLONG+BLSPAC
                           ELSE
                              HLIST(OPLS+1)=HLONG+2*BLSPAC
                           ENDIF
                        ENDIF
                        CALL IGSET('TXAL',0.)
                        CALL IGSET('TANG',0.)
                        CALL IGSET('CHHE',HC)
                        MCP=M
  35                    IF(LINE(I)(MCP:MCP).EQ.' '
     +                           .AND.MCP.LE.LL) THEN
                           MCP=MCP+1
                           GO TO 35
                        ENDIF
                        BLANK=0.
                        IF (MCP.NE.M.AND..NOT.
     +                      (FONT.LE.-8.AND.FONT.GE.-11)) THEN
                            BLANK=(MCP-M)*BLSPAC
                        ENDIF
                        CALL ITX(XB1+HLONG+BLANK,YB2-(ITMAR+I)*HL,
     +                           LINE(I)(M:LL))
                        FIRSTC=.FALSE.
                     ENDIF
                  ELSE
*                   finish of list command
                    CALL IGSET('TXAL',0.)
                    CALL IGSET('TANG',0.)
                    CALL IGSET('CHHE',HC)
                    MCP=1
  37                IF(LINE(I)(MCP:MCP).EQ.' '
     +                       .AND.MCP.LE.LL) THEN
                       MCP=MCP+1
                       GO TO 37
                    ENDIF
                    BLANK=0.
                    IF (MCP.NE.1)BLANK=(MCP-1)*BLSPAC
                    CALL ITX(XB1+HLONG+BLANK,YB2-(ITMAR+I)*HL,
     +                       LINE(I)(M:LL))
                  ENDIF
               ENDIF
   40       CONTINUE
         ENDIF
   50    KST=KST+1
         IF (KST.LE.JCD.AND.LSTCMD(KST,1).LE.JUP.AND.NOCMD) THEN
*          find next possible operational command
            GO TO 10
         ENDIF
      ENDIF
*
*     if there are'nt commands print all the line
*
      IF (NOCMD) THEN
         BLANK=0.
         CALL IGSET('TXAL',0.)
         CALL IGSET('TANG',0.)
         CALL IGSET('CHHE',HC)
         MCP=1
  55     IF(LINE(I)(MCP:MCP).EQ.' '.AND.MCP.LE.LL) THEN
            MCP=MCP+1
            GO TO 55
         ENDIF
         IF (MCP.NE.1) BLANK=(MCP-1)*BLSPAC
         CALL ITX(XB1+ADISP+BLANK,YB2-(ITMAR+I)*HL,LINE(I)(1:LL))
      ENDIF
      XB1=XB1OLD
      END
