*
* $Id: slidem.F,v 1.1.1.1 1996/03/01 11:39:29 mclareni Exp $
*
* $Log: slidem.F,v $
* Revision 1.1.1.1  1996/03/01 11:39:29  mclareni
* Paw
*
*
*CMZ :  2.04/00 25/10/93  13.03.34  by  Rene Brun
*-- Author :
*
*     DESCRIPTION OF MAIN ROUTINE
*
*     Recognise initialisation commands: <BOX..>,<TSIZE..>
*                                        <FONT..>,<MARGIN..>
*
*     After the identification of command <DRAW> start the text elaboration
*
*     Logic used for each line:
*
*          1.  Identification of key words that can modify line:
*                      <P>,<UL>,<OL>,</UL>,</OL>,<LI>
*          2.  Join current line with previous one (if possible)
*
*          3.  Extraction of commands
*
*          4.  Cut current line
*
*          5.  Go to next line
*
*-- Author : A.Risso
      SUBROUTINE SLIDEM(STR)
      CHARACTER*(*) STR
      COMMON /SLIDE0/ NBLINE
      COMMON /SLIDE1/ XB1,XB2,YB1,YB2,HC
      PARAMETER (ILINE=99)
      PARAMETER (MGUIDL=199)
      COMMON /KCGUID/ GUID(MGUIDL)
      CHARACTER*80 GUID
      CHARACTER*80 LINECP(99)
      CHARACTER*80 LINE(99)
      EQUIVALENCE (LINE(1),GUID(1)),(LINECP(1),GUID(100))
      COMMON /SLIDE5/CHBUF
      CHARACTER*1 CHBUF
      COMMON /SLIDE6/ SPACE
      PARAMETER (MAXCMD=1000)
      COMMON /SLIDE7/  LSTCMD(MAXCMD,2),PARCMD(MAXCMD,2),JCD
      COMMON /SLIDE8/ ITOT
      COMMON /SLIDE9/ BLSPAC
      COMMON /SLIDE11/ SYMLON
      COMMON /SLIDE12/ LNBUF1,LNBUF2
      CHARACTER*80 LNBUF1,LNBUF2
      COMMON /SLIDE20/ NEWLIN,NWLN
      CHARACTER*80 NEWLIN,NWLN
      COMMON /QUEST/IQUEST(100)
      PARAMETER (MAXTOK=10)
      INTEGER   TOK(2,MAXTOK)
      CHARACTER*20 CMD,CMD1,ORDSTP,ORCOPY,CHNUM
      LOGICAL FLAGP,EXIT,IMPOSE
      LOGICAL INSCMD,JUMPUP
      LOGICAL NOINS,JOIN
      COMMON /SLIDE13/REMCUT,LICOMM
      LOGICAL REMCUT,LICOMM
      COMMON /SLIDE15/ NWNB
      COMMON /SLIDE17/ CHMAX,CHMIN,CHMN,PRIMO1,
     +                 IC,RLNSPC,ITMAR,IBMAR
      LOGICAL PRIMO1
      COMMON /SLIDE18/ ERR
      COMMON /SLIDE19/ TDISP
      COMMON /SLIDE25/ LSTOPN
      COMMON /SLIDE27/ FONT,IFONT,OLDFNT,OLDPRE,PREC,OLDCOL,ICOL
      COMMON /SLIDE28/ FACTL
      COMMON /SLIDE30/ TSPACE(4,1)
      LOGICAL LI
      PARAMETER (MXCD=10)
      DIMENSION ICMD(MXCD),INDE(MXCD)
      COMMON /SLIDE31/STRPNT,SPNT
      CHARACTER*3 STRPNT,SPNT
      COMMON /SLIDE34/ FACT(13)
      COMMON /SLIDE35/ VALPRO,VALNPR
      COMMON /SLIDE36/ SPSY,ADDLET,ORDER
      LOGICAL ADDLET,ORDER
      COMMON /SLIDE40/ MODIF,XMP1,XMP2,CUTLN
      LOGICAL MODIF,XMP1,XMP2
      DIMENSION DISPL(10),ILEVEL(10)
      SAVE LLD2
*.______________________________________
*
*     line space
*
      SPACE=(XB2-XB1)-(HC*2)*(XB2-XB1)/100
      IF(STR(1:6).EQ.'<DRAW>')THEN

*
*        TSPACE(1,1) contains the number of time that <SPC> appear
*        TSPACE(2,1) contains the line in which <SPC> appear
*
         TSPACE(1,1)=0.
         TSPACE(2,1)=0.
*
         NWNBLN= NBLINE
         MODIF =.TRUE.
         XMP1=.FALSE.
         XMP2=.FALSE.
         CMD1  =' '
         ORDSTP=' '
         REMCUT=.FALSE.
         LICOMM=.FALSE.
         INSCMD=.FALSE.
         ADDLET=.FALSE.
         ORDER =.FALSE.
         ICOUNT=0
         IORDLS=0
         IAVOID=0
         CUTLN =0
         ITOT  =0
         DISP  =0.
         IN    =0
         I     =1
*
*        start_symbol of list lenght
*
  10     IF (FONT.LE.-8.AND.FONT.GE.-11) THEN
            IF(FONT.EQ.-8) THEN
               SPSY=1.5
            ELSEIF (FONT.EQ.-9)THEN
               SPSY=1.4
            ELSEIF (FONT.EQ.-10)THEN
               SPSY=1.5
            ELSEIF(FONT.EQ.-11)THEN
               SPSY=1.5
            ENDIF
            SPC1=FACT(-INT(FONT))*(1+SPSY)*HC
         ELSEIF (FONT.EQ.0.) THEN
            SYMLON=0
            CALL IGTEXT(1.,1.,STRPNT,HC,SYMLON,'S')
            SPC1=SYMLON+2*BLSPAC
         ELSE
            SPSY=0
            IF(FONT.EQ.-1) THEN
               SPSY=4
            ELSEIF (FONT.EQ.-2)THEN
               SPSY=4.8
            ELSEIF (FONT.EQ.-3)THEN
               SPSY=4.8
            ELSEIF(FONT.EQ.-4)THEN
               SPSY=4.2
            ELSEIF (FONT.EQ.-5)THEN
               SPSY=4.2
            ELSEIF (FONT.EQ.-6)THEN
               SPSY=4.6
            ELSEIF(FONT.EQ.-7)THEN
               SPSY=4.6
            ELSEIF (FONT.EQ.-12)THEN
               SPSY=4.3
            ELSEIF (FONT.EQ.-13)THEN
               SPSY=4.4
            ELSE
               SYMLON=0
               CALL IGTEXT(1.,1.,STRPNT,HC,SYMLON,'S')
            ENDIF
            IF(SPSY.EQ.0) THEN
               SPC1=SYMLON+2*BLSPAC
            ELSE
               SPC1=(2+SPSY)*BLSPAC
            ENDIF
         ENDIF
         SYMLON=SPC1
*
         JLONG=80
         NOINS  =.FALSE.
         FLAGP  =.FALSE.
         IMPOSE =.FALSE.
   20    NN=1
*
*        change font
*
         IPOS=INDEX(LINE(I),'<FONT')
         IF (IPOS.NE.0) THEN
            OLDPRE=PREC
            OLDFNT=FONT
            IPOS1 = INDEX(LINE(I)(IPOS:),'>')
            LNBUF1=LINE(I)(IPOS:IPOS+IPOS1-2)
            CALL KILEXP(LNBUF1,' ',' ',' ',MAXTOK,TOK,NTOK,' ')
            IF (NTOK.GT.3) THEN
            CALL IGERR('Number of font parameter not correct', 'APPLICA'
     +      //'TION SLIDE')
              ERR=7
              RETURN
            ENDIF
            IF (NTOK.GT.1) THEN
            IF (NTOK.GE.2)THEN
               CALL KICTON(LNBUF1(TOK(1,2):TOK(2,2)),JFONT,FONT)
               IF (IQUEST(1).NE.0) GO TO 18
            ENDIF
            IF (NTOK.EQ.3)THEN
               CALL KICTON(LNBUF1(TOK(1,3):TOK(2,3)),IPREC,PREC)
            ENDIF
  18       IF (IQUEST(1).NE.0.OR.IPOS1.LT.IPOS+TOK(2,NTOK)) THEN
             CALL IGERR('Font parameter(s) not correct', 'APPLICATION '
     +      //'SLIDE')
               ERR=7
               RETURN
            ENDIF
            ENDIF
            IF (FONT.EQ.0) THEN
                FACTL=1
            ELSEIF((FONT.LE.-1.AND.FONT.GE.-7).OR.
     +              FONT.EQ.-12.OR.FONT.EQ.-13) THEN
                FONT=JFONT
                FACTL=FACT(ABS(JFONT))
            ELSEIF(JFONT.LE.-8.AND.JFONT.GE.-11) THEN
                FONT=JFONT
                FACTL=1
            ELSE
                FACTL=1
            ENDIF
            CALL DELINE(I,NWNBLN)
            JCD=JCD+1
            IF (JCD.GT.MAXCMD) THEN
               ERR=2
               RETURN
            ENDIF
            LSTCMD(JCD,1)=ITOT+1
            LSTCMD(JCD,2)=-5
            PARCMD(JCD,1)=FONT
            PARCMD(JCD,2)=PREC
            IF(FONT.LE.-8.AND.FONT.GE.-11) THEN
               BLSPAC=FACT(-INT(FONT))*HC
*            Default value for initial left space for list
               VALNPR=3*BLSPAC
            ELSE
*            blank space : formula taked in IGTEXT_routine
               SCALE=HC/21
               BLSPAC=SCALE*7*FACTL
*            Default value for initial left space for list
               VALPRO=6*BLSPAC
            ENDIF
            GO TO 10
         ENDIF
*
*        find  paragraph command
*
*        <P>    end of paragraph
*        <UL>   start of unordered list
*        <LI>   element of list with star like start symbol
*        </UL>  end of unordered list
*        <OL>   start of ordered list
*        </OL>  end of ordered list
*
*
*         NOINS  =.FALSE.
*         FLAGP  =.FALSE.
*         IMPOSE =.FALSE.
*   20    NN=1
         LI=.FALSE.
*0
         CMD='<P>'
         K    = LENOCC(CMD)
         IPOS1 = INDEX(LINE(I),CMD(1:K))
         ICMD(NN)=IPOS1
         NN=NN+1
*1
         CMD='<UL>'
         K    = LENOCC(CMD)
         IPOS1 = INDEX(LINE(I),CMD(1:K))
         ICMD(NN)=IPOS1
         NN=NN+1
*2
         CMD='</UL>'
         K    = LENOCC(CMD)
         IPOS1 = INDEX(LINE(I),CMD(1:K))
         ICMD(NN)=IPOS1
         NN=NN+1
*3
         CMD='<LI>'
         K    = LENOCC(CMD)
         IPOS1 = INDEX(LINE(I),CMD(1:K))
         ICMD(NN)=IPOS1
         NN=NN+1
*5
         CMD='<OL>'
         K    = LENOCC(CMD)
         IPOS1 = INDEX(LINE(I),CMD(1:K))
         ICMD(NN)=IPOS1
         NN=NN+1
*6
         CMD='</OL>'
         K    = LENOCC(CMD)
         IPOS1 = INDEX(LINE(I),CMD(1:K))
         ICMD(NN)=IPOS1
*
         CALL SORTZV(ICMD,INDE,NN,-1,0,0)
*
*        find the first avalaible command ( ICMD(index).NE.0 )
*
         KK=1
   30    IF (KK.LE.NN) THEN
             IF (ICMD(INDE(KK)).EQ.0) THEN
                 KK=KK+1
                 GO TO 30
             ENDIF
         ENDIF
         IF (KK.LE.NN) THEN
            IF (ICMD(INDE(KK)).NE.0) THEN
*
*            execution of modify_text_command
*
              GOTO (40,50,60,70,90,100),INDE(KK)
            ELSE
              GO TO 140
            ENDIF
         ELSE
            GO TO 140
         ENDIF
*
*        end of paragraph
*
   40    CMD = '<P>'
         CMD1= '<P>'
         GO TO 110
*
*        start of unordered list
*
   50    CMD = '<UL>'
         CMD1= '<UL>'
         IN=0
         GO TO 110
*
*        end of unordered list
*
   60    CMD = '</UL>'
         CMD1= '</UL>'
         IN=0
         GO TO 110
*
*        element of list
*        initial symbol: star
*
   70    CMD ='<LI>'
         CMD1='<LI>'
         LICOMM=.TRUE.
         LI    =.TRUE.
         IN=1
         GO TO 110

*
*        start of ordered list
*
   90    CMD = '<OL>'
         CMD1= '<OL>'
         IN=0
         GO TO 110
*
*        end of ordered list
*
  100    CMD = '</OL>'
         CMD1= '</OL>'
         IN=0
         GO TO 110
*
  110    CONTINUE
         JUMPUP=.FALSE.
         IPOS=0
         CALL TKECMD(I,CMD,IPOS)
         JJ=MIN(80,LENOCC(LINE(I)))

         IF  ((CMD1.EQ.'<OL>'.OR.CMD1.EQ.'<UL>'.OR.
     +         CMD1.EQ.'</OL>'.OR.CMD1.EQ.'</UL>')
     +        .AND.JJ.EQ.0)  THEN
            CALL DELINE(I,NWNBLN)
            INSCMD=.TRUE.
            IF (CMD1.EQ.'</OL>')THEN
               CUTLN=1
               IMPOSE=.TRUE.
               REMCUT=.FALSE.
               LICOMM=.FALSE.
            ENDIF
            IF (CMD1.EQ.'</UL>') THEN
               CUTLN=1
               IMPOSE=.TRUE.
               REMCUT=.FALSE.
               LICOMM=.FALSE.
            ENDIF
            JUMPUP=.TRUE.
            GO TO 130
         ENDIF
         IF (CMD1.EQ.'<LI>'.AND.JJ.EQ.0) THEN
            CALL DELINE(I,NWNBLN)
            JJ=MIN(80,LENOCC(LINE(I)))
            IPOS=1
         ENDIF
         IF (IPOS.GT.JJ) THEN
*
*           command at the end of line
*
            IF ((IN.GT.0.OR.CMD1.EQ.'<UL>'.
     +           OR.CMD1.EQ.'</UL>'.OR.CMD1.EQ.'<OL>'.
     +           OR.CMD1.EQ.'</OL>').AND.I.LT.ILINE) THEN
*               put the start symbol of list in next line
               IF (I+1.GT.ILINE) THEN
                  ERR=1
                  RETURN
               ENDIF
               MM=LENOCC(CMD1)
               IF (I+1.GT.NWNBLN) THEN
                  NWNBLN=I+1
                  IF (NWNBLN.GT.ILINE) THEN
                      ERR=1
                      RETURN
                   ENDIF
                  LINE(I+1)=CMD1(1:MM)
               ELSE
                  LNBUF1=LINE(I+1)
                  LINE(I+1)=CMD1(1:MM)//LNBUF1
               ENDIF
            ENDIF
*
*           ipos=0 permit eventually link with remainder of line previous
*           flagp restore the correct situation for next line_elaboration
*           flagp=.true. means that next line will not link with this-one
*
            IPOS=0
            FLAGP=.TRUE.
            IF (CMD1.EQ.'<UL>'.OR.CMD1.EQ.'</UL>'.OR.
     +          CMD1.EQ.'<OL>'.OR.CMD1.EQ.'</OL>') THEN
               IMPOSE=.TRUE.
               INSCMD=.FALSE.
            ELSE
               IMPOSE=.FALSE.
            ENDIF
         ELSEIF (IPOS.GT.1.AND.JJ.GT.0) THEN
*           command is inside line
            LNBUF1=LINE(I)(IPOS:JJ)
            LNBUF2=LINE(I)(1:IPOS-1)
            IF (IN.GT.0.OR.CMD1.EQ.'<UL>'.
     +          OR.CMD1.EQ.'</UL>'.
     +          OR.CMD1.EQ.'<OL>'.
     +          OR.CMD1.EQ.'</OL>') THEN
*               put the start symbol of actual list
               CALL SUBSPC(I,IPOS)
               MM=LENOCC(CMD1)
               LNBUF1=CMD1(1:MM)//LINE(I)(IPOS:JJ)
            ENDIF
            LG1=LENOCC(LNBUF1)
            LG2=LENOCC(LNBUF2)
            IF (LG2.GT.0.AND.LG1.GT.0) THEN
               LINE(I)=LNBUF2
*               put newline in the text
               NWNBLN=NWNBLN+1
               IF (NWNBLN.GT.ILINE) THEN
                  ERR=1
                  RETURN
               ENDIF
               LNBUF2 =LINE(I+1)
               LINE(I+1)=LNBUF1
               DO 120 K=I+2,NWNBLN
                  LNBUF1 =LINE(K)
                  LINE(K)=LNBUF2
                  LNBUF2 =LNBUF1
  120          CONTINUE
            ELSEIF(LG1.GT.0.AND.LG2.EQ.0) THEN
               LINE(I)=LNBUF1
               GO TO 10
            ENDIF
            IPOS  = 0
            FLAGP =.TRUE.
            IF (CMD1.EQ.'<UL>'.OR.CMD1.EQ.'</UL>'.OR.
     +          CMD1.EQ.'<OL>'.OR.CMD1.EQ.'</OL>') THEN
               INSCMD=.FALSE.
               IMPOSE=.TRUE.
            ELSE
               IMPOSE=.FALSE.
            ENDIF
         ELSEIF (IPOS.EQ.1.AND.JJ.GT.0) THEN
*
*           command  in first position
*
            IF (IN.GT.0) THEN
*
*               put the start symbol of actual list
*
               IF (LSTOPN.EQ.0) THEN
                  ERR=8
                  CALL IGERR('Element of the list without open list'
     +            ,'APPLICATION SLIDE')
                  RETURN
               ENDIF
               CALL SUBSPC(I,IPOS)
               LNBUF1=LINE(I)
              IF (ORDER) THEN
                 ICOUNT=ICOUNT+1
                 CHNUM='I'
                 CALL KINTOC (ICOUNT,R,CHNUM)
                 ORCOPY=ORDSTP
                 LLD=LENOCC(ORDSTP)
                 IF (IORDLS.EQ.1) THEN
                  ORDSTP=CHNUM
                 ELSE
                  ORDSTP=ORCOPY(1:LLD)//'.'//CHNUM
                 ENDIF
                 ORCOPY=ORDSTP
                 LLD1=LENOCC(ORDSTP)
                 LINE(I)=ORDSTP(1:LLD1)//'<SPC1>'//LNBUF1
                 ORDSTP=ORCOPY(1:LLD)
*
*                start_symbol of ordered list lenght
*
                 IF (FONT.LE.-8.AND.FONT.GE.-11) THEN
                    SPSY=LLD1
                    SPC1=FACT(-INT(FONT))*(1+SPSY)*HC
                 ELSEIF((FONT.LE.-1.AND.FONT.GE.-7).OR. FONT.EQ.-
     +                   12.OR.FONT.EQ.-13) THEN
                    SYMLON=0
                    CALL IGTEXT(1.,1.,ORDSTP(1:LLD1),HC,SYMLON,'S')
                    SPC1=SYMLON*FACTL+2*BLSPAC
                 ELSE
                    SYMLON=0
                    CALL IGTEXT(1.,1.,ORDSTP(1:LLD1),HC,SYMLON,'S')
                    SPC1=SYMLON+2*BLSPAC
                 ENDIF
              ELSE
               IF (FONT.EQ.0.) THEN
                  LINE(I)=STRPNT(1:3)//'<SPC1>'//LNBUF1
               ELSEIF(FONT.LE.-8.AND.FONT.GE.-11) THEN
                  LINE(I)='<CF>'//SPNT(1:1)//'<SPC1>'//LNBUF1
                  ADDLET=.TRUE.
               ELSE
                  LINE(I)='<CF>'//SPNT//'<SPC1>'//LNBUF1
                  ADDLET=.TRUE.
               ENDIF
              ENDIF

               DISPL(LSTOPN)=SPC1
               IOLD=I
               REMCUT=.FALSE.
            ENDIF
*
*           in the special case when command is at the start of line I don't lin
*           line with previous one(CUTLN=1), but if no other modify_line command
*           (in actual line)I permit to link this line with next one.(see use of
*
            IPOS=0
            CUTLN=1
            IF (CMD1.EQ.'<OL>'.OR.CMD1.EQ.'<UL>') THEN
               INSCMD=.TRUE.
            ENDIF
            IF (CMD1.EQ.'</OL>'.OR.CMD1.EQ.'</UL>') THEN
               REMCUT=.FALSE.
               LICOMM=.FALSE.
               INSCMD=.TRUE.
            ENDIF
            IMPOSE=.TRUE.
            FLAGP =.TRUE.
            JUMPUP=.TRUE.
         ENDIF
  130    IF (INSCMD.AND.(CMD1.EQ.'<UL>'.OR.CMD1.EQ.'</UL>'.OR.
     +                   CMD1.EQ.'<OL>'.OR.CMD1.EQ.'</OL>')) THEN
            JCD=JCD+1
            IF (JCD.GT.MAXCMD) THEN
               ERR=2
               RETURN
            ENDIF
            IF (CMD1.EQ.'</UL>')  THEN
                IF(LSTOPN.EQ.0)THEN
                  CALL IGERR('TOO MANY CLOSE LIST',
     +                       'APPLICATION SLIDE')
                  RETURN
                ENDIF
                ILEVEL(LSTOPN)=0
                LSTOPN=LSTOPN-1
                IF (LSTOPN.GT.0)THEN
                 IF (ILEVEL(LSTOPN).EQ.2) THEN
                  ORDER=.TRUE.
                  LLD3=LENOCC(ORDSTP)
                  IF (IORDLS.EQ.1) THEN
                      CALL KICTON(ORDSTP(1:LLD3),IV,RV)
                  ELSEIF(LLD3.NE.0) THEN
                      CALL KICTON(ORDSTP(LLD2+2:LLD3),IV,RV)
                  ENDIF
                  ORCOPY=ORDSTP
                  IF (IORDLS.EQ.1) THEN
                      ORDSTP=' '
                  ELSE
                      ORDSTP=ORCOPY(1:LLD2)
                  ENDIF
                  ICOUNT=IV
                 ELSE
                  ORDER=.FALSE.
                 ENDIF
                ELSE
                 ORDER=.FALSE.
                ENDIF
            ENDIF
            IF (CMD1.EQ.'<UL>')  THEN
                LSTOPN=LSTOPN+1
                IF(LSTOPN.GT.10)THEN
                  CALL IGERR('TOO MANY CLOSE LIST',
     +                       'APPLICATION SLIDE')
                  ERR=7
                  RETURN
                ENDIF
                ILEVEL(LSTOPN)=1
                IF(ORDER)THEN
                   ORDER=.FALSE.
                ENDIF
            ENDIF
            IF (CMD1.EQ.'<OL>')  THEN
                LSTOPN=LSTOPN+1
                IF(LSTOPN.GT.10)THEN
                  CALL IGERR('TOO MANY CLOSE LIST',
     +                       'APPLICATION SLIDE')
                  ERR=7
                  RETURN
                ENDIF
                ILEVEL(LSTOPN)=2
                ORDER=.TRUE.
                IORDLS=IORDLS+1
                CHNUM='I'
                CALL KINTOC (ICOUNT,R,CHNUM)
                ICOUNT=0
                ORCOPY=ORDSTP
                LLD2=LENOCC(ORDSTP)
                IF (IORDLS.EQ.1) THEN
                  ORDSTP=' '
                ELSEIF(IORDLS.EQ.2) THEN
                  ORDSTP=CHNUM
                  ORCOPY=ORDSTP
                ELSE
                  ORDSTP=ORCOPY(1:LLD2)//'.'//CHNUM
                  ORCOPY=ORDSTP
                ENDIF
            ENDIF
            IF (CMD1.EQ.'</OL>')  THEN
                ILEVEL(LSTOPN)=0
                LSTOPN=LSTOPN-1
                IF(LSTOPN.GT.0)THEN
                 IF (ILEVEL(LSTOPN).EQ.2) THEN
                  ORDER=.TRUE.
                  IORDLS=IORDLS-1
                  IF (IORDLS.GT.0) THEN
                      LLD3=LENOCC(ORDSTP)
                      IF (IORDLS.EQ.1) THEN
                        CALL KICTON(ORDSTP(1:LLD3),IV,RV)
                      ELSEIF(LLD3.NE.0) THEN
                        CALL KICTON(ORDSTP(LLD2+2:LLD3),IV,RV)
                      ENDIF
                      ORCOPY=ORDSTP
                      IF (IORDLS.EQ.1) THEN
                         ORDSTP=' '
                         LLD2=1
                      ELSE
                         ORDSTP=ORCOPY(1:LLD2)
                      ENDIF
                      ICOUNT=IV
                  ELSE
                      ORDER=.FALSE.
                  ENDIF
                ELSE
                  IORDLS=IORDLS-1
                  ORDER=.FALSE.
                ENDIF
               ELSE
                 IORDLS=IORDLS-1
                 ORDER=.FALSE.
               ENDIF
            ENDIF
*
*            start of new indentation
*
            IF (CMD1.EQ.'<OL>'.OR.CMD1.EQ.'<UL>') THEN
               IF (DISP.LE.(0.00001)) THEN
                  IF(FONT.LE.-8.AND.FONT.GE.-11) THEN
                     DISP=DISP+VALNPR
                  ELSE
                     DISP=DISP+VALPRO
                  ENDIF
               ELSE
                  DISP=DISP+DISPL(LSTOPN-1)
               ENDIF
               LSTCMD(JCD,2)=-1
            ELSEIF (CMD1.EQ.'</OL>'.OR.CMD1.EQ.'</UL>') THEN
*
*               end of actual indentation
*
               IF(FONT.LE.-8.AND.FONT.GE.-11) THEN
                  IF (ABS(DISP-VALNPR).LE.(0.00001)) THEN
                     DISP=0
                  ELSE
                     DISP=DISP-DISPL(LSTOPN)
                  ENDIF
               ELSE
                  IF (ABS(DISP-VALPRO).LE.(0.00001)) THEN
                     DISP=0
                  ELSE
                     DISP=DISP-DISPL(LSTOPN)
                  ENDIF
               ENDIF
               LSTCMD(JCD,2)=0
            ENDIF
            LSTCMD(JCD,1)=ITOT+1
         ENDIF
         IF (JUMPUP) GO TO 20
*
*        link remainder  of previous line
*
  140    CONTINUE
         IF (IPOS.EQ.0.AND.MODIF) THEN
            IF (I.GT.1.AND.CUTLN.EQ.0) THEN
                IF (LINE(I).NE.' '.AND.LINE(I-1).NE.' ') THEN
                  EXIT=.FALSE.
                  JLONG = LENOCC(LINE(I-1))
                  LNBUF1 = LINE(I-1)(1:JLONG)
                  LNBUF2 = LINE(I)
                  LL=JLONG+1+LENOCC(LNBUF2)
                  LO=LL-(JLONG+1)
                  IF (LL.GT.80) THEN
*
*                   avoid the overflow of the buffers
*
                     NEWLIN=' '
                     NWLN =' '
                     J=80
                     CALL TKBLNK(I,J)
*
*                   take a word from the line
*
  150                IF (CHBUF.NE.' ') THEN
                        NEWLIN = CHBUF // NWLN
                        NWLN=NEWLIN
                        J = J - 1
                        IF (J.EQ.0)THEN
*
*                           exit=.true. avoid to join actual line
*                           to the previous one and delete it(LL=-1)
*
                           EXIT=.TRUE.
                        ELSEIF (J.GE.1) THEN
                           CHBUF = LINE(I)(J:J)
                           GO TO 150
                        ENDIF
                     ENDIF
                     IF (.NOT.EXIT) THEN
                        IF (J+JLONG+1.LE.80) THEN
                           LNBUF2=LINE(I)(1:J-1)
                           LO=J-1
                           LINE(I)=NEWLIN
                        ELSE
                           J=J-1
                           CHBUF=LINE(I)(J:J)
                           NEWLIN=' ' // NWLN
                           NWLN=NEWLIN
                           GO TO 150
                        ENDIF
                     ENDIF
                     LL=-1
                  ENDIF
                  IF (.NOT.EXIT) THEN
*
*                    join actual line with the previous one
*
                     LINE(I-1)=LNBUF1(1:JLONG)//' '//LNBUF2(1:LO)
                  ENDIF
                  IF (LL.NE.-1) THEN
*
*                    elimination of current line
*
                     M=I
  160                IF (M.LT.NWNBLN) THEN
                        LINE(M)=LINE(M+1)
                        M=M+1
                        GO TO 160
                     ENDIF
                     LINE(NWNBLN)=' '
                     NWNBLN=NWNBLN-1
                  ENDIF
                  IF (.NOT.EXIT) THEN
*
*                    re-elaboration of previous line
*
                     I=  I-1
                     ITOT=ITOT-JLONG
                  ENDIF


               ENDIF
            ENDIF
         ENDIF
         IF (IMPOSE) FLAGP=.FALSE.
         IF (.NOT.FLAGP) THEN
            CUTLN=0
         ELSE
            CUTLN=1
         ENDIF
         IF(XMP1) CUTLN=1
*
*        displacement for remainder (one or more lines
*        generated by cut_line)of elements of list
*
         IF (LICOMM.AND.REMCUT) THEN
            IF (IOLD.NE.I) THEN
               LNBUF1 =LINE(I)
               ILO=LENOCC(LNBUF1)
               IF (ILO.GT.0) THEN
                  LINE(I)='<SYM>'//LNBUF1
                  IOLD=I
               ENDIF
            ENDIF
            IAVOID=-1
         ENDIF
*
*        extraction of line internal command
*
         CALL XTRCMD(I)
         IF (ERR.EQ.2.OR.ERR.EQ.7) RETURN

         IF (LENOCC(LINE(I)).EQ.0.AND.I.LE.NWNBLN) THEN
*
*            this line have only command
*
            CALL DELINE(I,NWNBLN)
            IF(JCD.GT.0) THEN
               JJ=JCD
  170          IF (LSTCMD(JJ,1)-1.GT.ITOT) THEN
*
*                   all the command are applied at
*                   the first position of line
*
                  LSTCMD(JJ,1)=ITOT+1
                  JJ=JJ-1
                  IF (JJ.GE.1) GO TO 170
               ENDIF
            ENDIF
            IF(I.LE.NWNBLN) GO TO 10
         ENDIF
*
*        cut_line
*
         JOIN=.FALSE.
         HLONG=0.
         IF (TSPACE(2,1).NE.I)THEN
            TSPACE(1,1)=0
            TSPACE(2,1)=0
         ENDIF
         IF (IAVOID.EQ.-1) THEN
            IF(FONT.LE.-8.AND.FONT.GE.-11) THEN
               IF(ABS(DISP-VALNPR).GT.(0.00001)
     +                        .AND.REMCUT)THEN
                  TDISP=DISP+DISPL(LSTOPN)
               ELSEIF(ABS(DISP-TDISP).LE.(0.00001)
     +                        .AND.REMCUT)THEN
                  TDISP=TDISP+DISPL(LSTOPN)
               ENDIF
            ELSE
               IF(ABS(DISP-VALPRO).GT.(0.00001)
     +                        .AND.REMCUT)THEN
                  TDISP=DISP+DISPL(LSTOPN)
               ELSEIF(ABS(DISP-TDISP).LE.(0.00001)
     +                        .AND.REMCUT)THEN
                  TDISP=TDISP+DISPL(LSTOPN)
               ENDIF
            ENDIF
            IAVOID=0
         ELSE
            TDISP=DISP
         ENDIF
         TDISP=TDISP+TSPACE(1,1)*BLSPAC+TSPACE(3,1)+TSPACE(4,1)
*
*        next lines of code provide to calculate the longer of current line
*        and to add eventual space(s) for the elements of the list
*        conformed with the execution of command <SPC1>(that add
*        one or more spaces in output)
*
         IF (FONT.LE.-8.AND.FONT.GE.-11) THEN
            LLN=LENOCC(LINE(I))
            IF(ORDER.AND.LLN.GT.1.AND.IAVOID.EQ.0) LLN=LLN+1
            IF(ADDLET.AND.LLN.GT.1) LLN=LLN+SPSY
            HLONG=FACT(-INT(FONT))*LLN*HC
         ELSEIF (ADDLET) THEN
            CALL IGTEXT(1.,1.,LINE(I)(4:),HC,HLONG,'S')
            HLONG=HLONG+(SPSY+2)*BLSPAC/FACTL
         ELSEIF(ORDER.AND.FONT.NE.0..AND.IAVOID.EQ.0) THEN
            CALL IGTEXT(1.,1.,LINE(I)(INT(SPSY):),HC,HLONG,'S')
            HLONG=HLONG+(SPSY+2)*BLSPAC/FACTL
         ELSEIF(ORDER.AND.IAVOID.EQ.0) THEN
            CALL IGTEXT(1.,1.,LINE(I),HC,HLONG,'S')
            HLONG=HLONG+BLSPAC
         ELSE
            CALL IGTEXT(1.,1.,LINE(I),HC,HLONG,'S')
         ENDIF
         IF (HLONG*FACTL+TDISP.GT.SPACE.AND.MODIF) THEN
*
*            cut the line
*
            IF(LICOMM)REMCUT=.TRUE.
            CALL CUTLNE(I,NWNBLN,TDISP)
            IF (ERR.EQ.1) RETURN
            CUTLN=1
            IF (FLAGP) THEN
               IF (I+1.GT.ILINE) THEN
                  ERR=1
                  RETURN
               ENDIF
               MM=LENOCC(LINE(I+1))
               IF (.NOT.LI) THEN
*
*                    next line isn't remainder of list_element
*
                  LNBUF1=LINE(I+1)
                  JJ=LENOCC(CMD1)
                  LINE(I+1)=LNBUF1(1:MM)//CMD1(1:JJ)
               ENDIF
            ENDIF
         ENDIF
         TDISP=TDISP-TSPACE(1,1)*BLSPAC
         ADDLET=.FALSE.
         IF(XMP2)THEN
            XMP1=.FALSE.
            XMP2=.FALSE.
            CUTLN=0
            MODIF=.TRUE.
         ENDIF
*
*        computing of line lenght
*
         KK=LENOCC(LINE(I))
*
         ITOT=ITOT+KK
         I=I+1
         IF (I.LE.NWNBLN) GO TO 10
  180    NWNB=NWNBLN
         RETURN
      ENDIF
*
      IPOS=INDEX(STR,'<BOX')
      IF (IPOS.NE.0.AND.NBLINE.EQ.0)THEN
         LNBUF2=STR
         IPOS1 = INDEX(STR,'>')
         LNBUF1=STR(IPOS:IPOS1-1)
         STR=LNBUF1
         CALL KILEXP(STR,' ',' ',' ',MAXTOK,TOK,NTOK,' ')
         IF (NTOK.NE.5) THEN
            CALL IGERR('Number of BOX parameters not correct', 'APPLICA'
     +      //'TION SLIDE')
            ERR=7
            RETURN
         ENDIF
         CALL KICTON(STR(TOK(1,2):TOK(2,2)),IVAL,XB1)
         IF (IQUEST(1).NE.0) GO TO 190
         CALL KICTON(STR(TOK(1,3):TOK(2,3)),IVAL,XB2)
         IF (IQUEST(1).NE.0) GO TO 190
         CALL KICTON(STR(TOK(1,4):TOK(2,4)),IVAL,YB1)
         IF (IQUEST(1).NE.0) GO TO 190
         CALL KICTON(STR(TOK(1,5):TOK(2,5)),IVAL,YB2)
         IF (IQUEST(1).NE.0.OR.IPOS1.LT.IPOS+TOK(2,5)) GO TO 190
         IF (XB1.GE.XB2.OR.YB1.GE.YB2) GO TO 190
         IF (XB1.LT.0.OR.XB2.LT.0.OR.YB1.LT.0.OR.YB2.LT.0) GO TO 190
CCC         CALL IGBOX(XB1,XB2,YB1,YB2)
         RETURN
  190    ERR=7
         CALL IGERR('Box parameter not correct','APPLICATION SLIDE')
         RETURN
      ENDIF
*
      IPOS=INDEX(STR,'<FONT')
      IF (IPOS.NE.0.AND.NBLINE.EQ.0)THEN
         OLDPRE=PREC
         OLDFNT=FONT
         LNBUF2=STR
         IPOS1 = INDEX(STR,'>')
         LNBUF1=STR(IPOS:IPOS1-1)
         STR=LNBUF1
         CALL KILEXP(STR,' ',' ',' ',MAXTOK,TOK,NTOK,' ')
         IF (NTOK.GT.3) THEN
            CALL IGERR('Number of font parameter not correct', 'APPLICA'
     +      //'TION SLIDE')
            ERR=7
            RETURN
         ENDIF
         IF (NTOK.GT.1) THEN
         IF (NTOK.GE.2)THEN
            CALL KICTON(STR(TOK(1,2):TOK(2,2)),IFONT,FONT)
            IF (IQUEST(1).NE.0) GO TO 188
         ENDIF
         IF (NTOK.EQ.3)THEN
            CALL KICTON(STR(TOK(1,3):TOK(2,3)),IPREC,PREC)
         ENDIF
  188    IF (IQUEST(1).NE.0.OR.IPOS1.LT.IPOS+TOK(2,NTOK)) THEN
            CALL IGERR('Font parameter(s) not correct', 'APPLICATION '
     +      //'SLIDE')
            ERR=7
            RETURN
         ENDIF
         ENDIF
         IF (FONT.EQ.0) THEN
            CALL IGSET('TXFP',PREC)
         ELSEIF((FONT.LE.-1.AND.FONT.GE.-7).OR. FONT.EQ.-
     +   12.OR.FONT.EQ.-13) THEN
            FONT=IFONT
            CALL IGSET('TXFP',-(ABS(IFONT*10.)+PREC))
            IF (IC.EQ.1) FACTL=FACT(ABS(IFONT))
         ELSEIF(IFONT.LE.-8.AND.IFONT.GE.-11) THEN
            CALL IGSET('TXFP',-(ABS(IFONT*10.)+PREC))
            FONT=IFONT
         ELSE
            CALL IGSET('TXFP',-(ABS(IFONT*10.)+PREC))
         ENDIF
         STR=' '
      ENDIF
      IF (NBLINE.EQ.0) THEN
         IF(FONT.LE.-8.AND.FONT.GE.-11) THEN
            BLSPAC=FACT(-INT(FONT))*HC
*
*         Default value for initial left space for list
*
            VALNPR=3*BLSPAC
         ELSE
*
*         blank space : formula taked in IGTEXT_routine
*
            SCALE=HC/21
            BLSPAC=SCALE*7*FACTL
*
*         Default value for initial left space for list
*
            VALPRO=6*BLSPAC
         ENDIF
      ENDIF
*
      IPOS=INDEX(STR,'<MARGIN')
      IF (IPOS.NE.0.AND.NBLINE.EQ.0)THEN
         LNBUF2=STR
         IPOS1 = INDEX(STR,'>')
         LNBUF1=STR(IPOS:IPOS1-1)
         STR=LNBUF1
         CALL KILEXP(STR,' ',' ',' ',MAXTOK,TOK,NTOK,' ')
         IF (NTOK.GT.5) THEN
            CALL IGERR('Number of MARGIN parameters not correct', 'APPL'
     +      //'ICATION SLIDE')
            ERR=7
            RETURN
         ENDIF
         IF (NTOK.GT.1) THEN
         IF (NTOK.GE.2)THEN
            CALL KICTON(STR(TOK(1,2):TOK(2,2)),ILMAR,XLMAR)
            TSPACE(3,1)=XLMAR
            IF (IQUEST(1).NE.0) GO TO 197
         ENDIF
         IF (NTOK.GE.3)THEN
            CALL KICTON(STR(TOK(1,3):TOK(2,3)),IRMAR,RMAR)
            TSPACE(4,1)=RMAR
            IF (IQUEST(1).NE.0) GO TO 197
         ENDIF
         IF (NTOK.GE.4)THEN
            CALL KICTON(STR(TOK(1,4):TOK(2,4)),ITMAR,TMAR)
            IF (IQUEST(1).NE.0) GO TO 197
         ENDIF
         IF (NTOK.EQ.5)THEN
            CALL KICTON(STR(TOK(1,5):TOK(2,5)),IBMAR,BMAR)
         ENDIF
 197     IF (IQUEST(1).NE.0.OR.IPOS1.LT.IPOS+TOK(2,NTOK)
     +       .OR.XLMAR.LT.0.OR.RMAR.LT.0
     +       .OR.ITMAR.LT.0.OR.IBMAR.LT.0) THEN
            CALL IGERR('MARGIN parameters not correct', 'APPLICATI'
     +      //'ON SLIDE')
            ERR=7
            RETURN
         ENDIF
         ENDIF
         STR=' '
      ENDIF
      HC1=0
      HC2=0
      IPOS=INDEX(STR,'<TSIZE')
      IF (IPOS.NE.0.AND.NBLINE.EQ.0)THEN
         LNBUF2=STR
         IPOS1 = INDEX(STR,'>')
         LNBUF1=STR(IPOS:IPOS1-1)
         STR=LNBUF1
         CALL KILEXP(STR,' ',' ',' ',MAXTOK,TOK,NTOK,' ')
         IF (NTOK.GT.4) THEN
            CALL IGERR('Number of chmax parameter not correct',
     +                 'APPLICATION SLIDE')
            ERR=7
            RETURN
         ENDIF
         IF (NTOK.GT.1) THEN
         IF (NTOK.GE.2)THEN
             CALL KICTON(STR(TOK(1,2):TOK(2,2)),IVAL,HC2)
             IF (IQUEST(1).NE.0) GO TO 195
         ENDIF
         IF (NTOK.GE.3)THEN
             CALL KICTON(STR(TOK(1,3):TOK(2,3)),IVAL,HC1)
             IF (IQUEST(1).NE.0) GO TO 195
         ELSE
             HC1=HC2
         ENDIF
         IF (NTOK.EQ.4)CALL KICTON(STR(TOK(1,4):TOK(2,4)),IVAL,RLNSPC)
  195    IF (IQUEST(1).NE.0.OR.IPOS1.LT.IPOS+TOK(2,NTOK)) THEN
             CALL IGERR('CHMAX parameter not correct', 'APPLICATION '
     +                   //'SLIDE')
              ERR=7
              RETURN
         ENDIF
         ENDIF
         STR=' '
      ENDIF
      IF (IC.EQ.1.AND.PRIMO1) THEN
         IF (HC1.NE.0)THEN
            CHMAX=HC1
            CHSAVE=CHMAX
         ENDIF
         IF (HC2.NE.0) THEN
            CHMIN=HC2
            CHMN =HC2
         ENDIF
         PRIMO1=.FALSE.
         IF (CHMAX.LT.CHMIN) THEN
            CALL IGERR('CHMAX less than CHMIN', 'APPLICATION SLIDE')
            ERR=7
            RETURN
         ENDIF
         HC=CHMAX
      ENDIF
      IF (LENOCC(STR).GT.0) THEN
         NBLINE=NBLINE+1
         LINE(NBLINE) = STR
      ENDIF
      END
