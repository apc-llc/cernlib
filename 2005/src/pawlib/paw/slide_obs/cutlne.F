*
* $Id: cutlne.F,v 1.1.1.1 1996/03/01 11:39:28 mclareni Exp $
*
* $Log: cutlne.F,v $
* Revision 1.1.1.1  1996/03/01 11:39:28  mclareni
* Paw
*
*
*CMZ :  1.10/00 20/09/90  10.54.07  by  O.Couet
*-- Author : A.Risso
      SUBROUTINE CUTLNE(I,NWNBLN,DISP)
      COMMON /SLIDE1/XB1,XB2,YB1,YB2,HC
      PARAMETER (ILINE=99)
      PARAMETER (MGUIDL=199)
      COMMON /KCGUID/ GUID(MGUIDL)
      CHARACTER*80 GUID
      CHARACTER*80 LINECP(99)
      CHARACTER*80 LINE(99)
      EQUIVALENCE (LINE(1),GUID(1)),(LINECP(1),GUID(100))
      COMMON /SLIDE5/CHBUF
      CHARACTER*1   CHBUF
      COMMON /SLIDE6/SPACE
      PARAMETER (MAXCMD=1000)
      COMMON /SLIDE7/  LSTCMD(MAXCMD,2),PARCMD(MAXCMD,2),JCD
      COMMON /SLIDE8/ ITOT
      COMMON /SLIDE9/ BLSPAC
      COMMON /SLIDE18/ ERR
      COMMON /SLIDE20/ NEWLIN,NWLN
      CHARACTER*80 NEWLIN,NWLN
      COMMON /SLIDE27/ FONT,IFONT,OLDFNT,OLDPRE,PREC,OLDCOL,ICOL
      COMMON /SLIDE28/ FACTL
      COMMON /SLIDE34/ FACT(13)
      COMMON /SLIDE36/ SPSY,ADDLET,ORDER
      LOGICAL ADDLET,ORDER
      CHARACTER*80 LNBUF,LNBUF1
*.______________________________________
*
      NEWLIN=' '
      NWLN  =' '
      J=80
      CALL TKBLNK(I,J)
*
*          take a word from the line
*
   10 IF (CHBUF.NE.' ')THEN
         NEWLIN = CHBUF // NWLN
         NWLN=NEWLIN
         J = J - 1
         IF (J.EQ.0)THEN
*
*            line without space between the words
*
            ERR=3
            RETURN
         ELSE
            CHBUF = LINE(I)(J:J)
            GO TO 10
         ENDIF
      ENDIF
      LNBUF=LINE(I)(1:J-1)
*
*        next lines of code provide to calculate the longer of current line
*        and to add eventual space(s) for the elements of the list
*        conformed with the execution of command <SPC1>(that add
*        one or more spaces in output)
*
      IF (FONT.LE.-8.AND.FONT.GE.-11) THEN
         IF(ADDLET) THEN
            LLN=(J-1)+SPSY
         ELSE
            LLN=J-1
         ENDIF
         IF(ORDER.AND.LLN.GT.1)LLN=LLN+1
         HLONG=FACT(-INT(FONT))*LLN*HC
      ELSEIF (ADDLET) THEN
         CALL IGTEXT(1.,1.,LNBUF(4:),HC,HLONG,'S')
         HLONG=HLONG+(SPSY+2)*BLSPAC/FACTL
      ELSEIF(ORDER.AND.FONT.NE.0.) THEN
         CALL IGTEXT(1.,1.,LNBUF(INT(SPSY):),HC,HLONG,'S')
         HLONG=HLONG+(SPSY+2)*BLSPAC/FACTL
      ELSEIF(ORDER) THEN
         CALL IGTEXT(1.,1.,LNBUF,HC,HLONG,'S')
         HLONG=HLONG+BLSPAC
      ELSE
         CALL IGTEXT(1.,1.,LNBUF,HC,HLONG,'S')
      ENDIF

      IF (HLONG*FACTL+DISP.GT.SPACE)THEN
         OLDJ=J
         J=J-1
         CALL TKBLNK(I,J)
   20    NEWLIN = ' ' // NWLN
         NWLN = NEWLIN
         OLDJ=OLDJ-1
         IF (OLDJ.NE.J) GO TO 20
         GO TO 10
      ENDIF
*
*     reposition of start_point of command
*

      I1=LENOCC(LNBUF)
      I3=LENOCC(LINE(I))
         IF(JCD.GT.0) THEN
            JJ=JCD
   30       IF (LSTCMD(JJ,1).GT.ITOT+I1+1) THEN
               LSTCMD(JJ,1)=LSTCMD(JJ,1)-1
               JJ=JJ-1
               IF (JJ.GE.1) GO TO 30
            ENDIF
         ENDIF
*
*         put newline in the text
*
      NWNBLN= NWNBLN + 1
      IF (NWNBLN.GT.ILINE) THEN
         ERR=1
         RETURN
      ENDIF
      LINE(I)=LNBUF
      LNBUF1 =LINE(I+1)
      LINE(I+1)=NEWLIN
      DO 40 K=I+2,NWNBLN
         LNBUF =LINE(K)
         LINE(K)=LNBUF1
         LNBUF1 =LNBUF
   40 CONTINUE
      END
