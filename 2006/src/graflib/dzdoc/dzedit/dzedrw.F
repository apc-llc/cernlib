*
* $Id: dzedrw.F,v 1.2 1997/03/14 14:20:36 mclareni Exp $
*
* $Log: dzedrw.F,v $
* Revision 1.2  1997/03/14 14:20:36  mclareni
* WNT mods
*
* Revision 1.1.1.1.2.1  1997/01/21 11:25:01  mclareni
* All mods for Winnt 96a on winnt branch
*
* Revision 1.1.1.1  1996/03/04 16:13:22  mclareni
* Dzdoc/Zebpack
*
*
#include "dzdoc/pilot.h"
      SUBROUTINE DZEDRW
#include "dzeditkeep.inc"
#if defined(CERNLIB_BSLASH)
#include "dzdoc/bslash2.inc"
#endif
#if !defined(CERNLIB_BSLASH)
#include "dzdoc/bslash1.inc"
#endif
      CHARACTER*1 COPDTR,COPDT1
      CHARACTER*4 CHOPT, UPUPID
      PARAMETER (MXWILD=50)
      CHARACTER*8 CHWILD(MXWILD)
      INTEGER IIDATE(2),IITIME(2), KEYVEC(2)
      INTEGER IOPTP,IOPTL,IOPTC, IFLAG
 
*     graphics output
 
      CALL KUGETC(CHBSBK,NCHA)
      CALL KUGETC(CHBSUP,NCHA)
      CHBSEL(1:4) = CHBSBK
      CHBSEL(5:8) = CHBSUP
      IFLAG=1
      GOTO 10
 
      ENTRY DZEDRA
*     graphics for all top banks
      CHBSEL = '****NONE'
      IFLAG=2
 
   10 CONTINUE
      CALL KUGETS(CHMETA,NCMETA)
      CALL KUGETS(CHSGML,NCSGML)
      CALL KUGETS(CHPOST,NCPOST)
      CALL KUGETC(CHOPT,NCPYNO)
      CALL KUGETS(CTITLE,NCTITL)
      GOTO 15
 
*     HTML
 
      ENTRY DZEHTM
      CALL KUGETC(CHBSBK,NCHA)
      CALL KUGETC(CHBSUP,NCHA)
      NCMETA=0
      NCSGML=0
      NCPOST=0
      NCTITL=0
      CHBSEL(1:4) = CHBSBK
      CHBSEL(5:8) = CHBSUP
      CALL KUGETC(CHOPT,NCPYNO)
      IFLAG=3
*      GOTO 15
 
15    CONTINUE
      CALL KUCMD ('DZEDIT','   ','SW')
      CALL KUPVAL('DRAWONETREE','CHBSBK',0,0.,CHBSBK,'D')
      CALL KUPVAL('DRAWONETREE','CHBSUP',0,0.,CHBSUP,'D')
      CALL KUPVAL('LISTONEBANK','CHBSBK',0,0.,CHBSBK,'D')
      CALL KUPVAL('LISTONEBANK','CHBSUP',0,0.,CHBSUP,'D')
      CALL KUCMD (BS,' ','SW')
 
      IF(LUNRZ.EQ.0)THEN
         WRITE(LUNOUT,*)' No RZ-file open'
         GOTO 999
      ENDIF
      IOPTP=0
      IOPTL=0
      COPDTR='0'
*     select algorithm for 2nd level down banks
      IF(INDEX(CHOPT,'M').NE.0)THEN
          COPDT1=' '
      ELSE
          COPDT1='1'
      ENDIF
      IF(INDEX(CHOPT,'Q').NE.0)THEN
          IOPTQ=1
      ELSE
          IOPTQ=0
      ENDIF
      IF(INDEX(CHOPT,'P').NE.0)THEN
          IOPTP=1
      ELSEIF(INDEX(CHOPT,'L').NE.0)THEN
          IOPTL=1
      ENDIF
      IF(IOPTP.EQ.0 .AND. IOPTL.EQ.0 .AND. IFLAG.NE.3)THEN
*         WRITE(*,*)' Will use PostScript format'
         IOPTP=1
      ENDIF
 
      IF(INDEX(CHBSEL,'*').NE.0)THEN
         NFKEY = 0
         IP = 0
         LWILDC = .TRUE.
         IF(INDEX(CHOPT,'N').EQ.0)THEN
            IOPTC=1
         ELSE
            IOPTC=0
         ENDIF
         CALL CLTOU(CHBSEL)
   20    CALL DZDWCS(CHBSEL,KEYVEC,NFKEY)
         IF(KEYVEC(1) .NE. 0)THEN
            IP = IP+1
            CALL UHTOC(KEYVEC,4,CHWILD(IP),8)
            IF(IP .LT. MXWILD)GOTO 20
         ENDIF
         IF(IP .EQ. 0)THEN
            WRITE(*,*)' No bank found'
            GOTO 999
         ENDIF
         WRITE(*,*)' The following banks will be treated:'
         DO 30   I=1,IP
            WRITE(*,*)' ',CHWILD(I)
   30    CONTINUE
         IF(INDEX(CHOPT,'S').EQ.0)THEN
            CALL KUPROC(' Is this ok?',ANYCHA,NCH)
            IF(ANYCHA(1:1) .NE. 'Y' .OR. NCH .LE. 0)GOTO 999
         ENDIF
         IP1 = IP
      ELSE
         LWILDC = .FALSE.
         IF(INDEX(CHOPT,'C').NE.0)THEN
            IOPTC=1
         ELSE
            IOPTC=0
         ENDIF
      ENDIF
      IP = 0
*     loop on top banks
   40 CONTINUE
      IF(LWILDC)THEN
         IP = IP+1
*           if done, go and check the file
         IF(IP .GT. IP1) GOTO 60
         CHBSEL = CHWILD(IP)
      ENDIF
*     form the name for the output file
      IF(IOPTP .NE. 0 .AND. NCPOST .EQ. 0)THEN
         NCPOST = INDXBC(CHBSEL(1:4),' ')
         CHPOST=CHBSEL(1:NCPOST)//'.ps'
         NCPOST=NCPOST+3
         CALL CUTOL(CHPOST)
      ENDIF
 
      IF(LTOP .NE. 0)CALL MZDROP(IXDZDS,LTOP,'L')
      LTOP=0
      CALL DZDGTR(CHBSEL,'GR',LTOP)
      IF(LTOP .LE. 0)THEN
         WRITE(*,*)' Error from DZDGTR'
         IF(LTOP.EQ.0)THEN
            WRITE(*,*)' tree not found'
         ELSEIF(LTOP .EQ.-1)THEN
            WRITE(*,*)' bank has no down links'
            LTOP=0
         ENDIF
         IF(LWILDC)THEN
            GOTO 40
         ELSE
            GOTO 999
         ENDIF
      ENDIF
      ILCM = INDXBC(CHBSEL(1:4),' ')
      IF(LUNCOM.EQ.0)THEN
         LUNCOM=LUPCOM
         CALL KUOPEN(LUNCOM,CHBSEL(1:ILCM)//'.scratch',
     &   'UNKNOWN',ISTAT)
      ENDIF
      IF(LUNSGM .EQ. 0 .AND. IOPTQ.EQ.0)THEN
         ILCM = INDXBC(CHBSEL(1:4),' ')
         IF(NCMETA .EQ. 0)THEN
            IF(IOPTL.EQ.0)THEN
               CHMETA=CHBSEL(1:ILCM)//'.metaf'
               CALL CUTOL(CHMETA)
            ENDIF
         ENDIF
         IF(NCSGML .EQ. 0)THEN
            IF(IOPTL.EQ.0)THEN
               CHSGML=CHBSEL(1:ILCM)//'.sgml'
            ELSE
               CHSGML=CHBSEL(1:ILCM)//'.tex'
            ENDIF
            CALL CUTOL(CHSGML)
         ENDIF
         IF(IOPTL.EQ.0)THEN
            LUNMET=LUPMET
            CALL KUOPEN(LUNMET,CHMETA,'UNKNOWN',ISTAT)
         ENDIF
         LUNSGM=LUPSGM
         CALL KUOPEN(LUNSGM,CHSGML,'UNKNOWN',ISTAT)
         IF(IOPTL.EQ.0)THEN
            WRITE(LUNSGM,'(A)')'<!DOCTYPE SGMLGUID>'
            WRITE(LUNSGM,'(A)') '<SET TAG=FIG ITEM=FONT VALUE=TEXT>'
 
            WRITE(LUNSGM,'(A)') '<SET TAG=LISTING ITEM=FONT VALUE='
     +      //'MINUTE>'
            WRITE(LUNSGM,'(A)') '<SET TAG=LISTING ITEM=PLACE VALUE=ANY>'
 
            WRITE(LUNSGM,'(A)') '<SET TAG=FIG ITEM=PLACE VALUE=INLINE>'
 
            WRITE(LUNSGM,'(A)') '<GDOC>'
            WRITE(LUNSGM,'(A)') '<TITLEP>'
            IF(NCTITL.GT.0)THEN
               WRITE(LUNSGM,'(A,A)') '<TITLE>',CTITLE
               WRITE(LUNSGM,'(A)') '<TITLE>*'
            ENDIF
            WRITE(LUNSGM,'(A)') '<TITLE>ZEBRA bank documentation'
            WRITE(LUNSGM,'(A)') '<TITLE>from '
            WRITE(LUNSGM,'(A,A)')'<TITLE>', CHRZF
            WRITE(LUNSGM,'(A)') '<TITLE> banks below: '
         ENDIF
         IF(LWILDC)THEN
            DO 50   I=1,IP1
               IF(IOPTL.EQ.0)THEN
                  WRITE(LUNSGM,'(2A)')'<TITLE>',CHWILD(I)(1:4)
               ELSE
                  WRITE(LUNSGM,'(2A)') BS//BS,CHWILD(I)(1:4)
               ENDIF
   50       CONTINUE
         ELSE
            IF(IOPTL.EQ.0)THEN
               WRITE(LUNSGM,'(2A)')'<TITLE>',CHBSEL(1:4)
            ELSE
               WRITE(LUNSGM,'(2A)') BS//BS,CHBSEL(1:4)
            ENDIF
         ENDIF
         IF(IOPTP.EQ.0)THEN
            IF(IOPTL.EQ.0)THEN
               WRITE(LUNSGM,'(A)') '<AUTHOR>generated by DZEDIT - '
     +         //'DZDOC'
               WRITE(LUNSGM,'(A)') '<DATE>'
            ELSE
               WRITE(LUNSGM,'(A)')
     +         '} '//BS//'author {printed by DZEDIT - '
     +         //'DZDOC at}'
            ENDIF
         ELSE
            CALL DATIMH(IIDATE,IITIME)
            WRITE(LUNSGM,'(A,2A4)')'<TITLE>',IIDATE
         ENDIF
         IF(IOPTL.EQ.0)THEN
            WRITE(LUNSGM,'(A)') '</TITLEP>'
            WRITE(LUNSGM,'(A)') '<BODY>'
         ELSE
            WRITE(LUNSGM,'(10(A/))')
     +      BS//'maketitle',BS//'pagebreak'
         ENDIF
      ENDIF
      LD1 = LTOP
 
*     deactivate screen
 
      IF(IWKID.NE.0 .AND. IOPWKF .NE.0)THEN
         CALL IDAWK(IWKID)
      ENDIF
      CALL DZDDTR(0,LTOP,LUNSGM,LUNCOM,LUNMET,
     +            INTWST,ITYMET,COPDT1//'PRT')
      LTOP = LD1
      CALL MZGARB(2,0)
 
*     activate screen again
      IF(IWKID.NE.0 .AND. IOPWKF .NE.0)THEN
         CALL IACWK(IWKID)
      ENDIF
*     if all required loop on top banks
 
      IF(LWILDC) GOTO 40
 
   60 CONTINUE
      REWIND LUNCOM
      IF(IOPTC.NE.0)THEN
         CALL DZECHK
      ENDIF
*     output part
 
*     html
 
      IF(IFLAG.EQ.3)THEN
         LUNSGM=LUPSGM
         REWIND(LUNSGM)
         IF(INDEX(CHBSEL,'*').NE.0)THEN
            UPUPID='NONE'
         ELSE
*           find Up Bank
            NFKEY = 0
            IP = 0
            CHBSEL=CHBSEL(5:8)//'****'
   21       CALL DZDWCS(CHBSEL,KEYVEC,NFKEY)
            IF(KEYVEC(1) .NE. 0)THEN
               IP = IP+1
               CALL UHTOC(KEYVEC,4,CHWILD(IP),8)
               IF(IP .LT. MXWILD)GOTO 21
            ENDIF
            IF(IP .EQ. 0)THEN
               UPUPID='NONE'
            ELSE IF(IP.EQ.1)THEN
               UPUPID=CHWILD(1)(5:8)
            ELSE
               WRITE(*,*)
     +         'Up bank is not unique, please make your choice'
               DO 31   I=1,IP
   31          WRITE(*,*)' ',CHWILD(I)(5:8)
               WRITE(*,*)'Up bank?'
               READ(*,'(A4)')UPUPID
            ENDIF
            IF(INDEX(CHOPT,'1').NE.0)THEN
               IPF=1
            ELSE
               IPF=3
            ENDIF
            LUNHTM=LUPHTM
            CALL DZE2HT(LUNSGM,LUNHTM,IPF,UPUPID)
            LUNHTM=0
         ENDIF
*        open so it can be deleted later
         LUNMET=LUPMET
*        CALL KUOPEN(LUNMET,CHMETA,'UNKNOWN',ISTAT)
         GOTO 999
      ENDIF
 
      IF(IOPTQ.NE.0)GOTO 999
 
      IF(IOPTL.EQ.0)THEN
         IF(LUNMET.NE.0)CLOSE(LUNMET)
         LUNMET = 0
      ELSE
         REWIND LUNCOM
         CALL DZELIN(LUNCOM,LUNSGM)
         WRITE(LUNSGM,'(A)')BS//'end{document}'
         CLOSE(LUNSGM)
         LUNSGM=0
         WRITE(*,'(A,A)')' LaTeX output on: ',CHSGML
         GOTO 999
      ENDIF
      CALL DZEWEM(LUNSGM)
      CLOSE(LUNSGM)
      LUNSGM=0
      IF(IOPTP .NE. 0)THEN
         LUNPOS=LUPPOS
         CALL KUOPEN(LUNPOS,CHPOST,'UNKNOWN',ISTAT)
         LUNMET=LUPMET
         CALL KUOPEN(LUNMET,CHMETA,'OLD',ISTAT)
         LUNSGM=LUPSGM
         CALL KUOPEN(LUNSGM,CHSGML,'OLD',ISTAT)
         CALL DZESGP(LUNSGM,LUNMET,LUNPOS, 0)
*        open it again R/W, otherwise IBM cant DELETE it
         CLOSE(LUNSGM)
         CLOSE(LUNMET)
         IF(INDEX(CHOPT,'K').EQ.0)THEN
            CALL KUOPEN(LUNSGM,CHSGML,'UNKNOWN',ISTAT)
            CALL KUOPEN(LUNMET,CHMETA,'UNKNOWN',ISTAT)
         ELSE
            LUNSGM=0
            LUNMET=0
         ENDIF
         CLOSE(LUNPOS)
         LUNPOS=0
         WRITE(*,'(A,A)')' PostScript output on: ',CHPOST
      ENDIF
999   CONTINUE
      IF(LUNCOM.NE.0)THEN
         IF(INDEX(CHOPT,'K').EQ.0)THEN
            CALL KICLOS(LUNCOM,'DELETE',ISTAT)
         ELSE
            CALL KICLOS(LUNCOM,' ',ISTAT)
         ENDIF
         LUNCOM=0
      ENDIF
      IF(LUNSGM.NE.0)THEN
         IF(INDEX(CHOPT,'K').EQ.0)THEN
            CALL KICLOS(LUNSGM,'DELETE',ISTAT)
         ENDIF
         LUNSGM=0
      ENDIF
      IF(LUNPIC.NE.0)THEN
         IF(INDEX(CHOPT,'K').EQ.0)THEN
            CALL KICLOS(LUNPIC,'DELETE',ISTAT)
         ELSE
            CALL KICLOS(LUNPIC,' ',ISTAT)
         ENDIF
         LUNPIC=0
      ENDIF
      IF(LUNMET.NE.0)THEN
         IF(INDEX(CHOPT,'K').EQ.0)THEN
            CALL KICLOS(LUNMET,'DELETE',ISTAT)
         ENDIF
         LUNMET=0
      ENDIF
      CALL DZDPLN(0,' ',0)
      END
