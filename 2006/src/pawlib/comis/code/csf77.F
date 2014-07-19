*
* $Id: csf77.F,v 1.3 1996/04/15 09:22:40 cernlib Exp $
*
* $Log: csf77.F,v $
* Revision 1.3  1996/04/15 09:22:40  cernlib
* Replace tabs by spaces
*
* Revision 1.2  1996/04/02 22:09:52  thakulin
* Remove CERNLIB_PIAF macro and check the run time variable IPIAF in
* paw/pcmode.inc instead.
*
* Revision 1.1.1.1  1996/02/26 17:16:29  mclareni
* Comis
*
*
#include "comis/pilot.h"
#if defined(CERNLIB_PAW)
*CMZ :  1.18/14 12/01/95  16.40.27  by  Vladimir Berezhnoi
*-- Author : V.Berezhnoi
      SUBROUTINE CSF77(IFILE,OFILE,KERR)
***-------------------------------------
*    routine to convert CS to F77 syntax:
* 1) for watched common blocks fills listx of used vars
* 2) replaces 'type' by 'print *', 'input' by 'read(*,*)'
* 3) replaces ref. to vectors by ref. to Q(link(i)+offs)
* 4) replaces calls to watched routines (call HFILL(...)) by
*                        call jumpcn(jmp(line),[-]line,...)
*    if OFILE=' ' then just fills listx
***-------------------------------------
      CHARACTER *(*) IFILE, OFILE
#include "comis/cspar.inc"
#include "comis/mdpool.inc"
#include "comis/cstab.inc"
#include "comis/cspnts.inc"
#include "comis/csfmt.inc"
#include "comis/cslun.inc"
#include "comis/cskucs.inc"
#include "comis/cshfill.inc"
#include "comis/cscbwl.inc"
#include "paw/pcmode.inc"
      INTEGER CSLTGP, CSITGP
      COMMON/CSGSCM/IGSST,JGSST,NGSST,CSJUNK(3)
      CHARACTER *4 NMJMP
      CHARACTER CH1*1, NAME*32, LINE*80
      LOGICAL EXECST,ISVECT,ISPAWC,ISDATA,ISHFIL,OK,WASOPEN
      PARAMETER(MPAWC=1,MVECT=2,MDATA=3,MEXEC=4,MTYPE=5,MINP=6,MHFI=7)
      PARAMETER(MINCL=8)
      PARAMETER (LPFOR=72)
      MDEDIT=1
      KERR=0
      WASOPEN=.FALSE.
      CALL CSFILX(IFILE)
      IF(ISTFIL.EQ.0)GO TO 130
      IF(OFILE.NE.' ')THEN
**        CALL PALUNF(60,3,LUNOUT)
        CALL CSLUNF(LUNOUT)
        IF(LUNOUT.EQ.0)GO TO 130
        OPEN(LUNOUT,FILE=OFILE,STATUS='UNKNOWN',ERR=130)
        WASOPEN=.TRUE.
      ELSE
        LUNOUT=0
      ENDIF
      ICLHFI=1
*--  call comis translater
   10 ISTATE=5
*     ------
      KEYRD=0
      CALL CSRD(JGSST,NGSST)
      IF (NGSST .LE. 0) GO TO 140
      CALL CSSVPT
      CALL CCOPYS(JSMAIN,JID,8)
      NCIDEN=6
      NWIDEN=2
      CALL CSTRAN(IPCB,IPCE,KPRO,LSTCL)
*-- if error restore pointers and print err. message
      IF(KPRO.LE.0)THEN
        CALL CSRSPT(0)
        CALL CSRMCLL(LSTCL)
        CALL CSTRER(IGSST,IREJ)
        IGSST=1
        GO TO 130
      ENDIF
*-- remove routine from comis table
      IGP=CSLTGP(IPVS)
      IF(IGP.GT.0)THEN
        CALL CSDPRO(IGP)
      ELSE
        IADGP=0
        IFCS=0
        ITYPGP=ITYPGI
        IGP=CSITGP(IPVS)
      ENDIF
      CALL CSPDLS(IGP)
      CALL CSINCCL(IGP,LSTCL)
*--  fills listx
      I=IPLI
   20 IF(I.GT.0)THEN
        CALL CSLDLI(I)
        IF(ICBWL.GT.0 .AND. NUMGI.GT.0 .AND. LXXGLI.NE.0)THEN
*       try add var to used list
          IF(IQ(NUMGI+1).LE.2)CALL CSTADV(IGP)
        ENDIF
        I=IQ(I)
        GO TO 20
      ENDIF
      IF(LUNOUT.EQ.0)GO TO 120
      ISVECT=.FALSE.
      ISPAWC=.FALSE.
      EXECST=.FALSE.
      ISDATA=.FALSE.
      ISHFIL=.FALSE.
      IF(MODHFI.NE.0 .AND.NCLHFI.NE.0)ISHFIL=.TRUE.
      I=JPMB
   30 IF(I.EQ.0)GO TO 120
      CALL CSGETL(I,LINE,LP,MARK)
   40 IF(MARK.NE.0)THEN
        NOST=MARK/100
        NV=MARK-NOST*100
        IF(NOST.EQ.MINCL)THEN
          IF(INDEX(LINE(:LP),'?').NE.0)THEN
            LINE='*     INCLUDE ''comis.inc'' '
*--               1234567890123456789012345
            LP=LENOCC(LINE)
          ELSE
            LINE(1:1)='*'
          ENDIF
          WRITE(LUNOUT,10000,ERR=130)LINE(:LP)
          GO TO 30
        ENDIF
   50   IF(EXECST)THEN
          IPOS=1
          FMT=LINE
          IF(NOST.EQ.MTYPE .OR. NOST.EQ.MINP .OR. NOST.EQ.MHFI)THEN
   60       CALL CSNIDN(FMT(:LP),IPOS,NAME,LNAME )
            IF(LNAME.LE.0)GO TO 130
            IF(NAME(:LNAME).EQ.'TYPE')THEN
              FMT=FMT(:IPOS-LNAME-1) // 'PRINT *,' // FMT(IPOS+1:LP)
              IPOS=IPOS+4
              LP=LP+3
            ELSEIF(NAME(:LNAME).EQ.'INPUT')THEN
              FMT=FMT(:IPOS-LNAME-1) // 'READ(*,*)' //FMT(IPOS+1:LP)
              IPOS=IPOS+4
              LP=LP+3
            ELSEIF(NAME(:LNAME).EQ.'CALL')THEN
*--  if more then kjmphfi calls to hfi don't replace call
              IF(ICLHFI.GT.KJMPHFI)GO TO 69
              CALL CSNIDN(FMT(:LP),IPOS,NAME,LNAME )
              DO 65 KJ=1,MODHFI
                IF(NAME(:LNAME).EQ.HFINMS(KJ))GO TO 66
   65         CONTINUE
              GO TO 130
   66         IBEG=IPOS-LNAME-1
*-- must be '('
              CALL CSNLEX(FMT(:LP),IPOS,NAME,LNAME,LEXEM )
              ISIG=-1
              IPOSID=IPOS
              CALL CSNLEX(FMT(:LP),IPOSID,NAME,LNAME,LEXEM )
              IF(LEXEM.EQ.2)THEN
*-- for constant ID must be 'inumber' then ','
                CALL CSNLEX(FMT(:LP),IPOSID,NAME,LNAME,LEXEM )
                IF(NAME.EQ.',')ISIG=1
              ENDIF
*-- name='jumpc4/5(jmp(line),+/-line,':: -line if ID non const
              IF(NARGHFI(KJ).EQ.4)THEN
                WRITE(NAME,10400)NMJMP(:2),ICLHFI,ICLHFI*ISIG
              ELSEIF(NARGHFI(KJ).EQ.3)THEN
                WRITE(NAME,10300)NMJMP(:2),ICLHFI,ICLHFI*ISIG
              ELSE
                GOTO 130
              ENDIF
              CALL CSRBLK(NAME,LNAME)
              FMT=FMT(:IBEG) // NAME(:LNAME) //FMT(IPOS:LP)
              IPOS=IBEG+LNAME
              LP=LENOCC(FMT)
              JMPHFI(ICLHFI)=JADHFI(KJ)
              ICLHFI=ICLHFI+1
            ELSE
              GO TO 60
            ENDIF
          ENDIF
*-- vectors treatment
   69     IV=1
   70     IF(IV.LE.NV)THEN
   80       CALL CSNIDN(FMT(:LP),IPOS,NAME,LNAME )
            IF(LNAME.LE.0)GO TO 130
            IPF=IPOS-LNAME
            CALL CSREPV(NAME, IPOS, LP, LREP)
            IF(LREP.EQ.0)GO TO 80
            FMT(IPF:)=BUF(:LREP) // FMT(IPOS:LP)
            LP=LP+LREP-(IPOS-IPF)
            IPOS=IPF+LREP
            IV=IV+1
            GO TO 70
          ENDIF
   90     IPOS=7
          IF(LP.GT.LPFOR)THEN
*  split long line
  100       CALL CSNLEX(FMT(:LP),IPOS,NAME,LNAME,LEXEM)
            IF(IPOS.LT.LPFOR)GO TO 100
            IP1=IPOS-LNAME
            WRITE(LUNOUT,10000,ERR=130)FMT(:IP1-1)
            FMT='     +' // FMT(IP1:LP)
            LP=LP-IP1+7
            GO TO 90
          ENDIF
          WRITE(LUNOUT,10000,ERR=130)FMT(:LP)
          GO TO 30
        ELSEIF(NOST.EQ.MPAWC)THEN
          ISPAWC=.TRUE.
        ELSEIF(NOST.EQ.MVECT)THEN
          ISVECT=.TRUE.
  110     LINE(1:1)='*'
          IF (IPIAF.EQ.1) THEN
*  add line *CS*V*L*  vn1 ... vnn
            CALL CSPIWRVL(LUNOUT,LINE,LP,KERR)
            IF(KERR.NE.0)GO TO 130
          ENDIF
  111     WRITE(LUNOUT,10000,ERR=130)LINE(:LP)
          IF(I.EQ.0)GO TO 120
          CALL CSGETL(I,LINE,LP,MARK)
          CH1=LINE(1:1)
          IF(CH1.EQ.'C' .OR. CH1.EQ.'*')GO TO 111
          IF(LINE(6:6).NE.' ')GO TO 110
          GO TO 40
        ELSEIF(NOST.EQ.MDATA)THEN
          IF((ISVECT .OR. ISHFIL) .AND. .NOT.ISDATA)THEN
            ISDATA=.TRUE.
* define names
*------------------------------------------
            LNMJMP=0
            IF(ISVECT)THEN
              CALL CSDEFN(LUNOUT, ISPAWC, OK)
              IF(.NOT.OK)GO TO 130
              LNMJMP=1
            ENDIF
            IF(ISHFIL)THEN
              CALL CSUNAM(NMJMP,LNMJMP)
              IF(NMJMP.EQ.' ')GO TO 130
              WRITE(LUNOUT,10100,ERR=130)NMJMP(:2)
              WRITE(LUNOUT,10200,ERR=130)NMJMP(:2),KJMPHFI
            ENDIF
          ENDIF
        ELSEIF(NOST.GE.MEXEC)THEN
          IF((ISVECT .OR. ISHFIL) .AND. .NOT.ISDATA)THEN
            ISDATA=.TRUE.
* define names
*------------------------------------------
            LNMJMP=0
            IF(ISVECT)THEN
              CALL CSDEFN(LUNOUT, ISPAWC, OK)
              IF(.NOT.OK)GO TO 130
              LNMJMP=1
            ENDIF
            IF(ISHFIL)THEN
              CALL CSUNAM(NMJMP,LNMJMP)
              IF(NMJMP.EQ.' ')GO TO 130
              WRITE(LUNOUT,10100,ERR=130)NMJMP(:2)
              WRITE(LUNOUT,10200,ERR=130)NMJMP(:2),KJMPHFI
            ENDIF
          ENDIF
          EXECST=.TRUE.
          GO TO 50
        ENDIF
      ENDIF
      WRITE(LUNOUT,10000,ERR=130)LINE(:LP)
      GO TO 30
*-- restore pointers
  120 CALL CSRSPT(1)
      GO TO 10
  130 KERR=1
  140 KEYRD=0
      ISTATE=1
      MDEDIT=0
      IF(LUNOUT.GT.0)THEN
        CALL CSCLOS(LUNOUT)
        IF(WASOPEN)CLOSE(LUNOUT)
      ENDIF
      IF(ISTFIL.NE.0)THEN
        CLOSE(LUNFIL)
        ISTFIL=0
      ENDIF
10000 FORMAT(A)
10100 FORMAT('**--comis--**'/6X,'INTEGER ' ,A )
10200 FORMAT(6X,'COMMON/CSHFIJ/',A, '(' ,I4, ')' )
10300 FORMAT('jumpc4(',A, '(' ,I4, '),' ,I4, ',' )
10400 FORMAT('jumpc5(',A, '(' ,I4, '),' ,I4, ',' )
      END
#endif
