*
* $Id: cspdir1.F,v 1.1.1.1 1996/02/26 17:16:31 mclareni Exp $
*
* $Log: cspdir1.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:31  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :          13/02/96  14.50.39  by  Unknown
*-- Author : V.Berezhnoi
      SUBROUTINE CSPDIR(NOERR)
***-----------------------------
*    parsing of comis directives
***-----------------------------
#include "comis/cspar.inc"
#include "comis/csrec.inc"
#include "comis/cspnts.inc"
#include "comis/cslun.inc"
#include "comis/cskeys.inc"
         COMMON/CSERRNO/IERR
         CHARACTER NAME*256,NMFILE*256
         PARAMETER (NODIR=22)
         CHARACTER NMDIR(NODIR)*8,SHKEYS(4)*8
         INTEGER CSLTGP
****************************************************************
*     lexem definitions
      INTEGER LUNDEF,  LIDENT,  LINUM,  LRNUM, LDNUM, LHOLL
     +       ,DOTDOT, CCONST, DELIM, NOMORE
      PARAMETER (LUNDEF=0,LIDENT=1,LINUM=2,LRNUM=3,LDNUM=4,LHOLL=5,
     +           DOTDOT=6,CCONST=7,DELIM=8,NOMORE=9 )
*****************************************************************
      DATA NMDIR/
     + 'EDIT', 'FILE', 'LOGFILE', 'MAPFILE', 'LIBRARY', 'EOF',
*-       1       2       3           4          5         6
     + 'SHELL','FORTRAN', 'COMIS','SHOW','NOCHECKB', 'CHECKB',
*-       7        8          9      10     11          12
     + 'NOTRACE','TRACE', 'NOPARAM', 'PARAM', 'HELP',
*-        13       14        15        16       17
     + 'REMOVE','CLEAR', 'SETOPT', 'NOSOURCE', 'SOURCE'
*-        18      19       20         21          22
     +      /
      DATA SHKEYS/
     +    'MEMORY','ROUTINES','COMMONS','NAMES'  /
*-------------------------------------------------*
      NOERR=0
      IERR=0
      LR=LENOCC(REC)
      IPOS=1
      CALL CSNLEX(REC(:LR),IPOS,NAME,LNAME,LEXEM)
      CALL CSNLEX(REC(:LR),IPOS,NAME,LNAME,LEXEM)
*       print *,lexem,lname,name
      IF(LEXEM.NE.LIDENT)THEN
        NOERR=1
        GO TO 70
      ENDIF
      NOFIND=0
      CALL CLTOU(NAME(:LNAME))
      DO 100 I=1,NODIR
        IF(NAME(:LNAME).EQ.NMDIR(I)(:LNAME))THEN
          NOFIND=NOFIND+1
          KEY=I
        ENDIF
 100  CONTINUE
      IF(NOFIND.EQ.0)THEN
        NOERR=2
        GO TO 70
      ELSEIF(NOFIND.GT.1)THEN
        NOERR=5
        GO TO 70
      ENDIF
      GO TO (1, 2, 2, 2, 2, 6, 7, 8, 9,10,11,12,13,14,15,
     +       16,17,18,19,20,21,22
     +       ),KEY

*
*-----!EDIT [name]
*
 1    CALL CSNLEX(REC(:LR),IPOS,NAME,LNAME,LEXEM)
      IF(LEXEM.EQ.LIDENT)THEN
        CALL CLTOU(NAME(:LNAME))
      ELSEIF(LEXEM.EQ.NOMORE)THEN
        NAME='*MAIN1'
        LNAME=6
      ELSE
        NOERR=1
        GO TO 70
      ENDIF
      CALL CSEDIT(NAME(:LNAME))
      GO TO 70
*
*----!FILE or !LOG or !MAP or !LIB [[lun] [,]] filename|'filename'
*
 2    CALL CSNLEX(REC(:LR),IPOS,NAME,LNAME,LEXEM)
      IF(LEXEM.EQ.LINUM)THEN
        READ(NAME(:LNAME),'(I8)')LUN
        DO 66 K=1,4
        IF(LUNARR(K).EQ.LUN)THEN
          IF(K.NE.KEY)THEN
             NOERR=3
             GO TO 70
          ENDIF
        ENDIF
  66    CONTINUE
        IF(ISTARR(KEY).NE.0)THEN
          CLOSE(LUNARR(KEY))
          ISTARR(KEY)=0
        ENDIF
        LUNARR(KEY)=LUN
        CALL CSNLEX(REC(:LR),IPOS,NAME,LNAME,LEXEM)
        IF(NAME(:LNAME).EQ.',')THEN
          CALL CSNLEX(REC(:LR),IPOS,NAME,LNAME,LEXEM)
        ENDIF
      ENDIF
      IF(LEXEM.EQ.CCONST)THEN
        NMFILE=NAME(2:LNAME-1)
      ELSEIF(NAME.EQ.' ')THEN
        NOERR=4
        GO TO 70
      ELSE
        NMFILE=REC(IPOS-LNAME:LR)
      ENDIF
      CALL CSFLCH(NMFILE)
      GO TO (102,103,104,105),KEY-1
 102       CALL CSFILE(NMFILE)
           GO TO 70
 103   CALL CSLOG(NMFILE)
       GO TO 70
 104       CALL CSMAP(NMFILE)
           GO TO 70
 105   CALL CSLIB(NMFILE)
       GO TO 70
*
*-----!EOF
*
 6    IF(ISTFIL.NE.0)THEN
         CLOSE(LUNFIL)
         ISTFIL=0
      ENDIF
      GO TO 70
*
*-----!SHELL
*
 7    REC1=REC(IPOS:LR)
        CALL CSCLI(REC1)
      GO TO 70
*
*-----!FORTRAN
*
 8    IFORS=1
      GO TO 70
*
*-----!COMIS
*
 9    IFORS=0
      GO TO 70
*
*-----!SHOW what [what=NAMES: common_name]
*
 10   CALL CSNLEX(REC(:LR),IPOS,NAME,LNAME,LEXEM)
      IF(NAME.EQ.' ')THEN
        PRINT *,' !show parameters are:'
        PRINT *,' memory, routines, commons, names common_name.'
        PRINT *,' Ex: !show mem   or   !show names common_name '
        GO TO 70
      ENDIF
      IF(LEXEM.NE.LIDENT)THEN
        NOERR=1
        GO TO 70
      ENDIF
      CALL CLTOU(NAME(:LNAME))
      IF(SHKEYS(1)(:LNAME).EQ.NAME(:LNAME))THEN
        CALL CSSHMU
      ELSEIF(SHKEYS(2)(:LNAME).EQ.NAME(:LNAME))THEN
        CALL CSPTFS
      ELSEIF(SHKEYS(3)(:LNAME).EQ.NAME(:LNAME))THEN
        CALL CSPTCL
      ELSEIF(SHKEYS(4)(:LNAME).EQ.NAME(:LNAME))THEN
        CALL CSNLEX(REC(:LR),IPOS,NAME,LNAME,LEXEM)
        IF(LEXEM.NE.LIDENT)THEN
          NOERR=1
          GO TO 70
        ENDIF
        CALL CLTOU(NAME(:LNAME))
        CALL CSPTBL(NAME(:LNAME))
      ELSE
        NOERR=1
      ENDIF
      GO TO 70
*
*-----!NOCHECKB
*
 11   NCBARR=1
      GO TO 70
*
*-----!CHECKB
*
 12   NCBARR=0
      GO TO 70
*
*-----!NOTRACE
*
 13   NTRACE=1
      GO TO 70
*
*-----!TRACE
*
 14   NTRACE=0
      GO TO 70
*
*-----!NOPARAM
*
 15   NPARAM=1
      GO TO 70
*
*-----!PARAM
*
 16   NPARAM=0
      GO TO 70
*
*-----!HELP
*
 17   CALL CSDIRH
      GO TO 70
*
*-----!REMOVE name
*
 18   CALL CSNLEX(REC(:LR),IPOS,NAME,LNAME,LEXEM)
      IF(LEXEM.NE.LIDENT)THEN
        NOERR=4
        GO TO 70
      ENDIF
      CALL CLTOU(NAME(:LNAME))
      CALL CSCHID(NAME(:LNAME))
      IP=CSLTGP(IPVS)
      IF(IP.EQ.0)THEN
        PRINT *,' no such cs_routine'
      ELSE
        CALL CSDPRO(IP)
      ENDIF
      GO TO 70
*
*-----!CLEAR
*
 19   CALL CSDCLR
      GO TO 70
*
*-----!SETOPT string [,] what
*
 20   CONTINUE
      CALL CSNLEX(REC(:LR),IPOS,NAME,LNAME,LEXEM)
      IF(NAME.EQ.' ')THEN
        CALL CSHLOPT(' ',' ')
        GO TO 70
      ELSEIF(LEXEM.EQ.CCONST)THEN
        NMFILE=NAME(2:LNAME-1)
        LNMFIL=LNAME-2
        CALL CSNLEX(REC(:LR),IPOS,NAME,LNAME,LEXEM)
        IF(NAME(:LNAME).EQ.',')THEN
          CALL CSNLEX(REC(:LR),IPOS,NAME,LNAME,LEXEM)
        ENDIF
        IF(LEXEM.EQ.CCONST)THEN
          NAME=NAME(2:LNAME-1)
          LNAME=LNAME-2
        ELSEIF(LEXEM.EQ.LIDENT)THEN
          CONTINUE
        ELSE
          NOERR=1
          GO TO 70
        ENDIF
        CALL CSHLOPT(NMFILE(:LNMFIL),NAME(:LNAME))
      ELSE
        NOERR=1
      ENDIF
      GO TO 70
*
*-----!NOSOURCE
*
 21   ISTPM=0
      GO TO 70
*
*-----!SOURCE
*
 22   ISTPM=1
      GO TO 70
*
  70  IF(NOERR.GT.0)THEN
         CALL CSSOUT(REC(:LR))
         GO TO (71,72,73,74,75),NOERR
         PRINT *,'CSDIR: unknown error:',NOERR
         GO TO 77
  71     CALL CSSOUT('CSDIR: syntax error')
         GO TO 77
  72     CALL CSSOUT('CSDIR: Unknown directive')
         GO TO 77
  73     CALL CSSOUT('CSDIR: channel is reserved')
         GO TO 77
  74     CALL CSSOUT('CSDIR: missing parameter(s)')
         GO TO 77
  75     CALL CSSOUT('CSDIR: Ambiguous directive')
         GO TO 77
  77     CONTINUE
      ENDIF
      IF(IERR.NE.0)NOERR=IERR
      END
