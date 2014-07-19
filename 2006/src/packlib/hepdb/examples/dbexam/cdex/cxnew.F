*
* $Id: cxnew.F,v 1.1.1.1 1996/02/28 16:23:39 mclareni Exp $
*
* $Log: cxnew.F,v $
* Revision 1.1.1.1  1996/02/28 16:23:39  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CXNEW(LINE,JPAK,IFERR)
*     ----------------------****-====-
*                        -- Author :    Boris Khomenko   10/02/94
      IMPLICIT NONE
*
      CHARACTER LINE*(*)
      INTEGER   JPAK
*
*  (ICDXDIV)
#include "cdxdiv.inc"
*
#include "cdxluns.inc"
*
#include "cdxfils.inc"
*
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CC
CC+KEEP,CDXFILS.
CC*
CC      INTEGER MCDXFIL
CC      PARAMETER (MCDXFIL = 10)
CC      COMMON /CDXFILS/ NCDXFIL,KCDXFIL,
CC     +                 ICXFSTA(10),CDXFPRF(10),CDXFTOP(10),CDXFNAM(10)
CC      INTEGER   NCDXFIL, KCDXFIL, ICXFSTA
CC      CHARACTER CDXFPRF*2, CDXFTOP*8, CDXFNAM*40
CC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
*
#include "yeaaas.inc"
*
*  -- externals --
      INTEGER JBYT , LNBLNK
      INTEGER IFSHEQC
*
*  -- initialization flag --
      INTEGER IFINIT
*  -- default/set parameters
*                  for CDNEW --
*     .... NPAIR ....
      INTEGER   LVPAIR , NDPAIR(2) , NPAIR
*
*     .... NRECS ....
      INTEGER   LVRECS , NDRECS(2) , NRECS
*
*     .... NTOP ....
      INTEGER   LVNTOP , NDNTOP(2) , NUTOP
*
*     .... LRECL ....
      INTEGER   LVLREC , NDLREC(2) , LRECL
*
*     .... CHOPT ....
      INTEGER   LVOPT
      CHARACTER CHDOPT(2)*6         , CHOPT*6
*
#if !defined(CERNLIB_IBMVM)
      STATIC  IFINIT
      STATIC    LVPAIR , NDPAIR
      STATIC    LVRECS , NDRECS
      STATIC    LVNTOP , NDNTOP
      STATIC    LVLREC , NDLREC
      STATIC    LVOPT,CHDOPT
#endif
*  -- other parameters for CDNEW --
      CHARACTER CHTOP*8 , CHFILE*40
      INTEGER   LUNDB , LUNFZ , IDIV , IRC
*  - - - - - - - - - - - - - -
      INTEGER   LVNEW ,LVOLD
      INTEGER   IPNEW , IVOLD
      REAL      RVNEW , RVOLD
      CHARACTER*6 CVNEW , CVOLD
*  ----------------------------
*
*  -- Command string analyse variables --
      INTEGER IFTX , IFNX , LPRF
      CHARACTER PRFX*8 , PRF*8
      INTEGER NUMB
      REAL    FNUM
*
*  -- Local variables --
*
      INTEGER IFOPT , IFERR , IFQUI , IFTOP
      CHARACTER XX*2 , YES*1
      CHARACTER MESL*78
      INTEGER   LMES
      INTEGER   J1 , J2 , LR , LFIL , LL
      INTEGER   KK , JJ , JE , K , J
*
      DATA IFINIT/0/
*
      IF(IFINIT.EQ.0) THEN
        IFINIT=1
*
        LVPAIR=1
        LVRECS=1
        LVNTOP=1
        LVLREC=1
        LVOPT=1
        NDPAIR(1)=1
        NDRECS(1)=5000
        NDNTOP(1)=0
        NDLREC(1)=1024
        CHDOPT(1)='F'
      ENDIF
*
      IFOPT=0
      IFERR=0
      IFQUI=0
      IFTOP=0
      YES=YEAAAS
*
   12 CONTINUE
      CALL TEXINS(LINE,JPAK,IFTX,PRFX,NUMB,FNUM)
      IFNX=JBYT(IFTX,1,2)
      CALL CLTOU(PRFX)
      LPRF=LNBLNK(PRFX)
*
*     -- An item may be:
*        Name/Value (-set parameter)  xxxx (-chtop, then ->) filename
      IF(LPRF.GT.0.AND.IFNX.EQ.3) THEN
*       -- it seems to be an item from:
*                   NPAIR, NRECS, NUTOP, LRECL
        IFOPT=IFOPT+1
*
        CALL TEXINS(LINE,JPAK,IFTX,PRF,NUMB,FNUM)
        CALL CLTOU(PRF)
*
        LVNEW=0
        IF(PRF.EQ.'=') LVNEW=1
*
        IF     (IFSHEQC(PRFX,'NP*AIR').GT.0) THEN
          CALL UXNEWIP('CDNEW ','NPair',LVNEW,NUMB,LVPAIR,NDPAIR)
*
        ELSE IF(IFSHEQC(PRFX,'NR*ECS').GT.0) THEN
          CALL UXNEWIP('CDNEW ','NRecs',LVNEW,NUMB,LVRECS,NDRECS)
*
        ELSE IF(IFSHEQC(PRFX,'NU*TOP').GT.0) THEN
          CALL UXNEWIP('CDNEW ','NUtop',LVNEW,NUMB,LVNTOP,NDNTOP)
*
        ELSE IF(IFSHEQC(PRFX,'LR*ECL').GT.0) THEN
          CALL UXNEWIP('CDNEW ','LRecl',LVNEW,NUMB,LVLREC,NDLREC)
*
        ELSE IF(IFSHEQC(PRFX,'CHO*PT').GT.0.OR.
     +          IFSHEQC(PRFX,'O*PT  ').GT.0    ) THEN
          CALL UXNEWCP('CDNEW ','ChOpt',LVNEW, PRF,LVOPT,CHDOPT)
*
        ELSE
*       -- signal an INVALID SET PARAMETER --
          CALL UXCPARM('CDNEW ',PRFX,0,' ',0,' ',' ')
        ENDIF
*
      ELSE IF(PRFX.EQ.'YES') THEN
*     -- it is a "YES" --
        YES='Y'
        CALL CDX_MESS('>>CDNEW : "YES"')
*
      ELSE IF(PRFX.EQ.'ASK') THEN
*     -- it is a "ASK" --
        YES='-'
        CALL CDX_MESS('>>CDNEW : "ASK"')
*
      ELSE IF(LPRF.GT.0) THEN
*     -- it seems to be a new file's CHTOP (CH*4?)
        IFTOP=1
      ENDIF
      IF(IFNX.GT.0.AND.IFERR+IFQUI+IFTOP.EQ.0) GOTO 12
*
      IF(IFOPT.LE.0.AND.IFTOP.LE.0) THEN
*     -- show the parameters setting --
        CALL CDX_MESS('>>CDNEW -- parameters -- <<')
        CALL UXIPARM(' ','NPair',LVPAIR,NDPAIR(LVPAIR),0,  0,NDPAIR(1))
        CALL UXIPARM(' ','NRecs',LVRECS,NDRECS(LVRECS),0,  0,NDRECS(1))
        CALL UXIPARM(' ','NUtop',LVNTOP,NDNTOP(LVNTOP),0,  0,NDNTOP(1))
        CALL UXIPARM(' ','LRecl',LVLREC,NDLREC(LVLREC),0,  0,NDLREC(1))
        CALL UXCPARM(' ','ChOpt',LVOPT ,CHDOPT(LVOPT ),0,' ',CHDOPT(1))
*     -- show the call format --
        CALL CDX_MESS('Call format:')
        CALL CDX_MESS('> CDNEW [ par/val ... ] [ chtop filename ]')
      ENDIF
*
      IF(IFTOP.GT.0.AND.IFERR+IFQUI.EQ.0) THEN
        LL=LNBLNK(PRFX)
*       -- Find a place in the CDXFILS table
        KK=0
        DO K=1,MCDXFIL
          IF(KK.EQ.0.AND.CDXFPRF(K).EQ.' ') KK=K
        ENDDO
*
        IF(KK.GT.0) THEN
*       -- OK with a place in CDXFILS table
*         -- make a specific prefix like "$x"
          XX='$'//CHAR(ICHAR('A')-1+KK)
          CHTOP =PRFX
*         -- get a file definition
          CALL TEXSTR(LINE,JPAK,IFTX,CHFILE)
          IF(IFTX/4.LE.0) THEN
            CHFILE=CHTOP(:LNBLNK(CHTOP))//'.rz'
            CALL CDX_MESS(
     +      '>>CDNEW : EMPTY filename, CHTOP//".rz" generated')
          ENDIF
          LFIL=LNBLNK(CHFILE)
*
C         ICXFSTA(KK)=-99
*         -- other call parameters --
          LUNDB=LUNAMES+KK
          NPAIR=NDPAIR(LVPAIR)
          NRECS=NDRECS(LVRECS)
          NUTOP=NDNTOP(LVNTOP)
          LRECL=NDLREC(LVLREC)
          CHOPT=CHDOPT(LVOPT)
*
          CALL CDX_MESS('>>CALL ...')
          MESL='>>CDNEW('
          WRITE(MESL(9:),1111)
     +         LUNDB,ICDXDIV,NPAIR,NRECS,NRECS,NUTOP,LRECL,CHOPT
 1111     FORMAT(I3,',CHTOP,CHFILE,',
     +           6(I5,','),'"',A5,'",IRC)')
          LMES=INDEX(MESL,')')
          CALL CDX_MESS(MESL(:LMES))
*
          CALL CDX_MESS('      lundb              '//
     +    ' idiv npair nrecs=nrfmt nutop lrecl  chopt')
*
          CALL CDX_MESS('            chtop='//CHTOP)
          CALL CDX_MESS
     +           ('                  chfile='//CHFILE(:LFIL))
*
          IF(YES.NE.'Y') CALL CDX_ANSW('OK ? (y/n , <cr>=y )',YES)
          IF(YES.EQ.' ') YES='Y'
          CALL CLTOU(YES)
*
          IF(YES.EQ.'Y') THEN
            CALL CDNEW(LUNDB,CHTOP,CHFILE,
     +                 ICDXDIV,NPAIR,NRECS,NRECS,NUTOP,LRECL,CHOPT,IRC)
*           -- report the Return Code --
            CALL UXIRCM('CDNEW ',IRC)
*           -- store file info into DB files table --
            NCDXFIL=MAX0(NCDXFIL,KK)
            ICXFSTA(KK)=IABS(IRC)+1
            CDXFPRF(KK)=XX
            CDXFTOP(KK)=CHTOP
            CDXFNAM(KK)=CHFILE
            MESL='  CDNEW >>'
            CALL UXFILLN(KK,MESL(12:),LMES)
            CALL CDX_MESS(MESL(:LMES+11))
          ELSE
            CALL CDX_MESS('>>CDNEW not called (NOK)')
          ENDIF
        ELSE
          CALL CDX_MESS('>>CDNEW: too many DBFiles')
        ENDIF
      ENDIF
      END
