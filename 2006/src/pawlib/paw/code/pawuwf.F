*
* $Id: pawuwf.F,v 1.4 1998/09/02 09:52:44 couet Exp $
*
* $Log: pawuwf.F,v $
* Revision 1.4  1998/09/02 09:52:44  couet
* - Bug fixed in case of option P. The previous version was not working properly.
*
* Revision 1.3  1997/05/13 11:54:17  couet
* - call csinc1
*
* Revision 1.2  1996/09/11 14:59:35  couet
* - Hgetnt and Hgetn2 (old qp routines) are now replaced by hntld
*
* Revision 1.1.1.1  1996/03/01 11:38:44  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :          13/02/96  16.02.40  by  O.Couet
*-- Author :    Rene Brun   03/01/89
      SUBROUTINE PAWUWF
*
*        Generates skeleton for a user selection function
*
#include "hbook/hcbook.inc"
#include "paw/pawcom.inc"
#include "paw/pcchar.inc"
#include "paw/quest.inc"
#include "paw/pcslas.inc"
#include "paw/pntold.inc"
#include "paw/pawlun.inc"
      DIMENSION    IOPT(3)
      EQUIVALENCE (IOPTE,IOPT(1)),(IOPTP,IOPT(2)),(IOPTT,IOPT(3))
      CHARACTER*11 CHTAGS(6)
      CHARACTER*9  CHVAR(6)
      CHARACTER*8  CHVART,CHOPT,CHVAR1
      CHARACTER*1  CHS,QUOTE
      LOGICAL      NTNEW
*
      SAVE        QUOTE
      DATA        QUOTE  /''''/
*
*     called by UWFUNC
*     ================
      IF(IQUEST(99).NE.-1)THEN
         CALL KUGETC(CHID,N)
         CALL HNTLD(CHID)
         IF(IQUEST(1).LT.0)GO TO 99
         IF(ID.NE.0.AND.LCID.LE.0)GO TO 99
         CALL KUGETF(CHTITL,NCH)
         IF(NCH.LE.0)GO TO 99
         CALL KUGETC(CHOPT,N)
      ELSE
*
*     called by COMIS
*     ===============
         CALL CSINC1(CHTITL)
         NCH=LENOCC(CHTITL)
         CHOPT=' '
      ENDIF
      CALL UOPTC(CHOPT,'EPT',IOPT)
      NTNEW = .NOT.NTOLD
      IF (NTNEW) THEN
         IF (IOPTP .NE. 0)
     +      PRINT *,' UWFUNC: option P not valid for new Ntuple.'
         IF (IOPTT .NE. 0)
     +      PRINT *,' UWFUNC: option T not valid for new Ntuple.'
         IOPTP  = 0
         IOPTT  = 0
         ITRUNC = 0
      ENDIF
*
*           Check for .
*
      IOPTI=0
      IP=INDEX(CHTITL,'.')
      IF(IP.GT.0)THEN
         IF(IP.EQ.NCH)THEN
            NCH=NCH+1
            CHTITL(NCH:NCH)='F'
         ENDIF
         IF(CHTITL(IP+1:IP+1).NE.'F'.AND.CHTITL(IP+1:IP+1).NE.'f')THEN
            IOPTI=1
            IOPTP  = 0
            IOPTT  = 0
            ITRUNC = 0
         ENDIF
         CHFILE=CHTITL
         CHTEMP=CHTITL(1:IP-1)//'()'
         NCHF=IP-1
      ELSE
#if defined(CERNLIB_UNIX)
         CHFILE=CHTITL(1:NCH)//'.F'
#endif
#if defined(CERNLIB_APOLLO)
         CHFILE=CHTITL(1:NCH)//'.FTN'
#endif
#if defined(CERNLIB_VAX)
         CHFILE=CHTITL(1:NCH)//'.FOR'
#endif
#if defined(CERNLIB_IBM)
         CHFILE=CHTITL(1:NCH)//'.FORTRAN'
#endif
         CHTEMP=CHTITL(1:NCH)//'()'
         NCHF=NCH
      ENDIF
*
      CALL PALUNF(70,3,LUN)
      IF(LUN.EQ.0)GO TO 99
#if defined(CERNLIB_APOLLO)||defined(CERNLIB_UNIX)
*        CALL CUTOL(CHFILE)
#endif
*
      CALL KUOPEN(LUN,CHFILE,'UNKNOWN',ISTAT)
      IF(ISTAT.NE.0)GO TO 99
      LUNIT(LUN)=9
*
      IF (NTNEW) THEN
         CHOPT='P'
         IF(IOPTI.NE.0)CHOPT='PI'
         CALL HUWFUN(LUN, ID, CHTEMP(1:NCHF), ITRUNC,CHOPT)
         GOTO 30
      ENDIF
*
      NCH=LENOCC(CHTEMP)
      IF(IOPTI.EQ.0)THEN
         WRITE(LUN,1000)CHTEMP(1:NCH)
      ELSE
         WRITE(LUN,1001)
      ENDIF
      NVAR=IQ(LCID+2)
      ITAG1=IQ(LCID+10)
      KLOOP=2
      IF(IOPTP.NE.0.OR.IOPTT.NE.0)THEN
         KLOOP=3
      ENDIF
      DO 20 K=1,KLOOP
         DO 10 I=1,NVAR,6
            IF(I+5.GT.NVAR)THEN
               JMAX=MOD(NVAR,6)
            ELSE
               JMAX=6
            ENDIF
            DO 5 J=1,JMAX
               IVAR=I+J-1
               J1=2*I+2*J-4
               CALL UHTOC(IQ(LCID+ITAG1+J1),4,CHVART,8)
               IF(CHVART.EQ.' ')THEN
                  IF(IVAR.LT.10)THEN
                     WRITE(CHVART,5100)IVAR
                  ELSEIF(IVAR.LT.100)THEN
                     WRITE(CHVART,5200)IVAR
                  ELSE
                     WRITE(CHVART,5300)IVAR
                  ENDIF
               ENDIF
               IF (J1.EQ.0) CHVAR1=CHVART
               IF(K.EQ.3)THEN
                  IF(IVAR.LT.NVAR)THEN
                     CHTAGS(J)=QUOTE//CHVART//QUOTE//','
                  ELSE
                     CHTAGS(J)=QUOTE//CHVART//QUOTE//'/'
                  ENDIF
               ENDIF
               IF(IVAR.LT.NVAR)THEN
                  CHVAR(J)=CHVART//','
               ELSE
                  CHVAR(J)=CHVART
               ENDIF
               DO 2 L=1,8
                  CHS=CHVAR(J)(L:L)
                  IF(    CHS.EQ.'('.OR.CHS.EQ.')'
     +               .OR.CHS.EQ.'/'.OR.CHS.EQ.BSLASH
     +               .OR.CHS.EQ.'+'.OR.CHS.EQ.'-'
     +               .OR.CHS.EQ.'*'.OR.CHS.EQ.'.')THEN
                     IF(L.EQ.8)THEN
                        CHVAR(J)(L:L)=' '
                     ELSE
                        CHVAR(J)(L:L)='x'
                     ENDIF
                  ENDIF
   2           CONTINUE
   5        CONTINUE
*
            IF(K.NE.3)THEN
               WRITE(LUN,2000)(CHVAR(L),L=1,JMAX)
            ELSE
               WRITE(LUN,2000)(CHTAGS(L),L=1,JMAX)
            ENDIF
  10     CONTINUE
         IF(K.EQ.1) THEN
            WRITE(LUN,2500)
            WRITE(LUN,3000)
         ENDIF
         IF(K.EQ.2.AND.KLOOP.EQ.3)THEN
            WRITE(LUN,3010)NVAR,CHVAR1,NVAR
         ENDIF
  20  CONTINUE
*
      IF(IOPTP.NE.0)THEN
         WRITE(LUN,3500)CHTEMP(1:NCHF),NVAR
      ELSE
         IF(IOPTI.EQ.0)WRITE(LUN,4000)CHTEMP(1:NCHF)
      ENDIF
*
  30  CALL PACLOS(LUN)
      LUNIT(LUN)=0
*
*          Edit file if option 'E'
*
      IF(IOPTE.NE.0)CALL KUEDIT(CHFILE,ISTAT)
*
 1000 FORMAT(6X,'REAL FUNCTION ',A,/,6X,'REAL')
 1001 FORMAT(6X,'REAL')
 2000 FORMAT(5X,'+',6(A))
 2500 FORMAT(
     + '*',/,
     + '      LOGICAL         CHAIN',/,
     + '      CHARACTER*128   CFILE',/,
     + '*',/,
     + '      COMMON /PAWCHN/ CHAIN, NCHEVT, ICHEVT',/,
     + '      COMMON /PAWCHC/ CFILE',/,
     + '*')
 3000 FORMAT(6X,'COMMON/PAWIDN/IDNEVT,OBS(13),')
 3010 FORMAT(6X,'DIMENSION XDUMMY(',I3,')',/,
     +6X,'EQUIVALENCE (XDUMMY(1),',A,')',/,
     +6X,'CHARACTER*8 CHTAGS(',I3,')',/,6X,'DATA CHTAGS/')
 3500 FORMAT('*',/,6X,A,'=1.',/,
     +6X,'PRINT 1000,IDNEVT',/,
     +6X,'DO 10 I=1,',I3,/,
     +9X,'PRINT 2000,I,CHTAGS(I),XDUMMY(I)',/,
     +'  10  CONTINUE',/,'*',/,
     +' 1000 FORMAT(8H IDNEVT=,I5)',/,
     +' 2000 FORMAT(5X,I3,5X,A,1H=,G14.7)',/,
     +6X,'END')
 4000 FORMAT('*',/,6X,A,'=1.',/,6X,'END')
 5100 FORMAT('V_',I1,5X)
 5200 FORMAT('V_',I2,4X)
 5300 FORMAT('V_',I3,3X)
  99  END
