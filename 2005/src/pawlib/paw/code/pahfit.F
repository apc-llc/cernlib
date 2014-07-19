*
* $Id: pahfit.F,v 1.4 1997/04/24 07:06:26 couet Exp $
*
* $Log: pahfit.F,v $
* Revision 1.4  1997/04/24 07:06:26  couet
* - remove the CALL CLTOU before CSEXPR. This call is now done in CSEXPR itself
*
* Revision 1.3  1997/04/23 15:01:09  couet
* - a # was added at the end of the string given as parameter to csexpr. This
* was useless because csexpr add the # itself. These caused errors like:
*
*  CS-TR-ERR: routine _001, line    0
*  FUNCTION _001(X,Y,Z) _001=X**2# ^ #
*   syntax error
*
* Revision 1.2  1996/10/21 16:32:01  couet
* - New functions to compile comis program before calling csexpr
*
* Revision 1.1.1.1  1996/03/01 11:38:40  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  2.07/19 04/12/95  18.09.23  by  O.Couet
*-- Author :
      SUBROUTINE PAHFIT
*
*           /HISTOGRAM/FIT
*           Fitting and Smoothing
*
#include "hbook/hcbook.inc"
#include "paw/pawcom.inc"
#include "hbook/hcfitr.inc"
#include "hbook/hcbits.inc"
#include "paw/pcpatl.inc"
#include "paw/pcchar.inc"
#include "paw/pcbuff.inc"
#include "paw/quest.inc"
#include "paw/pawcfu.inc"
*
      EXTERNAL PAWFU1
*
      COMMON/PAWPAR/DPAR(100)
      DOUBLE PRECISION DPAR
*
      CHARACTER*80 CHMESS
      CHARACTER*4 CHPAR
      CHARACTER*4 CHOPT
      INTEGER NPAR, NDF
      DIMENSION VALP(50),SIGPAR(50)
      DIMENSION COV(1000),PMIN(50),PMAX(50),STEP(50)
      EQUIVALENCE (PAWBUF(1),VALP(1)),(PAWBUF(101),SIGPAR(1))
      EQUIVALENCE (PAWBUF(501),COV(1)),(PAWBUF(1501),PMIN(1))
      EQUIVALENCE (PAWBUF(1601),PMAX(1)),(PAWBUF(1701),STEP(1))
      EXTERNAL PAWFUD
      DOUBLE PRECISION PAWFUD
C.
C.    ------------------------------------------------------------------
C.
      CALL KUPATL(CHPATL,NPAR)
*
      IF(CHPATL.EQ.'SMOOTH')THEN
         CALL KUGETC(CHID,N)
         IDOLD=1
         CALL HGETID(CHID)
         IDOLD=0
         IF(LCID.LE.0)GO TO 999
         CALL KUGETC(CHOPT,N)
         CALL KUGETR(SENSIT)
         CALL KUGETR(SMOOTH)
         CALL PAHSMO(ID,CHOPT,SENSIT,SMOOTH,NPAR,CHI2,NDF,IERSMO)
         IF (IERSMO .NE. 0) THEN
            WRITE (CHMESS, 10000) IERSMO
            CALL HBUG (CHMESS, 'PAHFIT', ID)
         END IF
         CALL HSETCD
         GO TO 999
      ENDIF
*
      IF(CHPATL.EQ.'SPLINE')THEN
         CALL KUGETC(CHID,N)
         CALL HGETID(CHID)
         IF(LCID.LE.0)GO TO 999
         CALL HNOENT(ID,N)
         IF(N.LE.0)GO TO 999
         CALL KUGETI(IC)
         CALL KUGETI(N)
         CALL KUGETI(IKX)
         CALL HDCOFL
         IF(I230.EQ.0)THEN
            CALL HSPLI1(ID,IC,N,IKX,CHI2)
         ELSE
            CALL HSPLI2(ID,N,N,IKX,IKX)
         ENDIF
         CALL HSETCD
         GO TO 999
      ENDIF
*
      IF(CHPATL.EQ.'FUNCTION')THEN
         CALL KUGETC(CHID,N)
         CALL HGETID(CHID)
         IF(LCID.LE.0)GO TO 999
         CALL KUGETF(CHTEMP,NCH)
         CALL PAWCS
         CALL PAWFCA (CHTEMP,NCH,JADF,0)
         IF (JADF.LE.0) THEN
            CALL PAEXPR(IERR)
            IF (IERR.NE.0) GOTO 999
            IF (NCH.GT.NCHAR-1) THEN
               PRINT*, '*** String is too long'
               GOTO 999
            ENDIF
            CALL CSEXPR (CHTEMP,JADF)
            IF (JADF.LE.0) GOTO 999
         ENDIF
         CALL HFUNC(ID,PAWFU1)
         CALL HSETCD
         GO TO 999
      ENDIF
*
      IF(CHPATL.EQ.'PARAM')THEN
         CALL KUGETC(CHID,N)
         CALL HGETID(CHID)
         IF(LCID.LE.0)GO TO 999
         CALL KUGETI(IC)
         CALL KUGETR(R2MIN)
         CALL KUGETI(MAXPOW)
         CALL KUOPEN(89,'FPARAM.DAT','UNKNOWN',ISTAT)
         CALL HSETPR('PLUN',89.)
         CALL KUALFA
         CALL HPARAM(ID,IC,R2MIN,MAXPOW,DPAR,VALP,NCO)
         CALL PACLOS(89)
         CALL HSETCD
         GO TO 999
      ENDIF
*
      IF(CHPATL.EQ.'HSETPR')THEN
         CALL KUGETC(CHPAR,NCH)
         CALL KUGETR(VALUE)
         CALL HSETPR(CHPAR,VALUE)
      ENDIF
*
10000 FORMAT (1X, 'Error', I5, ' during smoothing.')

  999 END
