*
* $Id: cdmdir.F,v 1.1.1.1 1996/02/28 16:24:16 mclareni Exp $
*
* $Log: cdmdir.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:16  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDMDIR (PATH, NKEX, CHFOR, CHTAG, MXKP, IPREC, DELTA,
     +                   CHOPT, IRC)
*     ================================================================
*
************************************************************************
*                                                                      *
*        SUBR. CDMDIR (PATH, NKEX, CHFOR, CHTAG, MXKP, IPREC, DELTA,   *
*                      CHOPT, IRC*)                                    *
*                                                                      *
*   Creates a HEPDB directory with standard keys plus NKEX extra keys  *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     PATH     Character string describing the pathname                *
*     NKEX     Number of user keys                                     *
*     CHFOR    Character string specifying the user key type           *
*     CHTAG    String of 8-character tags for the user key elements    *
*     MXKP     Maximum number of objects in a partitioned directory    *
*     IPREC    Precision word sepcifying the number of significant     *
*              digits to be stored; (If IPREC > 0, data are stored     *
*              with IPREC significant digits right to the decimal      *
*              points; if IPREC < 0, data are stored with IPREC        *
*              insignificant digits left to the decimal point.)        *
*     DELTA    Variable specifying the absolute value below which data *
*              is treated as zero                                      *
*     CHOPT    Character string with any of the following characters   *
*          C   Data in the directory will be compressed by default     *
*          P   Create partitioned subdirectories for the pathname      *
*     IRC      Return code (see below)                                 *
*                                                                      *
*   Called by user                                                     *
*                                                                      *
*   Error Condition :                                                  *
*                                                                      *
*     IRC       =  0 : No error                                        *
*               = 43 : Illegal number of user keys                     *
*               = 44 : Cannot find the top directory name              *
*                      (wrong initialisation)                          *
*               = 45 : Illegal Path name                               *
*                                                                      *
************************************************************************
*
#include "hepdb/caopti.inc"
#include "hepdb/caopts.inc"
#include "hepdb/cdcblk.inc"
#include "hepdb/ckkeys.inc"
#include "hepdb/ctpath.inc"
      CHARACTER*(*)   PATH, CHTAG(*), CHFOR, CHOPT
      CHARACTER       PATHN*80, CFOR*100
*
*     ------------------------------------------------------------------
*
* *** Decode the character option
*
      CALL CDOPTS (CHOPT, IRC)
      IF (IRC.NE.0) GO TO 999
      KEY7CK = 0
*
* *** Remove imbedded blanks from the path name
*
      CALL CDSBLC (PATH, PAT2CT, NCH)
      IF (NCH.EQ.0) THEN
        IRC    = 45
#if defined(CERNLIB__DEBUG)
        IF (IDEBCD.GT.0) THEN
          PAT2CT = PATH
          CALL CDPRNT (LPRTCD, '(/,'' CDMDIR : Illegal path name '//
     +         PAT2CT//''')', IARGCD, 0)
        ENDIF
#endif
        GO TO 999
      ENDIF
*
* *** Load top directory information
*
      IF (PAT2CT(1:2).EQ.'//')  THEN
        CALL CDTOPN (PAT2CT, TOP2CT, NCHAR)
        PAT3CT = '//'//TOP2CT(1:NCHAR)
        CALL CDLDUP (PAT3CT,  1, IRC)
      ELSE IF (PAT2CT(1:1).EQ.'~' .OR. PAT2CT(1:1).EQ.'_') THEN
        CALL RZNDIR (PAT3CT, 'R')
        IF (IQUEST(1).NE.0) THEN
          IRC    = 44
#if defined(CERNLIB__DEBUG)
          IF (IDEBCD.GT.0) CALL CDPRNT(LPRTCD, '(/,'' CDMDIR : Cannot'//
     +       ' find the top directory '')', IARGCD, 0)
#endif
          GO TO 999
        ENDIF
        CALL CDLDUP (PAT3CT,  1, IRC)
      ELSE
        CALL CDLDUP (PAT2CT, -1, IRC)
      ENDIF
      IF (IRC.NE.0)  GO TO 999
*
* *** Get the complete path name
*
      IF (PAT2CT(1:2).EQ.'//')  THEN
*
        PATHN  = PAT2CT
      ELSE IF (PAT2CT(1:1).EQ.'/') THEN
*
        PATHN  = '//'//TOPNCD(1:NCHRCD)//PAT2CT
#if defined(CERNLIB_APOF77)||defined(CERNLIB_BSLASH)||defined(CERNLIB_HPUX)||defined(CERNLIB_IBMRT)||defined(CERNLIB_QMALPH)||defined(CERNLIB_QMVAOS)
      ELSE IF (PAT2CT(1:1).EQ.'\\') THEN
#endif
#if (!defined(CERNLIB_APOF77))&&(!defined(CERNLIB_BSLASH))&&(!defined(CERNLIB_HPUX))&&(!defined(CERNLIB_IBMRT))&&(!defined(CERNLIB_QMALPH))&&(!defined(CERNLIB_QMVAOS))
      ELSE IF (PAT2CT(1:1).EQ.CSTRCA(92:92)) THEN
#endif
        IL     = 0
        DO 10 I0 = 1, MAXLCD
          IF (PAT1CT(I0:I0).EQ.'/') IL = I0
   10   CONTINUE
        IF (IL.LE.1) THEN
          IRC    = 45
#if defined(CERNLIB__DEBUG)
          IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDMDIR : '//
     +       'Illegal path name '//PAT2CT//''')', IARGCD, 0)
#endif
          GO TO 999
        ENDIF
        PATHN  = PAT1CT(1:IL-1)//'/'//PAT2CT(2:NCH)
      ELSE IF (PAT2CT(1:1).EQ.'~' .OR. PAT2CT(1:1).EQ.'_') THEN
*
        IL     = LENOCC (PAT1CT)
        PATHN  = PAT1CT(1:IL)//'/'//PAT2CT(2:NCH)
      ELSE
*
        IL     = LENOCC (PAT1CT)
        PATHN  = PAT1CT(1:IL)//'/'//PAT2CT(1:NCH)
      ENDIF
*
* *** Check the number of user keys
*
      IF (NKEX.LT.0.OR.NKEX+2*NPARCD.GT.MXDMCK-NOF2CK) THEN
        IRC        = 43
        IQUEST(11) = NKEX
        IQUEST(12) = MXDMCK - NOF2CK - 2*NPARCD
#if defined(CERNLIB__DEBUG)
        IF (IDEBCD.GT.0) THEN
          CALL CDPRNT (LPRTCD, '(/,'' CDMDIR : Illegal number of user'//
     +         ' keys '',2I12,'' for '',/,''        '//PAT2CT//''')',
     +         IQUEST(11), 2)
        ENDIF
#endif
        GO TO 999
      ENDIF
*
* *** Prepare the Tag and Type of the keys
*
      NSYS   = NOF2CK + NPARCD * 2
      CFOR   = CHFTCK(1:NSYS)
      DO I = 1, NSYS
        CTAGCK(I) = CHTGCK(I)
      ENDDO
      DO I = 1, NPARCD
        CFOR(NOF1CK+2*I-1:NOF1CK+2*I-1) = 'I'
        CFOR(NOF1CK+2*I  :NOF1CK+2*I  ) = 'I'
        CTAGCK(NOF1CK+2*I-1) = 'STR_VAL'//CALFCA(27+I)
        CTAGCK(NOF1CK+2*I)   = 'END_VAL'//CALFCA(27+I)
      ENDDO
      DO K = 1, NKEX
        CTAGCK(NSYS+K) = CHTAG(K)
        CFOR(NSYS+K:NSYS+K) = CHFOR(K:K)
      ENDDO
      NWKEY  = NOF2CK + 2*NPARCD + NKEX
*
* *** Now create the directory
*
      IF (MXKP.LE.0) THEN
        MXKPU  = MXKPCK
      ELSE
        MXKPU  = MXKP
      ENDIF
      CALL CDMKDI (PATHN, NWKEY, CFOR, CTAGCK, MXKPU, IPREC, DELTA,
     +             CHOPT, IRC)
*                                                             END CDMDIR
  999 END
