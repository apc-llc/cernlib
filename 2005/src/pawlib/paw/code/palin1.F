*
* $Id: palin1.F,v 1.2 1996/09/11 14:59:31 couet Exp $
*
* $Log: palin1.F,v $
* Revision 1.2  1996/09/11 14:59:31  couet
* - Hgetnt and Hgetn2 (old qp routines) are now replaced by hntld
*
* Revision 1.1.1.1  1996/03/01 11:38:41  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  2.03/00 18/06/93  14.40.00  by  Rene Brun
*-- Author :    J.Le Foll   01/03/90
      SUBROUTINE PALIN1(IDN,NCH,IFROM,ITOM,NVARS,INORM,IOPTP,IFLGIT)
C
C     dump of NTUPLE contents.
C
C     INPUT:  IDN, NCH, IFROM, ITO, NVARS, INORM, IOPTP, IFLGIT.
C
C     output:
C
C     IDN    = NTUPLE identifier
C     NCH    = unused
C     IFROM  = first entry to dump
C     ITOM   = last entry to dump
C     NVARS  = number of variables to be used
C     (NVARS = 0 means that first 20 variables of the NTUPLE are used.)
C     IFLGIT = 0 for interactive use, non-zero otherwise
C     INORM  = flag for normalisation of the variable
C     IOPTP  = flag for printing more results for the analysis
C
C     adapted from PASCAN1
C
C           Author : Joseph Le Foll
C
#include "hbook/hcbook.inc"
#include "hbook/hcunit.inc"
#include "paw/pcchar.inc"
      CHARACTER*1 CCHAR
      CHARACTER*8 CHS,CHS2
      LOGICAL     HNTNEW
C
      PARAMETER (KNTMAX=20)
      DIMENSION IVARJ(KNTMAX)
*
*.___________________________________________
*
      NPASS = 0
      INTFLJ = IFLGIT
C
      DO 10 I = 1,KNTMAX
         IVARJ(I) = 0
   10 CONTINUE
C
      CALL HGNPAR(IDN,'PALIN1')
      IF(LCIDN.LE.0)RETURN
*-- works only with old Ntuple for the time being
      IF (HNTNEW(IDN)) RETURN
      NDIM=IQ(LCIDN+2)
      ITO=MIN(ITOM,IQ(LCIDN+3))
      IF ( NVARS.GT.NDIM )NVARS=NDIM
      IF(NVARS.GT.KNTMAX)NVARS=KNTMAX
C
      CCHAR = ' '
C
C     read the variables
C
      IF ( INTFLJ.EQ.0 ) THEN
         DO 60 I = 1, NVARS
            IVARJ(I) = 0
   20       CONTINUE
C
C          decoding within PAW for name/number of element
C
            CALL KUGETC(CHTEMP,NCH)
            IF ( CHTEMP.EQ.'#' ) RETURN
            DO 50 INDEX = 1, NDIM
               CALL UHTOC(IQ(LCIDN+IQ(LCIDN+10)+2*(INDEX-1)),4,CHS,8)
               CALL CLTOU(CHS)
               DO 30 IND1 = 1, 7
                  IF ( CHS(IND1:IND1).NE.' ' ) THEN
                     CHS2 = CHS(IND1:)
                     GO TO 40
                  ENDIF
   30          CONTINUE
   40          CONTINUE
               IF ( CHS2.EQ.' ' ) GO TO 50
               IF ( CHS2.EQ.CHTEMP ) THEN
                  IVARJ(I) = INDEX
                  GO TO 60
               ENDIF
C
               CHS = ' '
               IF(INDEX.GE.100)THEN
                  WRITE (CHS,10200) INDEX
               ELSEIF( INDEX.GE.10 )THEN
                  WRITE (CHS,10100) INDEX
               ELSEIF( INDEX.LT.10 )THEN
                  WRITE (CHS,10000) INDEX
               ENDIF
10000      FORMAT (I1)
10100      FORMAT (I2)
10200      FORMAT (I3)
               IF ( CHS.EQ.CHTEMP ) IVARJ(I) = INDEX
   50       CONTINUE
C
            IF ( IVARJ(I).EQ.0 ) THEN
               WRITE (LOUT,10300) CHTEMP
               GO TO 20
            ENDIF
10300      FORMAT (' ==> ERROR in NTDUMP: variable ',A8,' not known')
   60    CONTINUE
      ENDIF
C
      IF ( NVARS.LE.0 ) THEN
         NVARS = KNTMAX
         IF ( NVARS.GT.NDIM ) NVARS = NDIM
         DO 70 I = 1, NVARS
            IVARJ(I) = I
   70    CONTINUE
      ENDIF
C
C  call to the reduction routines
C
      CALL LINTRA(IDN,NDIM,NVARS,IFROM,ITO,IVARJ,INORM,IOPTP)
C
   80 CONTINUE
C
      END
