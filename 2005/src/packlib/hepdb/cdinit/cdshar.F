*
* $Id: cdshar.F,v 1.1.1.1 1996/02/28 16:24:15 mclareni Exp $
*
* $Log: cdshar.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:15  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDSHAR(LUNDB,LUNFZ,CHTOP,CHFILE,LRECL,IDIV,CHOPT,IRC)
 
      CHARACTER*(*) CHTOP,CHFILE,CHOPT
 
      CHARACTER*20  CHRZOP,CHCDOP
 
      CHARACTER*10  RZOPTS
      CHARACTER*13  CDOPTS
 
      PARAMETER     (NRZOPT=10)
      PARAMETER     (NCDOPT=13)
 
      DIMENSION     IOPTRZ(NRZOPT)
      DIMENSION     IOPTCD(NCDOPT)
 
      DATA          RZOPTS/'LNSU1WYCXP'/
      DATA          CDOPTS/'SU1LDCXMZQEPA'/
*
*     As CDUPDT, but forcing Shared Update mode (i.e. locks)
*
      IRC = 0
*
*     Build options for RZOPEN: X default, N means native, not NEW
*
      CALL UOPTC(CHOPT,RZOPTS,IOPTRZ)
 
      IOPTN = IOPTRZ(2)
      IF(IOPTN.EQ.0) IOPTRZ(9) = 1
*
*     Compulsory options: Shared, Update
*
      IOPTRZ(3) = 1
      IOPTRZ(4) = 1
*
*     Forbidden options: New
*
      IOPTRZ(2) = 0
 
      CHRZOP = ' '
      J      = 0
 
      DO 10 I=1,NRZOPT
         IF(IOPTRZ(I).NE.0) THEN
            J = J + 1
            CHRZOP(J:J) = RZOPTS(I:I)
         ENDIF
   10 CONTINUE
*
*     Open the database file
*
      CALL RZOPEN(LUNDB,CHTOP,CHFILE,CHRZOP,LRECL,IRC)
 
      IF(IRC.NE.0) GOTO 99
*
*     Build options for CDINIT
*
      CALL UOPTC(CHOPT,CDOPTS,IOPTCD)
      IF(IOPTN.EQ.0) IOPTCD(7) = 1
*
*     Public mode is forbidden
*
      IOPTCD(12) = 0
*
*     Shared update mode is compulsory
*
      IOPTCD(1)  = 1
      IOPTCD(2)  = 1
 
      CHCDOP = ' '
      J      = 0
 
      DO 20 I=1,NCDOPT
         IF(IOPTCD(I).NE.0) THEN
            J = J + 1
            CHCDOP(J:J) = CDOPTS(I:I)
         ENDIF
   20 CONTINUE
*
*     Initialise HEPDB for this file
*
      CALL CDINIT(IDIV,LUNDB,LUNFZ,CHTOP,NPAIR,NRECS,NTOP,CHCDOP,IRC)
 
99    END
 
