*
* $Id: fmbook.F,v 1.1.1.1 1996/03/07 15:18:01 mclareni Exp $
*
* $Log: fmbook.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:01  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMBOOK (GENEN, KEYS, LADDR, LSUP, JBIAS, IRC)
*
************************************************************************
*
*        SUBR. FMBOOK (GENEN, KEYS, LADDR*,*LSUP*, JBIAS, IRC*)
*
*
#include "fatmen/fatbank.inc"
#include "fatmen/fatpara.inc"
*
      CHARACTER       GENEN*(*)
      DIMENSION       LSUP(9)
*
      PARAMETER     (LKEYFA=10)
      DIMENSION     INKEYS(LKEYFA),KEYS(LKEYFA)
      CHARACTER*8   CHTAG(LKEYFA)
      CHARACTER*10  CHFOR
      DATA CHTAG/'Num.Id.',5*'Fname'
     +          ,'cp.level','loc.code','medium','nm.banks'/
      DATA CHFOR/'IHHHHHIIII'/
*
*______________________________________________________________________
      IRC  = 0
      NCH  = LENOCC(GENEN)
      NCHD = INDEXB(GENEN(1:NCH),'/') -1
*
* ***  Create the bank
*
      CALL MZBOOK (IDIVFA, LADDR, LSUP, JBIAS, 'DSFA', 0, 0, NWDSFA,
     +             IODSFA, 0)
*
*     Zero/blank it according to I/O characteristic
*
      CALL DZZERO(IDIVFA,LADDR)
      IQUEST(1) = 0
*
*     and fill it in ...
*
      CALL FAFILL(LADDR+KOFUFA,GENEN(1:NCHD),GENEN(NCHD+2:NCH),
     +KEYS,'DISK','U')
*
*     Check if this generic name already exists in RZ file
*
      CALL FMEXST(GENEN(1:NCH),IRET)
      IQUEST(11) = IRET
*
*     Check if this directory already exists
*
      CALL FACDIR(GENEN(1:NCHD),' ')
      IF((IQUEST(1).NE.0).AND.(IDEBFA.GE.1)) PRINT *,
     +  'FMBOOK. directories will be automatically created by server'
      IQUEST(12) = IQUEST(1)
 
      END
 
