*
* $Id: hkind.F,v 1.1.1.1 1996/01/16 17:07:40 mclareni Exp $
*
* $Log: hkind.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:40  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.21/09 28/02/94  15.24.54  by  Rene Brun
*-- Author :
      SUBROUTINE HKIND(IDD,KIND,CHOPT)
*.==========>
*.      returns the attributes of the histogram ID as follows:
*.      if CHOPT=' ' only KIND(1) is filled
*.      KIND(1) =
*.           -1 - unknown kind of histogram
*.            0 - ID does not exits
*.            1 - one-dim plot
*.            2 - two-dim plot
*.            3 - table
*.            4 - n-tuple
*.            8 - profile
*.
*.      if CHOPT='A' all the 32 status bits are returned in array KIND(32)
*.
*.           I1     HBOOK1
*.           I2     HBOOK2
*.           I3     HTABLE
*.           I4     NTUPLE
*.           I5     AUTOMATIC BINNING
*.           I6     VARIABLE BIN SIZE HISTOGRAM
*.           I7     HBSTAT
*.           I8     PROFILE HISTOGRAM
*.           I9     HBARX
*.           I10    HBARY
*.           I11    HERROR
*.           I12    HFUNC
*.           I13    HROTAT
*.           I14    HPRFUN
*.           I15    HPRLOW
*.           I16    HPRHIS
*.           I17    HBIGBI
*.           I18    HNORMA
*.           I19    HSCALE
*.           I20    HMAXIM
*.           I21    HMINIM
*.           I22    HINTEG
*.           I23    H2PAGE
*.           I24    H1EVLI
*.           I25    HPRSTA
*.           I26    HLOGAR
*.           I27    HBLACK
*.           I28    HSTAR
*.           I29    HPRCHA
*.           I30    HPRCON
*.           I31    HPRERR
*.
*..=========> ( R.Brun, Kees van der Poel )
#include "hbook/hcbook.inc"
#include "hbook/hcbits.inc"
      DIMENSION KIND(32)
      CHARACTER*(*) CHOPT
*.___________________________________________
      CALL HFIND(IDD,'HKIND ')
      IF(LCID .EQ. 0) THEN
         KIND(1)=0
         RETURN
      ENDIF
      CALL HDCOFL
      CALL HUOPTC(CHOPT,'A',IOPTA)
      IF(IOPTA.NE.0)THEN
         CALL UCOPY(I1,KIND,32)
         RETURN
      ENDIF
*
      IF(I1.NE.0) THEN
         IF(I8.EQ.0) THEN
*- one-dim
            KIND(1)=1
         ELSE
*- profile
            KIND(1)=8
         ENDIF
      ELSEIF(I2.NE.0)  THEN
*- two-dim
         KIND(1)=2
      ELSEIF(I3.NE.0)  THEN
*- table
         KIND(1)=3
      ELSEIF(I4.NE.0)  THEN
*- n-tuple
         KIND(1)=4
      ELSE
*- unknown
         KIND(1)=-1
      ENDIF
*
      END
