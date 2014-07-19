*
* $Id: kxlisv.F,v 1.4 1997/03/11 09:43:44 cremel Exp $
*
* $Log: kxlisv.F,v $
* Revision 1.4  1997/03/11 09:43:44  cremel
* Bug correction in command WAIT (after answering "Q" to a "WAIT" statement,
* the ability to issue several HISTOGRAM/PLOT statements on a single line,
* separated by semicolons, disappears).
* kexec.c : add a test on the variable "kc_flags.in_macro".
* kuvers.c : increase version number to 2.07/13
*
* Revision 1.3  1997/02/18 16:53:11  cremel
* Bug correction i(can crash with Paw++) for the command "VECTOR/LIST" i
* (UHTOC(IQ(LVECN+9),4,TYPE,1) instead of UHTOC(IQ(LVECN+9),4,VTYPE,4))
* and reformatting of the output to make it look better.
*
* Revision 1.2  1996/10/01 07:31:18  gunter
* Use correct types of variables to print; ie. vector name and type are
* first coverted to CHARACTER using uhtoc, and then printed. Printing
* integer with A format can crash a program on AIX with f77/f90, if env.
* variable XLFRTEOPTS=err_recovery=no:langlvl=90std
*
* ----------------------------------------------------------------------
*
* Revision 1.1.1.1  1996/03/08 15:32:54  mclareni
* Kuip
*
*
#include "kuip/pilot.h"
*CMZ :  2.00/11 21/01/93  11.39.08  by  Alfred Nathaniel
*-- Author :
      SUBROUTINE KXLISV
*
********************************************************************************
*
* Execution routine for command '/VECTOR/LIST'
*
* N.B. See KUVECT for description of IQUEST (Q or IQ)
*
********************************************************************************
*
#include "kuip/kcgen.inc"
#include "kuip/kcvect.inc"
      CHARACTER*32 VNAME
      CHARACTER*1 TYPE
      CALL KUALFA
      IF (NUMVEC.LE.1) THEN
        PRINT *,'*** No vector defined'
      ELSE
C       PRINT *,'Vector Name                       Type    Length'//
C    +          '    Dim-1    Dim-2    Dim-3'
        WRITE (6,1010) 'Vector Name                     ', 'Type', 
     +    'Length', ' Dim-1', ' Dim-2', ' Dim-3'
        PRINT *
        LVECN=LQ(LVEC-1)
        LVECN=LQ(LVECN)
        DO 10 I=2,NUMVEC
          NDIMS=IQ(LVECN+14)
          NNN=10+NDIMS
          IF (NDIMS.EQ.1.AND.IQ(LVECN+11).EQ.1) NNN=10
          CALL UHTOC(IQ(LVECN+1),4,VNAME,32)
          CALL UHTOC(IQ(LVECN+9),4,TYPE,1)
          WRITE (6,1000) VNAME,TYPE,(IQ(LVECN+J),J=10,NNN)
1000      FORMAT (1X,A32,8X,A1,1X,I6,3(3X,I6))
1010      FORMAT (1X,A32,5X,A4,1X,A6,3(3X,A6))
          LVECN=LQ(LVECN)
10      CONTINUE
        PRINT *
        NNN=NUMVEC-1
        PRINT *,'Total of ',NNN,' Vector(s)'
      ENDIF
999   END



