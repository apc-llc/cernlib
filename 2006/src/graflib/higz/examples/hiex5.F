*
* $Id: hiex5.F,v 1.1.1.1 1996/02/14 13:10:21 mclareni Exp $
*
* $Log: hiex5.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:21  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.15/00 10/07/92  09.47.38  by  O.Couet
*-- Author :
      SUBROUTINE HIEX5
*.==========>
*.           Example using HIGZ and GKS metafiles
*..=========>
*
*
*          Open HIGZ metafile
*          and repeat previous examples
*
      PRINT *,' WRITING HIGZ METAFILE'
      CALL IGZSET('Z')
      CALL IZOPEN(1,'Pictures','higz.rz','AN',1024,ISTAT)
      CALL IZPICT('ZEBRA','M')
      CALL HIEX1
      CALL IZPICT('SOFT-TABLE','M')
      CALL HIEX2
      CALL IZPICT('TOPONIUM','M')
      CALL HIEX3
      CALL IZPICT('GRAPH','M')
      CALL HIEX4
      CALL IZOUT('GRAPH',ICYCLE)
      CALL IGSA (1)
*
*          Open GKS metafile
*          and repeat previous examples
*
      PRINT *,' WRITING GKS METAFILE (BE PATIENT !)'
      CALL IGZSET('G')
      CALL IGMETA(-10,0)
      CALL HIEX1
      CALL HIEX2
      CALL HIEX3
      CALL HIEX4
      CALL IGMETA(0,0)
*
      END
