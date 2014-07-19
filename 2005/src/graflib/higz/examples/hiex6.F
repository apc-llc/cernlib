*
* $Id: hiex6.F,v 1.1.1.1 1996/02/14 13:10:21 mclareni Exp $
*
* $Log: hiex6.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:21  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.15/00 10/07/92  09.47.38  by  O.Couet
*-- Author :
      SUBROUTINE HIEX6
*.==========>
*.           Example to show how to display pictures in HIGZ files
*.           Invokes the HIGZ editor.
*..=========>
      CHARACTER*10 STR
      DATA ICYCLE/999/
*
*           List contents of the ZEBRA/RZ file
*
      CALL RZLDIR(' ',' ')
*
*           Read some pictures into memory and display
*
      CALL IGSET('AURZ',0.)
      CALL IZIN('ZEBRA',ICYCLE)
      CALL IZPICT('ZEBRA','D')
      CALL IRQST(1,1,ISTAT,NCH,STR)
      CALL IZIN('TOPONIUM',ICYCLE)
      CALL IZPICT('TOPONIUM','D')
      CALL IRQST(1,1,ISTAT,NCH,STR)
*
*           Edit PICT4
*           Select options in the graphics menu
*           For example select the item ARROW in the
*           menu 'PRIMITIVES', select the type of arrow
*           by clicking in the box 'ATTR' and try to superimpose
*           a double-arrow on the picture.
*           Try to change the font and the font size for the top graphs
*           Note that the HIGZ graphics editor can be invoked
*           from PAW (PICTURE/MODIFY command).
*
      CALL IZGED('GRAPH',' ')
*
      END


