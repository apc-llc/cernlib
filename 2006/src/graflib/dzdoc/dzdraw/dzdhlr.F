*
* $Id: dzdhlr.F,v 1.1.1.1 1996/03/04 16:13:17 mclareni Exp $
*
* $Log: dzdhlr.F,v $
* Revision 1.1.1.1  1996/03/04 16:13:17  mclareni
* Dzdoc/Zebpack
*
*
#include "dzdoc/pilot.h"
      SUBROUTINE DZDHLR
#include "zebra/zunit.inc"
      IF(IQPRNT.EQ.6)CALL IGTERM
      WRITE(*,'(10(A/))')
     &' -----------------------------------------',
     &' Directories are displayed like diamond shaped',
     &' boxes, keys as rectangular boxes',
     &' the sequence number and cycle of keys are',
     &' displayed left of the shaded box (S SNr, C CNr)',
     &' if the key contains a vector its marked with a V',
     &' if not all keys are displayed, the most recent ones',
     &' are shown'
 
      WRITE(*,'(15(A/))')
     &' -----------------------------------------',
     &' Activate locator at the following places:',
     &' -----------------------------------------',
     &' In shaded diamond     => go down the tree',
     &' In Up-dir diamond     => go up the tree',
     &' In the big dir-box    => list directory',
     &' In the big key-box    => input d/s (call RZIN',
     &' Above key-box         => list complete Key info',
     &' In Continue box       => continue drawing dirs/keys',
     &'                          keys or dirs may be skipped by',
     &'                          modifying Key/Dir-SeqNr'
      END
***********************************************************************
