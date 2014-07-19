*
* $Id: dzdhlp.F,v 1.1.1.1 1996/03/04 16:13:07 mclareni Exp $
*
* $Log: dzdhlp.F,v $
* Revision 1.1.1.1  1996/03/04 16:13:07  mclareni
* Dzdoc/Zebpack
*
*
#include "dzdoc/pilot.h"
      SUBROUTINE DZDHLP
      WRITE(*,'(10(A/))')
     &' -----------------------------------------',
     &' Activate locator at the following places:',
     &' -----------------------------------------',
     &' In shaded bank center => go down the tree',
     &' In Up-bank box:       => go up the tree',
     &' In Next-link arrow    => step in linear structure',
     &' Above shaded center   => display documentation',
     &'                          if no doc exists print a template',
     &' Just left of center   => display system words',
     &' In the big data box   => DZSHOW the bank',
     &' Above big data box    => display data words with doc',
     &' In Continue box       => continue drawing down banks'
      WRITE(*,'(10(A/))')
     &' ------------',
     &' Doc Version:',
     &' ------------',
     &' An INTEGER number >= 0',
     & 'If it is >0 then:',
     &' Decode the version field in the bank documentation',
     &' when showing data words with doc and try to find requested one',
     &' Possible dots in the documentation are ignored,',
     &' i.e. 1.4.5 is equivalent to 145',
     &' When displaying documentation all versions will be shown'
 
      WRITE(*,'(10(A/))')
     &' ----------------------------------',
     &' Parameters (DZSHOW, DZDDWD, DZDTMP):',
     &' ----------------------------------',
     &' DZDOC-Opt:',
     &'    character option,see ZEBRA-manual (Z=hexadecimal)',
     &'    plus the following:',
     &'       W1,W2,W3 bitwise dump with 1,2,3 field width',
     &'       W0       also display bit values = 0',
     &'       C        display only contents of data (no doc,no seq#',
     &'                useful if data should read by a program'
      WRITE(*,'(10(A/))')
     &' FIRST   first and last data word to dump',
     &' LAST    if 0 all data are dumped',
     &' UNIT    = 6 => standard output (screen)',
     &'         otherwise on file FORxxx, xxx = UNIT',
     &'         if UNIT changes a previous file is closed'
 
      WRITE(*,'(10(A/))')
     &' DZDTMP-Opt: char-option for Doc-template',
     &'             T do it for tree below selected bank ',
     &'             L follow next links ',
     &'             A generate Author tag ',
     &'             V generate Version tag ',
     &'             D generate Division tag ',
     &'             A generate Store tag '
 
      WRITE(*,'(10(A/))')
     &' --------------------',
     &' Modify data in banks',
     &' --------------------',
     &' You may modify data in the currently selected bank',
     &' REAL values should contain a . (dot), they are read',
     &' with F12.5, INTEGERs are read as hex,if they are preceeded',
     &' by a $. System words may be modified by selecting FIRST',
     &' and LAST word zero or negative, its however your own risk!!.'
      RETURN
      END
***********************************************************************
