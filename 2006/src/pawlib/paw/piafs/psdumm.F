
* $Id: psdumm.F,v 1.4 1996/09/10 13:37:40 couet Exp $
*
* $Log: psdumm.F,v $
* Revision 1.4  1996/09/10 13:37:40  couet
* - NT/MERGE is obsolete (use HMERGE instead)
*
* Revision 1.3  1996/09/09 14:52:26  couet
* - old qp code removed
*
* Revision 1.2  1996/04/24 09:36:44  dinofm
* HPLFR3 and IGCOLM dummy entries have been removed
*
* Revision 1.1.1.1  1996/03/01 11:39:27  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  2.05/15 22/07/94  15.34.34  by  Fons Rademakers
*-- Author :    Alfred Nathaniel   01/05/93
      SUBROUTINE PSDUMM
*
* Server dummy routines
*
      CHARACTER NAME*6

      RETURN

*
* Unused definition routines
*
      ENTRY FUNDEF
      RETURN

      ENTRY GRADEF
      RETURN

      ENTRY PICDEF
      RETURN

      ENTRY OBSDEF
      RETURN

      ENTRY NETDEF
      RETURN

      ENTRY PVEDEF
      RETURN

*
* Unused /HISTOGRAM action routines
*
      ENTRY PAFITH
      NAME='PAFITH'
      GOTO 1

      ENTRY PAHBOK
      NAME='PAHBOK'
      GOTO 1

      ENTRY PAHIO
      NAME='PAHIO'
      GOTO 1

      ENTRY PAHOPE
      NAME='PAHOPE'
      GOTO 1

      ENTRY PAHFIT
      NAME='PAHFIT'
      GOTO 1

      ENTRY PAHVEC
      NAME='PAHVEC'
      GOTO 1

      ENTRY PAHSET
      NAME='PAHSET'
      GOTO 1

*
* Unused /NTUPLE action routines
*
      ENTRY PAWUWF
      NAME='PAWUWF'
      GOTO 1

      ENTRY PALINT
      NAME='PALINT'
      GOTO 1

*
* Unused /FORTRAN action routines
*
      ENTRY PASIGM
      NAME='PASIGM'
      GOTO 1

*
* Unused /ZEBRA action routines
*
      ENTRY PAZFZ
      NAME='PAZFZ'
      GOTO 1

      ENTRY PAZDZ
      NAME='PAZDZ'
      GOTO 1

*
* Unused HPLOT routines
*
      ENTRY HPLABL
      NAME='HPLABL'
      GOTO 1

      ENTRY HPLAX
      NAME='HPLAX'
      GOTO 1

      ENTRY HPLAX2
      NAME='HPLAX2'
      GOTO 1

      ENTRY HPLCHA
      NAME='HPLCHA'
      GOTO 1

      ENTRY HPLDAT
      NAME='HPLDAT'
      GOTO 1

      ENTRY HPLEGO
      NAME='HPLEGO'
      GOTO 1

      ENTRY HPLERR
      NAME='HPLERR'
      GOTO 1

      ENTRY HPLFIL
      NAME='HPLFIL'
      GOTO 1

      ENTRY HPLFRA
      NAME='HPLFRA'
      GOTO 1

      ENTRY HPLGIV
      NAME='HPLGIV'
      GOTO 1

      ENTRY HPLINT
      NAME='HPLINT'
      GOTO 1

      ENTRY HPLNEW
      NAME='HPLNEW'
      GOTO 1

      ENTRY HPLNT
      NAME='HPLNT'
      GOTO 1

      ENTRY HPLOC
      NAME='HPLOC'
      GOTO 1

      ENTRY HPLSET
      NAME='HPLSET'
      GOTO 1

      ENTRY HPLSOF
      NAME='HPLSOF'
      GOTO 1

      ENTRY HPLSUR
      NAME='HPLSUR'
      GOTO 1

      ENTRY HPLSYM
      NAME='HPLSYM'
      GOTO 1

      ENTRY HPLTAB
      NAME='HPLTAB'
      GOTO 1

      ENTRY HPLTOC
      NAME='HPLTOC'
      GOTO 1

      ENTRY HPLWIR
      NAME='HPLWIR'
      GOTO 1

      ENTRY HPLZOM
      NAME='HPLZOM'
      GOTO 1

      ENTRY HPLGZO
      NAME='HPLGZO'
      GOTO 1

      ENTRY HPLONT
      NAME='HPLONT'
      GOTO 1

      ENTRY HPLATT
      NAME='HPLATT'
      GOTO 1

      ENTRY HPLZON
      NAME='HPLZON'
      GOTO 1

*
* Unused HIGZ routines
*
      ENTRY IATR3
      NAME='IATR3'
      GOTO 1

      ENTRY ICLRWK
      NAME='ICLRWK'
      GOTO 1

      ENTRY IFA
      NAME='IFA'
      GOTO 1

      ENTRY IGARC
      NAME='IGARC'
      GOTO 1

      ENTRY IGAXIS
      NAME='IGAXIS'
      GOTO 1

      ENTRY IGBOX
      NAME='IGBOX'
      GOTO 1

      ENTRY IGEND
      NAME='IGEND'
      GOTO 1

      ENTRY IGERR
      NAME='IGERR'
      GOTO 1

      ENTRY IGHIST
      NAME='IGHIST'
      GOTO 1

      ENTRY IGINIT
      NAME='IGINIT'
      GOTO 1

      ENTRY IGPHST
      NAME='IGPHST'
      GOTO 1

      ENTRY IGHTOR
      NAME='IGHTOR'
      GOTO 1

      ENTRY IGLBL
      NAME='IGLBL'
      GOTO 1

      ENTRY IGLEV
      NAME='IGLEV'
      GOTO 1

      ENTRY IGMESS
      NAME='IGMESS'
      GOTO 1

      ENTRY IGMETA
      NAME='IGMETA'
      GOTO 1

      ENTRY IGPARF
      NAME='IGPARF'
      GOTO 1

      ENTRY IGPAVE
      NAME='IGPAVE'
      GOTO 1

      ENTRY IGPIE
      NAME='IGPIE'
      GOTO 1

      ENTRY IGRAPH
      NAME='IGRAPH'
      GOTO 1

      ENTRY IGRNG
      NAME='IGRNG'
      GOTO 1

      ENTRY IGSA
      NAME='IGSA'
      GOTO 1

      ENTRY IGTCCH
      NAME='IGTCCH'
      GOTO 1

      ENTRY IGTERM
      NAME='IGTERM'
      GOTO 1

      ENTRY IGTEXT
      NAME='IGTEXT'
      GOTO 1

      ENTRY IGWKTY
      NAME='IGWKTY'
      GOTO 1

      ENTRY IGZSET
      NAME='IGZSET'
      GOTO 1

      ENTRY IHCSPE
      NAME='IHCSPE'
      GOTO 1

      ENTRY IHISO
      NAME='IHISO'
      GOTO 1

      ENTRY IHLIGH
      NAME='IHLIGH'
      GOTO 1

      ENTRY IHPFF
      NAME='IHPFF'
      GOTO 1

      ENTRY IHPROP
      NAME='IHPROP'
      GOTO 1

      ENTRY IHSVIE
      NAME='IHSVIE'
      GOTO 1

      ENTRY IHWTON
      NAME='IHWTON'
      GOTO 1

      ENTRY IRQLC
      NAME='IRQLC'
      GOTO 1

      ENTRY IRQST
      NAME='IRQST'
      GOTO 1

      ENTRY ISCR
      NAME='ISCR'
      GOTO 1

      ENTRY ISCHH
      NAME='ISCHH'
      GOTO 1

      ENTRY ISCLIP
      NAME='ISCLIP'
      GOTO 1

      ENTRY ISELNT
      NAME='ISELNT'
      GOTO 1

      ENTRY ISFAIS
      NAME='ISFAIS'
      GOTO 1

      ENTRY ISFASI
      NAME='ISFASI'
      GOTO 1

      ENTRY ISFACI
      NAME='ISFACI'
      GOTO 1

      ENTRY ISLN
      NAME='ISLN'
      GOTO 1

      ENTRY ISMK
      NAME='ISMK'
      GOTO 1

      ENTRY ISVP
      NAME='ISVP'
      GOTO 1

      ENTRY ISWN
      NAME='ISWN'
      GOTO 1

      ENTRY ITX
      NAME='ITX'
      GOTO 1

      ENTRY IZFILE
      NAME='IZFILE'
      GOTO 1

      ENTRY IZITOC
      NAME='IZITOC'
      GOTO 1

      ENTRY IZRTOC
      NAME='IZRTOC'
      GOTO 1

      ENTRY IGQWK
      NAME='IGQWK'
      GOTO 1

      ENTRY IGDWK
      NAME='IGDWK'
      GOTO 1

      ENTRY IPM3
      NAME='IPM3'
      GOTO 1

      ENTRY ISMKSC
      NAME='ISMKSC'
      GOTO 1

      ENTRY IGONT
      NAME='IGONT'
      GOTO 1

      ENTRY IGSTR
*-- dummy routine also in HIGZ so don't print anything
      RETURN

 1    CONTINUE
      PRINT *,' Server dummy '//NAME//' called!!!'
      END


