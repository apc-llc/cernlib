/*
 *  c_decl.c  --
 *	Commonblock should be declared in one file.
 *
 *  Original: 12-Oct-1995 15:53
 *
 *  Author:   Maarten Ballintijn <Maarten.Ballintijn@cern.ch>
 *
 *  $Id: c_decl.c,v 1.9 1999/06/28 15:08:19 couet Exp $
 *
 *  $Log: c_decl.c,v $
 *  Revision 1.9  1999/06/28 15:08:19  couet
 *  - use now cfortran.h in $CVSCOSRC
 *
 *  Revision 1.8  1996/04/23 18:37:41  maartenb
 *  - Add RCS keywords
 *
 *
 */

/* This is an enormous hack !! should go into cfortran.h */

#include	<cfortran/cfortran.h>

#ifndef __sgi
#define extern
#endif

#include	"c_hcbits.h"
#include	"c_hcbook.h"
#include	"c_hcdire.h"
#include	"c_hcflag.h"
#include	"c_hck.h"
#include	"c_hcntpaw.h"
#include	"c_hcpiaf.h"
#include	"c_hcunit.h"
#include	"c_pawchn.h"
#include	"c_pawcom.h"
#include	"c_pawidn.h"
#include	"c_pcmask.h"
#include	"c_pcmatw.h"
#include	"c_pcaddr.h"
#include	"c_pntold.h"
#include	"c_quest.h"


void
qp_pull_c_decl_obj( void )
{
}
