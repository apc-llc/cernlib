/*
 *  hcbook.h  --
 *	Map the /PAWC/ and /HCBOOK/ commons
 *
 *  Original:  3-Apr-1995 12:02
 *
 *  Author:   Maarten Ballintijn <Maarten.Ballintijn@cern.ch>
 *
 *  $Id: c_hcbook.h,v 1.5 1999/06/28 15:08:21 couet Exp $
 *
 *  $Log: c_hcbook.h,v $
 *  Revision 1.5  1999/06/28 15:08:21  couet
 *  - use now cfortran.h in $CVSCOSRC
 *
 *  Revision 1.4  1996/04/23 18:37:42  maartenb
 *  - Add RCS keywords
 *
 *
 */

#ifndef CERN_HCBOOK
#define CERN_HCBOOK

#include	<cfortran/cfortran.h>


typedef struct {
	int	npaw;
	int	ixpawc;
	int	ihdiv;
	int	ixhigz;
	int	ixku;
	float	fenc[5];
	union	{
			int	_lmain;
			int	_lq[1];
		} lq_base;
	int	dummy_filler[7];
	union	{
			float	_q[1];
			int	_iq[1];
		} iq_base;
	float	hcv[9981];
} pawc_def;

#define	lmain	lq_base._lmain
#define	lq_eqv	lq_base._lq

#define	q_eqv	iq_base._q
#define	iq_eqv	iq_base._iq

#define PAWC COMMON_BLOCK(PAWC,pawc)
COMMON_BLOCK_DEF(pawc_def,PAWC);


typedef struct {
	int	hversn;
	int	ihwork;
	int	lhbook;
	int	lhplot;
	int	lgtit;
	int	lhwork;
	int	lcdir;
	int	lsdir;
	int	lids;
	int	ltab;
	int	lcid;
	int	lcont;
	int	lscat;
	int	lprox;
	int	lproy;
	int	lslix;
	int	lsliy;
	int	lbanx;
	int	lbany;
	int	lprx;
	int	lpry;
	int	lfix;
	int	llid;
	int	lr1;
	int	lr2;
	int	lname;
	int	lchar;
	int	lint;
	int	lreal;
	int	lblok;
	int	llblk;
	int	lbufm;
	int	lbuf;
	int	ltmpm;
	int	ltmp;
	int	ltmp1;
	int	lhplip;
	int	lhdum[9];
	int	lhfit;
	int	lfunc;
	int	lhfco;
	int	lhfna;
	int	lcidn;
} hcbook_def;

#define HCBOOK COMMON_BLOCK(HCBOOK,hcbook)
COMMON_BLOCK_DEF(hcbook_def,HCBOOK);

#include	"c_hck.h"

#endif	/*	CERN_HCBOOK	*/
