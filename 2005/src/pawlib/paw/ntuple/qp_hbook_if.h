/*
 *  qp_hbook_if.h  --
 *	Interface of convenience routines for hbook.
 *
 *  Original:  8-May-1995 22:54
 *
 *  Author:   Maarten Ballintijn <Maarten.Ballintijn@cern.ch>
 *
 *  $Id: qp_hbook_if.h,v 1.9 1996/09/12 09:28:57 couet Exp $
 *
 *  $Log: qp_hbook_if.h,v $
 *  Revision 1.9  1996/09/12 09:28:57  couet
 *  - after NTUPLE commands using a full path name ie:
 *
 *    NT/PLOT //lun1/10.x
 *
 *    The current working directory was changed to //lun1
 *
 *  Revision 1.8  1996/04/23 18:38:44  maartenb
 *  - Add RCS keywords
 *
 *
 */

#ifndef CERN_QP_HBOOK_IF
#define CERN_QP_HBOOK_IF

#include	"cern_types.h"
#include	"hbook_defs.h"
#include	"smap.h"


extern char	h_rwn_chtitl[128];
extern int	h_rwn_nvar;
extern char	h_rwn_tags[MAX_RWN_COLS][9];
extern float	h_rwn_rlow[MAX_RWN_COLS];
extern float	h_rwn_rhigh[MAX_RWN_COLS];


int
h_load_nt(
	char *		id_string,
	char **		id_path,
	int *		idp
	);

void
h_reset_dir(
	   );

int
h_load_histo(
	char *		id_string,
	int *		idp,
	int *		id_dim
	);

void
h_hnocol(
	int		id,
	int *		colp
	);

void
h_hnoent(
	int		idn,
	bool		use_chain,
	int *		ep
	);

bool
h_flag_1d(
	int		id
	);

bool
h_flag_2d(
	int		id
	);

bool
h_flag_profile(
	int		id
	);


int
h_rwn_getInfo(
	int		idn
);


int
h_rwn_getIndex(
	int		idn,
	char *		const name
);


SMap
h_get_labels(
	int		id,
	char		*chopt
);


void
h_hbook1_labels(
	int		idh,
	char *		title,
	SMap		label_list
);


void
h_hbook2_labels(
	int		idh,
	char *		title,
	SMap		labelx_list,
	SMap		labely_list,
	int		nbin[],
	float		min[],
	float		max[]
);

#endif	/*	CERN_QP_HBOOK_IF	*/
