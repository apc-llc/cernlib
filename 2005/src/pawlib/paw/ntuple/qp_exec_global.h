/********************************************************************/
/********************************************************************/
/*  Global variables for the routines of "qp_execute.c" and 
 *  "qp_exec_event.c"
 *
 *  Initially, this two files were together, but the resulting file
 *  was too big to be optimized under Solaris
 *
 ********************************************************************/
/********************************************************************/







Extern bool	exe_verbose;		/* print frames and event numbers */
Extern bool	timing;			/* print execution times */


Extern int	math_error;

/* cut call stack */

Extern Segment	cut_call_stack[ MAX_CUTS ];
Extern int	cut_index_stack[ MAX_CUTS ];
Extern UInt32	cut_opc_stack[ MAX_CUTS ];
Extern int	cut_call_sp;


/* cut cache */

Extern Segment	cut_segment[ MAX_CUTS ];
Extern DataType	cut_type[ MAX_CUTS ];
Extern bool	cut_has_shape[ MAX_CUTS ];

Extern CutGraf 	*cut_data[ MAX_CUTS ];

Extern bool	cut_valid[ MAX_CUTS ];
Extern int	cut_size[ MAX_CUTS ];
Extern int	cut_bsize[ MAX_CUTS ];
Extern void	*cut_value[ MAX_CUTS ];


/* CWN index block */

typedef struct _ref_cwn_ {
	Int32		*p;		/* pointer to the data */
	Int32		*p_cs;		/* pointer to the comis common block */
					/* when data is in the cache */
	Float32		*p_rwn;		/* pointer to the cache when RWN fields are */
					/* loaded into the cache */
	unsigned int	step;		/* step size through buffer (in Int32's) */
	int		isize;		/* number of bytes in a string */
	bool		varDim;		/* indicates variable lenght array */
	int		var_base;	/* number of elements in non var dimensions */
	int		var_index;	/* index of the dimensioning var */
	String		name;		/* keep reference for reporting */
} RefCWN;

Extern int	CWNCount;
Extern RefCWN	*CWNBlock;


/* Mask data structures */

Extern Mask	*MaskList[MAX_MASKS];
Extern int	nmasks;

           
/* some statistics */

Extern long	stat_opc;
Extern long	stat_event;
Extern long	stat_selected;

